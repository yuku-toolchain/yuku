import type { AliasMap, AliasName } from "../ast/aliases";
import { WalkContext } from "../ast/context";
import { CHILD_KEYS } from "../ast/generated";
import { findAll } from "../ast/utils";
import { _walk, _walkAsync, type Visitors } from "../ast/walk";
import { BindingFlags, type Decoded, type Semantic } from "../core/decode";
import { run, sourceText } from "../core/wasm";
import type { ParseOptions } from "../parser";
import type {
  Comment,
  Diagnostic,
  Identifier,
  JSXIdentifier,
  Node,
  NodeOfType,
  NodeType,
  Program,
  TokenList,
} from "../types";
import type { Analyzer } from "./analyzer";

/** Kinds and modifiers of a {@link Binding}, tested with `has` and `hasAll`. */
export interface BindingFlagMap {
  /** `var`, a parameter, or a catch variable. */
  readonly FunctionScopedVariable: number;
  /** `let`, `const`, `using`, or `await using`. */
  readonly BlockScopedVariable: number;
  readonly Function: number;
  readonly Class: number;
  readonly RegularEnum: number;
  readonly ConstEnum: number;
  /** A namespace with runtime content. */
  readonly ValueModule: number;
  readonly Interface: number;
  readonly TypeAlias: number;
  /** `<T>`, `infer T`, or a mapped type key. */
  readonly TypeParameter: number;
  /** A namespace of any kind. */
  readonly NamespaceModule: number;
  readonly ValueImport: number;
  /** `import type` or `import { type x }`. */
  readonly TypeImport: number;
  /** `const`, `using`, or `await using`. */
  readonly Const: number;
  /** `declare`. */
  readonly Ambient: number;
  readonly Parameter: number;
  readonly CatchVariable: number;
  /** Declared by `export`, or implicitly in ambient code with no export statement. */
  readonly Exported: number;
  /** Declared by `export default`. */
  readonly Default: number;
  readonly EnumMember: number;
  /** Any variable, parameters and catch variables included. */
  readonly Variable: number;
  /** Any import, value or type. */
  readonly Import: number;
  /** Visible at runtime. */
  readonly ValueSpace: number;
  /** Usable as a type. */
  readonly TypeSpace: number;
  /** What a dotted type name can start from. */
  readonly NamespaceSpace: number;
}

/**
 * The declaration space a name resolves in, where bindings outside it do not shadow. `"namespace"`
 * is `ns` in `ns.T`, `"typeof"` is `x` in `typeof x`, and `"any"` is `x` in `export { x }`.
 */
export type Space = "value" | "type" | "namespace" | "typeof" | "any";

export type ScopeKind =
  | "global"
  | "module"
  | "function"
  | "functionBody"
  | "block"
  | "class"
  | "staticBlock"
  | "expressionName"
  | "tsModule";

/**
 * How a module is imported. `"named"` includes default imports, `"sideEffect"` includes
 * `export {} from "m"`, and `"augmentation"` is `declare module "m" {}` in a module.
 */
export type ImportKind =
  | "named"
  | "namespace"
  | "sideEffect"
  | "importEquals"
  | "dynamic"
  | "require"
  | "augmentation";

/**
 * How a name is exported. `"reExport"` is `export { x } from "m"`, `"namespace"` is
 * `export * as ns from "m"`, `"equals"` is `export = x`, and `"global"` is `export as namespace N`.
 */
export type ExportKind = "named" | "reExport" | "namespace" | "star" | "equals" | "global";

/** What a script reads from CommonJS and `import.meta`. */
export interface ModuleFlags {
  readonly usesRequire: boolean;
  readonly usesModule: boolean;
  readonly usesExports: boolean;
  readonly usesImportMeta: boolean;
}

/** Where a name is defined. A null binding is a whole module namespace. */
export interface Definition {
  readonly module: Module;
  readonly binding: Binding | null;
  /** The bindings that `declare module` blocks merge into it. */
  readonly augmentations: Binding[];
}

export interface Capture {
  readonly binding: Binding;
  /** Its uses inside the function. */
  readonly references: Reference[];
  readonly isWritten: boolean;
}

export type SemanticWalkHandler<T extends Node = Node> = (
  node: T,
  ctx: SemanticWalkContext<T>,
) => void;

export interface SemanticWalkHooks<T extends Node = Node> {
  enter?: SemanticWalkHandler<T>;
  leave?: SemanticWalkHandler<T>;
}

/** {@link Visitors} whose handlers receive a {@link SemanticWalkContext}. */
export type SemanticVisitors = {
  [K in NodeType]?: SemanticWalkHandler<NodeOfType<K>> | SemanticWalkHooks<NodeOfType<K>>;
} & {
  [A in AliasName]?: SemanticWalkHandler<AliasMap[A]> | SemanticWalkHooks<AliasMap[A]>;
} & {
  enter?: SemanticWalkHandler;
  leave?: SemanticWalkHandler;
};

export type AsyncSemanticWalkHandler<T extends Node = Node> = (
  node: T,
  ctx: SemanticWalkContext<T>,
) => void | Promise<void>;

export interface AsyncSemanticWalkHooks<T extends Node = Node> {
  enter?: AsyncSemanticWalkHandler<T>;
  leave?: AsyncSemanticWalkHandler<T>;
}

export type AsyncSemanticVisitors = {
  [K in NodeType]?:
    | AsyncSemanticWalkHandler<NodeOfType<K>>
    | AsyncSemanticWalkHooks<NodeOfType<K>>;
} & {
  [A in AliasName]?: AsyncSemanticWalkHandler<AliasMap[A]> | AsyncSemanticWalkHooks<AliasMap[A]>;
} & {
  enter?: AsyncSemanticWalkHandler;
  leave?: AsyncSemanticWalkHandler;
};

type Row<T> = new (module: Module, semantic: Semantic, id: number) => T;
type Mutable<T> = { -readonly [K in keyof T]: T[K] };

export class Scope {
  readonly module: Module;
  readonly id: number;
  private semantic: Semantic;

  /** @internal */
  constructor(module: Module, semantic: Semantic, id: number) {
    this.module = module;
    this.id = id;
    this.semantic = semantic;
  }
  get kind(): ScopeKind {
    return this.semantic.scope.kind(this.id);
  }
  get strict(): boolean {
    return this.semantic.scope.strict(this.id);
  }
  /** The node that creates it. */
  get node(): Node {
    return this.semantic.scope.node(this.id);
  }
  get parent(): Scope | null {
    const parent = this.semantic.scope.parentId(this.id);
    return parent === null ? null : this.module.scopes[parent]!;
  }
  /** Where a `var` declared in it lands. */
  get hoistTarget(): Scope {
    return this.module.scopes[this.semantic.scope.hoistTargetId(this.id)]!;
  }
  /** The bindings it declares. */
  get bindings(): Binding[] {
    return this.module._scopeBindings(this.id);
  }
  /** A binding it declares by name. */
  find(name: string): Binding | null {
    for (const binding of this.bindings) if (binding.name === name) return binding;
    return null;
  }
  /** Whether `other` is this scope or inside it. */
  contains(other: Scope): boolean {
    for (let scope: Scope | null = other; scope !== null; scope = scope.parent) {
      if (scope === this) return true;
    }
    return false;
  }
  /** This scope, then each parent. */
  *ancestors(): IterableIterator<Scope> {
    for (let scope: Scope | null = this; scope !== null; scope = scope.parent) yield scope;
  }
}

/** A declared name. Merged declarations, such as overloads, share one. */
export class Binding {
  readonly module: Module;
  readonly id: number;
  private semantic: Semantic;

  /** @internal */
  constructor(module: Module, semantic: Semantic, id: number) {
    this.module = module;
    this.id = id;
    this.semantic = semantic;
  }
  get name(): string {
    return this.semantic.symbol.name(this.id);
  }
  /** A {@link BindingFlags} bitset. */
  get flags(): number {
    return this.semantic.symbol.flags(this.id);
  }
  get scope(): Scope {
    return this.module.scopes[this.semantic.symbol.scopeId(this.id)]!;
  }
  /** Each declaration's name node. */
  get declarations(): Node[] {
    const { symbol } = this.semantic;
    const out = new Array<Node>(symbol.declCount(this.id));
    for (let i = 0; i < out.length; i++) out[i] = symbol.declNode(this.id, i);
    return out;
  }
  /** Its uses in this module. */
  get references(): Reference[] {
    return this.module._bindingReferences(this.id);
  }
  /** Whether any flag in `mask` is set. */
  has(mask: number): boolean {
    return (this.flags & mask) !== 0;
  }
  /** Whether every flag in `mask` is set. */
  hasAll(mask: number): boolean {
    return (this.flags & mask) === mask;
  }
  /** Whether a name resolving in `space` can bind to it. */
  visibleIn(space: Space): boolean {
    // an import aliases a binding of a space one file cannot know
    if (this.has(BindingFlags.Import)) return true;
    switch (space) {
      case "value":
      case "typeof":
        return this.has(BindingFlags.ValueSpace);
      case "type":
        return this.has(BindingFlags.TypeSpace);
      case "namespace":
        return this.has(BindingFlags.NamespaceSpace);
      case "any":
        return true;
    }
    throw new TypeError('`space` must be "value", "type", "namespace", "typeof", or "any"');
  }
  /** Where it is defined, following imports and module augmentations. Links. */
  definition(): Definition | null {
    return this.module.analyzer._definitionOf(this);
  }
  /** Its uses across the project, through every import of it. Links. */
  findReferences(): Reference[] {
    return this.module.analyzer._referencesOf(this);
  }
}

/** One use of a name. */
export class Reference {
  readonly module: Module;
  readonly id: number;
  private semantic: Semantic;

  /** @internal */
  constructor(module: Module, semantic: Semantic, id: number) {
    this.module = module;
    this.id = id;
    this.semantic = semantic;
  }
  get name(): string {
    return this.semantic.reference.name(this.id);
  }
  get scope(): Scope {
    return this.module.scopes[this.semantic.reference.scopeId(this.id)]!;
  }
  get node(): Identifier | JSXIdentifier {
    return this.semantic.reference.node(this.id);
  }
  get space(): Space {
    return this.semantic.reference.space(this.id);
  }
  /** Whether the use is erased with the types. */
  get inTypePosition(): boolean {
    return this.semantic.reference.inTypePosition(this.id);
  }
  /** Whether it assigns to the name. */
  get isWrite(): boolean {
    return this.semantic.reference.isWrite(this.id);
  }
  /** Null for a global or undeclared name. */
  get binding(): Binding | null {
    const binding = this.semantic.reference.symbolId(this.id);
    return binding === null ? null : this.module.bindings[binding]!;
  }
}

export class Import {
  readonly module: Module;
  readonly id: number;
  /** @internal */
  _resolved: Module | null = null;
  private semantic: Semantic;

  /** @internal */
  constructor(module: Module, semantic: Semantic, id: number) {
    this.module = module;
    this.id = id;
    this.semantic = semantic;
  }
  get kind(): ImportKind {
    return this.semantic.import.kind(this.id);
  }
  /** The imported name of a `"named"` record, `"default"` for a default import. */
  get name(): string | null {
    return this.kind === "named" ? this.semantic.import.name(this.id) : null;
  }
  /** The binding it declares. */
  get local(): Binding | null {
    const binding = this.semantic.import.symbolId(this.id);
    return binding === null ? null : this.module.bindings[binding]!;
  }
  /** Whether it binds a whole module, as `"namespace"` and `"importEquals"` do. */
  get isNamespace(): boolean {
    const kind = this.kind;
    return kind === "namespace" || kind === "importEquals";
  }
  get specifier(): string {
    return this.semantic.import.specifier(this.id);
  }
  get typeOnly(): boolean {
    return this.semantic.import.typeOnly(this.id);
  }
  get phase(): "source" | "defer" | null {
    return this.semantic.import.phase(this.id);
  }
  /** The specifier, the declaration, or the call. */
  get node(): Node {
    return this.semantic.import.node(this.id);
  }
  /** @internal */
  get _scope(): Scope {
    return this.module.scopes[this.semantic.import.scopeId(this.id)!]!;
  }
  /** Null outside the project. Links. */
  get resolvedModule(): Module | null {
    this.module.analyzer._link(this.module);
    return this._resolved;
  }
}

export class Export {
  readonly module: Module;
  readonly id: number;
  /** @internal */
  _resolved: Module | null = null;
  private semantic: Semantic;

  /** @internal */
  constructor(module: Module, semantic: Semantic, id: number) {
    this.module = module;
    this.id = id;
    this.semantic = semantic;
  }
  get kind(): ExportKind {
    return this.semantic.export.kind(this.id);
  }
  /** The exported name, null for `"star"`, `"equals"`, and `"global"`. */
  get name(): string | null {
    const kind = this.kind;
    return kind === "named" || kind === "reExport" || kind === "namespace"
      ? this.semantic.export.name(this.id)
      : null;
  }
  /** The name of `export as namespace N`. */
  get globalName(): string | null {
    return this.kind === "global" ? this.semantic.export.name(this.id) : null;
  }
  /** The binding it exports from this module. */
  get local(): Binding | null {
    const binding = this.semantic.export.symbolId(this.id);
    return binding === null ? null : this.module.bindings[binding]!;
  }
  /** The module it re-exports from. */
  get specifier(): string | null {
    const kind = this.kind;
    return kind === "reExport" || kind === "namespace" || kind === "star"
      ? this.semantic.export.specifier(this.id)
      : null;
  }
  /** The name a `"reExport"` takes from its module. */
  get fromName(): string | null {
    return this.kind === "reExport" ? this.semantic.export.fromName(this.id) : null;
  }
  get typeOnly(): boolean {
    return this.semantic.export.typeOnly(this.id);
  }
  /** The specifier, the declared name, or the statement. */
  get node(): Node {
    return this.semantic.export.node(this.id);
  }
  /** The module it re-exports from, null outside the project. Links. */
  get resolvedModule(): Module | null {
    this.module.analyzer._link(this.module);
    return this._resolved;
  }
}

/** A {@link WalkContext} with the semantic model at the current node. */
export class SemanticWalkContext<T extends Node = Node> extends WalkContext<T> {
  readonly module: Module;

  /** @internal */
  constructor(module: Module) {
    super();
    this.module = module;
  }
  get scope(): Scope {
    return this.module.scopeOf(this._node!);
  }
  get binding(): Binding | null {
    return this.module.bindingOf(this._node!);
  }
  get reference(): Reference | null {
    return this.module.referenceOf(this._node!);
  }
}

/** One analyzed file. Every query is local except those that link. */
export class Module {
  readonly analyzer: Analyzer;
  readonly path: string;
  readonly source: string;
  /** @internal */
  _deps: Module[] = [];
  /** @internal */
  _dependents: Module[] = [];
  private decoded: Decoded;
  private semantic: Semantic;
  private scopeRows: Scope[] | null = null;
  private bindingRows: Binding[] | null = null;
  private referenceRows: Reference[] | null = null;
  private unresolved: Reference[] | null = null;
  private importRows: Import[] | null = null;
  private exportRows: Export[] | null = null;
  private bindingsByScope: Binding[][] | null = null;
  private referencesByBinding: Reference[][] | null = null;
  private declarations: Map<number, number> | null = null;
  private referenceNodes: Map<number, number> | null = null;
  private exportMap: Map<string, Export> | null = null;
  private starExports: Export[] | null = null;
  private importByBinding: Map<Binding, Import> | null = null;

  /** @internal */
  constructor(
    analyzer: Analyzer,
    path: string,
    source: string | Uint8Array,
    options: ParseOptions,
  ) {
    this.analyzer = analyzer;
    this.path = path;
    this.source = sourceText(source);
    this.decoded = run("analyze", this.source, { ...options, path });
    this.semantic = this.decoded.semantic;
  }

  /** False once its path is set again or deleted. */
  get isCurrent(): boolean {
    return this.analyzer.module(this.path) === this;
  }
  /** Its nodes are the objects every query returns. */
  get ast(): Program {
    return this.decoded.program;
  }
  get comments(): Comment[] {
    return this.decoded.comments;
  }
  /** With {@link ParseOptions.tokens}. */
  get tokens(): TokenList | undefined {
    return this.decoded.tokens;
  }
  get diagnostics(): Diagnostic[] {
    return this.decoded.diagnostics;
  }

  /** Every scope, indexed by id. The first is the global scope. */
  get scopes(): Scope[] {
    return (this.scopeRows ??= this.rows(Scope, this.semantic.scope.count));
  }
  /** Where top-level code runs, the module scope or the global scope of a script. */
  get rootScope(): Scope {
    const scopes = this.scopes;
    return scopes.length > 1 && scopes[1]!.kind === "module" ? scopes[1]! : scopes[0]!;
  }
  /** Every binding, indexed by id. */
  get bindings(): Binding[] {
    return (this.bindingRows ??= this.rows(Binding, this.semantic.symbol.count));
  }
  /** Every name in use, in source order. */
  get references(): Reference[] {
    return (this.referenceRows ??= this.rows(Reference, this.semantic.reference.count));
  }
  /** References with no binding, such as globals. */
  get unresolvedReferences(): Reference[] {
    return (this.unresolved ??= this.references.filter((reference) => reference.binding === null));
  }
  get imports(): Import[] {
    return (this.importRows ??= this.rows(Import, this.semantic.import.count));
  }
  get exports(): Export[] {
    return (this.exportRows ??= this.rows(Export, this.semantic.export.count));
  }
  get moduleFlags(): ModuleFlags {
    return this.semantic.moduleFlags;
  }
  /** The modules it imports from. Links. */
  get dependencies(): Module[] {
    this.analyzer._link(this);
    return this._deps;
  }
  /** The modules that import from it. Links. */
  get dependents(): Module[] {
    this.analyzer._link(this);
    return this._dependents;
  }

  /** The binding a node declares or refers to. */
  bindingOf(node: Node): Binding | null {
    const index = this.decoded.indexOf(node);
    if (index === undefined) return null;
    const declared = this.declarationMap().get(index);
    if (declared !== undefined) return this.bindings[declared]!;
    return this.referenceOf(node)?.binding ?? null;
  }

  referenceOf(node: Node): Reference | null {
    const index = this.decoded.indexOf(node);
    if (index === undefined) return null;
    const reference = this.referenceMap().get(index);
    return reference === undefined ? null : this.references[reference]!;
  }

  /** The innermost scope around a node, the root scope for a node added later. */
  scopeOf(node: Node): Scope {
    const index = this.decoded.indexOf(node);
    if (index === undefined) return this.rootScope;
    return this.scopes[this.semantic.nodeScope(index)]!;
  }

  parentOf(node: Node): Node | null {
    // the hashbang is synthesized in JavaScript
    if (node?.type === "Hashbang") return node === this.ast.hashbang ? this.ast : null;
    const index = this.decoded.indexOf(node);
    if (index === undefined) return null;
    const parent = this.decoded.parentIndex(index);
    return parent < 0 ? null : this.decoded.nodeOf(parent);
  }

  /** The node, then each parent up to the root. */
  *ancestors(node: Node): IterableIterator<Node> {
    for (let current: Node | null = node; current !== null; current = this.parentOf(current)) {
      yield current;
    }
  }

  /** The innermost node containing a UTF-16 offset. */
  nodeAt(offset: number): Node | null {
    let node: Node = this.ast;
    if (!spans(node, offset)) return null;
    for (let child = childAt(node, offset); child !== null; child = childAt(node, offset)) {
      node = child;
    }
    return node;
  }

  /** Resolves a name as code at `from` would. */
  lookup(
    name: string,
    { from = this.rootScope, space = "value" }: { from?: Scope; space?: Space } = {},
  ): Binding | null {
    // mirrors reference resolution in the binder
    const argumentsBarrier = name === "arguments" && (space === "value" || space === "typeof");
    for (let scope: Scope | null = from; scope !== null; scope = scope.parent) {
      const found = scope.find(name);
      if (found !== null && found.visibleIn(space)) return found;
      const shared = this.sharedBinding(scope, name, space);
      if (shared !== null) return shared;
      if (argumentsBarrier && isArgumentsScope(scope)) return null;
    }
    return null;
  }

  /** The outer bindings a function uses. Throws for a node that is not a function. */
  capturesOf(fn: Node): Capture[] {
    const index = this.decoded.indexOf(fn);
    if (index === undefined) {
      throw new TypeError("capturesOf: node does not belong to this module's AST");
    }
    const { scope, symbol, reference } = this.semantic;
    const functionScope = this.semantic.nodeScope(index);
    if (scope.kind(functionScope) !== "function" || scope.nodeIndex(functionScope) !== index) {
      throw new TypeError("capturesOf: node does not create a function scope");
    }
    const start = this.decoded.startOf(index);
    const end = this.decoded.endOf(index);
    const captures = new Map<number, Mutable<Capture>>();
    for (let i = 0; i < reference.count; i++) {
      const target = reference.symbolId(i);
      if (target === null || reference.inTypePosition(i)) continue;
      if (reference.start(i) < start || reference.end(i) > end) continue;
      let id: number | null = symbol.scopeId(target);
      while (id !== null && id !== functionScope) id = scope.parentId(id);
      if (id !== null) continue;
      let capture = captures.get(target);
      if (capture === undefined) {
        capture = { binding: this.bindings[target]!, references: [], isWritten: false };
        captures.set(target, capture);
      }
      capture.references.push(this.references[i]!);
      if (reference.isWrite(i)) capture.isWritten = true;
    }
    return [...captures.values()];
  }

  /** Every name it exports, through `export *` and module augmentations. Links. */
  exportedNames(): string[] {
    return this.collectExportedNames(new Set());
  }

  /** The binding behind one of its exports. Links. */
  resolveExport(name: string): Definition | null {
    return this.analyzer._resolveExport(this, name);
  }

  /** Walks its AST, or the subtree under `root`, with the semantic context. */
  walk(visitors: SemanticVisitors, root?: Node): void {
    _walk(root ?? this.ast, visitors as Visitors<never>, undefined, new SemanticWalkContext(this));
  }

  walkAsync(visitors: AsyncSemanticVisitors, root?: Node): Promise<void> {
    return _walkAsync(
      root ?? this.ast,
      visitors as Visitors<never>,
      undefined,
      new SemanticWalkContext(this),
    );
  }

  /** Every node of the given types, in source order. */
  findAll<K extends NodeType>(type: K): NodeOfType<K>[];
  findAll<K extends NodeType>(types: readonly K[]): NodeOfType<K>[];
  findAll<K extends NodeType>(types: K | readonly K[]): NodeOfType<K>[] {
    return findAll(this.ast, types);
  }

  /** @internal */
  _scopeBindings(scope: number): Binding[] {
    if (this.bindingsByScope === null) {
      const lists = Array.from({ length: this.semantic.scope.count }, (): Binding[] => []);
      for (const binding of this.bindings) lists[binding.scope.id]!.push(binding);
      this.bindingsByScope = lists;
    }
    return this.bindingsByScope[scope]!;
  }

  /** @internal */
  _bindingReferences(binding: number): Reference[] {
    if (this.referencesByBinding === null) {
      const lists = Array.from({ length: this.semantic.symbol.count }, (): Reference[] => []);
      for (const reference of this.references) {
        const target = this.semantic.reference.symbolId(reference.id);
        if (target !== null) lists[target]!.push(reference);
      }
      this.referencesByBinding = lists;
    }
    return this.referencesByBinding[binding]!;
  }

  /** @internal */
  _exportMap(): Map<string, Export> {
    // the export entry partition of ParseModule, 16.2.1.7.1
    if (this.exportMap === null) {
      const map = new Map<string, Export>();
      const stars: Export[] = [];
      for (const record of this.exports) {
        if (record.kind === "star") stars.push(record);
        else if (record.name !== null && !map.has(record.name)) map.set(record.name, record);
      }
      this.exportMap = map;
      this.starExports = stars;
    }
    return this.exportMap;
  }

  /** @internal */
  _starExports(): Export[] {
    this._exportMap();
    return this.starExports!;
  }

  /** @internal */
  _importOf(binding: Binding): Import | undefined {
    if (this.importByBinding === null) {
      const map = new Map<Binding, Import>();
      for (const record of this.imports) {
        const local = record.local;
        if (local !== null) map.set(local, record);
      }
      this.importByBinding = map;
    }
    return this.importByBinding.get(binding);
  }

  // GetExportedNames, 16.2.1.7.2.1
  private collectExportedNames(seen: Set<Module>): string[] {
    if (seen.has(this)) return [];
    seen.add(this);
    this.analyzer._link(this);
    const names = new Set(this._exportMap().keys());
    for (const name of this.analyzer._addedExports(this)) names.add(name);
    for (const star of this._starExports()) {
      if (star._resolved === null) continue;
      for (const name of star._resolved.collectExportedNames(seen)) {
        if (name !== "default") names.add(name);
      }
    }
    return [...names];
  }

  private sharedBinding(scope: Scope, name: string, space: Space): Binding | null {
    const next = this.semantic.scope.nextBodyId;
    const shared = scope.kind === "tsModule" ? BindingFlags.Exported : BindingFlags.EnumMember;
    for (let body = next(scope.id); body !== null && body !== scope.id; body = next(body)) {
      const found = this.scopes[body]!.find(name);
      if (found !== null && found.has(shared) && found.visibleIn(space)) return found;
    }
    return null;
  }

  private rows<T>(Row: Row<T>, count: number): T[] {
    const out = new Array<T>(count);
    for (let i = 0; i < count; i++) out[i] = new Row(this, this.semantic, i);
    return out;
  }

  private declarationMap(): Map<number, number> {
    if (this.declarations === null) {
      const map = new Map<number, number>();
      const { symbol } = this.semantic;
      for (let id = 0; id < symbol.count; id++) {
        const count = symbol.declCount(id);
        for (let i = 0; i < count; i++) map.set(symbol.declNodeIndex(id, i), id);
      }
      this.declarations = map;
    }
    return this.declarations;
  }

  private referenceMap(): Map<number, number> {
    if (this.referenceNodes === null) {
      const map = new Map<number, number>();
      const { reference } = this.semantic;
      for (let i = 0; i < reference.count; i++) map.set(reference.nodeIndex(i), i);
      this.referenceNodes = map;
    }
    return this.referenceNodes;
  }
}

function isArgumentsScope(scope: Scope): boolean {
  if (scope.kind === "staticBlock") return true;
  return scope.kind === "function" && scope.node.type !== "ArrowFunctionExpression";
}

function spans(node: unknown, offset: number): node is Node {
  if (node === null || typeof node !== "object") return false;
  const { start, end } = node as Node;
  return start <= offset && offset < end;
}

function childAt(node: Node, offset: number): Node | null {
  for (const key of CHILD_KEYS[node.type] ?? []) {
    const value = (node as unknown as Record<string, unknown>)[key];
    if (Array.isArray(value)) {
      for (const child of value) if (spans(child, offset)) return child;
    } else if (spans(value, offset)) {
      return value;
    }
  }
  return null;
}
