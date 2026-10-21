import { BindingFlags, type Semantic } from "../core/decode";
import type { Diagnostic, Identifier, JSXIdentifier, Node } from "../types";
import { rename } from "./rename";
import type { SourceFile } from "./source-file";

/** Kinds and modifiers of a {@link Binding}, tested with `has`. */
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
  readonly file: SourceFile;
  readonly binding: Binding | null;
  /** The bindings that `declare module` blocks merge into it. */
  readonly augmentations: readonly Binding[];
}

export interface Capture {
  readonly binding: Binding;
  /** Its uses inside the function. */
  readonly references: readonly Reference[];
  readonly isWritten: boolean;
}

type Row<T> = new (model: Model, id: number) => T;

export class Scope {
  readonly id: number;
  private model: Model;

  /** @internal */
  constructor(model: Model, id: number) {
    this.model = model;
    this.id = id;
  }
  get file(): SourceFile {
    return this.model.file;
  }
  get kind(): ScopeKind {
    return this.model.semantic.scope.kind(this.id);
  }
  get strict(): boolean {
    return this.model.semantic.scope.strict(this.id);
  }
  /** The node that creates it. */
  get node(): Node {
    return this.model.semantic.scope.node(this.id);
  }
  get parent(): Scope | null {
    const parent = this.model.semantic.scope.parentId(this.id);
    return parent === null ? null : this.model.scopes[parent]!;
  }
  /** Where a `var` declared in it lands. */
  get hoistTarget(): Scope {
    return this.model.scopes[this.model.semantic.scope.hoistTargetId(this.id)]!;
  }
  /** The bindings it declares. */
  get bindings(): readonly Binding[] {
    return this.model.scopeBindings(this.id);
  }
  /** A binding it declares by name. */
  find(name: string): Binding | null {
    return this.bindings.find((binding) => binding.name === name) ?? null;
  }
  /** Resolves a name as code in this scope would. */
  lookup(name: string, space: Space = "value"): Binding | null {
    const argumentsBarrier = name === "arguments" && (space === "value" || space === "typeof");
    for (let scope: Scope | null = this; scope !== null; scope = scope.parent) {
      const found = scope.find(name);
      if (found !== null && found.visibleIn(space)) return found;
      const shared = this.model.sharedBinding(scope, name, space);
      if (shared !== null) return shared;
      if (argumentsBarrier && isArgumentsScope(scope)) return null;
    }
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
  readonly id: number;
  private model: Model;

  /** @internal */
  constructor(model: Model, id: number) {
    this.model = model;
    this.id = id;
  }
  get file(): SourceFile {
    return this.model.file;
  }
  get name(): string {
    return this.model.bindingName(this.id);
  }
  /** A {@link BindingFlags} bitset. */
  get flags(): number {
    return this.model.semantic.symbol.flags(this.id);
  }
  get scope(): Scope {
    return this.model.scopes[this.model.semantic.symbol.scopeId(this.id)]!;
  }
  /** Each declaration's name node. */
  get declarations(): readonly Node[] {
    const { symbol } = this.model.semantic;
    return Array.from({ length: symbol.declCount(this.id) }, (_, i) => symbol.declNode(this.id, i));
  }
  /** Its uses in this file. */
  get references(): readonly Reference[] {
    return this.model.bindingReferences(this.id);
  }
  get import(): Import | null {
    return this.model.importOf(this);
  }
  /** Whether any flag in `mask` is set. */
  has(mask: number): boolean {
    return (this.flags & mask) !== 0;
  }
  /** Whether a name resolving in `space` can bind to it. */
  visibleIn(space: Space): boolean {
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
  /** Where it is defined, following imports and module augmentations. */
  definition(): Definition | null {
    return this.file._project._definitionOf(this);
  }
  /** Its uses across the project, through every import of it. */
  findReferences(): readonly Reference[] {
    return this.file._project._referencesOf(this);
  }
  /** Renames it everywhere, keeping export names, or changes nothing and reports why. */
  rename(name: string): Diagnostic[] {
    const diagnostics = rename(this, name);
    if (diagnostics.length === 0) this.model.setBindingName(this.id, name);
    return diagnostics;
  }
}

/** One use of a name. */
export class Reference {
  readonly id: number;
  private model: Model;

  /** @internal */
  constructor(model: Model, id: number) {
    this.model = model;
    this.id = id;
  }
  get file(): SourceFile {
    return this.model.file;
  }
  get name(): string {
    const binding = this.model.semantic.reference.symbolId(this.id);
    if (binding === null) return this.model.semantic.reference.name(this.id);
    return this.model.bindingName(binding);
  }
  get scope(): Scope {
    return this.model.scopes[this.model.semantic.reference.scopeId(this.id)]!;
  }
  get node(): Identifier | JSXIdentifier {
    return this.model.semantic.reference.node(this.id);
  }
  get space(): Space {
    return this.model.semantic.reference.space(this.id);
  }
  /** Whether the use is erased with the types. */
  get inTypePosition(): boolean {
    return this.model.semantic.reference.inTypePosition(this.id);
  }
  /** Whether it assigns to the name. */
  get isWrite(): boolean {
    return this.model.semantic.reference.isWrite(this.id);
  }
  /** Null for a global or undeclared name. */
  get binding(): Binding | null {
    const binding = this.model.semantic.reference.symbolId(this.id);
    return binding === null ? null : this.model.bindings[binding]!;
  }
}

export class Import {
  readonly id: number;
  /** @internal */
  _resolved: SourceFile | null = null;
  private model: Model;

  /** @internal */
  constructor(model: Model, id: number) {
    this.model = model;
    this.id = id;
  }
  get file(): SourceFile {
    return this.model.file;
  }
  get kind(): ImportKind {
    return this.model.semantic.import.kind(this.id);
  }
  /** The imported name of a `"named"` record, `"default"` for a default import. */
  get name(): string | null {
    return this.kind === "named" ? this.model.semantic.import.name(this.id) : null;
  }
  /** The binding it declares. */
  get local(): Binding | null {
    const binding = this.model.semantic.import.symbolId(this.id);
    return binding === null ? null : this.model.bindings[binding]!;
  }
  get specifier(): string {
    return this.model.semantic.import.specifier(this.id);
  }
  get typeOnly(): boolean {
    return this.model.semantic.import.typeOnly(this.id);
  }
  get phase(): "source" | "defer" | null {
    return this.model.semantic.import.phase(this.id);
  }
  /** The specifier, the declaration, or the call. */
  get node(): Node {
    return this.model.semantic.import.node(this.id);
  }
  /** Null outside the project. */
  get resolvedFile(): SourceFile | null {
    this.file._project._link(this.file);
    return this._resolved;
  }
  /** @internal */
  get _scope(): Scope {
    return this.model.scopes[this.model.semantic.import.scopeId(this.id)!]!;
  }
}

export class Export {
  readonly id: number;
  /** @internal */
  _resolved: SourceFile | null = null;
  private model: Model;

  /** @internal */
  constructor(model: Model, id: number) {
    this.model = model;
    this.id = id;
  }
  get file(): SourceFile {
    return this.model.file;
  }
  get kind(): ExportKind {
    return this.model.semantic.export.kind(this.id);
  }
  /** The exported name, or N of `export as namespace N`, null for `"star"` and `"equals"`. */
  get name(): string | null {
    const kind = this.kind;
    return kind === "star" || kind === "equals" ? null : this.model.semantic.export.name(this.id);
  }
  /** The binding it exports from this file. */
  get local(): Binding | null {
    const binding = this.model.semantic.export.symbolId(this.id);
    return binding === null ? null : this.model.bindings[binding]!;
  }
  /** The module it re-exports from. */
  get specifier(): string | null {
    const kind = this.kind;
    return kind === "reExport" || kind === "namespace" || kind === "star"
      ? this.model.semantic.export.specifier(this.id)
      : null;
  }
  /** The name a `"reExport"` takes from its module. */
  get fromName(): string | null {
    return this.kind === "reExport" ? this.model.semantic.export.fromName(this.id) : null;
  }
  get typeOnly(): boolean {
    return this.model.semantic.export.typeOnly(this.id);
  }
  /** The specifier, the declared name, or the statement. */
  get node(): Node {
    return this.model.semantic.export.node(this.id);
  }
  /** The file it re-exports from, null outside the project. */
  get resolvedFile(): SourceFile | null {
    this.file._project._link(this.file);
    return this._resolved;
  }
}

export class Model {
  readonly file: SourceFile;
  readonly semantic: Semantic;
  private scopeRows: Scope[] | null = null;
  private bindingRows: Binding[] | null = null;
  private referenceRows: Reference[] | null = null;
  private importRows: Import[] | null = null;
  private exportRows: Export[] | null = null;
  private bindingsByScope: Binding[][] | null = null;
  private referencesByBinding: Reference[][] | null = null;
  private declarationsByNode: Map<number, number> | null = null;
  private referencesByNode: Map<number, number> | null = null;
  private importsByBinding: Map<Binding, Import> | null = null;
  private exportIndex: { byName: Map<string, Export>; stars: Export[] } | null = null;
  private names = new Map<number, string>();

  constructor(file: SourceFile, semantic: Semantic) {
    this.file = file;
    this.semantic = semantic;
  }

  get scopes(): Scope[] {
    return (this.scopeRows ??= this.rows(Scope, this.semantic.scope.count));
  }
  get bindings(): Binding[] {
    return (this.bindingRows ??= this.rows(Binding, this.semantic.symbol.count));
  }
  get references(): Reference[] {
    return (this.referenceRows ??= this.rows(Reference, this.semantic.reference.count));
  }
  get imports(): Import[] {
    return (this.importRows ??= this.rows(Import, this.semantic.import.count));
  }
  get exports(): Export[] {
    return (this.exportRows ??= this.rows(Export, this.semantic.export.count));
  }
  get rootScope(): Scope {
    const scopes = this.scopes;
    return scopes.length > 1 && scopes[1]!.kind === "module" ? scopes[1]! : scopes[0]!;
  }

  bindingName(id: number): string {
    return this.names.get(id) ?? this.semantic.symbol.name(id);
  }

  setBindingName(id: number, name: string): void {
    this.names.set(id, name);
  }

  scopeBindings(scope: number): Binding[] {
    if (this.bindingsByScope === null) {
      const lists = Array.from({ length: this.semantic.scope.count }, (): Binding[] => []);
      for (const binding of this.bindings) lists[binding.scope.id]!.push(binding);
      this.bindingsByScope = lists;
    }
    return this.bindingsByScope[scope]!;
  }

  bindingReferences(binding: number): Reference[] {
    if (this.referencesByBinding === null) {
      const lists = Array.from({ length: this.semantic.symbol.count }, (): Reference[] => []);
      const { reference } = this.semantic;
      for (const row of this.references) {
        const target = reference.symbolId(row.id);
        if (target !== null) lists[target]!.push(row);
      }
      this.referencesByBinding = lists;
    }
    return this.referencesByBinding[binding]!;
  }

  declarationAt(index: number): Binding | null {
    if (this.declarationsByNode === null) {
      const map = new Map<number, number>();
      const { symbol } = this.semantic;
      for (let id = 0; id < symbol.count; id++) {
        const count = symbol.declCount(id);
        for (let i = 0; i < count; i++) map.set(symbol.declNodeIndex(id, i), id);
      }
      this.declarationsByNode = map;
    }
    const id = this.declarationsByNode.get(index);
    return id === undefined ? null : this.bindings[id]!;
  }

  referenceAt(index: number): Reference | null {
    if (this.referencesByNode === null) {
      const map = new Map<number, number>();
      const { reference } = this.semantic;
      for (let i = 0; i < reference.count; i++) map.set(reference.nodeIndex(i), i);
      this.referencesByNode = map;
    }
    const id = this.referencesByNode.get(index);
    return id === undefined ? null : this.references[id]!;
  }

  importOf(binding: Binding): Import | null {
    if (this.importsByBinding === null) {
      const map = new Map<Binding, Import>();
      for (const record of this.imports) {
        const local = record.local;
        if (local !== null) map.set(local, record);
      }
      this.importsByBinding = map;
    }
    return this.importsByBinding.get(binding) ?? null;
  }

  exportMap(): Map<string, Export> {
    return this.indexExports().byName;
  }

  starExports(): Export[] {
    return this.indexExports().stars;
  }

  sharedBinding(scope: Scope, name: string, space: Space): Binding | null {
    const next = this.semantic.scope.nextBodyId;
    const shared = scope.kind === "tsModule" ? BindingFlags.Exported : BindingFlags.EnumMember;
    for (let body = next(scope.id); body !== null && body !== scope.id; body = next(body)) {
      const found = this.scopes[body]!.find(name);
      if (found !== null && found.has(shared) && found.visibleIn(space)) return found;
    }
    return null;
  }

  private indexExports(): { byName: Map<string, Export>; stars: Export[] } {
    if (this.exportIndex === null) {
      const byName = new Map<string, Export>();
      const stars: Export[] = [];
      for (const record of this.exports) {
        if (record.kind === "star") stars.push(record);
        else if (record.kind !== "global" && record.name !== null && !byName.has(record.name)) {
          byName.set(record.name, record);
        }
      }
      this.exportIndex = { byName, stars };
    }
    return this.exportIndex;
  }

  private rows<T>(Row: Row<T>, count: number): T[] {
    return Array.from({ length: count }, (_, id) => new Row(this, id));
  }
}

function isArgumentsScope(scope: Scope): boolean {
  if (scope.kind === "staticBlock") return true;
  return scope.kind === "function" && scope.node.type !== "ArrowFunctionExpression";
}
