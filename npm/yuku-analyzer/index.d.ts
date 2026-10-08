import type {
  Comment,
  Core,
  Diagnostic,
  FileOptions,
  Identifier,
  JSXIdentifier,
  Node,
  NodeOfType,
  NodeType,
  Program,
  TokenList,
  WalkContext,
} from "@yuku-toolchain/types";
import type { AliasMap, AliasName } from "yuku-ast";

interface ParseOptions {
  /** @default true */
  preserveParens?: boolean;
  /** @default false */
  attachComments?: boolean;
  /** Keep every token in {@link Module.tokens}. @default false */
  tokens?: boolean;
}

interface SetFileOptions extends Omit<FileOptions, "path">, ParseOptions {}

interface AnalyzeOptions extends FileOptions, ParseOptions, Pick<AnalyzerOptions, "core"> {}

interface AnalyzerOptions {
  /**
   * The core that analyzes, from `load` in `yuku-core` or `@yuku-core/wasm`.
   * @default the native core
   */
  core?: Core;
  /**
   * Maps an import specifier to the path of a file in the project. Return `false` for a module
   * outside the project, such as a package, and `null` when it cannot be resolved, which is
   * reported as a warning. Defaults to relative paths, probing extensions and index files as
   * TypeScript does.
   */
  resolve?: (specifier: string, importer: string) => string | false | null;
}

/** Kinds and modifiers of a {@link Binding}, tested with `has` and `hasAll`. */
declare const BindingFlags: {
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
};

/**
 * The declaration space a name resolves in. A binding outside it does not shadow.
 *
 * - `"value"`: runtime uses
 * - `"type"`: type positions
 * - `"namespace"`: the start of a dotted name, `ns` in `ns.T` and `import x = ns.T`
 * - `"typeof"`: a value inside a type, `x` in `typeof x`
 * - `"any"`: alias positions, `x` in `export { x }`
 */
type Space = "value" | "type" | "namespace" | "typeof" | "any";

type ScopeKind =
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
 * - `"named"`: `import x from "m"` and `import { x } from "m"`
 * - `"namespace"`: `import * as ns from "m"`
 * - `"sideEffect"`: `import "m"`
 * - `"importEquals"`: `import ns = require("m")`
 * - `"dynamic"`: `import("m")`
 * - `"require"`: `require("m")`
 * - `"augmentation"`: `declare module "m" {}` in a module, merging its declarations into `m`
 */
type ImportKind =
  | "named"
  | "namespace"
  | "sideEffect"
  | "importEquals"
  | "dynamic"
  | "require"
  | "augmentation";

/**
 * - `"named"`: `export const x`, `export { x }`, `export default x`
 * - `"reExport"`: `export { x as y } from "m"`
 * - `"namespace"`: `export * as ns from "m"`
 * - `"star"`: `export * from "m"`
 * - `"equals"`: `export = x`
 * - `"global"`: `export as namespace N`
 */
type ExportKind = "named" | "reExport" | "namespace" | "star" | "equals" | "global";

/** A set of modules and the links between them. */
declare class Analyzer {
  constructor(options?: AnalyzerOptions);
  /** Analyzes a file, replacing any module at the same path. */
  setFile(path: string, source: string, options?: SetFileOptions): Module;
  /** Returns whether the file was in the project. */
  deleteFile(path: string): boolean;
  module(path: string): Module | undefined;
  readonly modules: ReadonlyMap<string, Module>;
  /** Every module's diagnostics, and the links that fail. */
  readonly diagnostics: Diagnostic[];
  /** Links the project now. Every cross-file query links on demand otherwise. */
  link(): void;
}

/** One analyzed file. Every query is local except those that link. */
interface Module {
  readonly analyzer: Analyzer;
  readonly path: string;
  readonly source: string;
  /** Its nodes are the objects every query returns. */
  readonly ast: Program;
  readonly comments: Comment[];
  /** With {@link ParseOptions.tokens}. */
  readonly tokens?: TokenList;
  readonly diagnostics: Diagnostic[];
  /** False once its path is set again or deleted. */
  readonly isCurrent: boolean;

  /** Every scope, indexed by id. The first is the global scope. */
  readonly scopes: Scope[];
  /** Where top-level code runs, the module scope or the global scope of a script. */
  readonly rootScope: Scope;
  /** Every binding, indexed by id. */
  readonly bindings: Binding[];
  /** Every name in use, in source order. */
  readonly references: Reference[];
  /** References with no binding, such as globals. */
  readonly unresolvedReferences: Reference[];
  readonly imports: Import[];
  readonly exports: Export[];
  readonly moduleFlags: ModuleFlags;
  /** The modules it imports from. Links. */
  readonly dependencies: Module[];
  /** The modules that import from it. Links. */
  readonly dependents: Module[];

  /** The binding a node declares or refers to. */
  bindingOf(node: Node): Binding | null;
  referenceOf(node: Node): Reference | null;
  /** The innermost scope around a node, the root scope for a node added later. */
  scopeOf(node: Node): Scope;
  parentOf(node: Node): Node | null;
  /** The node, then each parent up to the root. */
  ancestors(node: Node): IterableIterator<Node>;
  /** The innermost node containing a UTF-16 offset. */
  nodeAt(offset: number): Node | null;
  /** Resolves a name as code at `from` would. */
  lookup(name: string, options?: { from?: Scope; space?: Space }): Binding | null;
  /** The outer bindings a function uses. Throws for a node that is not a function. */
  capturesOf(fn: Node): Capture[];
  /** Every name it exports, through `export *` and module augmentations. Links. */
  exportedNames(): string[];
  /** The binding behind one of its exports. Links. */
  resolveExport(name: string): Definition | null;
  /** Walks its AST, or the subtree under `root`, with the semantic context. */
  walk(visitors: SemanticVisitors, root?: Node): void;
  walkAsync(visitors: AsyncSemanticVisitors, root?: Node): Promise<void>;
  /** Every node of the given types, in source order. */
  findAll<K extends NodeType>(type: K): NodeOfType<K>[];
  findAll<K extends NodeType>(types: readonly K[]): NodeOfType<K>[];
}

interface Scope {
  readonly module: Module;
  readonly id: number;
  readonly kind: ScopeKind;
  readonly strict: boolean;
  /** The node that creates it. */
  readonly node: Node;
  readonly parent: Scope | null;
  /** Where a `var` declared in it lands. */
  readonly hoistTarget: Scope;
  /** The bindings it declares. */
  readonly bindings: Binding[];
  /** A binding it declares by name. */
  find(name: string): Binding | null;
  /** Whether `other` is this scope or inside it. */
  contains(other: Scope): boolean;
  /** This scope, then each parent. */
  ancestors(): IterableIterator<Scope>;
}

/** A declared name. Merged declarations, such as overloads, share one. */
interface Binding {
  readonly module: Module;
  readonly id: number;
  readonly name: string;
  /** A {@link BindingFlags} bitset. */
  readonly flags: number;
  readonly scope: Scope;
  /** Each declaration's name node. */
  readonly declarations: Node[];
  /** Its uses in this module. */
  readonly references: Reference[];
  /** Whether any flag in `mask` is set. */
  has(mask: number): boolean;
  /** Whether every flag in `mask` is set. */
  hasAll(mask: number): boolean;
  /** Whether a name resolving in `space` can bind to it. */
  visibleIn(space: Space): boolean;
  /**
   * Where it is defined, following imports across modules and a module augmentation to the binding
   * it merges into, itself otherwise. Links.
   */
  definition(): Definition | null;
  /** Its uses across the project, through every import of it. Links. */
  findReferences(): Reference[];
}

/** One use of a name. */
interface Reference {
  readonly module: Module;
  readonly id: number;
  readonly name: string;
  readonly scope: Scope;
  readonly node: Identifier | JSXIdentifier;
  readonly space: Space;
  /** Whether the use is erased with the types. */
  readonly inTypePosition: boolean;
  /** Whether it assigns, as `x = 1`, `x++`, and `for (x of xs)` do. */
  readonly isWrite: boolean;
  /** Null for a global or undeclared name. */
  readonly binding: Binding | null;
}

interface Import {
  readonly module: Module;
  readonly id: number;
  readonly kind: ImportKind;
  /** The imported name of a `"named"` record, `"default"` for a default import. */
  readonly name: string | null;
  /** The binding it declares. */
  readonly local: Binding | null;
  /** Whether it binds a whole module, as `"namespace"` and `"importEquals"` do. */
  readonly isNamespace: boolean;
  readonly specifier: string;
  readonly typeOnly: boolean;
  readonly phase: "source" | "defer" | null;
  /** The specifier, the declaration, or the call. */
  readonly node: Node;
  /** Null outside the project. Links. */
  readonly resolvedModule: Module | null;
}

interface Export {
  readonly module: Module;
  readonly id: number;
  readonly kind: ExportKind;
  /** The exported name, null for `"star"`, `"equals"`, and `"global"`. */
  readonly name: string | null;
  /** The name of `export as namespace N`. */
  readonly globalName: string | null;
  /** The binding it exports from this module. */
  readonly local: Binding | null;
  /** The module it re-exports from. */
  readonly specifier: string | null;
  /** The name a `"reExport"` takes from its module. */
  readonly fromName: string | null;
  readonly typeOnly: boolean;
  /** The specifier, the declared name, or the statement. */
  readonly node: Node;
  /** The module it re-exports from, null outside the project. Links. */
  readonly resolvedModule: Module | null;
}

/** What a script reads from CommonJS and `import.meta`. */
interface ModuleFlags {
  readonly usesRequire: boolean;
  readonly usesModule: boolean;
  readonly usesExports: boolean;
  readonly usesImportMeta: boolean;
}

/** Where a name is defined. A null binding is a whole module namespace. */
interface Definition {
  readonly module: Module;
  readonly binding: Binding | null;
  /** The bindings that `declare module` blocks in other modules merge into it. */
  readonly augmentations: Binding[];
}

interface Capture {
  readonly binding: Binding;
  /** Its uses inside the function. */
  readonly references: Reference[];
  readonly isWritten: boolean;
}

/** The {@link WalkContext} of `yuku-ast`, with the semantic model at the current node. */
declare class SemanticWalkContext<T extends Node = Node> extends WalkContext<T> {
  readonly module: Module;
  readonly scope: Scope;
  readonly binding: Binding | null;
  readonly reference: Reference | null;
}

type SemanticWalkHandler<T extends Node = Node> = (node: T, ctx: SemanticWalkContext<T>) => void;

interface SemanticWalkHooks<T extends Node = Node> {
  enter?: SemanticWalkHandler<T>;
  leave?: SemanticWalkHandler<T>;
}

/** Handlers keyed by node type or alias group, with `enter` and `leave` for every node. */
type SemanticVisitors = {
  [K in NodeType]?: SemanticWalkHandler<NodeOfType<K>> | SemanticWalkHooks<NodeOfType<K>>;
} & {
  [A in AliasName]?: SemanticWalkHandler<AliasMap[A]> | SemanticWalkHooks<AliasMap[A]>;
} & {
  enter?: SemanticWalkHandler;
  leave?: SemanticWalkHandler;
};

type AsyncSemanticWalkHandler<T extends Node = Node> = (
  node: T,
  ctx: SemanticWalkContext<T>,
) => void | Promise<void>;

interface AsyncSemanticWalkHooks<T extends Node = Node> {
  enter?: AsyncSemanticWalkHandler<T>;
  leave?: AsyncSemanticWalkHandler<T>;
}

type AsyncSemanticVisitors = {
  [K in NodeType]?:
    | AsyncSemanticWalkHandler<NodeOfType<K>>
    | AsyncSemanticWalkHooks<NodeOfType<K>>;
} & {
  [A in AliasName]?: AsyncSemanticWalkHandler<AliasMap[A]> | AsyncSemanticWalkHooks<AliasMap[A]>;
} & {
  enter?: AsyncSemanticWalkHandler;
  leave?: AsyncSemanticWalkHandler;
};

/** Analyzes one file. */
declare function analyze(source: string, options?: AnalyzeOptions): Module;

export {
  analyze,
  Analyzer,
  BindingFlags,
  type AnalyzeOptions,
  type AnalyzerOptions,
  type AsyncSemanticVisitors,
  type AsyncSemanticWalkHandler,
  type AsyncSemanticWalkHooks,
  type Binding,
  type Capture,
  type Definition,
  type Export,
  type ExportKind,
  type Import,
  type ImportKind,
  type Module,
  type ModuleFlags,
  type Reference,
  type Scope,
  type ScopeKind,
  type SemanticVisitors,
  type SemanticWalkContext,
  type SemanticWalkHandler,
  type SemanticWalkHooks,
  type SetFileOptions,
  type Space,
};
