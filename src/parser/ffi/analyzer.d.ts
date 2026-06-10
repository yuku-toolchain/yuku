/**
 * yuku-analyzer: semantic analysis for JavaScript and TypeScript.
 *
 * One native call per file produces the AST plus the full semantic
 * model: scopes, symbols, resolved references, and module records.
 * Every query after that is local JS over compact tables, and every
 * node returned by a semantic query is the same object you reach by
 * walking `module.ast`.
 *
 * AST node types come from `yuku-parser` (a peer dependency); the
 * objects are identical to the ones it produces.
 */

import type {
  AliasName,
  AliasNodes,
  Comment,
  Diagnostic,
  DiagnosticSeverity,
  Identifier,
  JSXIdentifier,
  Node,
  NodeOfType,
  NodeType,
  Program,
  ScanCursor as BaseScanCursor,
  SourceLang,
  SourceLocation,
  SourceType,
  WalkContext as BaseWalkContext,
} from "yuku-parser";

/** A diagnostic produced by {@link Analyzer.link}. */
interface LinkDiagnostic {
  severity: DiagnosticSeverity;
  message: string;
  /** Path of the module the diagnostic belongs to. */
  module: string;
  start: number;
  end: number;
}

/** Options for {@link Analyzer.addFile}. */
interface AddFileOptions {
  /**
   * Language variant. Defaults to the file extension via
   * {@link langFromPath}.
   */
  lang?: SourceLang;
  /**
   * Parse as a script or an ES module. Defaults to the file extension
   * via {@link sourceTypeFromPath}.
   */
  sourceType?: SourceType;
  /**
   * Represent parenthesized expressions as `ParenthesizedExpression`
   * nodes.
   * @default true
   */
  preserveParens?: boolean;
  /**
   * Allow `return` at the top level.
   * @default false
   */
  allowReturnOutsideFunction?: boolean;
  /**
   * Attach comments to their host AST nodes.
   * @default false
   */
  attachComments?: boolean;
}

/** Options for {@link Analyzer}. */
interface AnalyzerOptions {
  /**
   * Host module resolution. Maps an import specifier and the importing
   * module's path to the path of an added file, or null for external
   * modules. Defaults to relative-path resolution among added files
   * with standard extension and index probing.
   */
  resolve?: (specifier: string, importerPath: string) => string | null;
}

/**
 * Bit flags describing a {@link Symbol}: which declaration kinds it
 * carries (one symbol can merge several under TS declaration merging)
 * and its modifiers. Combine with `&`/`|`, or use {@link Symbol.has}.
 */
declare const SymbolFlags: {
  /** `var`, parameter, or catch variable. */
  readonly FunctionScopedVariable: number;
  /** `let`, `const`, `using`, `await using`. */
  readonly BlockScopedVariable: number;
  /** Function declaration or expression. */
  readonly Function: number;
  /** Class declaration or expression. */
  readonly Class: number;
  /** TS `enum`. */
  readonly RegularEnum: number;
  /** TS `const enum`. */
  readonly ConstEnum: number;
  /** TS namespace with runtime content. */
  readonly ValueModule: number;
  /** TS `interface`. */
  readonly Interface: number;
  /** TS `type` alias. */
  readonly TypeAlias: number;
  /** TS `<T>`, `infer T`, or mapped-type key. */
  readonly TypeParameter: number;
  /** TS namespace of any kind. */
  readonly NamespaceModule: number;
  /** Value (or unspecified-kind) import binding. */
  readonly Import: number;
  /** `import type` / `import { type x }` binding. */
  readonly TypeImport: number;
  /** `const` or `using` binding. */
  readonly Const: number;
  /** TS `declare`. */
  readonly Ambient: number;
  /** Function or method parameter. */
  readonly Parameter: number;
  /** `catch (e)` binding. */
  readonly CatchVariable: number;
  /** Exported from its module. */
  readonly Exported: number;
  /** The default export. */
  readonly Default: number;
};

/** What kind of construct created a {@link Scope}. */
type ScopeKind =
  | "global"
  | "module"
  | "function"
  | "block"
  | "class"
  | "staticBlock"
  | "expressionName"
  | "tsModule";

/** A lexical scope in a module's scope tree. */
interface Scope {
  /** The owning module. */
  readonly module: Module;
  /** Stable id, the index into {@link Module.scopes}. */
  readonly id: number;
  readonly kind: ScopeKind;
  /** Whether this scope is in strict mode. */
  readonly strict: boolean;
  /** The AST node that created this scope. */
  readonly node: Node;
  /** The parent scope, or null for the global scope. */
  readonly parent: Scope | null;
  /** The nearest scope (or self) where `var` declarations land. */
  readonly hoistTarget: Scope;
  /** Symbols declared directly in this scope. */
  readonly bindings: Symbol[];
  /** Looks up `name` declared directly in this scope. */
  find(name: string): Symbol | null;
  /** True when `other` is this scope or a descendant of it. */
  contains(other: Scope): boolean;
  /** Walks from this scope up to the global scope, inclusive. */
  ancestors(): IterableIterator<Scope>;
}

/**
 * A declared binding. One symbol can have several declarations under
 * TS declaration merging (overloads, class + interface, namespace
 * merges).
 */
interface Symbol {
  /** The owning module. */
  readonly module: Module;
  /**
   * Stable id, the index into {@link Module.symbols}. Deterministic per
   * parse: `(module.path, id)` is a persistable key.
   */
  readonly id: number;
  readonly name: string;
  /** Raw {@link SymbolFlags} bitset. */
  readonly flags: number;
  /** The scope this symbol is declared in. */
  readonly scope: Scope;
  /** Every declarator node, in source order. */
  readonly declarations: Node[];
  /** Every resolved use site within this module, in source order. */
  readonly references: Reference[];
  /** True when any flag in `mask` is set. */
  has(mask: number): boolean;
  /** True when every flag in `mask` is set. */
  hasAll(mask: number): boolean;
  /** `var`/`let`/`const` bindings, parameters and catch bindings included. */
  readonly isVariable: boolean;
  readonly isFunction: boolean;
  readonly isClass: boolean;
  readonly isImported: boolean;
  readonly isExported: boolean;
  /** True for `const` and `using` bindings. */
  readonly isConst: boolean;
  readonly isParameter: boolean;
  readonly isCatchParam: boolean;
  /** True for `import type` / `import { type x }` bindings. */
  readonly isTypeOnly: boolean;
  readonly isDefaultExport: boolean;
  /** Visible at runtime: variables, functions, classes, enums, value namespaces. */
  readonly inValueSpace: boolean;
  /** Referencable from TS type positions: classes, enums, interfaces, aliases. */
  readonly inTypeSpace: boolean;
  /**
   * The defining site of this symbol, following import/re-export chains
   * across modules. Shorthand for {@link Analyzer.definitionOf}.
   */
  definition(): Definition | null;
}

/** One use of a name: a single identifier in reference position. */
interface Reference {
  /** The owning module. */
  readonly module: Module;
  /** Stable id, the index into {@link Module.references}. */
  readonly id: number;
  readonly name: string;
  /** The scope the reference occurs in. */
  readonly scope: Scope;
  /** The identifier node, the same object as in the walked AST. */
  readonly node: Identifier | JSXIdentifier;
  /** `"value"` for runtime uses, `"type"` for TS type-position uses. */
  readonly kind: "value" | "type";
  /**
   * True when this reference (re)assigns its binding: assignment
   * targets, `++`/`--` operands, for-in/of iteration variables, and
   * destructuring assignment leaves. Compound targets (`+=`) both read
   * and write.
   */
  readonly isWrite: boolean;
  /** The symbol this resolves to, or null for free/global names. */
  readonly symbol: Symbol | null;
}

/**
 * The semantic walk context: yuku-parser's {@link BaseWalkContext} (the
 * same position info and mutation operations, exact same semantics)
 * plus the module's semantic surface. One object is reused across the
 * whole walk; do not hold onto it across nodes.
 *
 * On mutation: semantic tables are a snapshot of the parsed source, so
 * new nodes have no symbols, references, or spans of their own.
 * Analyze, transform, then print (or re-analyze the printed output for
 * fresh semantics).
 */
declare class WalkContext<T extends Node = Node> extends BaseWalkContext<T> {
  /** The module being walked. Every semantic query is in reach. */
  readonly module: Module;
  /**
   * The innermost scope at the current node, replayed from the native
   * scope tree (catch-scope sharing, named-expression scopes, and
   * hoist targets are all exact).
   */
  readonly scope: Scope;
  /** Shorthand for `module.symbolOf(node)`. */
  readonly symbol: Symbol | null;
  /** Shorthand for `module.referenceOf(node)`. */
  readonly reference: Reference | null;
}

/** Handler invoked with the precisely-typed node and the walk context. */
type WalkHandler<T extends Node = Node> = (node: T, ctx: WalkContext<T>) => void;

/** Enter/leave pair for one node type. */
interface WalkHooks<T extends Node = Node> {
  enter?: WalkHandler<T>;
  leave?: WalkHandler<T>;
}

/**
 * Visitors passed to {@link Module.walk}: keys are node `type` strings
 * or alias group names (handlers receive the matching node type), plus
 * optional `enter` / `leave` catch-alls. Order per node: catch-all
 * `enter`, alias enters, typed enter, children, typed leave, alias
 * leaves, catch-all `leave`.
 */
type Visitors = {
  [K in NodeType]?: WalkHandler<NodeOfType<K>> | WalkHooks<NodeOfType<K>>;
} & {
  [A in AliasName]?: WalkHandler<AliasNodes<A>> | WalkHooks<AliasNodes<A>>;
} & {
  enter?: WalkHandler;
  leave?: WalkHandler;
};

/**
 * The semantic scan cursor: yuku-parser's {@link BaseScanCursor} plus
 * symbol and reference lookups. Both are index-keyed, so a scan can
 * resolve semantics without materializing any AST nodes.
 */
interface ScanCursor<T extends Node = Node> extends BaseScanCursor<T> {
  /** The module being scanned. */
  readonly module: Module;
  /** Shorthand for `module.symbolOf(node)`, without materializing. */
  readonly symbol: Symbol | null;
  /** Shorthand for `module.referenceOf(node)`, without materializing. */
  readonly reference: Reference | null;
}

/** Scan handlers keyed by node `type`, plus the universal `enter`. */
type ScanVisitors = {
  [K in NodeType]?: (cursor: ScanCursor<NodeOfType<K>>) => void;
} & {
  enter?: (cursor: ScanCursor) => void;
};

/** A free variable of a function, as reported by {@link Module.capturesOf}. */
interface Capture {
  /** The outer binding being closed over. */
  readonly symbol: Symbol;
  /** The capturing reference sites inside the function. */
  readonly references: Reference[];
  /** True when the function writes to the binding. */
  readonly isWritten: boolean;
}

/** One imported binding (or side-effect import) of a module. */
interface Import {
  /** The importing module. */
  readonly module: Module;
  /** Stable id, the index into {@link Module.imports}. */
  readonly id: number;
  /** The local binding symbol, or null for side-effect imports. */
  readonly local: Symbol | null;
  /**
   * The imported export name, `"default"` for default imports (the
   * spec models default as a name). Null for namespace and side-effect
   * imports.
   */
  readonly name: string | null;
  /** True for `import * as ns`. */
  readonly isNamespace: boolean;
  /** True for bare `import "m"`. */
  readonly isSideEffect: boolean;
  /** True for `import type` / `import { type x }`. */
  readonly typeOnly: boolean;
  /** Stage 3 phase modifier, or null. */
  readonly phase: "source" | "defer" | null;
  readonly specifier: string;
  /** The specifier node (the declaration for side-effect imports). */
  readonly node: Node;
  /** The defining module, or null when external. Links on demand. */
  readonly resolvedModule: Module | null;
}

/** One exported name of a module. */
interface Export {
  /** The exporting module. */
  readonly module: Module;
  /** Stable id, the index into {@link Module.exports}. */
  readonly id: number;
  /** The exported name (`"default"` included), or null for `export *`. */
  readonly name: string | null;
  /** True for `export * from "m"` without an alias. */
  readonly isStar: boolean;
  readonly typeOnly: boolean;
  /** The backing local symbol, or null (re-exports, anonymous defaults). */
  readonly local: Symbol | null;
  /** The re-export source specifier, or null for local exports. */
  readonly specifier: string | null;
  /** The name taken from the source module, or null (namespace / `export *`). */
  readonly fromName: string | null;
  /** True for `export *` and `export * as ns from "m"`. */
  readonly isNamespaceReexport: boolean;
  readonly node: Node;
  /** The re-export source module, or null. Links on demand. */
  readonly resolvedModule: Module | null;
}

/**
 * One analyzed source file: its AST, per-file semantics, and module
 * records. Created by {@link Analyzer.addFile}. All queries are local
 * (no native calls).
 */
interface Module {
  readonly analyzer: Analyzer;
  readonly path: string;
  readonly source: string;

  /**
   * The ESTree / TypeScript-ESTree program. Lazily decoded. Nodes are
   * identity-shared with every semantic query result.
   *
   * Nodes are plain mutable objects: edit them in place, mutate them
   * during a walk with {@link WalkContext.replace} and friends, and
   * print the result with `yuku-codegen`. Semantic tables stay a
   * snapshot of the parsed source and do not track mutations.
   */
  readonly ast: Program;
  /** Syntax and semantic diagnostics for this file. */
  readonly diagnostics: Diagnostic[];
  /** Every comment in source order. */
  readonly comments: Comment[];
  /** Sorted offsets where each line begins. */
  readonly lineStarts: number[];
  /** Resolves an offset to a `(line, column)` pair. */
  locOf(offset: number): SourceLocation;
  /** Like {@link locOf} with a hint line for near-constant lookups. */
  locNear(offset: number, hintLine: number): SourceLocation;

  /** Every lexical scope; index is the scope id. `scopes[0]` is global. */
  readonly scopes: Scope[];
  /** The scope top-level code runs in: module scope, or global for scripts. */
  readonly rootScope: Scope;
  /** Every declared symbol; index is the symbol id. */
  readonly symbols: Symbol[];
  /** Every identifier reference in source order; index is the reference id. */
  readonly references: Reference[];
  /** References resolving to no binding: globals and free names. */
  readonly unresolvedReferences: Reference[];

  /**
   * The symbol a node refers to: its own symbol for a declaration
   * identifier, the resolved symbol for a reference identifier. Null
   * for nodes that are neither, or for unresolved references.
   */
  symbolOf(node: Node): Symbol | null;
  /** The reference recorded for an identifier node, or null. */
  referenceOf(node: Node): Reference | null;
  /** The innermost scope whose extent contains `node`. */
  scopeOf(node: Node): Scope;
  /**
   * Walks the scope chain from `from` (default: the root scope) to
   * find the nearest binding of `name`.
   */
  resolve(name: string, from?: Scope): Symbol | null;
  /**
   * The free variables of a function or arrow: every binding referenced
   * inside it (nested closures included, value positions only) that is
   * declared outside it. Shadowing- and alias-correct, because it rides
   * the resolved reference table.
   *
   * Throws when `node` is not a function of this module's AST.
   */
  capturesOf(node: Node): Capture[];

  /**
   * Walks the AST (or the subtree under `root`) with semantic context.
   * Scope information is replayed from the native scope tree, so
   * non-scope nodes pay a single type lookup and nothing else.
   */
  walk(visitors: Visitors, root?: Node): void;

  /**
   * Readonly buffer scan with semantic context: visits the parsed node
   * records directly without materializing AST objects, and resolves
   * symbols and references in index space. Many times faster than
   * {@link walk} for sparse queries; call {@link ScanCursor.node} to
   * materialize a matched node (identity-shared with the walked AST).
   */
  scan(visitors: ScanVisitors): void;
  /** Collects every node of the given type(s), in source order. */
  findAll<K extends NodeType>(type: K): NodeOfType<K>[];
  findAll<K extends NodeType>(types: readonly K[]): NodeOfType<K>[];

  /** Import records, in source order. */
  readonly imports: Import[];
  /** Export records, in source order. */
  readonly exports: Export[];
  /** Modules this module imports from. Links on demand. */
  readonly dependencies: Module[];
  /** Modules that import from this module. Links on demand. */
  readonly dependents: Module[];
}

/** A symbol's defining site, possibly in another module. */
interface Definition {
  readonly module: Module;
  /**
   * The defining symbol, or null when the definition is a whole module
   * namespace (`import * as ns`, `export * as ns`).
   */
  readonly symbol: Symbol | null;
}

/** A cross-module reference, as reported by {@link Analyzer.referencesOf}. */
interface ModuleReference {
  readonly module: Module;
  readonly reference: Reference;
}

/**
 * The project: a set of analyzed modules and the links between them.
 *
 * ```ts
 * const analyzer = new Analyzer();
 * analyzer.addFile("src/app.tsx", source);
 * const sym = analyzer.module("src/app.tsx")!.symbolOf(node);
 * const def = sym?.definition();
 * ```
 */
declare class Analyzer {
  constructor(options?: AnalyzerOptions);

  /**
   * Parses and analyzes one file natively, returning its {@link Module}.
   * Adding an existing path replaces it and marks the graph for
   * relinking.
   */
  addFile(path: string, source: string, options?: AddFileOptions): Module;
  /** Removes a file. Returns whether it existed. */
  removeFile(path: string): boolean;
  /** The module added under `path`, if any. */
  module(path: string): Module | undefined;
  /** All modules, keyed by path. */
  readonly modules: ReadonlyMap<string, Module>;
  /** Graph-level diagnostics. Links on demand. */
  readonly diagnostics: LinkDiagnostic[];

  /**
   * Joins imports to exports across every added module: resolves
   * specifiers through the host resolver, populates
   * {@link Import.resolvedModule}, {@link Module.dependencies} /
   * {@link Module.dependents}, and reports missing exports.
   *
   * Calling this is optional: every cross-file surface links on demand
   * after files change. Call it explicitly to control when the work
   * (and its diagnostics) happen.
   */
  link(): void;

  /**
   * Follows import -> export -> re-export chains to the symbol that
   * actually defines `symbol`. A null `symbol` in the result means a
   * module namespace. Returns null when the chain leaves the added
   * file set (external modules).
   */
  definitionOf(symbol: Symbol): Definition | null;

  /**
   * Every reference to `symbol` across the whole graph: local uses
   * plus uses of every import binding that resolves back to it.
   */
  referencesOf(symbol: Symbol): ModuleReference[];
}

/** Resolves a {@link SourceLang} from a file path's extension. */
declare function langFromPath(path: string): SourceLang;

/** Resolves a {@link SourceType} from a file path's extension. */
declare function sourceTypeFromPath(path: string): SourceType;

export {
  Analyzer,
  SymbolFlags,
  langFromPath,
  sourceTypeFromPath,
  type AddFileOptions,
  type AnalyzerOptions,
  type Capture,
  type Definition,
  type Export,
  type Import,
  type LinkDiagnostic,
  type Module,
  type ModuleReference,
  type NodeOfType,
  type NodeType,
  type Reference,
  type ScanCursor,
  type ScanVisitors,
  type Scope,
  type ScopeKind,
  type Symbol,
  type Visitors,
  type WalkContext,
  type WalkHandler,
  type WalkHooks,
};
