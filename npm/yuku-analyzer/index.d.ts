/**
 * yuku-analyzer: semantic analysis for JavaScript and TypeScript.
 */

import type {
  Comment,
  Diagnostic,
  DiagnosticSeverity,
  Identifier,
  JSXIdentifier,
  Node,
  NodeOfType,
  NodeType,
  Program,
  SourceLang,
  SourceType,
  WalkContext as BaseWalkContext,
} from "@yuku-toolchain/types";

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
 * and its modifiers, plus a few composite categories. Every categorical
 * question about a symbol is `symbol.has(SymbolFlags.X)` (any of the
 * bits) or `symbol.hasAll(...)` (all of them); there is one way to ask.
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
  /** A value import binding (`import x` / `import { x }`). */
  readonly ValueImport: number;
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

  /** Composite: any variable (`var` / `let` / `const`, params, catch). */
  readonly Variable: number;
  /** Composite: any import binding, value or `import type`. */
  readonly Import: number;
  /** Composite: visible at runtime (var, function, class, enum, value namespace). */
  readonly ValueSpace: number;
  /** Composite: referencable from a type position (class, enum, interface, alias, type param). */
  readonly TypeSpace: number;
  /** Composite: what a dotted type name starts from (namespace, enum). */
  readonly NamespaceSpace: number;
};

/**
 * The declaration space a reference position resolves in, matching
 * TypeScript name resolution. A binding outside a reference's space
 * does not shadow: an inner `const T` never captures a type-position
 * `T` away from an outer `type T`, and vice versa.
 *
 * - `"value"`: runtime uses
 * - `"type"`: annotations, heritage clauses, type arguments
 * - `"namespace"`: the qualifier of a dotted type name (`ns.T`, `E.A`)
 * - `"typeof"`: value uses inside a type (`typeof x`, `x is T` params)
 * - `"any"`: alias positions accepting every space (`export { x }`,
 *   `export default x`, `export = x`, `import a = x`)
 */
type Space = "value" | "type" | "namespace" | "typeof" | "any";

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
  /**
   * True when any flag in `mask` is set. The single way to ask what a
   * symbol is: `symbol.has(SymbolFlags.Function)`, or a composite like
   * `symbol.has(SymbolFlags.ValueSpace)`.
   */
  has(mask: number): boolean;
  /** True when every flag in `mask` is set. */
  hasAll(mask: number): boolean;
  /** Occupies value space: exists at runtime. */
  readonly inValueSpace: boolean;
  /** Occupies TS type space: referencable from annotations. */
  readonly inTypeSpace: boolean;
  /** A dotted type name can start from it: namespaces and enums. */
  readonly inNamespaceSpace: boolean;
  /**
   * True when a reference resolving in `space` may bind to this
   * symbol, the acceptance rule of name resolution. Import bindings
   * alias symbols of unknowable space and are visible in every space.
   */
  visibleIn(space: Space): boolean;
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
  /** The declaration {@link Space} this position resolves in. */
  readonly space: Space;
  /**
   * True when the position sits inside a type-only subtree (`"type"`,
   * `"namespace"`, `"typeof"` spaces): erased at compile time, so a
   * rename tool can change a value without touching a same-named type.
   */
  readonly inTypePosition: boolean;
  /**
   * True when this reference (re)assigns its binding: assignment
   * targets, `++`/`--` operands, for-in/of iteration variables, and
   * destructuring assignment leaves. Compound targets (`+=`) both read
   * and write.
   */
  readonly isWrite: boolean;
  /**
   * The symbol this resolves to, or null when no binding is visible
   * in this reference's space (globals, undeclared names).
   */
  readonly symbol: Symbol | null;
}

/**
 * The semantic walk context: the toolchain's {@link BaseWalkContext} (the
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
 * Visitors passed to {@link Module.walk}: keys are node `type` strings, plus optional `enter` /
 * `leave` catch-alls. Order per node: catch-all `enter`, typed enter,
 * children, typed leave, catch-all `leave`.
 */
type Visitors = {
  [K in NodeType]?: WalkHandler<NodeOfType<K>> | WalkHooks<NodeOfType<K>>;
} & {
  enter?: WalkHandler;
  leave?: WalkHandler;
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
  /**
   * The exported name (`"default"` included), or null for `export *`,
   * `export =`, and `export as namespace`.
   */
  readonly name: string | null;
  /** True for `export * from "m"` without an alias. */
  readonly isStar: boolean;
  /** True for TS `export = expr` (the module's entire export value). */
  readonly isExportEquals: boolean;
  /** The TS `export as namespace N` global name, or null. */
  readonly globalName: string | null;
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
  /**
   * The innermost scope whose extent contains `node`, or the module's
   * root scope for a node not produced by this module's analysis.
   */
  scopeOf(node: Node): Scope;
  /**
   * The node that structurally contains `node`. Null at the program
   * root and for a node not part of this module's AST. Lets you walk
   * upward from a node you already hold, with no ancestor stack.
   */
  parentOf(node: Node): Node | null;
  /**
   * Walks the scope chain from `from` (default: the root scope) to
   * find the nearest binding of `name` visible in `space` (default:
   * `"value"`, resolving like runtime code). A binding outside the
   * space does not shadow, the walk keeps going. `"any"` matches by
   * name alone.
   */
  resolve(name: string, from?: Scope, space?: Space): Symbol | null;
  /**
   * The free variables of a function or arrow: every binding referenced
   * inside it (nested closures included, value positions only) that is
   * declared outside it. Shadowing- and alias-correct, because it rides
   * the resolved reference table.
   *
   * Only bindings count: `this`, `arguments`, and unresolved/global
   * names carry no symbol and never appear. Module-scope and import
   * bindings count like any other outer binding; filter on the
   * symbol's scope to narrow.
   *
   * Throws when `node` is not a function of this module's AST.
   */
  capturesOf(node: Node): Capture[];
  /**
   * Every name this module exports, directly or through `export *`
   * chains (the spec's GetExportedNames). Ambiguous star names are
   * included; `"default"` never crosses an `export *` boundary. Links
   * on demand.
   */
  exportedNames(): string[];

  /**
   * Walks the AST (or the subtree under `root`) with semantic context.
   * Scope information is replayed from the native scope tree, so
   * non-scope nodes pay a single type lookup and nothing else.
   */
  walk(visitors: Visitors, root?: Node): void;

  /** Collects every node of the given type(s), in source order. */
  findAll<K extends NodeType>(type: K): NodeOfType<K>[];
  findAll<K extends NodeType>(types: Iterable<K>): NodeOfType<K>[];

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
   * {@link Module.dependents}, and reports unresolvable or ambiguous
   * names. Resolution follows the spec's ResolveExport semantics: a
   * name supplied by multiple `export *` declarations through
   * different bindings is an error, and `"default"` is never satisfied
   * by `export *`.
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
   * file set (external modules), does not resolve, or is ambiguous.
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
  type Scope,
  type ScopeKind,
  type Space,
  type Symbol,
  type Visitors,
  type WalkContext,
  type WalkHandler,
  type WalkHooks,
};
