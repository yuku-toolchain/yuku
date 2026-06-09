/**
 * yuku-analyzer: semantic analysis for JavaScript and TypeScript.
 *
 * One native call per file produces the AST plus the full semantic
 * model: scopes, symbols, resolved references, and module records.
 * Every query after that is local JS over compact tables, and every
 * node returned by a semantic query is the same object you reach by
 * walking `module.ast`.
 */

/** How the source code should be parsed. */
type SourceType = "script" | "module";

/** Language variant of the source code. */
type SourceLang = "js" | "ts" | "jsx" | "tsx" | "dts";

/**
 * An ESTree / TypeScript-ESTree AST node as produced by yuku-parser.
 * Nodes carry byte spans as `start`/`end`. For fully typed AST shapes,
 * use the node types from the `yuku-parser` package; the objects are
 * identical.
 */
interface AstNode {
  type: string;
  start: number;
  end: number;
  [key: string]: unknown;
}

/** The root AST node of a module. */
interface Program extends AstNode {
  type: "Program";
  body: AstNode[];
  sourceType: SourceType;
}

/** Severity level of a {@link Diagnostic}. */
type DiagnosticSeverity = "error" | "warning" | "hint" | "info";

/** A labeled source span attached to a {@link Diagnostic}. */
interface DiagnosticLabel {
  start: number;
  end: number;
  message: string;
}

/** A diagnostic produced during parsing or semantic analysis. */
interface Diagnostic {
  severity: DiagnosticSeverity;
  message: string;
  help: string | null;
  start: number;
  end: number;
  labels: DiagnosticLabel[];
}

/** A diagnostic produced by {@link Analyzer.link}. */
interface LinkDiagnostic {
  severity: DiagnosticSeverity;
  message: string;
  /** Path of the module the diagnostic belongs to. */
  module: string;
  start: number;
  end: number;
}

/** A source comment with its span. */
interface Comment {
  type: "Line" | "Block";
  value: string;
  start: number;
  end: number;
}

/** A `(line, column)` pair. Lines are 1-based, columns 0-based. */
interface SourceLocation {
  line: number;
  column: number;
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
 * and its modifiers. Combine with `&`/`|`.
 *
 * ```ts
 * if (sym.flags & SymbolFlags.Import) { ... }
 * ```
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
  readonly node: AstNode;
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
  readonly declarations: AstNode[];
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
  readonly node: AstNode;
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
 * Per-node context handed to walk handlers. One object is reused across
 * the whole walk; do not hold onto it across nodes.
 */
interface WalkContext {
  /** The module being walked. Every semantic query is in reach. */
  readonly module: Module;
  /**
   * The innermost scope at the current node, replayed from the native
   * scope tree (catch-scope sharing, named-expression scopes, and
   * hoist targets are all exact).
   */
  readonly scope: Scope;
  /** The parent node, or null at the walk root. */
  readonly parent: AstNode | null;
  /** Shorthand for `module.symbolOf(node)`. */
  readonly symbol: Symbol | null;
  /** Shorthand for `module.referenceOf(node)`. */
  readonly reference: Reference | null;
  /** A copy of the ancestor chain, walk root first. */
  ancestors(): AstNode[];
  /** Do not descend into this node's children. `leave` still fires. */
  skip(): void;
  /** End the walk immediately. No further handlers fire. */
  stop(): void;
  /**
   * Replaces the current node in its parent. In `enter`, the walk
   * continues into the replacement's children and `leave` fires on the
   * replacement. The walk root cannot be replaced.
   *
   * Semantic tables are a snapshot of the parsed source: new nodes have
   * no symbols, references, or spans of their own. Analyze, transform,
   * then print (or re-analyze the printed output for fresh semantics).
   */
  replace(node: AstNode): void;
}

type WalkHandler = (node: AstNode, ctx: WalkContext) => void;

interface WalkHooks {
  enter?: WalkHandler;
  leave?: WalkHandler;
}

/**
 * A walk visitor: type-keyed handlers (a function is an enter handler,
 * an object provides enter and/or leave) plus optional catch-alls.
 * Order per node: catch-all `enter`, typed enter, children, typed
 * leave, catch-all `leave`.
 */
type Visitor = WalkHooks & {
  [nodeType: string]: WalkHandler | WalkHooks | undefined;
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
  readonly node: AstNode;
  /** The source module after {@link Analyzer.link}, or null when external. */
  readonly resolvedModule: Module | null;
}

/** One exported name of a module. */
interface Export {
  /** The exporting module. */
  readonly module: Module;
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
  readonly node: AstNode;
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
   * Nodes are plain mutable objects: edit them in place, replace them
   * during a walk with {@link WalkContext.replace}, and print the
   * result with `yuku-codegen`. Semantic tables stay a snapshot of the
   * parsed source and do not track mutations.
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
  symbolOf(node: AstNode): Symbol | null;
  /** The reference recorded for an identifier node, or null. */
  referenceOf(node: AstNode): Reference | null;
  /** The innermost scope whose extent contains `node`. */
  scopeOf(node: AstNode): Scope;
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
  capturesOf(node: AstNode): Capture[];

  /**
   * Walks the AST (or the subtree under `root`) with semantic context.
   * Scope information is replayed from the native scope tree, so
   * non-scope nodes pay a single type lookup and nothing else.
   */
  walk(visitor: Visitor, root?: AstNode): void;
  /** Collects every node of the given type(s), in source order. */
  findAll(types: string | string[]): AstNode[];

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
 * analyzer.link();
 * const sym = analyzer.module("src/app.tsx").symbolOf(node);
 * const def = analyzer.definitionOf(sym);
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
  type AstNode,
  type Capture,
  type Comment,
  type Definition,
  type Diagnostic,
  type DiagnosticLabel,
  type DiagnosticSeverity,
  type Export,
  type Import,
  type LinkDiagnostic,
  type Module,
  type ModuleReference,
  type Program,
  type Reference,
  type Scope,
  type ScopeKind,
  type SourceLang,
  type SourceLocation,
  type SourceType,
  type Symbol,
  type Visitor,
  type WalkContext,
  type WalkHandler,
  type WalkHooks,
};
