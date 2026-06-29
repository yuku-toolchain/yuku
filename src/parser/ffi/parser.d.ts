// yuku-parser's public type surface. The AST, diagnostic, and traversal type
// model lives in @yuku-toolchain/types and is re-exported here for backward
// compatibility; this file adds only the parser's own parse/walk API.

import type {
  Comment,
  Diagnostic,
  Node,
  NodeOfType,
  NodeType,
  Program,
  SourceLang,
  SourceType,
  WalkContext,
} from "@yuku-toolchain/types";

export * from "@yuku-toolchain/types";

// Parsing

/** Options for configuring the parser. */
interface ParseOptions {
  /**
   * Parse as a classic script or an ES module.
   * Module mode enables `import`/`export`, `import.meta`, top-level `await`,
   * and strict mode.
   * @default "module"
   */
  sourceType?: SourceType;
  /**
   * Language variant controls which syntax extensions are enabled.
   * @default "js"
   */
  lang?: SourceLang;
  /**
   * When true, parenthesized expressions are represented as
   * `ParenthesizedExpression` nodes in the AST. When false,
   * parentheses are stripped and only the inner expression is kept.
   * @default true
   */
  preserveParens?: boolean;
  /**
   * Allow `return` statements outside of functions, at the top level.
   * @default false
   */
  allowReturnOutsideFunction?: boolean;
  /**
   * Run semantic analysis after parsing and include semantic errors
   * (e.g. duplicate declarations, invalid `break`/`continue` targets)
   * alongside syntax errors. This requires a separate AST pass and may
   * affect performance slightly.
   * @default false
   */
  semanticErrors?: boolean;
  /**
   * Also attach each comment to the AST node it sits next to, via
   * {@link BaseNode.comments}. The flat {@link ParseResult.comments} list is
   * always present regardless.
   * @default false
   */
  attachComments?: boolean;
}

/** The result returned by the parser. */
interface ParseResult {
  /** Root ESTree/TypeScript-ESTree AST node. */
  program: Program;
  /** Every comment in source order, each with its source span. */
  comments: Comment[];
  /** Syntax diagnostics, and semantic diagnostics when {@link ParseOptions.semanticErrors} is enabled. */
  diagnostics: Diagnostic[];
}

/**
 * Parse JS/TS source code and return an ESTree / TypeScript-ESTree compatible AST.
 */
export function parse(source: string, options?: ParseOptions): ParseResult;

// Walking

/** A visitor function for one node type. */
type WalkHandler<T extends Node = Node, S = unknown> = (node: T, ctx: WalkContext<T, S>) => void;

/** Enter/leave hooks for one node type. */
interface WalkHooks<T extends Node = Node, S = unknown> {
  enter?: WalkHandler<T, S>;
  leave?: WalkHandler<T, S>;
}

/**
 * Handlers keyed by node `type`, or the universal `enter`/`leave`. A bare
 * function is an enter handler; per node `enter` runs before children and
 * `leave` after.
 */
type Visitors<S = unknown> = {
  [K in NodeType]?: WalkHandler<NodeOfType<K>, S> | WalkHooks<NodeOfType<K>, S>;
} & {
  enter?: WalkHandler<Node, S>;
  leave?: WalkHandler<Node, S>;
};

/**
 * Walk an AST depth-first, dispatching to typed visitors and mutating
 * in place. Traversal order is driven by tables generated from the
 * parser's AST definition, so it can never drift. Returns the root.
 */
export function walk<T extends Node, S = unknown>(root: T, visitors: Visitors<S>, state?: S): T;

/**
 * Resolves a {@link SourceLang} from a file path's extension.
 *
 * - `.d.ts`, `.d.mts`, `.d.cts` → `"dts"`
 * - `.tsrx` → `"tsrx"`
 * - `.tsx` → `"tsx"`
 * - `.ts`, `.mts`, `.cts` → `"ts"`
 * - `.jsx` → `"jsx"`
 * - everything else → `"js"`
 */
export function langFromPath(path: string): SourceLang;

/**
 * Resolves a {@link SourceType} from a file path's extension.
 *
 * - `.cjs`, `.cts` → `"script"`
 * - everything else → `"module"`
 */
export function sourceTypeFromPath(path: string): SourceType;

export type { ParseOptions, ParseResult, Visitors, WalkHandler, WalkHooks };
