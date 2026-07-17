import type {
  Comment,
  Diagnostic,
  Program,
  SourceLang,
  SourceType,
} from "@yuku-toolchain/types";

export * from "@yuku-toolchain/types";

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

/**
 * Resolves a {@link SourceLang} from a file path's extension.
 *
 * - `.d.ts`, `.d.mts`, `.d.cts` → `"dts"`
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

export type { ParseOptions, ParseResult };
