import type {
  Comment,
  Diagnostic,
  Program,
  SourceLang,
  SourceType,
  TokenKindMap,
  TokenList,
} from "@yuku-toolchain/types";

export * from "@yuku-toolchain/types";

interface ParseOptions {
  /**
   * Parse as a classic script, an ES module, or a CommonJS module.
   * Module mode enables `import`/`export`, `import.meta`, top-level `await`,
   * and strict mode. CommonJS mode parses script code whose top level behaves
   * like a function body, allowing top-level `return`, `new.target`, and
   * `using`. The AST's `Program.sourceType` is always `"script"` or
   * `"module"`, per ESTree.
   * @default "module"
   */
  sourceType?: SourceType;
  /** @default "js" */
  lang?: SourceLang;
  /**
   * When true, parenthesized expressions are represented as
   * `ParenthesizedExpression` nodes in the AST. When false,
   * parentheses are stripped and only the inner expression is kept.
   * @default true
   */
  preserveParens?: boolean;
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
  /**
   * Keep every token, see {@link ParseResult.tokens}.
   * @default false
   */
  tokens?: boolean;
}

interface ParseResult {
  program: Program;
  /** Every comment in source order, each with its source span. */
  comments: Comment[];
  /** Every token in source order, with {@link ParseOptions.tokens}. */
  tokens?: TokenList;
  /** Syntax diagnostics, and semantic diagnostics when {@link ParseOptions.semanticErrors} is enabled. */
  diagnostics: Diagnostic[];
}

export function parse(source: string, options?: ParseOptions): ParseResult;

/** Every token kind by name, `tokens.kind(i) === TokenKind.Arrow`. */
export const TokenKind: TokenKindMap;
export type TokenKind = TokenKindMap[keyof TokenKindMap];

export { langFromPath, sourceTypeFromPath } from "yuku-engine";

export type { ParseOptions, ParseResult };
