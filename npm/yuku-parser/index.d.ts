import type {
  Comment,
  Core,
  Diagnostic,
  FileOptions,
  Program,
  TokenKindMap,
  TokenList,
} from "@yuku-toolchain/types";

export * from "@yuku-toolchain/types";

interface ParseOptions extends FileOptions {
  /**
   * The core that parses, from `load` in `yuku-core` or `@yuku-core/wasm`.
   * @default the native core
   */
  core?: Core;
  /**
   * Keep `ParenthesizedExpression` nodes. When false, only the inner expression is kept.
   * @default true
   */
  preserveParens?: boolean;
  /**
   * Also report the early errors that need scopes, such as redeclarations.
   * @default false
   */
  semanticErrors?: boolean;
  /**
   * Also attach each comment to its node, see {@link BaseNode.comments}.
   * @default false
   */
  attachComments?: boolean;
  /**
   * Keep every token in {@link ParseResult.tokens}.
   * @default false
   */
  tokens?: boolean;
}

interface ParseResult {
  program: Program;
  /** Every comment, in source order. */
  comments: Comment[];
  /** Every token, with {@link ParseOptions.tokens}. */
  tokens?: TokenList;
  diagnostics: Diagnostic[];
}

export function parse(source: string, options?: ParseOptions): ParseResult;

/** Every token kind by name, `tokens.kind(i) === TokenKind.Arrow`. */
export const TokenKind: TokenKindMap;
export type TokenKind = TokenKindMap[keyof TokenKindMap];

export { langFromPath, sourceTypeFromPath } from "yuku-core";

export type { ParseOptions, ParseResult };
