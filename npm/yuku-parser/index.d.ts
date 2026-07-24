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

/** Options for configuring the parser. */
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
   * Also return the token stream as {@link ParseResult.tokens}. Comments
   * stay separate in {@link ParseResult.comments}. Zero cost when disabled.
   * @default false
   */
  tokens?: boolean;
}

/** Espree-style token type names. */
type TokenType =
  | "Identifier"
  | "Keyword"
  | "Punctuator"
  | "String"
  | "Numeric"
  | "RegularExpression"
  | "Template"
  | "Boolean"
  | "Null"
  | "PrivateIdentifier"
  | "JSXIdentifier"
  | "JSXText";

/**
 * A token with its raw source text and span. Tokens are in source order
 * and never overlap. Offsets are UTF-16 code units, like every other span
 * the package exposes.
 */
interface Token {
  type: TokenType;
  /** The raw source text of the token. */
  value: string;
  start: number;
  end: number;
}

/** The result returned by the parser. */
interface ParseResult {
  /** Root ESTree/TypeScript-ESTree AST node. */
  program: Program;
  /** Every comment in source order, each with its source span. */
  comments: Comment[];
  /** Syntax diagnostics, and semantic diagnostics when {@link ParseOptions.semanticErrors} is enabled. */
  diagnostics: Diagnostic[];
  /** Every token in source order. Present only when {@link ParseOptions.tokens} is enabled. */
  tokens?: Token[];
}

/**
 * Parse JS/TS source code and return an ESTree / TypeScript-ESTree compatible AST.
 */
export function parse(source: string, options?: ParseOptions): ParseResult;

// Deprecated walking surface. Walking moved to the yuku-ast package,
// these delegate there and will be removed in an upcoming minor version.

/** @deprecated Import `WalkHandler` from the yuku-ast package instead. */
type WalkHandler<T extends Node = Node, S = unknown> = (node: T, ctx: WalkContext<T, S>) => void;

/** @deprecated Import `WalkHooks` from the yuku-ast package instead. */
interface WalkHooks<T extends Node = Node, S = unknown> {
  enter?: WalkHandler<T, S>;
  leave?: WalkHandler<T, S>;
}

/** @deprecated Import `Visitors` from the yuku-ast package instead. */
type Visitors<S = unknown> = {
  [K in NodeType]?: WalkHandler<NodeOfType<K>, S> | WalkHooks<NodeOfType<K>, S>;
} & {
  enter?: WalkHandler<Node, S>;
  leave?: WalkHandler<Node, S>;
};

/**
 * @deprecated Walking moved to the yuku-ast package. Install yuku-ast
 * and import `walk` from there. Removed in an upcoming minor version.
 */
export function walk<T extends Node, S = unknown>(root: T, visitors: Visitors<S>, state?: S): T;

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
 * - `.cjs`, `.cts` → `"commonjs"`
 * - everything else → `"module"`
 */
export function sourceTypeFromPath(path: string): SourceType;

export type {
  ParseOptions,
  ParseResult,
  Token,
  TokenType,
  Visitors,
  WalkHandler,
  WalkHooks,
};
