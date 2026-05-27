import type { Comment, Program } from "yuku-parser";

/** Whitespace mode for the generated output. */
export type Format = "pretty" | "compact";

/**
 * Quote style for emitted string literals.
 *
 * - `"preserve"`: reuse each literal's raw source text verbatim (quotes and
 *   escapes exactly as written).
 * - `"double"` / `"single"`: re-escape from the decoded value using that quote.
 */
export type Quotes = "preserve" | "double" | "single";

/**
 * Comment passthrough filter.
 *
 * - `false`: drop all comments.
 * - `true` or `"all"`: emit every comment.
 * - `"some"`: emit legal headers, JSDoc, and `@__*__` annotations.
 * - `"line"`: emit `// ...` only.
 * - `"block"`: emit block comments only.
 */
export type Comments = boolean | "all" | "some" | "line" | "block";

export type { Comment };

/** Source-map configuration. Pass to `CodegenOptions.sourceMaps` to enable. */
export interface SourceMapOptions {
  /**
   * UTF-16 line-start offsets, taken straight from the parser's
   * `ParseResult.lineStarts`. Required: this is what maps generated positions
   * back to the original source.
   */
  lineStarts: number[];
  /** Output filename, embedded as the map's `file`. */
  file?: string;
  /** Source filename, embedded as the single entry of `sources`. */
  sourceFileName?: string;
  /** Prefix embedded as `sourceRoot`. */
  sourceRoot?: string;
  /** When set, embedded as the single entry of the map's `sourcesContent`. */
  sourcesContent?: string;
}

/** Codegen options shared by `print`, `strip`, and `minify`. */
export interface CodegenOptions {
  /** @default "pretty" */
  format?: Format;
  /**
   * Spaces per indentation level. Used only when `format = "pretty"`.
   * @default 2
   */
  indent?: number;
  /** @default "preserve" */
  quotes?: Quotes;
  /**
   * Enable Source Map V3 output. Pass a {@link SourceMapOptions} object. Its
   * `lineStarts` (from the parser) is required. The rest of the metadata
   * (`file`, `sources`, `sourcesContent`, `sourceRoot`) is optional. Omit to
   * disable.
   * @default undefined
   */
  sourceMaps?: SourceMapOptions;
  /**
   * Comment passthrough filter. Defaults to `"some"`, which preserves
   * legal headers, JSDoc, and tree-shaking annotations.
   * @default "some"
   */
  comments?: Comments;
}

/** A codegen-detected problem in the input AST. */
export interface Diagnostic {
  message: string;
  /** Byte offset where the problem starts. */
  start: number;
  /** Byte offset where the problem ends. */
  end: number;
}

/** Source Map V3. */
export interface SourceMap {
  version: 3;
  file: string | null;
  sourceRoot: string | null;
  sources: string[];
  sourcesContent: (string | null)[] | null;
  names: string[];
  /** VLQ-encoded mappings string. */
  mappings: string;
}

/** Result of a codegen run. */
export interface CodegenResult {
  code: string;
  /** Empty when codegen succeeded cleanly. */
  errors: Diagnostic[];
  /** `null` unless `sourceMaps` was enabled. */
  map: SourceMap | null;
}

/** Renders the AST verbatim, preserving TypeScript syntax. */
export function print(program: Program, options?: CodegenOptions): CodegenResult;

/** Renders the AST as JavaScript, dropping TypeScript-specific syntax. */
export function strip(program: Program, options?: CodegenOptions): CodegenResult;

/**
 * Renders the AST with size-reducing rewrites. Combine with
 * `format: "compact"` for full minification.
 */
export function minify(program: Program, options?: CodegenOptions): CodegenResult;
