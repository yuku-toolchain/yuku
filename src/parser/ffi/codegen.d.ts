import type { Comment, Program } from "@yuku-toolchain/types";

/** Whitespace mode for the generated output. */
export type Format = "pretty" | "compact";

/**
 * Quote style for emitted string literals.
 *
 * - `"preserve"`: keep each literal's original quote style (single vs double);
 *   synthetic nodes default to double.
 * - `"double"` / `"single"`: force that quote.
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
   * The original source text. Required to emit a map; without it, `map` is
   * `null`.
   */
  source: string;
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
   * `source` (the original source text) is required. The rest of the metadata
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
