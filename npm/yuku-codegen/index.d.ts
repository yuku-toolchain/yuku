import type { Comment, Program } from "@yuku-toolchain/types";

/** Whitespace mode for the generated output. */
export type Format = "pretty" | "compact";

/**
 * Quote style for string literals. `"preserve"` keeps each literal's source
 * quote; `"shortest"` picks the quote with fewer escapes, double on a tie.
 */
export type Quotes = "preserve" | "double" | "single" | "shortest";

/**
 * Comment passthrough filter. `"some"` emits legal headers, JSDoc, and
 * `@__*__` annotations; `true`/`false` are sugar for `"all"`/`"none"`.
 */
export type Comments = boolean | "all" | "some" | "none" | "line" | "block";

export type { Comment };

/** Source-map configuration. Pass to `GenerateOptions.sourceMap` to enable. */
export interface SourceMapOptions {
  /** The original source text. Required to emit a map. */
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

/** Minification switches. Enabled switches override `format` and `quotes`. */
export interface MinifyOptions {
  /** Emit compact whitespace. @default false */
  whitespace?: boolean;
  /**
   * Apply size-reducing syntax rewrites (`!0`, `void 0`, `1e6`, `obj.foo`, ...).
   * @default false
   */
  syntax?: boolean;
  /** Use shortest quotes. @default false */
  quotes?: boolean;
}

/** Options for `generate`. Transformations are independent flags and compose freely. */
export interface GenerateOptions {
  /**
   * Drop TypeScript-only syntax and emit plain JavaScript. Constructs with no
   * JavaScript equivalent (`enum`, `namespace`, ...) are reported in `errors`
   * and elided.
   * @default false
   */
  strip?: boolean;
  /**
   * `true` enables every {@link MinifyOptions} switch for maximum
   * minification; pass an object for fine-grained control.
   * @default false
   */
  minify?: boolean | MinifyOptions;
  /** @default "pretty" */
  format?: Format;
  /**
   * Spaces per indentation level in pretty format.
   * @default 2
   */
  indent?: number;
  /** @default "preserve" */
  quotes?: Quotes;
  /** @default "some" */
  comments?: Comments;
  /** Pass to emit a Source Map V3. Omit to disable. */
  sourceMap?: SourceMapOptions;
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
export interface GenerateResult {
  code: string;
  /** Empty when codegen succeeded cleanly. */
  errors: Diagnostic[];
  /** `null` unless `sourceMap` was enabled. */
  map: SourceMap | null;
}

/** Renders the AST back to source code. */
export function generate(program: Program, options?: GenerateOptions): GenerateResult;

/** @deprecated Options type of the deprecated entry points. */
export type CodegenOptions = GenerateOptions & {
  /** @deprecated Use `sourceMap`. */
  sourceMaps?: SourceMapOptions;
};

/** @deprecated Use {@link GenerateResult}. */
export type CodegenResult = GenerateResult;

/** @deprecated Use {@link generate}. */
export function print(program: Program, options?: CodegenOptions): GenerateResult;

/** @deprecated Use `generate(program, { strip: true, ... })`. */
export function strip(program: Program, options?: CodegenOptions): GenerateResult;

/** @deprecated Use `generate(program, { minify: true, ... })`. */
export function minify(program: Program, options?: CodegenOptions): GenerateResult;
