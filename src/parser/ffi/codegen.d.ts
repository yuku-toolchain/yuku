import type { Program } from "yuku-parser";

/** Whitespace mode for the generated output. */
export type Format = "pretty" | "compact";

/** Quote style for emitted string literals. */
export type Quotes = "double" | "single";

/** Source-map configuration. Pass to `CodegenOptions.sourceMaps` to enable. */
export interface SourceMapOptions {
  /** Original source the AST was parsed from. Required. */
  source: string;
  /** Output filename, embedded as the map's `file`. */
  file?: string;
  /** Source filename, embedded as the single entry of `sources`. */
  sourceFileName?: string;
  /** Prefix embedded as `sourceRoot`. */
  sourceRoot?: string;
  /**
   * When true, embeds `source` into `sourcesContent`.
   * @default false
   */
  sourcesContent?: boolean;
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
  /** @default "double" */
  quotes?: Quotes;
  /** Pass to enable source maps. Omit to disable. */
  sourceMaps?: SourceMapOptions;
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
export function print(estree: Program, options?: CodegenOptions): CodegenResult;

/** Renders the AST as JavaScript, dropping TypeScript-specific syntax. */
export function strip(estree: Program, options?: CodegenOptions): CodegenResult;

/**
 * Renders the AST with size-reducing rewrites. Combine with
 * `format: "compact"` for full minification.
 */
export function minify(estree: Program, options?: CodegenOptions): CodegenResult;
