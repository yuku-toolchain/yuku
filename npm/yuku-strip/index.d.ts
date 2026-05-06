/** How the source code should be parsed. */
type SourceType = "script" | "module";

/** Language variant of the source code. */
type SourceLang = "js" | "ts" | "jsx" | "tsx" | "dts";

/** Whitespace mode for the output. */
type Format = "pretty" | "compact";

/** Quote style for emitted string literals. */
type Quotes = "double" | "single";

/** Source map output mode. */
type SourceMapMode = "none" | "v3";

/** Options for `strip`. */
interface StripOptions {
  /**
   * Parse as a classic script or an ES module.
   * @default "module"
   */
  sourceType?: SourceType;
  /**
   * Language variant of the input source.
   * @default "ts"
   */
  lang?: SourceLang;
  /**
   * Output whitespace mode.
   * @default "pretty"
   */
  format?: Format;
  /**
   * Spaces per indentation level (used only when `format === "pretty"`).
   * @default 2
   */
  indent?: number;
  /**
   * Quote style for emitted string literals.
   * @default "double"
   */
  quotes?: Quotes;
  /**
   * Append a trailing newline to the output if missing.
   * @default true
   */
  finalNewline?: boolean;
  /**
   * Source map output mode.
   * @default "none"
   */
  sourcemap?: SourceMapMode;
  /**
   * Filename to record in the source map's `sources`.
   * @default "input"
   */
  sourceFilename?: string;
  /**
   * Whether to embed the original source bytes in `sourcesContent`.
   * @default true
   */
  sourceContent?: boolean;
}

/** A codegen-detected problem in the input source. */
interface Diagnostic {
  message: string;
  /** Byte offset. */
  start: number;
  /** Byte offset. */
  end: number;
}

/** A v3 source map ready for `.map` files or browser dev tools. */
interface SourceMap {
  version: 3;
  file?: string;
  sourceRoot?: string;
  sources: string[];
  sourcesContent?: (string | null)[];
  names: string[];
  /** Base64 VLQ-encoded mappings. */
  mappings: string;
}

interface StripResult {
  /** Generated JavaScript. */
  code: string;
  /** Source map, when `sourcemap !== "none"`. */
  map: SourceMap | null;
  /** Codegen-detected problems. Empty when codegen succeeded cleanly. */
  errors: Diagnostic[];
}

/**
 * Strips TypeScript syntax from `source`, returning JavaScript.
 */
export function strip(source: string, options?: StripOptions): StripResult;

export type { StripOptions, StripResult, SourceMap, Diagnostic, SourceType, SourceLang, Format, Quotes, SourceMapMode };
