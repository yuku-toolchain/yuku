/** How the source code should be parsed. */
type SourceType = "script" | "module";

/** Language variant of the source code. */
type SourceLang = "js" | "ts" | "jsx" | "tsx" | "dts";

/** Whitespace mode for the output. */
type Format = "pretty" | "compact";

/** Quote style for emitted string literals. */
type Quotes = "double" | "single";

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
}

/** A codegen-detected problem in the input source. */
interface Diagnostic {
  message: string;
  /** Byte offset. */
  start: number;
  /** Byte offset. */
  end: number;
}

interface StripResult {
  /** Generated JavaScript. */
  code: string;
  /** Codegen-detected problems. Empty when codegen succeeded cleanly. */
  errors: Diagnostic[];
}

/**
 * Strips TypeScript syntax from `source`, returning JavaScript.
 */
export function strip(source: string, options?: StripOptions): StripResult;

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

export type { StripOptions, StripResult, Diagnostic, SourceType, SourceLang, Format, Quotes };
