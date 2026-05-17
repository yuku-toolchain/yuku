import type { Program } from "yuku-parser";

/** Whitespace mode for the generated output. */
type Format = "pretty" | "compact";

/** Quote style for emitted string literals. */
type Quotes = "double" | "single";

/**
 * Codegen mode.
 *
 * - `"print"`: render the AST verbatim, preserving TypeScript syntax.
 * - `"strip"`: render the AST as JavaScript, dropping TypeScript-specific
 *   syntax (type annotations, declarations, etc).
 * - `"minify"`: apply size-reducing substitutions during emit. Combine with
 *   `format: "compact"` for fully minified output.
 */
type Mode = "print" | "strip" | "minify";

/** Output format configuration. */
interface FormatOptions {
  /**
   * Whitespace mode.
   * @default "pretty"
   */
  format?: Format;
  /**
   * Spaces per indentation level. Only used when `format === "pretty"`.
   * @default 2
   */
  indent?: number;
  /**
   * Quote style for emitted string literals.
   * @default "double"
   */
  quotes?: Quotes;
}

/** Options for {@link generate}. */
interface GenerateOptions {
  /**
   * Codegen mode.
   * @default "print"
   */
  mode?: Mode;
  /** Output format configuration. */
  format?: FormatOptions;
}

/** A codegen-detected problem in the input AST. */
interface Diagnostic {
  message: string;
  /** Byte offset. */
  start: number;
  /** Byte offset. */
  end: number;
}

/** Result of a codegen run. */
interface GenerateResult {
  /** Generated source code. */
  code: string;
  /** Codegen-detected problems. Empty when codegen succeeded cleanly. */
  errors: Diagnostic[];
}

/**
 * Generates source code from an ESTree AST.
 */
export function generate(estree: Program, options?: GenerateOptions): GenerateResult;

export type { GenerateOptions, FormatOptions, GenerateResult, Diagnostic, Format, Quotes, Mode };
