import type { Program } from "yuku-parser";

/** Whitespace mode for the generated output. */
type Format = "pretty" | "compact";

/** Quote style for emitted string literals. */
type Quotes = "double" | "single";

/** Codegen options shared by `print`, `strip`, and `minify`. */
interface CodegenOptions {
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

/** A codegen-detected problem in the input AST. */
interface Diagnostic {
  message: string;
  /** Byte offset. */
  start: number;
  /** Byte offset. */
  end: number;
}

/** Result of a codegen run. */
interface CodegenResult {
  /** Generated source code. */
  code: string;
  /** Codegen-detected problems. Empty when codegen succeeded cleanly. */
  errors: Diagnostic[];
}

/** Renders the AST verbatim, preserving TypeScript syntax. */
export function print(estree: Program, options?: CodegenOptions): CodegenResult;

/** Renders the AST as JavaScript, dropping TypeScript-specific syntax. */
export function strip(estree: Program, options?: CodegenOptions): CodegenResult;

/** Renders the AST with size-reducing substitutions. Combine with `format: "compact"` for fully minified output. */
export function minify(estree: Program, options?: CodegenOptions): CodegenResult;

export type { CodegenOptions, CodegenResult, Diagnostic, Format, Quotes };
