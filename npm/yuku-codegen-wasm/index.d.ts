/** Minification switches. */
export interface MinifyOptions {
  /** Emit compact whitespace. */
  whitespace?: boolean;
  /** Apply size-reducing syntax rewrites. */
  syntax?: boolean;
  /** Use shortest quotes. */
  quotes?: boolean;
}

/** Options for `generate`. Transformations are independent flags. */
export interface GenerateOptions {
  /** Drop TypeScript-only syntax and emit plain JavaScript. */
  strip?: boolean;
  /** `true` enables every switch for maximum minification. */
  minify?: boolean | MinifyOptions;
  /** @default "pretty" */
  format?: "pretty" | "compact";
  /** Spaces per indentation level in pretty format. @default 2 */
  indent?: number;
  /** @default "preserve" */
  quotes?: "preserve" | "double" | "single" | "shortest";
  /** @default "some" */
  comments?: boolean | "all" | "some" | "none" | "line" | "block";
}

/** Renders the AST back to source code. */
export function generate(program: any, options?: GenerateOptions): string;

/** @deprecated Use {@link generate}. */
export function print(program: any): string;
/** @deprecated Use `generate(program, { strip: true })`. */
export function strip(program: any): string;
/** @deprecated Use `generate(program, { minify: true })`. */
export function minify(program: any): string;
