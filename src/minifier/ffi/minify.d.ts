/** How the source code should be parsed. */
type SourceType = "script" | "module";

/** Language variant of the source code. */
type SourceLang = "js" | "ts" | "jsx" | "tsx" | "dts";

/** Quote style for emitted string literals. */
type Quotes = "double" | "single";

/** Identifier mangling configuration. */
interface MangleOptions {
  /**
   * Rename local bindings to short names.
   * @default true
   */
  enabled?: boolean;
  /**
   * Skip renaming `function foo()` declarations and named function expressions.
   * @default false
   */
  keepFnames?: boolean;
  /**
   * Skip renaming `class Foo {}` declarations and named class expressions.
   * @default false
   */
  keepClassnames?: boolean;
}

/** Output format configuration. */
interface FormatOptions {
  /**
   * Quote style for emitted string literals.
   * @default "double"
   */
  quotes?: Quotes;
}

/** Options for `minify`. */
interface MinifyOptions {
  /**
   * Parse as a classic script or an ES module.
   * @default "module"
   */
  sourceType?: SourceType;
  /**
   * Language variant of the input source.
   * @default "js"
   */
  lang?: SourceLang;
  /**
   * Identifier mangling configuration.
   */
  mangle?: MangleOptions;
  /**
   * Output format configuration.
   */
  format?: FormatOptions;
}

/** A codegen-detected problem in the input source. */
interface Diagnostic {
  message: string;
  /** Byte offset. */
  start: number;
  /** Byte offset. */
  end: number;
}

interface MinifyResult {
  /** Minified JavaScript. */
  code: string;
  /** Codegen-detected problems. Empty when codegen succeeded cleanly. */
  errors: Diagnostic[];
}

/**
 * Minifies `source`, returning compact JavaScript.
 */
export function minify(source: string, options?: MinifyOptions): MinifyResult;

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

export type {
  MinifyOptions,
  MangleOptions,
  FormatOptions,
  MinifyResult,
  Diagnostic,
  SourceType,
  SourceLang,
  Quotes,
};
