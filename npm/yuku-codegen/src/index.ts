import type { Comment, Diagnostic, Program } from "@yuku-toolchain/types";

import { print, type PrintOptions } from "./printer.js";
import { encodeMappings, Mappings } from "./sourcemap.js";

/** Whitespace mode for the generated output. */
export type Format = "pretty" | "compact";

/**
 * Quote style for string literals. `"preserve"` keeps each literal's source quote,
 * `"shortest"` picks the quote with fewer escapes, double on a tie.
 */
export type Quotes = "preserve" | "double" | "single" | "shortest";

/**
 * Comment passthrough filter. `"some"` emits legal headers, JSDoc, and `@`/`#` annotations,
 * `true` and `false` are sugar for `"all"` and `"none"`.
 */
export type Comments = boolean | "all" | "some" | "none" | "line" | "block";

export type { Comment, Diagnostic };

/** Source map configuration. Pass to `GenerateOptions.sourceMap` to enable. */
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
  /** Apply size-reducing syntax rewrites (`!0`, `1e6`, `obj.foo`, ...). @default false */
  syntax?: boolean;
  /** Use shortest quotes. @default false */
  quotes?: boolean;
}

/** Options for `generate`. Transformations are independent flags and compose freely. */
export interface GenerateOptions {
  /**
   * Drop TypeScript-only syntax and emit plain JavaScript. Constructs with no JavaScript
   * equivalent (`enum`, `namespace`, ...) are reported in `diagnostics` and elided.
   * @default false
   */
  strip?: boolean;
  /**
   * `true` enables every {@link MinifyOptions} switch for maximum minification, pass an object
   * for fine-grained control.
   * @default false
   */
  minify?: boolean | MinifyOptions;
  /** @default "pretty" */
  format?: Format;
  /** Spaces per indentation level in pretty format, from 0 to 255. @default 2 */
  indent?: number;
  /** @default "preserve" */
  quotes?: Quotes;
  /** @default "some" */
  comments?: Comments;
  /** Pass to emit a Source Map V3. Omit to disable. */
  sourceMap?: SourceMapOptions;
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
  diagnostics: Diagnostic[];
  /** `null` unless `sourceMap` was enabled. */
  map: SourceMap | null;
}

const QUOTES: readonly Quotes[] = ["preserve", "double", "single", "shortest"];
const COMMENTS = ["none", "all", "some", "line", "block"] as const;

/** Renders the AST back to source code. */
export function generate(program: Program, options: GenerateOptions = {}): GenerateResult {
  if (program?.type !== "Program") {
    throw new TypeError("Expected a `Program` node, such as `parse(source).program`");
  }
  const printOptions = resolveOptions(options);
  const sourceMap = options.sourceMap;
  if (sourceMap == null) return { ...print(program, printOptions, null), map: null };
  if (typeof sourceMap.source !== "string") {
    throw new TypeError("`sourceMap.source` must be the original source text");
  }
  const mappings = new Mappings(Math.max(sourceMap.source.length >>> 3, 1024));
  const { code, diagnostics } = print(program, printOptions, mappings);
  const map: SourceMap = {
    version: 3,
    file: sourceMap.file ?? null,
    sourceRoot: sourceMap.sourceRoot ?? null,
    sources: [sourceMap.sourceFileName ?? ""],
    sourcesContent: sourceMap.sourcesContent != null ? [sourceMap.sourcesContent] : null,
    names: [],
    mappings: encodeMappings(code, sourceMap.source, mappings),
  };
  return { code, diagnostics, map };
}

function resolveOptions(options: GenerateOptions): PrintOptions {
  const minify =
    options.minify === true
      ? { whitespace: true, syntax: true, quotes: true }
      : options.minify || {};

  const format = minify.whitespace === true ? "compact" : (options.format ?? "pretty");
  if (format !== "pretty" && format !== "compact") {
    throw new TypeError('`format` must be "pretty" or "compact"');
  }

  const indent = options.indent ?? 2;
  if (!Number.isInteger(indent) || indent < 0 || indent > 255) {
    throw new RangeError("`indent` must be an integer from 0 to 255");
  }

  const quotes = minify.quotes === true ? "shortest" : (options.quotes ?? "preserve");
  if (!QUOTES.includes(quotes)) {
    throw new TypeError('`quotes` must be "preserve", "double", "single", or "shortest"');
  }

  const comments =
    options.comments === true
      ? "all"
      : options.comments === false
        ? "none"
        : (options.comments ?? "some");
  if (!COMMENTS.includes(comments)) {
    throw new TypeError(
      '`comments` must be a boolean or "all", "some", "none", "line", or "block"',
    );
  }

  return {
    strip: options.strip === true,
    minify: minify.syntax === true,
    pretty: format === "pretty",
    indent,
    quotes,
    comments,
  };
}
