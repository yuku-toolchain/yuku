import type { Comment, Diagnostic, Program } from "@yuku-toolchain/types";

import { print, type PrintOptions } from "./printer.js";
import { encodeMappings, Mappings } from "./sourcemap.js";
import { isJSXFactoryName } from "./jsx.js";

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

/** Classic JSX runtime configuration. Factory bindings must already be in scope. */
export interface JSXOptions {
  /** @default "classic" */
  runtime?: "classic";
  /** Identifier or dotted name used to construct elements. @default "React.createElement" */
  pragma?: string;
  /** Identifier or dotted name used for fragments. @default "React.Fragment" */
  pragmaFrag?: string;
  /**
   * Annotate factory calls with a `__PURE__` comment so bundlers can drop unused
   * elements.
   * @default `true` for the React factories, `false` for custom factories
   */
  pure?: boolean;
}

export interface GenerateOptions {
  /**
   * Drop TypeScript-only syntax and emit plain JavaScript. Constructs with no JavaScript
   * equivalent (`enum`, `namespace`, ...) are reported in `diagnostics` and elided.
   * @default false
   */
  strip?: boolean;
  /** Preserve JSX, or lower it with a configurable classic runtime. @default false */
  jsx?: boolean | "preserve" | JSXOptions;
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
  sourceMap?: SourceMapOptions;
}

export interface SourceMap {
  version: 3;
  file: string | null;
  sourceRoot: string | null;
  sources: string[];
  sourcesContent: (string | null)[] | null;
  names: string[];
  mappings: string;
}

export interface GenerateResult {
  code: string;
  diagnostics: Diagnostic[];
  map: SourceMap | null;
}

const QUOTES: readonly Quotes[] = ["preserve", "double", "single", "shortest"];
const COMMENTS = ["none", "all", "some", "line", "block"] as const;
const DEFAULT_JSX = {
  pragma: "React.createElement",
  pragmaFrag: "React.Fragment",
  pure: true,
};

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
    jsx: resolveJSX(options.jsx),
    minify: minify.syntax === true,
    pretty: format === "pretty",
    indent,
    quotes,
    comments,
  };
}

function resolveJSX(jsx: GenerateOptions["jsx"]): PrintOptions["jsx"] {
  if (jsx === undefined || jsx === false || jsx === "preserve") return null;
  if (jsx === true) return DEFAULT_JSX;
  if (jsx === null || typeof jsx !== "object" || Array.isArray(jsx)) {
    throw new TypeError('`jsx` must be a boolean, "preserve", or a JSX options object');
  }
  if (jsx.runtime !== undefined && jsx.runtime !== "classic") {
    throw new TypeError('`jsx.runtime` must be "classic"');
  }
  const pragma = jsx.pragma === undefined ? DEFAULT_JSX.pragma : jsx.pragma;
  const pragmaFrag = jsx.pragmaFrag === undefined ? DEFAULT_JSX.pragmaFrag : jsx.pragmaFrag;
  if (!isJSXFactoryName(pragma) || !isJSXFactoryName(pragmaFrag)) {
    throw new TypeError('`jsx.pragma` and `jsx.pragmaFrag` must be identifiers or dotted names');
  }
  if (jsx.pure !== undefined && typeof jsx.pure !== "boolean") {
    throw new TypeError("`jsx.pure` must be a boolean");
  }
  // only the bundled factories are known-pure entry points
  const pure = jsx.pure ??
    (pragma === DEFAULT_JSX.pragma && pragmaFrag === DEFAULT_JSX.pragmaFrag);
  return { pragma, pragmaFrag, pure };
}
