import { SourceFile } from "../file/source-file";
import { assertNode } from "../traverse/walk";
import type { Diagnostic, Node } from "../types";
import { printNode, type PrinterOptions, type Printers } from "./printer";
import { encodeMappings, Mappings } from "./sourcemap";

export type Format = "pretty" | "compact";

/** Quote style for string literals. `"shortest"` picks the one needing fewer escapes. */
export type Quotes = "preserve" | "double" | "single" | "shortest";

/** Which comments to print. `"some"` keeps legal comments, JSDoc and `@` or `#` annotations. */
export type Comments = "all" | "some" | "none" | "line" | "block";

export type { Printer, Printers } from "./printer";

export interface SourceMapOptions {
  /** Output filename, embedded as the map's `file`. */
  file?: string;
  /** Prefix embedded as `sourceRoot`. */
  sourceRoot?: string;
  /** @default false */
  sourcesContent?: boolean;
}

export interface PrintOptions {
  /**
   * Drop TypeScript-only syntax and emit plain JavaScript. Constructs with no JavaScript
   * equivalent (`enum`, `namespace`, ...) are reported in `diagnostics` and elided.
   * @default false
   */
  strip?: boolean;
  /** Rewrite syntax to shorter forms, such as `!0` and `obj.foo`. @default false */
  minify?: boolean;
  /** @default "pretty" */
  format?: Format;
  /** Spaces per indentation level in pretty format, from 0 to 255. @default 2 */
  indent?: number;
  /** @default "preserve" */
  quotes?: Quotes;
  /** @default "all" */
  comments?: Comments;
  /** Needs a file, for its source and path. @default false */
  sourceMap?: boolean | SourceMapOptions;
  /** Prints the node types the printer does not know, such as a dialect's, by `type`. */
  printers?: Printers;
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

export interface PrintResult {
  code: string;
  diagnostics: Diagnostic[];
  map: SourceMap | null;
}

const QUOTES = ["preserve", "double", "single", "shortest"] as const;
const COMMENTS = ["none", "all", "some", "line", "block"] as const;

/** Prints a file, or any single node, back to source. */
export function print(target: SourceFile | Node, options: PrintOptions = {}): PrintResult {
  const file = target instanceof SourceFile ? target : null;
  const node = target instanceof SourceFile ? target.program : target;
  assertNode(node, "print");
  const printerOptions = resolveOptions(options, file?.path ?? null);
  const sourceMap = options.sourceMap ?? false;
  if (sourceMap === false) return { ...printNode(node, printerOptions, null), map: null };
  if (file === null) throw new TypeError("a source map needs the source, so print the file");
  const { source, path } = file;
  const mappings = new Mappings(Math.max(source.length >>> 3, 1024));
  const { code, diagnostics } = printNode(node, printerOptions, mappings);
  const settings: SourceMapOptions = sourceMap === true ? {} : sourceMap;
  const map: SourceMap = {
    version: 3,
    file: settings.file ?? null,
    sourceRoot: settings.sourceRoot ?? null,
    sources: [path ?? ""],
    sourcesContent: settings.sourcesContent === true ? [source] : null,
    names: [],
    mappings: encodeMappings(code, source, mappings),
  };
  return { code, diagnostics, map };
}

function resolveOptions(options: PrintOptions, path: string | null): PrinterOptions {
  const format = options.format ?? "pretty";
  if (format !== "pretty" && format !== "compact") {
    throw new TypeError('`format` must be "pretty" or "compact"');
  }

  const indent = options.indent ?? 2;
  if (!Number.isInteger(indent) || indent < 0 || indent > 255) {
    throw new RangeError("`indent` must be an integer from 0 to 255");
  }

  const quotes = options.quotes ?? "preserve";
  if (!QUOTES.includes(quotes)) {
    throw new TypeError('`quotes` must be "preserve", "double", "single", or "shortest"');
  }

  const comments = options.comments ?? "all";
  if (!COMMENTS.includes(comments)) {
    throw new TypeError('`comments` must be "all", "some", "none", "line", or "block"');
  }

  const printers = options.printers ?? {};
  if (typeof printers !== "object" || printers === null) {
    throw new TypeError("`printers` must be an object of functions by node type");
  }

  return {
    strip: options.strip === true,
    minify: options.minify === true,
    pretty: format === "pretty",
    indent,
    quotes,
    comments,
    printers,
    path,
  };
}
