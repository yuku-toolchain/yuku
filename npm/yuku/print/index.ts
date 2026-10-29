import { assertNode } from "../ast/walk";
import { decodeDiagnostics } from "../core/decode";
import { encode } from "../core/encode";
import { isTs, shippedCore, treeFlags } from "../core/read";
import { SourceFile } from "../file/source-file";
import type { Diagnostic, Node } from "../types";

export type Format = "pretty" | "compact";

/** Quote style for string literals. `"shortest"` picks the one needing fewer escapes. */
export type Quotes = "preserve" | "double" | "single" | "shortest";

/** Which comments to print. `"some"` keeps legal comments, JSDoc and `@` or `#` annotations. */
export type Comments = "all" | "some" | "none" | "line" | "block";

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
  /**
   * Rewrite syntax to shorter forms, such as `!0` and `obj.foo`, and default to the compact
   * format, the shortest quotes, and `"some"` comments.
   * @default false
   */
  minify?: boolean;
  /** @default "pretty", or "compact" when minifying */
  format?: Format;
  /** Spaces per indentation level in pretty format, from 0 to 255. @default 2 */
  indent?: number;
  /** @default "preserve", or "shortest" when minifying */
  quotes?: Quotes;
  /** @default "all", or "some" when minifying */
  comments?: Comments;
  /** Needs a file, for its source and path. @default false */
  sourceMap?: boolean | SourceMapOptions;
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
const OPTIONS: ReadonlySet<string> = new Set([
  "strip",
  "minify",
  "format",
  "indent",
  "quotes",
  "comments",
  "sourceMap",
]);
const SOURCE_MAP_OPTIONS: ReadonlySet<string> = new Set(["file", "sourceRoot", "sourcesContent"]);

const STRIP = 1 << 0;
const MINIFY = 1 << 1;
const COMPACT = 1 << 2;
const SOURCE_MAP = 1 << 3;
const QUOTES_SHIFT = 4;
const COMMENTS_SHIFT = 6;
const INDENT_SHIFT = 9;
const RESULT_HEADER_BYTES = 12;

const utf8 = new TextDecoder("utf-8", { ignoreBOM: true });

/** Prints a file, or any single node, back to source. */
export function print(target: SourceFile | Node, options: PrintOptions = {}): PrintResult {
  const file = target instanceof SourceFile ? target : null;
  const node = file === null ? target : file.program;
  assertNode(node, "print");
  const settings = sourceMapOf(options.sourceMap ?? false);
  let bits = optionsOf(options);
  if (settings !== null) {
    if (file === null) throw new TypeError("a source map needs the source, so print the file");
    bits |= SOURCE_MAP;
  }
  if (node.type === "Hashbang") return { code: `#!${node.value}`, diagnostics: [], map: null };

  const tree = encode(node, file !== null && isTs(file.lang), false);
  const core = file === null ? shippedCore : file._core;
  const flags = file === null ? 0 : treeFlags(file.lang, file.sourceType);
  const source = settings === null ? "" : file!.source;
  const buffer = core.backend().runTree("printTree", tree, source, flags, bits);

  const header = new DataView(buffer, 0, RESULT_HEADER_BYTES);
  const codeEnd = RESULT_HEADER_BYTES + header.getUint32(0, true);
  const mappingsEnd = codeEnd + header.getUint32(4, true);
  const bytes = new Uint8Array(buffer);
  const path = file?.path ?? null;
  const code = utf8.decode(bytes.subarray(RESULT_HEADER_BYTES, codeEnd));
  const count = header.getUint32(8, true);
  const diagnostics = decodeDiagnostics(buffer, (mappingsEnd + 3) & ~3, count, path);
  if (settings === null) return { code, diagnostics, map: null };
  const map: SourceMap = {
    version: 3,
    file: settings.file ?? null,
    sourceRoot: settings.sourceRoot ?? null,
    sources: [path ?? ""],
    sourcesContent: settings.sourcesContent === true ? [source] : null,
    names: [],
    mappings: utf8.decode(bytes.subarray(codeEnd, mappingsEnd)),
  };
  return { code, diagnostics, map };
}

function sourceMapOf(sourceMap: boolean | SourceMapOptions): SourceMapOptions | null {
  if (sourceMap === false) return null;
  if (sourceMap === true) return {};
  if (typeof sourceMap !== "object" || sourceMap === null) {
    throw new TypeError("`sourceMap` must be true, false, or an object of options");
  }
  for (const key in sourceMap) {
    if (!SOURCE_MAP_OPTIONS.has(key)) throw new TypeError(`\`${key}\` is not a sourceMap option`);
  }
  const content = sourceMap.sourcesContent;
  if (content !== undefined && typeof content !== "boolean") {
    throw new TypeError("`sourcesContent` must be true or false");
  }
  return sourceMap;
}

function optionsOf(options: PrintOptions): number {
  for (const key in options) {
    if (!OPTIONS.has(key)) throw new TypeError(`\`${key}\` is not a print option`);
  }

  const minify = options.minify ?? false;
  if (typeof minify !== "boolean") throw new TypeError("`minify` must be true or false");

  const format = options.format ?? (minify ? "compact" : "pretty");
  if (format !== "pretty" && format !== "compact") {
    throw new TypeError('`format` must be "pretty" or "compact"');
  }

  const indent = options.indent ?? 2;
  if (!Number.isInteger(indent) || indent < 0 || indent > 255) {
    throw new RangeError("`indent` must be an integer from 0 to 255");
  }

  const quotes = QUOTES.indexOf(options.quotes ?? (minify ? "shortest" : "preserve"));
  if (quotes < 0) {
    throw new TypeError('`quotes` must be "preserve", "double", "single", or "shortest"');
  }

  const comments = COMMENTS.indexOf(options.comments ?? (minify ? "some" : "all"));
  if (comments < 0) {
    throw new TypeError('`comments` must be "all", "some", "none", "line", or "block"');
  }

  let bits = (quotes << QUOTES_SHIFT) | (comments << COMMENTS_SHIFT) | (indent << INDENT_SHIFT);
  if (options.strip === true) bits |= STRIP;
  if (minify) bits |= MINIFY;
  if (format === "compact") bits |= COMPACT;
  return bits;
}
