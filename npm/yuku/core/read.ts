import type { ParseOptions } from "../file/source-file";
import type { SourceLang, SourceType } from "../types";
import { addons } from "#native";
import shipped from "#wasm";
import { Core, type Entry } from "./core";

export interface Read {
  readonly buffer: ArrayBuffer;
  readonly lang: SourceLang;
  readonly sourceType: SourceType;
}

const LANGS: Record<string, number> = { js: 0, ts: 1, jsx: 2, tsx: 3, dts: 4 };
const SOURCE_TYPES: Record<string, number> = { script: 0, module: 1, commonjs: 2 };
const LANG_SHIFT = 2;
const PRESERVE_PARENS = 1 << 5;
const SEMANTIC_ERRORS = 1 << 6;
const ATTACH_COMMENTS = 1 << 7;
const TOKENS = 1 << 8;

export const shippedCore: Core = new Core({ wasm: shipped, addons });
const utf8 = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
const lossyUtf8 = new TextDecoder("utf-8", { ignoreBOM: true });

export function sourceText(source: string | Uint8Array): { text: string; malformed: number } {
  if (typeof source === "string") return { text: source, malformed: -1 };
  if (!(source instanceof Uint8Array)) {
    throw new TypeError("`source` must be a string or UTF-8 bytes");
  }
  try {
    return { text: utf8.decode(source), malformed: -1 };
  } catch {
    const valid = source.subarray(0, malformedAt(source));
    return { text: lossyUtf8.decode(source), malformed: lossyUtf8.decode(valid).length };
  }
}

export function read(source: string, options: ParseOptions, entry: Entry): Read {
  const backend = (options.core ?? shippedCore).backend();
  const path = options.path ?? "";
  const lang = options.lang ?? langFromPath(path);
  if (LANGS[lang] === undefined) {
    throw new TypeError('`lang` must be "js", "jsx", "ts", "tsx", or "dts"');
  }
  const sourceType = options.sourceType ?? sourceTypeFromPath(path);
  if (SOURCE_TYPES[sourceType] === undefined) {
    throw new TypeError('`sourceType` must be "module", "script", or "commonjs"');
  }
  let flags = treeFlags(lang, sourceType);
  if (options.preserveParens !== false) flags |= PRESERVE_PARENS;
  if (options.semanticErrors === true) flags |= SEMANTIC_ERRORS;
  if (options.attachComments === true) flags |= ATTACH_COMMENTS;
  if (options.tokens === true) flags |= TOKENS;
  const buffer = backend.run(entry, source, flags);
  return { buffer, lang, sourceType };
}

export function treeFlags(lang: SourceLang, sourceType: SourceType): number {
  return SOURCE_TYPES[sourceType]! | (LANGS[lang]! << LANG_SHIFT);
}

export function isTs(lang: SourceLang): boolean {
  return lang === "ts" || lang === "tsx" || lang === "dts";
}

function langFromPath(path: string): SourceLang {
  if (/\.d\.[cm]?ts$/.test(path)) return "dts";
  if (path.endsWith(".tsx")) return "tsx";
  if (/\.[cm]?ts$/.test(path)) return "ts";
  if (path.endsWith(".jsx")) return "jsx";
  return "js";
}

function sourceTypeFromPath(path: string): SourceType {
  return /\.c[jt]s$/.test(path) ? "commonjs" : "module";
}

function malformedAt(bytes: Uint8Array): number {
  let i = 0;
  while (i < bytes.length) {
    const lead = bytes[i]!;
    if (lead < 0x80) {
      i++;
      continue;
    }
    let size = 4;
    let low = lead === 0xf0 ? 0x90 : 0x80;
    let high = lead === 0xf4 ? 0x8f : 0xbf;
    if (lead >= 0xc2 && lead <= 0xdf) {
      size = 2;
    } else if (lead >= 0xe0 && lead <= 0xef) {
      size = 3;
      low = lead === 0xe0 ? 0xa0 : 0x80;
      high = lead === 0xed ? 0x9f : 0xbf;
    } else if (lead < 0xf0 || lead > 0xf4) {
      return i;
    }
    for (let k = 1; k < size; k++) {
      const byte = bytes[i + k];
      if (byte === undefined || byte < low || byte > high) return i;
      low = 0x80;
      high = 0xbf;
    }
    i += size;
  }
  return bytes.length;
}
