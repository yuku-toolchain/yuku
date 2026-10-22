import { bundled } from "#module";
import type { ParseOptions } from "../file/source-file";
import type { SourceLang, SourceType } from "../types";
import { addChildKeys } from "./child-keys";
import { NodeBuffer } from "./node";

export interface Shape {
  type: string;
  fields: string[];
  list: number;
  properties: Record<string, unknown>;
}

interface Dialect {
  files: { extension: string; lang: SourceLang }[];
  shapes: Shape[];
}

interface Exports {
  memory: WebAssembly.Memory;
  alloc(len: number): number;
  free(pointer: number, len: number): void;
  parse(pointer: number, len: number, flags: number): number;
  analyze(pointer: number, len: number, flags: number): number;
  dialect(): number;
}

interface Instance {
  exports: Exports;
  dialect: Dialect | null;
  grownAt: number;
}

export interface Read {
  readonly buffer: ArrayBuffer;
  readonly lang: SourceLang;
  readonly sourceType: SourceType;
  readonly shapes: Shape[] | null;
}

const LANGS: Record<string, number> = { js: 0, ts: 1, jsx: 2, tsx: 3, dts: 4 };
const SOURCE_TYPES: Record<string, number> = { script: 0, module: 1, commonjs: 2 };
const LANG_SHIFT = 2;
const PRESERVE_PARENS = 1 << 5;
const SEMANTIC_ERRORS = 1 << 6;
const ATTACH_COMMENTS = 1 << 7;
const TOKENS = 1 << 8;
const DIALECT = 1 << 9;
const UTF16 = 1 << 10;

const MEMORY_BYTES_MAX = 128 * 1024 * 1024;
const MEMORY_IDLE_MS = 10_000;

const encoder = new TextEncoder();
const utf8 = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
const lossyUtf8 = new TextDecoder("utf-8", { ignoreBOM: true });
const instances = new WeakMap<WebAssembly.Module, Instance>();

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

export function read(source: string, options: ParseOptions, entry: "parse" | "analyze"): Read {
  const instance = instanceOf(options.wasm ?? bundled());
  const path = options.path ?? "";
  const { dialect } = instance;
  const file = dialect === null || options.lang !== undefined ? undefined : fileOf(dialect, path);
  const lang = options.lang ?? file?.lang ?? langFromPath(path);
  const langBits = LANGS[lang];
  if (langBits === undefined) {
    throw new TypeError('`lang` must be "js", "jsx", "ts", "tsx", or "dts"');
  }
  const sourceType = options.sourceType ?? sourceTypeFromPath(path);
  const sourceTypeBits = SOURCE_TYPES[sourceType];
  if (sourceTypeBits === undefined) {
    throw new TypeError('`sourceType` must be "module", "script", or "commonjs"');
  }
  let flags = sourceTypeBits | (langBits << LANG_SHIFT);
  if (options.preserveParens !== false) flags |= PRESERVE_PARENS;
  if (options.semanticErrors === true) flags |= SEMANTIC_ERRORS;
  if (options.attachComments === true) flags |= ATTACH_COMMENTS;
  if (options.tokens === true) flags |= TOKENS;
  if (file !== undefined) flags |= DIALECT;
  const buffer = call(instance, entry, source, flags);
  return { buffer, lang, sourceType, shapes: file?.shapes ?? null };
}

function call(
  instance: Instance,
  entry: "parse" | "analyze",
  source: string,
  flags: number,
): ArrayBuffer {
  const { memory, alloc, free } = instance.exports;
  const bytes = memory.buffer.byteLength;
  const capacity = source.length * (NodeBuffer === undefined ? 3 : 2) || 1;
  const pointer = alloc(capacity) >>> 0;
  const written = writeSource(memory.buffer, pointer, capacity, source);
  const encoding = NodeBuffer === undefined ? 0 : UTF16;
  const result = instance.exports[entry](pointer, written, flags | encoding) >>> 0;
  if (memory.buffer.byteLength > bytes) instance.grownAt = Date.now();
  free(pointer, capacity);
  if (result === 0) throw new Error("yuku: out of memory");
  const size = new DataView(memory.buffer).getUint32(result, true);
  const buffer = memory.buffer.slice(result + 4, result + 4 + size);
  free(result, 4 + size);
  return buffer;
}

/** The `lang` a path's extension implies. */
export function langFromPath(path: string): SourceLang {
  if (/\.d\.[cm]?ts$/.test(path)) return "dts";
  if (path.endsWith(".tsx")) return "tsx";
  if (/\.[cm]?ts$/.test(path)) return "ts";
  if (path.endsWith(".jsx")) return "jsx";
  return "js";
}

/** The `sourceType` a path implies, `"commonjs"` for `.cjs` and `.cts`. */
export function sourceTypeFromPath(path: string): SourceType {
  return /\.c[jt]s$/.test(path) ? "commonjs" : "module";
}

function instanceOf(module: WebAssembly.Module): Instance {
  let instance = instances.get(module);
  if (instance === undefined) {
    const exports = instantiate(module);
    instance = { exports, dialect: dialectOf(exports), grownAt: 0 };
    instances.set(module, instance);
  } else if (instance.exports.memory.buffer.byteLength > MEMORY_BYTES_MAX) {
    if (Date.now() - instance.grownAt > MEMORY_IDLE_MS) instance.exports = instantiate(module);
  }
  return instance;
}

function instantiate(module: WebAssembly.Module): Exports {
  return new WebAssembly.Instance(module).exports as unknown as Exports;
}

function writeSource(
  memory: ArrayBuffer,
  pointer: number,
  capacity: number,
  source: string,
): number {
  // WebContainers' encodeInto repeats its last count for an empty string
  if (source.length === 0) return 0;
  if (NodeBuffer !== undefined) {
    return NodeBuffer.from(memory, pointer, capacity).write(source, "utf16le");
  }
  return encoder.encodeInto(source, new Uint8Array(memory, pointer, capacity)).written;
}

function dialectOf(exports: Exports): Dialect | null {
  const pointer = exports.dialect();
  if (pointer === 0) return null;
  const size = new DataView(exports.memory.buffer).getUint32(pointer, true);
  const json = utf8.decode(new Uint8Array(exports.memory.buffer, pointer + 4, size));
  const dialect = JSON.parse(json) as Dialect;
  for (const shape of dialect.shapes) addChildKeys(shape.type, shape.fields);
  return dialect;
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

function fileOf(dialect: Dialect, path: string): { lang: SourceLang; shapes: Shape[] } | undefined {
  const { files, shapes } = dialect;
  const file = path === "" ? files[0] : files.find(({ extension }) => path.endsWith(extension));
  return file === undefined ? undefined : { lang: file.lang, shapes };
}
