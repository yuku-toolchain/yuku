import { yuku } from "#module";
import type { ParseOptions, ParseResult } from "../parser";
import type { SourceLang, SourceType } from "../types";
import { addChildKeys } from "./child-keys";
import { decode, type Decoded } from "./decode";
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

const LANGS: Record<string, number> = { js: 0, ts: 1, jsx: 2, tsx: 3, dts: 4 };
const SOURCE_TYPES: Record<string, number> = { script: 0, module: 1, commonjs: 2 };

const MEMORY_BYTES_MAX = 128 * 1024 * 1024;
const MEMORY_IDLE_MS = 10_000;

const encoder = new TextEncoder();
const utf8 = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
const instances = new WeakMap<WebAssembly.Module, Instance>();

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

export function sourceText(source: string | Uint8Array): string {
  return typeof source === "string" ? source : utf8.decode(source);
}

export function run(entry: "parse", source: string, options: ParseOptions): ParseResult;
export function run(entry: "analyze", source: string, options: ParseOptions): Decoded;
export function run(
  entry: "parse" | "analyze",
  source: string,
  options: ParseOptions,
): ParseResult | Decoded {
  const instance = instanceOf(options.wasm ?? yuku);
  const { exports, dialect } = instance;
  const { memory, alloc, free } = exports;
  const bytes = memory.buffer.byteLength;
  const capacity = source.length * 3 || 1;
  const pointer = alloc(capacity) >>> 0;
  const written = writeSource(memory.buffer, pointer, capacity, source);
  const result = exports[entry](pointer, written, flagsOf(options, dialect)) >>> 0;
  if (memory.buffer.byteLength > bytes) instance.grownAt = Date.now();
  free(pointer, capacity);
  if (result === 0) throw new Error("yuku: out of memory");
  const size = new DataView(memory.buffer).getUint32(result, true);
  const buffer = memory.buffer.slice(result + 4, result + 4 + size);
  free(result, 4 + size);
  return decode(buffer, source, options.path || null, dialect?.shapes ?? null);
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
  if (NodeBuffer !== undefined) return NodeBuffer.from(memory, pointer, capacity).write(source);
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

function flagsOf(options: ParseOptions, dialect: Dialect | null): number {
  const path = options.path ?? "";
  const file = options.lang === undefined && dialect !== null ? fileOf(dialect, path) : undefined;
  const lang = LANGS[options.lang ?? file?.lang ?? langFromPath(path)];
  if (lang === undefined) {
    throw new TypeError('`lang` must be "js", "jsx", "ts", "tsx", or "dts"');
  }
  const sourceType = SOURCE_TYPES[options.sourceType ?? sourceTypeFromPath(path)];
  if (sourceType === undefined) {
    throw new TypeError('`sourceType` must be "module", "script", or "commonjs"');
  }
  let flags = sourceType | (lang << 2);
  if (options.preserveParens !== false) flags |= 1 << 5;
  if (options.semanticErrors) flags |= 1 << 6;
  if (options.attachComments) flags |= 1 << 7;
  if (options.tokens) flags |= 1 << 8;
  if (file !== undefined) flags |= 1 << 9;
  return flags;
}

function fileOf(dialect: Dialect, path: string): Dialect["files"][number] | undefined {
  if (path === "") return dialect.files[0];
  return dialect.files.find((file) => path.endsWith(file.extension));
}
