import shipped from "#wasm";
import type { SourceLang } from "../types";
import { decodeBase64 } from "./base64";
import { addChildKeys } from "./child-keys";
import type { Backend, Entry } from "./read";

export interface Shape {
  type: string;
  fields: string[];
  list: number;
  properties: Record<string, unknown>;
}

export interface Dialect {
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

interface Instance extends Backend {
  exports: Exports;
  grownAt: number;
}

interface NodeBufferConstructor {
  from(
    buffer: ArrayBuffer,
    offset: number,
    length: number,
  ): { write(text: string, encoding: "utf16le"): number };
  from(text: string, encoding: "base64"): Uint8Array<ArrayBuffer>;
}

interface NodeGlobals {
  process?: { versions?: { node?: string } };
  Buffer?: NodeBufferConstructor;
}

const UTF16 = 1 << 10;

const MEMORY_BYTES_MAX = 128 * 1024 * 1024;
const MEMORY_IDLE_MS = 10_000;

const encoder = new TextEncoder();
const utf8 = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
const instances = new WeakMap<WebAssembly.Module, Instance>();
const NodeBuffer: NodeBufferConstructor | undefined =
  (globalThis as NodeGlobals).process?.versions?.node === undefined
    ? undefined
    : (globalThis as NodeGlobals).Buffer;
let compiled: WebAssembly.Module | null = null;

/** The WebAssembly module the package ships, compiled on first use. */
export function shippedModule(): WebAssembly.Module {
  if (compiled === null) {
    compiled = typeof shipped === "string" ? new WebAssembly.Module(bytesOf(shipped)) : shipped;
  }
  return compiled;
}

/** The instance of `module` that parses for the package, made on first use. */
export function wasmBackend(module: WebAssembly.Module): Backend {
  let instance = instances.get(module);
  if (instance === undefined) {
    instance = createInstance(module);
    instances.set(module, instance);
  } else if (instance.exports.memory.buffer.byteLength > MEMORY_BYTES_MAX) {
    if (Date.now() - instance.grownAt > MEMORY_IDLE_MS) instance.exports = instantiate(module);
  }
  return instance;
}

function createInstance(module: WebAssembly.Module): Instance {
  const exports = instantiate(module);
  const instance: Instance = {
    exports,
    dialect: dialectOf(exports),
    grownAt: 0,
    run: (entry, source, flags) => call(instance, entry, source, flags),
  };
  return instance;
}

function call(instance: Instance, entry: Entry, source: string, flags: number): ArrayBuffer {
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

function bytesOf(base64: string): Uint8Array<ArrayBuffer> {
  const typed = Uint8Array as { fromBase64?(text: string): Uint8Array<ArrayBuffer> };
  if (typed.fromBase64 !== undefined) return typed.fromBase64(base64);
  if (NodeBuffer !== undefined) return NodeBuffer.from(base64, "base64");
  return decodeBase64(base64);
}
