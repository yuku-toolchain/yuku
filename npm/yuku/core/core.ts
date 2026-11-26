import { decodeBase64 } from "./base64";
import type { Encoded } from "./encode";

/** A platform as `${process.platform}-${process.arch}` names it. */
export type Platform =
  | "darwin-arm64"
  | "darwin-x64"
  | "linux-arm64"
  | "linux-x64"
  | "win32-arm64"
  | "win32-x64";

export interface CoreOptions {
  /** The core as WebAssembly: a module, base64, or a function returning one if no addon loads. */
  wasm: WebAssembly.Module | string | (() => WebAssembly.Module | string);
  /**
   * The core compiled to a Node-API addon for each platform, as
   * `() => new URL("./native/darwin-arm64.node", import.meta.url)`, so bundlers find it.
   */
  addons?: Partial<Record<Platform, () => URL>>;
}

/**
 * Yuku's compiled Zig core, which `parse` and `Project` run. Its Node-API addon runs where one
 * loads, and its WebAssembly everywhere else.
 */
export class Core {
  private readonly options: CoreOptions;
  private native: Backend | null | undefined;
  private module: WebAssembly.Module | undefined;

  constructor(options: CoreOptions) {
    const wasm = options?.wasm;
    if (
      typeof wasm !== "function" &&
      typeof wasm !== "string" &&
      !(wasm instanceof WebAssembly.Module)
    ) {
      throw new TypeError("`wasm` must be a WebAssembly.Module, base64, or a function to load one");
    }
    this.options = options;
  }

  /** @internal */
  backend(): Backend {
    if (this.native === undefined) this.native = nativeBackend(this.options.addons);
    return this.native ?? wasmBackend((this.module ??= compile(this.options.wasm)));
  }
}

interface Backend {
  run(entry: Entry, source: string, flags: number, options: number): ArrayBuffer;
  runTree(
    entry: TreeEntry,
    tree: Encoded,
    source: string,
    flags: number,
    options: number,
  ): ArrayBuffer;
}

type Entry = "parse" | "analyze" | "printSource";
type TreeEntry = "printTree" | "analyzeTree";

type Tree = [nodes: Int32Array, extras: Int32Array, comments: Int32Array, pool: Uint16Array];

interface Addon {
  parse(source: string, flags: number): ArrayBuffer;
  analyze(source: string, flags: number): ArrayBuffer;
  printSource(source: string, flags: number, options: number): ArrayBuffer;
  printTree(...args: [...Tree, source: string, flags: number, options: number]): ArrayBuffer;
  analyzeTree(...args: [...Tree, flags: number]): ArrayBuffer;
}

interface Exports {
  memory: WebAssembly.Memory;
  alloc(len: number): number;
  free(pointer: number, len: number): void;
  parse(pointer: number, len: number, flags: number): number;
  analyze(pointer: number, len: number, flags: number): number;
  printSource(pointer: number, len: number, flags: number, options: number): number;
  printTree(block: number, flags: number, options: number): number;
  analyzeTree(block: number, flags: number): number;
}

interface Instance extends Backend {
  module: WebAssembly.Module;
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

interface Globals {
  process?: {
    platform?: string;
    arch?: string;
    versions?: { node?: string };
    dlopen?(module: { exports: unknown }, path: string): void;
  };
  Deno?: { permissions?: { querySync?(descriptor: { name: "ffi" }): { state: string } } };
  Buffer?: NodeBufferConstructor;
}

const UTF16 = 1 << 9;
const TREE_HEADER_WORDS = 5;
const BIG_ENDIAN = new Uint8Array(new Uint16Array([1]).buffer)[0] === 0;

const MEMORY_BYTES_MAX = 128 * 1024 * 1024;
const MEMORY_IDLE_MS = 10_000;

const globals = globalThis as Globals;
const encoder = new TextEncoder();
const instances = new WeakMap<WebAssembly.Module, Instance>();
const NodeBuffer = globals.process?.versions?.node === undefined ? undefined : globals.Buffer;

const ENCODING = NodeBuffer === undefined ? 0 : UTF16;
const BYTES_PER_UNIT = NodeBuffer === undefined ? 3 : 2;

function nativeBackend(addons: CoreOptions["addons"]): Backend | null {
  const { process } = globals;
  if (addons === undefined || typeof process?.dlopen !== "function") return null;
  if (!ffiGranted()) return null;
  const module = { exports: {} as Partial<Addon> };
  try {
    const url = addons[`${process.platform}-${process.arch}` as Platform]?.();
    if (url?.protocol !== "file:") return null;
    process.dlopen(module, pathOf(url, process.platform === "win32"));
  } catch {
    return null;
  }
  const addon = module.exports;
  for (const entry of ["parse", "analyze", "printSource", "printTree", "analyzeTree"] as const) {
    if (typeof addon[entry] !== "function") return null;
  }
  const native = addon as Addon;
  const { printTree, analyzeTree } = native;
  return {
    run: (entry, source, flags, options) => native[entry](source, flags, options),
    runTree: (entry, { nodes, extras, comments, pool }, source, flags, options) =>
      entry === "printTree"
        ? printTree(nodes, extras, comments, pool, source, flags, options)
        : analyzeTree(nodes, extras, comments, pool, flags),
  };
}

function ffiGranted(): boolean {
  const { Deno } = globals;
  if (Deno === undefined) return true;
  try {
    return Deno.permissions?.querySync?.({ name: "ffi" }).state === "granted";
  } catch {
    return false;
  }
}

function pathOf(url: URL, windows: boolean): string {
  const path = decodeURIComponent(url.pathname);
  if (!windows) return path;
  return url.host === "" ? path.slice(1) : `//${url.host}${path}`;
}

function compile(wasm: CoreOptions["wasm"]): WebAssembly.Module {
  if (typeof wasm === "function") wasm = wasm();
  return typeof wasm === "string" ? new WebAssembly.Module(bytesOf(wasm)) : wasm;
}

function wasmBackend(module: WebAssembly.Module): Backend {
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
  const instance: Instance = {
    module,
    exports: instantiate(module),
    grownAt: 0,
    run(entry, source, flags, options) {
      const capacity = source.length * BYTES_PER_UNIT || 1;
      return invoke(instance, capacity, ({ memory, [entry]: run }, pointer) => {
        const written = writeSource(memory.buffer, pointer, capacity, source);
        return run(pointer, written, flags | ENCODING, options);
      });
    },
    runTree(entry, { nodes, extras, comments, pool }, source, flags, options) {
      const words = TREE_HEADER_WORDS + nodes.length + extras.length + comments.length;
      const poolBytes = (pool.length * 2 + 3) & ~3;
      const sourceCapacity = source.length * BYTES_PER_UNIT;
      return invoke(instance, words * 4 + poolBytes + sourceCapacity, (exports, pointer) => {
        const { buffer } = exports.memory;
        const block = new Int32Array(buffer, pointer, words);
        block[0] = nodes.length;
        block[1] = extras.length;
        block[2] = comments.length;
        block[3] = pool.length;
        block.set(nodes, TREE_HEADER_WORDS);
        block.set(extras, TREE_HEADER_WORDS + nodes.length);
        block.set(comments, TREE_HEADER_WORDS + nodes.length + extras.length);
        const units = new Uint16Array(buffer, pointer + words * 4, pool.length);
        units.set(pool);
        const sourceAt = pointer + words * 4 + poolBytes;
        block[4] = writeSource(buffer, sourceAt, sourceCapacity, source);
        if (BIG_ENDIAN) {
          swapWords(block);
          for (let i = 0; i < units.length; i++) units[i] = (units[i]! << 8) | (units[i]! >>> 8);
        }
        return exports[entry](pointer, flags | ENCODING, options);
      });
    },
  };
  return instance;
}

function invoke(
  instance: Instance,
  capacity: number,
  call: (exports: Exports, pointer: number) => number,
): ArrayBuffer {
  const { exports } = instance;
  const { memory, alloc, free } = exports;
  const bytes = memory.buffer.byteLength;
  try {
    const pointer = alloc(capacity) >>> 0;
    if (pointer === 0) throw new Error("yuku: out of memory");
    const result = call(exports, pointer) >>> 0;
    if (memory.buffer.byteLength > bytes) instance.grownAt = Date.now();
    free(pointer, capacity);
    if (result === 0) throw new Error("yuku: out of memory, or the tree is malformed");
    const size = new DataView(memory.buffer).getUint32(result, true);
    const buffer = memory.buffer.slice(result + 4, result + 4 + size);
    free(result, 4 + size);
    return buffer;
  } catch (error) {
    instance.exports = instantiate(instance.module);
    throw error;
  }
}

function swapWords(words: Int32Array): void {
  for (let i = 0; i < words.length; i++) {
    const v = words[i]!;
    words[i] = (v << 24) | ((v & 0xff00) << 8) | ((v >>> 8) & 0xff00) | (v >>> 24);
  }
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

function bytesOf(base64: string): Uint8Array<ArrayBuffer> {
  const typed = Uint8Array as { fromBase64?(text: string): Uint8Array<ArrayBuffer> };
  if (typed.fromBase64 !== undefined) return typed.fromBase64(base64);
  if (NodeBuffer !== undefined) return NodeBuffer.from(base64, "base64");
  return decodeBase64(base64);
}
