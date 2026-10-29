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
  /** The core compiled to WebAssembly, as a module or in base64. */
  wasm: WebAssembly.Module | string;
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
    if (typeof options?.wasm !== "string" && !(options?.wasm instanceof WebAssembly.Module)) {
      throw new TypeError("`wasm` must be a WebAssembly.Module or base64");
    }
    this.options = options;
  }

  /** @internal */
  backend(): Backend {
    if (this.native === undefined) this.native = nativeBackend(this.options.addons);
    if (this.native !== null) return this.native;
    if (this.module === undefined) this.module = compile(this.options.wasm);
    return wasmBackend(this.module);
  }
}

/** A loaded core, its Node-API addon or a WebAssembly instance. */
export interface Backend {
  run(entry: Entry, source: string, flags: number): ArrayBuffer;
  /** `source` is empty unless a source map needs it. */
  runTree(
    entry: TreeEntry,
    tree: Encoded,
    source: string,
    flags: number,
    options: number,
  ): ArrayBuffer;
}

export type Entry = "parse" | "analyze";
export type TreeEntry = "printTree" | "analyzeTree";

interface Addon {
  parse(source: string, flags: number): ArrayBuffer;
  analyze(source: string, flags: number): ArrayBuffer;
  printTree(
    nodes: Int32Array,
    extras: Int32Array,
    comments: Int32Array,
    pool: Uint16Array,
    source: string,
    flags: number,
    options: number,
  ): ArrayBuffer;
  analyzeTree(
    nodes: Int32Array,
    extras: Int32Array,
    comments: Int32Array,
    pool: Uint16Array,
    flags: number,
  ): ArrayBuffer;
}

interface Exports {
  memory: WebAssembly.Memory;
  alloc(len: number): number;
  free(pointer: number, len: number): void;
  parse(pointer: number, len: number, flags: number): number;
  analyze(pointer: number, len: number, flags: number): number;
  printTree(block: number, flags: number, options: number): number;
  analyzeTree(block: number, flags: number): number;
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

const UTF16 = 1 << 10;
const TREE_HEADER_WORDS = 5;
const BIG_ENDIAN = new Uint8Array(new Uint16Array([1]).buffer)[0] === 0;

const MEMORY_BYTES_MAX = 128 * 1024 * 1024;
const MEMORY_IDLE_MS = 10_000;

const globals = globalThis as Globals;
const encoder = new TextEncoder();
const instances = new WeakMap<WebAssembly.Module, Instance>();
const NodeBuffer = globals.process?.versions?.node === undefined ? undefined : globals.Buffer;

/** The addon in `addons` for this platform, or `null` where none loads. */
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
  for (const entry of ["parse", "analyze", "printTree", "analyzeTree"] as const) {
    if (typeof addon[entry] !== "function") return null;
  }
  const { parse, analyze, printTree, analyzeTree } = addon as Addon;
  return {
    run: (entry, source, flags) => (entry === "parse" ? parse : analyze)(source, flags),
    runTree: (entry, tree, source, flags, options) =>
      entry === "printTree"
        ? printTree(tree.nodes, tree.extras, tree.comments, tree.pool, source, flags, options)
        : analyzeTree(tree.nodes, tree.extras, tree.comments, tree.pool, flags),
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

function compile(wasm: WebAssembly.Module | string): WebAssembly.Module {
  return typeof wasm === "string" ? new WebAssembly.Module(bytesOf(wasm)) : wasm;
}

/** The instance of `module` that parses for the package, made on first use. */
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
  const exports = instantiate(module);
  const instance: Instance = {
    exports,
    grownAt: 0,
    run: (entry, source, flags) => call(instance, entry, source, flags),
    runTree: (entry, tree, source, flags, options) =>
      callTree(instance, entry, tree, source, flags, options),
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
  return resultOf(instance, result);
}

function callTree(
  instance: Instance,
  entry: TreeEntry,
  tree: Encoded,
  source: string,
  flags: number,
  options: number,
): ArrayBuffer {
  const { memory, alloc, free } = instance.exports;
  const bytes = memory.buffer.byteLength;
  const { nodes, extras, comments, pool } = tree;
  const words = TREE_HEADER_WORDS + nodes.length + extras.length + comments.length;
  const poolBytes = (pool.length * 2 + 3) & ~3;
  const sourceCapacity = source.length * (NodeBuffer === undefined ? 3 : 2);
  const capacity = words * 4 + poolBytes + sourceCapacity;
  const pointer = alloc(capacity) >>> 0;
  const block = new Int32Array(memory.buffer, pointer, words);
  block[0] = nodes.length;
  block[1] = extras.length;
  block[2] = comments.length;
  block[3] = pool.length;
  block.set(nodes, TREE_HEADER_WORDS);
  block.set(extras, TREE_HEADER_WORDS + nodes.length);
  block.set(comments, TREE_HEADER_WORDS + nodes.length + extras.length);
  const units = new Uint16Array(memory.buffer, pointer + words * 4, pool.length);
  units.set(pool);
  const sourceAt = pointer + words * 4 + poolBytes;
  block[4] = writeSource(memory.buffer, sourceAt, sourceCapacity, source);
  if (BIG_ENDIAN) {
    swapWords(block);
    for (let i = 0; i < units.length; i++) units[i] = (units[i]! << 8) | (units[i]! >>> 8);
  }
  const encoding = NodeBuffer === undefined ? 0 : UTF16;
  const result =
    entry === "printTree"
      ? instance.exports.printTree(pointer, flags | encoding, options) >>> 0
      : instance.exports.analyzeTree(pointer, flags | encoding) >>> 0;
  if (memory.buffer.byteLength > bytes) instance.grownAt = Date.now();
  free(pointer, capacity);
  return resultOf(instance, result);
}

function resultOf(instance: Instance, result: number): ArrayBuffer {
  const { memory, free } = instance.exports;
  if (result === 0) throw new Error("yuku: out of memory, or the tree is malformed");
  const size = new DataView(memory.buffer).getUint32(result, true);
  const buffer = memory.buffer.slice(result + 4, result + 4 + size);
  free(result, 4 + size);
  return buffer;
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
