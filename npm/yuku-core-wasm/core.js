const SOURCE_TYPES = { script: 0, module: 1, commonjs: 2 };
const LANGS = { js: 0, ts: 1, jsx: 2, tsx: 3, dts: 4 };

// linear memory never shrinks, so after a huge file a source too small to need it (at most 64
// bytes per character) gets a fresh instance
const MEMORY_BYTES_MAX = 128 * 1024 * 1024;

export async function instantiate(source, write) {
  let input = await source;
  if (typeof input === "string" || input instanceof URL) input = await fetch(input);
  if (typeof input.arrayBuffer === "function") {
    if (!input.ok) {
      throw new Error(`@yuku-core/wasm: ${input.url || "the response"} has status ${input.status}`);
    }

    const streaming = input.headers.get("content-type")?.startsWith("application/wasm");
    input = streaming ? await WebAssembly.compileStreaming(input) : await input.arrayBuffer();
  }
  const module = input instanceof WebAssembly.Module ? input : await WebAssembly.compile(input);
  return coreOf(module, write);
}

export function instantiateSync(source, write) {
  const module = source instanceof WebAssembly.Module ? source : new WebAssembly.Module(source);
  return coreOf(module, write);
}

function coreOf(module, write) {
  let exports = new WebAssembly.Instance(module).exports;

  function run(entry, source, options) {
    if (typeof source !== "string") {
      throw new TypeError("@yuku-core/wasm: the source must be a string");
    }
    const grown = exports.memory.buffer.byteLength > MEMORY_BYTES_MAX;
    if (grown && source.length * 64 < MEMORY_BYTES_MAX) {
      exports = new WebAssembly.Instance(module).exports;
    }
    const { memory, alloc, free } = exports;
    const capacity = source.length * 3 || 1;
    const pointer = alloc(capacity);
    const view = new Uint8Array(memory.buffer, pointer, capacity);
    const written = source.length === 0 ? 0 : write(source, view);
    const result = exports[entry](pointer, written, flagsOf(options));
    free(pointer, capacity);
    if (result === 0) throw new Error("@yuku-core/wasm: out of memory");
    const size = new DataView(memory.buffer).getUint32(result, true);
    const buffer = memory.buffer.slice(result + 4, result + 4 + size);
    free(result, 4 + size);
    return buffer;
  }

  return {
    parse: (source, options) => run("parse", source, options),
    analyze: (source, options) => run("analyze", source, options),
  };
}

function flagsOf(options) {
  let flags = (SOURCE_TYPES[options.sourceType] ?? 1) | ((LANGS[options.lang] ?? 0) << 2);
  if (options.preserveParens !== false) flags |= 1 << 5;
  if (options.semanticErrors) flags |= 1 << 6;
  if (options.attachComments) flags |= 1 << 7;
  if (options.tokens) flags |= 1 << 8;
  return flags;
}
