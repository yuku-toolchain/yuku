const SOURCE_TYPES = { script: 0, module: 1, commonjs: 2 };
const LANGS = { js: 0, ts: 1, jsx: 2, tsx: 3, dts: 4 };

export async function load(source = new URL("./yuku-core.wasm", import.meta.url)) {
  let input = await source;
  if (typeof input === "string" || input instanceof URL) input = await fetch(input);
  // a `Response` by its shape, as reading the global warns on some runtimes
  if (typeof input.arrayBuffer === "function") {
    if (!input.ok) {
      throw new Error(`@yuku-core/wasm: ${input.url || "the response"} has status ${input.status}`);
    }
    // streaming compilation requires the `application/wasm` content type
    const streaming = input.headers.get("content-type")?.startsWith("application/wasm");
    input = streaming ? await WebAssembly.compileStreaming(input) : await input.arrayBuffer();
  }
  const module = input instanceof WebAssembly.Module ? input : await WebAssembly.compile(input);
  return coreOf(await WebAssembly.instantiate(module));
}

export function loadSync(source) {
  const module = source instanceof WebAssembly.Module ? source : new WebAssembly.Module(source);
  return coreOf(new WebAssembly.Instance(module));
}

function coreOf({ exports }) {
  return {
    parse: (source, options) => run(exports, exports.parse, source, options),
    analyze: (source, options) => run(exports, exports.analyze, source, options),
  };
}

function run({ memory, alloc, free }, entry, source, options) {
  const length = source.length;
  const pointer = alloc(length || 1);
  new Uint8Array(memory.buffer, pointer, length).set(source);
  const result = entry(pointer, length, flagsOf(options));
  free(pointer, length || 1);
  if (result === 0) throw new Error("@yuku-core/wasm: out of memory");
  // a call can grow the memory, which detaches every earlier view of it
  const size = new DataView(memory.buffer).getUint32(result, true);
  const buffer = memory.buffer.slice(result + 4, result + 4 + size);
  free(result, 4 + size);
  return buffer;
}

function flagsOf(options) {
  let flags = (SOURCE_TYPES[options.sourceType] ?? 1) | ((LANGS[options.lang] ?? 0) << 2);
  if (options.preserveParens !== false) flags |= 1 << 5;
  if (options.semanticErrors) flags |= 1 << 6;
  if (options.attachComments) flags |= 1 << 7;
  if (options.tokens) flags |= 1 << 8;
  return flags;
}
