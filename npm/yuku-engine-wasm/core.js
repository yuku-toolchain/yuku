const WASM_URL = new URL("./yuku-engine.wasm", import.meta.url);

const SOURCE_TYPES = { script: 0, module: 1, commonjs: 2 };
const LANGS = { js: 0, ts: 1, jsx: 2, tsx: 3, dts: 4 };

// `readSync` returns the module's bytes where the runtime can read files, so no `init` is needed
export function createEngine(readSync) {
  let engine = null;

  function exportsOf() {
    if (engine === null) {
      if (readSync === null) {
        throw new Error(
          "yuku-engine: call `await init()` from @yuku-engine/wasm before the first call",
        );
      }
      engine = new WebAssembly.Instance(new WebAssembly.Module(readSync(WASM_URL))).exports;
    }
    return engine;
  }

  return {
    async init(wasm) {
      if (engine !== null) return;
      if (wasm === undefined && readSync !== null) exportsOf();
      else engine = await instantiate(wasm === undefined ? WASM_URL : wasm);
    },
    parse(bytes, options) {
      return run(exportsOf(), "parse", bytes, options);
    },
    analyze(bytes, options) {
      return run(exportsOf(), "analyze", bytes, options);
    },
  };
}

function run(engine, entry, bytes, options = {}) {
  const { memory, alloc, free } = engine;
  const length = bytes.length;
  const source = alloc(length || 1);
  new Uint8Array(memory.buffer, source, length).set(bytes);
  const result = engine[entry](source, length, flagsOf(options));
  free(source, length || 1);
  if (result === 0) throw new Error("yuku-engine: the WebAssembly build ran out of memory");
  // a call can grow the memory, which detaches every earlier view of it
  const size = new DataView(memory.buffer).getUint32(result, true);
  const buffer = memory.buffer.slice(result + 4, result + 4 + size);
  free(result, 4 + size);
  return buffer;
}

async function instantiate(wasm) {
  const source = await wasm;
  if (typeof source === "string" || source instanceof URL) return instantiate(fetch(source));
  let module = source;
  if (source instanceof Response) {
    // streaming compilation requires the `application/wasm` content type
    const type = source.headers.get("content-type");
    module =
      type !== null && type.startsWith("application/wasm")
        ? await WebAssembly.compileStreaming(source)
        : await WebAssembly.compile(await source.arrayBuffer());
  } else if (!(source instanceof WebAssembly.Module)) {
    module = await WebAssembly.compile(source);
  }
  return (await WebAssembly.instantiate(module)).exports;
}

function flagsOf(options) {
  let flags = (SOURCE_TYPES[options.sourceType] ?? 1) | ((LANGS[options.lang] ?? 0) << 2);
  if (options.preserveParens !== false) flags |= 1 << 5;
  if (options.semanticErrors) flags |= 1 << 6;
  if (options.attachComments) flags |= 1 << 7;
  if (options.tokens) flags |= 1 << 8;
  return flags;
}
