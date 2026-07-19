const wasmUrl = new URL("./yuku-analyzer.wasm", import.meta.url);

async function instantiate() {
  if (wasmUrl.protocol !== "file:") {
    try {
      return (await WebAssembly.instantiateStreaming(fetch(wasmUrl))).instance;
    } catch {
      const bytes = await (await fetch(wasmUrl)).arrayBuffer();
      return (await WebAssembly.instantiate(bytes)).instance;
    }
  }
  const { readFile } = await import("node:fs/promises");
  return (await WebAssembly.instantiate(await readFile(wasmUrl))).instance;
}

const { memory, alloc, free, analyze } = (await instantiate()).exports;
const encoder = new TextEncoder();

const LANGS = { js: 0, ts: 1, jsx: 2, tsx: 3, dts: 4 };

function packFlags(o = {}) {
  let f = (LANGS[o.lang] ?? 0) << 1;
  if (o.sourceType === "script") f |= 1 << 0;
  if (o.preserveParens !== false) f |= 1 << 4;
  if (o.allowReturnOutsideFunction) f |= 1 << 5;
  if (o.attachComments) f |= 1 << 7;
  return f;
}

export default {
  analyze(source, options) {
    const bytes = encoder.encode(source);
    const srcLen = bytes.length;
    const srcPtr = alloc(srcLen || 1);
    // Growing wasm memory detaches memory.buffer, so re-view after every call.
    new Uint8Array(memory.buffer, srcPtr, srcLen).set(bytes);

    const ptr = analyze(srcPtr, srcLen, packFlags(options));
    free(srcPtr, srcLen || 1);
    if (ptr === 0) throw new Error("yuku-analyzer-wasm: failed to analyze source");

    const len = new DataView(memory.buffer).getUint32(ptr, true);
    const buffer = memory.buffer.slice(ptr + 4, ptr + 4 + len);
    free(ptr, 4 + len);
    return buffer;
  },
};
