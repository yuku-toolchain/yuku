import { encode } from "./encode.js";

const wasmUrl = new URL("./yuku-codegen.wasm", import.meta.url);

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

const { memory, alloc, free, codegen } = (await instantiate()).exports;
const decoder = new TextDecoder();

const QUOTES = { preserve: 0, double: 1, single: 2, shortest: 3 };
const COMMENTS = { none: 0, false: 0, all: 1, true: 1, some: 2, line: 3, block: 4 };

export function generate(program, options = {}) {
  const m = options.minify === true ? { whitespace: true, syntax: true, quotes: true } : (options.minify ?? {});
  const flags =
    (options.strip ? 1 : 0) |
    (m.syntax ? 2 : 0) |
    (m.whitespace || options.format === "compact" ? 4 : 0) |
    ((m.quotes ? 3 : (QUOTES[options.quotes] ?? 0)) << 3) |
    ((COMMENTS[options.comments] ?? 2) << 5) |
    (((options.indent ?? 2) & 0xff) << 8);
  const ast = new Uint8Array(encode(program, null));
  const inPtr = alloc(ast.length || 1);
  // Growing wasm memory detaches memory.buffer, so re-view after every call.
  new Uint8Array(memory.buffer, inPtr, ast.length).set(ast);

  const ptr = codegen(inPtr, ast.length, flags);
  free(inPtr, ast.length || 1);
  if (ptr === 0) throw new Error("yuku-codegen-wasm: failed to generate code");

  const len = new DataView(memory.buffer).getUint32(ptr, true);
  const code = decoder.decode(new Uint8Array(memory.buffer, ptr + 4, len));
  free(ptr, 4 + len);
  return code;
}

/** @deprecated Use `generate(program)`. */
export function print(program) {
  return generate(program);
}

/** @deprecated Use `generate(program, { strip: true })`. */
export function strip(program) {
  return generate(program, { strip: true });
}

/** @deprecated Use `generate(program, { minify: true })`. */
export function minify(program) {
  return generate(program, { minify: true });
}
