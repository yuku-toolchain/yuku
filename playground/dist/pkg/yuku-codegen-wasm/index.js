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

function run(op, program) {
  const ast = new Uint8Array(encode(program, null));
  const inPtr = alloc(ast.length);
  // Growing wasm memory detaches memory.buffer, so re-view after every call.
  new Uint8Array(memory.buffer, inPtr, ast.length).set(ast);

  const ptr = codegen(inPtr, ast.length, op);
  free(inPtr, ast.length);
  if (ptr === 0) throw new Error("yuku-codegen-wasm: failed to generate code");

  const len = new DataView(memory.buffer).getUint32(ptr, true);
  const code = decoder.decode(new Uint8Array(memory.buffer, ptr + 4, len));
  free(ptr, 4 + len);
  return code;
}

export function print(program) {
  return run(0, program);
}

export function strip(program) {
  return run(1, program);
}

export function minify(program) {
  return run(2, program);
}
