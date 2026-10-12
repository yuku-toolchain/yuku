import { Buffer } from "node:buffer";
import { readFile } from "node:fs/promises";
import { instantiate, instantiateSync } from "./core.js";

// `fetch` reads no `file:` URLs here
export function load(source = readFile(new URL("./yuku-core.wasm", import.meta.url))) {
  return instantiate(source, write);
}

export function loadSync(source) {
  return instantiateSync(source, write);
}

function write(source, view) {
  return Buffer.from(view.buffer, view.byteOffset, view.length).write(source);
}
