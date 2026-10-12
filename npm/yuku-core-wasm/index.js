import { instantiate, instantiateSync } from "./core.js";

export function load(source = new URL("./yuku-core.wasm", import.meta.url)) {
  return instantiate(source, encodeInto);
}

export function loadSync(source) {
  return instantiateSync(source, encodeInto);
}

const encoder = new TextEncoder();

function encodeInto(source, view) {
  return encoder.encodeInto(source, view).written;
}
