import { createRequire } from "node:module";
import { load } from "./binding.js";

const require = createRequire(import.meta.url);

let engine;

export function parse(source, options) {
  return current().parse(source, options);
}

export function analyze(source, options) {
  return current().analyze(source, options);
}

function current() {
  if (engine === undefined) engine = open();
  return engine;
}

function open() {
  try {
    return load();
  } catch (nativeError) {
    try {
      return require("@yuku-engine/wasm");
    } catch (wasmError) {
      throw new Error(
        "yuku-engine: neither the native binary nor @yuku-engine/wasm could be loaded.\n\n" +
          `${nativeError.message}\n\n${wasmError.message}`,
        { cause: nativeError },
      );
    }
  }
}
