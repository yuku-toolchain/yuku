import { readFile } from "node:fs/promises";
import { load as loadFrom } from "./index.js";

export { loadSync } from "./index.js";

// `fetch` reads no `file:` URLs here
export function load(source = readFile(new URL("./yuku-core.wasm", import.meta.url))) {
  return loadFrom(source);
}
