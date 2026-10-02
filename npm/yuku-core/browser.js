export { fileOptions, langFromPath, sourceTypeFromPath } from "./options.js";

export function load() {
  throw new Error(
    "yuku-core: the native core does not run here. " +
      "Load the WebAssembly core from @yuku-core/wasm and pass it in as `core`.",
  );
}
