import { load as loadBinding } from "./binding.js";

export { fileOptions, langFromPath, sourceTypeFromPath } from "./options.js";

export function load() {
  try {
    return loadBinding();
  } catch (error) {
    throw new Error(
      `${error.message}\nThe WebAssembly core from @yuku-core/wasm runs on every platform.`,
      { cause: error },
    );
  }
}
