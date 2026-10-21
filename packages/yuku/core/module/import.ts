import yuku from "#wasm";

export function bundled(): WebAssembly.Module {
  return yuku;
}
