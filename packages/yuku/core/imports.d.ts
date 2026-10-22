declare module "#native" {
  export function nativeBackend(): import("./read").Backend | null;
}

declare module "#wasm" {
  const shipped: WebAssembly.Module | string;
  export default shipped;
}

declare module "*.wasm" {
  const base64: string;
  export default base64;
}

declare module "*.wasm?module" {
  const module: WebAssembly.Module;
  export default module;
}
