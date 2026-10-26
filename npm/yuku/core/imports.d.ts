declare module "#native" {
  export const addons: import("./core").CoreOptions["addons"];
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

declare module "node:module" {
  export function createRequire(path: string): { resolve(request: string): string };
}

declare module "node:url" {
  export function pathToFileURL(path: string): URL;
}
