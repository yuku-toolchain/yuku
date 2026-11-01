declare module "#native" {
  export const addons: import("./core").CoreOptions["addons"];
}

declare module "#wasm" {
  const shipped: import("./core").CoreOptions["wasm"];
  export default shipped;
}

declare module "*.wasm?module" {
  const module: WebAssembly.Module;
  export default module;
}

declare module "node:module" {
  export function createRequire(path: string): {
    (id: "./inline.cjs"): string;
    resolve(request: string): string;
  };
}

declare module "node:url" {
  export function pathToFileURL(path: string): URL;
}
