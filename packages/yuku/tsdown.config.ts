import { defineConfig } from "tsdown";

export default defineConfig({
  entry: {
    index: "index.ts",
    "module/inline": "core/module/inline.ts",
    "module/import": "core/module/import.ts",
  },
  outDir: "dist",
  clean: true,
  format: "esm",
  platform: "neutral",
  target: "es2020",
  hash: false,
  treeshake: true,
  copy: [{ from: "core/yuku.wasm", to: "dist" }],
  loader: { ".wasm": "base64" },
  deps: { neverBundle: [/^#/] },
  outputOptions: {
    chunkFileNames: "shared/[name].js",
    paths: { "#wasm": "../yuku.wasm?module" },
  },
});
