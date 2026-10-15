import { defineConfig } from "tsdown";

export default defineConfig({
  entry: {
    index: "parser/index.ts",
    analyzer: "analyzer/index.ts",
    ast: "ast/index.ts",
    codegen: "codegen/index.ts",
    types: "types/index.ts",
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
