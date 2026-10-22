import { defineConfig } from "tsdown";

export default defineConfig({
  entry: {
    index: "index.ts",
    "native/load": "core/native/load.ts",
    "native/none": "core/native/none.ts",
    "wasm/inline": "core/wasm/inline.ts",
    "wasm/import": "core/wasm/import.ts",
  },
  outDir: "dist",
  clean: true,
  format: "esm",
  platform: "neutral",
  target: "es2020",
  hash: false,
  treeshake: true,
  copy: [
    { from: "core/yuku.wasm", to: "dist" },
    { from: "native/*.node", to: "dist/native" },
  ],
  loader: { ".wasm": "base64" },
  dts: { entry: "index.ts" },
  deps: { neverBundle: [/^#/] },
  outputOptions: { chunkFileNames: "shared/[name].js" },
  plugins: [
    {
      name: "keep-wasm-imports",
      resolveId: (id: string) => (id.endsWith(".wasm?module") ? { id, external: "absolute" } : null),
    },
  ],
});
