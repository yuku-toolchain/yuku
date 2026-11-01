import { readFileSync } from "node:fs";
import { defineConfig } from "tsdown";

export default defineConfig({
  entry: {
    index: "index.ts",
    utils: "ast/utils.ts",
    "native/addons": "core/native/addons.ts",
    "native/none": "core/native/none.ts",
    "wasm/import": "core/wasm/import.ts",
    "wasm/require": "core/wasm/require.ts",
  },
  outDir: "dist",
  clean: true,
  format: "esm",
  platform: "neutral",
  target: "es2020",
  hash: false,
  treeshake: true,
  dts: { entry: ["index.ts", "ast/utils.ts"] },
  deps: { neverBundle: [/^#/, /^node:/] },
  outputOptions: { chunkFileNames: "shared/[name].js" },
  plugins: [
    {
      name: "wasm",
      resolveId: (id: string) => (id.endsWith(".wasm?module") ? { id, external: "absolute" } : null),
      generateBundle() {
        const base64 = readFileSync(new URL("../wasm/yuku.wasm", import.meta.url), "base64");
        const source = `module.exports = "${base64}";\n`;
        this.emitFile({ type: "asset", fileName: "wasm/inline.cjs", source });
      },
    },
  ],
});
