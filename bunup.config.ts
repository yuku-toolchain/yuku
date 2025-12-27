import { defineConfig } from 'bunup';
import { copy } from 'bunup/plugins';

export default defineConfig([
  {
    name: "node",
    entry: "npm/index.ts",
    outDir: "npm/dist"
  },
  {
    name: "browser",
    entry: "npm/browser.ts",
    target: "browser",
    outDir: "npm/dist",
    plugins: [copy("zig-out/yuku_js.wasm").to("yuku.wasm")]
  },
])
