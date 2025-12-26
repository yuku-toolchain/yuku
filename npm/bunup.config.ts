import { defineConfig } from 'bunup';
import { copy } from 'bunup/plugins';

export default defineConfig([
  {
    name: "node",
    entry: "src/index.ts",
  },
  {
    name: "browser",
    entry: "src/browser.ts",
    target: "browser",
    plugins: [copy("../zig-out/yuku_js.wasm").to("yuku.wasm")]
  },
])
