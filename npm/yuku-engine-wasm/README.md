# @yuku-engine/wasm

The WebAssembly build of [Yuku](https://yuku.fyi), for browsers, edge runtimes, and platforms without a native binary. Install it and load it once, and every Yuku package runs on it, with the same API and byte-identical results.

## Install

```bash
npm install @yuku-engine/wasm
```

## Usage

```js
import { init } from "@yuku-engine/wasm";
import { parse } from "yuku-parser";

await init();
parse("const x = 1;");
```

Call `init` once, before the first call to any Yuku package. It fetches `@yuku-engine/wasm/yuku-engine.wasm` by default, or takes a URL, a `Response`, the module's bytes, or a compiled `WebAssembly.Module`, for runtimes that import `.wasm` files as modules.

## License

MIT
