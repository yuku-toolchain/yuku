# @yuku-core/wasm

Yuku's WebAssembly core, for browsers, edge runtimes, and platforms without a native build. Every [Yuku](https://yuku.fyi) package runs on it, with the same API and byte-identical results.

## Install

```bash
npm install @yuku-core/wasm
```

## Usage

Load the core once and pass it to every package.

```js
import { load } from "@yuku-core/wasm";
import { parse } from "yuku-parser";
import { Analyzer } from "yuku-analyzer";

const core = await load();

parse("const x = 1;", { core });
new Analyzer({ core });
```

`load()` loads `@yuku-core/wasm/yuku-core.wasm`, fetching it in browsers and reading it from disk in Node.js and Bun. To load it from elsewhere, pass a URL, a `Response`, the module's bytes, or a compiled `WebAssembly.Module`.

Runtimes that import `.wasm` files as compiled modules, such as Cloudflare Workers, load the core synchronously with `loadSync`, which takes the module or its bytes.

```js
import { loadSync } from "@yuku-core/wasm";
import wasm from "@yuku-core/wasm/yuku-core.wasm";

const core = loadSync(wasm);
```

## License

MIT
