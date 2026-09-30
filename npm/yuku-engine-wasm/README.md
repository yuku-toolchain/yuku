# @yuku-engine/wasm

The WebAssembly build of the Yuku toolchain, for browsers, edge runtimes, and platforms without a native binary. Load it once, and every Yuku package runs on it.

```bash
npm install @yuku-engine/wasm
```

```js
import { init } from "@yuku-engine/wasm";

await init();
```

See [yuku.fyi](https://yuku.fyi/#webassembly) for documentation.

## License

MIT
