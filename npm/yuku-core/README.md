# yuku-core

Yuku's native core, the compiled code every [Yuku](https://yuku.fyi) package runs on. It is installed with them and used by default, so you only import it to choose the core explicitly.

```js
import { load } from "yuku-core";
import { parse } from "yuku-parser";

const core = load();

parse("const x = 1;", { core });
```

`load()` loads the build for your platform. In browsers, edge runtimes, and on platforms without a native build, use [`@yuku-core/wasm`](https://www.npmjs.com/package/@yuku-core/wasm) the same way.

## License

MIT
