# @yuku-analyzer/wasm

The [`yuku-analyzer`](https://www.npmjs.com/package/yuku-analyzer) semantic
analyzer as a single WebAssembly module: scopes, symbols, resolved references,
and cross-file linking, with the same API as the native package.

## Usage

```js
import { analyze } from "@yuku-analyzer/wasm";

const m = analyze("const x = 1; x;");
const x = m.rootScope.find("x");
console.log(x.references.length); // 1
```
