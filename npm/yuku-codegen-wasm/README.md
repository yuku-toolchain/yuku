# yuku-codegen-wasm

The [`yuku-codegen`](https://www.npmjs.com/package/yuku-codegen) code generator
as a single WebAssembly module. Takes a `Program` from `yuku-parser-wasm` and
returns generated source.

## Usage

```js
import { parse } from "yuku-parser-wasm";
import { print, strip, minify } from "yuku-codegen-wasm";

const { program } = parse("const x: number = 1", { lang: "ts" });

print(program); // formatted source
strip(program); // TypeScript stripped to JavaScript
minify(program); // compact, comments dropped
```
