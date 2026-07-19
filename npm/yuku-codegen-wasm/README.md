# @yuku-codegen/wasm

The [`yuku-codegen`](https://www.npmjs.com/package/yuku-codegen) code generator
as a single WebAssembly module. Takes a `Program` from `@yuku-parser/wasm` and
returns generated source.

## Usage

```js
import { parse } from "@yuku-parser/wasm";
import { generate } from "@yuku-codegen/wasm";

const { program } = parse("const x: number = 1", { lang: "ts" });

generate(program); // formatted source
generate(program, { strip: true }); // TypeScript stripped to JavaScript
generate(program, { minify: true }); // minified
generate(program, { strip: true, minify: true }); // both
```
