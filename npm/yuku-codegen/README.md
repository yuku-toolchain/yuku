# yuku-codegen

A fast code generator for any ESTree / TypeScript-ESTree AST, with type stripping, minification, and source maps. Part of [Yuku](https://yuku.fyi).

```bash
npm install yuku-codegen
```

```js
import { generate } from "yuku-codegen";
import { parse } from "yuku-parser";

generate(parse("const x: number = 1;", { lang: "ts" }).program, { strip: true }).code;
// "const x = 1;"
```

See [yuku.fyi/parser/codegen](https://yuku.fyi/parser/codegen/) for documentation.

## License

MIT
