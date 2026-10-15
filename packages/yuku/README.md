# yuku

[Yuku](https://yuku.fyi) is a JavaScript and TypeScript compiler toolchain written in Zig.

```bash
npm install yuku
```

It has no dependencies.

## Usage

Parse JavaScript, TypeScript, or JSX.

```js
import { parse } from "yuku";

const { program, comments, diagnostics } = parse("const x: number = 1;", { lang: "ts" });
```

Resolve scopes, bindings, and references, and link imports across files.

```js
import { analyze } from "yuku/analyzer";

const module = analyze("const double = (n) => n * 2; double(21);");

module.rootScope.find("double").references.length; // 1
```

Walk, build, and check nodes.

```js
import { walk } from "yuku/ast";

walk(program, {
  Identifier(node) {
    console.log(node.name);
  },
});
```

Print an AST back to source, with type stripping, minification, and source maps.

```js
import { generate } from "yuku/codegen";

generate(program, { strip: true }).code; // "const x = 1;"
```

Type every node.

```ts
import type { Node, Program } from "yuku/types";
```

It runs in Node.js 16.4 or newer, Bun, Deno, browsers, and edge runtimes such as Cloudflare Workers and Vercel Edge.

## Documentation

| Import          | Docs                                                              |
| --------------- | ----------------------------------------------------------------- |
| `yuku`          | [Parser](https://yuku.fyi/parser/#javascript)                     |
| `yuku/analyzer` | [Semantic analysis](https://yuku.fyi/parser/semantic/#javascript) |
| `yuku/ast`      | [AST](https://yuku.fyi/parser/ast/#javascript)                    |
| `yuku/codegen`  | [Codegen](https://yuku.fyi/parser/codegen/#javascript)            |
| `yuku/types`    | [AST](https://yuku.fyi/parser/ast/#javascript)                    |

To parse a [dialect](https://yuku.fyi/parser/dialect/#javascript), pass its WebAssembly build as the `wasm` option.

## License

MIT
