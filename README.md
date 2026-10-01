<!-- markdownlint-disable first-line-h1 -->

<!-- markdownlint-start-capture -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="docs/assets/logo.svg" alt="Logo" width="300" />
  
  <br>
  <br>

[![NPM Version](https://img.shields.io/npm/v/yuku-parser?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku-parser)
[![NPM Downloads](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Farshad-yaseen%2Fstatic%2Fmain%2Fbadges%2Fyuku-downloads.json&logo=npm&logoColor=212121&labelColor=ffc44e&color=212121)](https://npmtrends.com/yuku-analyzer-vs-yuku-codegen-vs-yuku-parser)
[![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

Yuku is a high-performance JavaScript and TypeScript compiler toolchain written in Zig. Spec-compliant, zero dependencies, fast by design.

[Try it in the playground →](https://playground.yuku.fyi)

</div>

## Documentation

Visit [yuku.fyi](https://yuku.fyi) for the documentation. Each npm package documents its JavaScript API in its README.

## Parser

### JavaScript

```bash
npm install yuku-parser
```

```js
import { parse } from "yuku-parser";

const { program, comments, diagnostics } = parse("const x = 1 + 2;");
```

Outputs an [ESTree](https://github.com/estree/estree) / [TypeScript-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree)-compatible AST matching [Oxc](https://oxc.rs). Runs 3-10x faster than alternatives on npm, and in browsers with [`@yuku-engine/wasm`](https://www.npmjs.com/package/@yuku-engine/wasm).

### Zig

```bash
zig fetch --save git+https://github.com/yuku-toolchain/yuku.git
```

```zig
var tree = try parser.parse(allocator, "const x = 5;", .{});
defer tree.deinit();
```

[Read the parser documentation →](https://yuku.fyi/parser)

## Codegen

```bash
npm install yuku-codegen
```

```js
import { parse } from "yuku-parser";
import { generate } from "yuku-codegen";

generate(parse("const x = 1 + 2;").program).code;
// "const x = 1 + 2;"

generate(parse("const x: number = 1;", { lang: "ts" }).program, { strip: true }).code;
// "const x = 1;"

generate(parse("const enabled = true;").program, { minify: true }).code;
// "const enabled=!0"
```

Emits a Source Map V3 in the same pass, and runs 2.6x faster than `@babel/generator`, or 3x with source maps on:

```js
const { program } = parse(source);
const { code, map } = generate(program, { sourceMap: { source } });
```

[Read the yuku-codegen documentation →](https://www.npmjs.com/package/yuku-codegen)

## Analyzer

```bash
npm install yuku-analyzer
```

```js
import { Analyzer } from "yuku-analyzer";

const project = new Analyzer();

project.setFile("a.ts", `export const value = 1;`);
project.setFile("b.ts", `export { value as renamed } from "./a.ts";`);
project.setFile("c.ts", `import { renamed } from "./b.ts"; renamed;`);

project.module("c.ts").rootScope.find("renamed").definition().binding.name;
// "value"
```

Scopes, bindings, resolved references, closures, and cross-file module linking, computed in one native pass, 15–20× faster than `@typescript-eslint/scope-manager` and resolving names as the TypeScript checker does.

[Read the yuku-analyzer documentation →](https://www.npmjs.com/package/yuku-analyzer)

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for setup, testing, and playground instructions.

## License

Yuku is free and open-source software licensed under the [MIT License](LICENSE).
