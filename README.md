<!-- markdownlint-disable first-line-h1 -->

<!-- markdownlint-start-capture -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="/docs/public/logo.svg" alt="Logo" width="300" />
  
  <br>
  <br>

[![NPM Version](https://img.shields.io/npm/v/yuku-parser?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku-parser)
[![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

Yuku is a high-performance JavaScript and TypeScript compiler toolchain written in Zig. Spec-compliant, zero dependencies, fast by design.

[Try it in the playground →](https://playground.yuku.fyi)

</div>

## 📖 Documentation

Visit [yuku.fyi](https://yuku.fyi) for the full documentation, guides, and API reference.

## Parser

### JavaScript

```bash
npm install yuku-parser
```

```js
import { parse } from "yuku-parser";

const { program, comments, diagnostics } = parse("const x = 1 + 2;");
```

Outputs an [ESTree](https://github.com/estree/estree) / [TS-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree)-compatible AST matching [Oxc](https://oxc.rs). Runs 4-16x faster than alternatives on npm.

Walk the AST with a typed visitor:

```js
import { parse, walk } from "yuku-parser";

const { program } = parse("const x = 1 + 2;");

walk(program, {
  Identifier(node) {
    console.log(node.name); // x
  },
});
```

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
import { print, strip, minify } from "yuku-codegen";

print(parse("const x = 1 + 2;").program).code;
// "const x = 1 + 2;"

strip(parse("const x: number = 1;", { lang: "ts" }).program).code;
// "const x = 1;"

minify(parse("const enabled = true;").program, { format: "compact" }).code;
// "const enabled=!0;"
```

Emits a Source Map V3 in the same pass, ~2x faster than `@babel/generator` with source maps on:

```js
const { program, lineStarts } = parse(source);
const { code, map } = print(program, { sourceMaps: { lineStarts } });
```

[Read the codegen documentation →](https://yuku.fyi/codegen)

## Analyzer

```bash
npm install yuku-analyzer
```

Scopes, symbols, resolved references, closures, and cross-file module linking in one native pass. Up to 15x faster than `eslint-scope`, `@typescript-eslint/scope-manager`, and Babel.

```js
import { Analyzer, SymbolFlags } from "yuku-analyzer";

const a = new Analyzer();
a.addFile("math.ts", `export const add = (x: number, y: number) => x + y;`);
a.addFile("app.ts", `import { add } from "./math.ts"; add(1, 2); add(3, 4);`);

const app = a.module("app.ts");

// walk with semantic context
app.walk({
  Identifier(node, ctx) {
    console.log(node.name, ctx.scope.kind, ctx.symbol, ctx.reference);
  },
});

const add = app.rootScope.find("add");
add.has(SymbolFlags.Import); // true

const def = add.definition();
def.module.path; // "math.ts"
def.symbol.has(SymbolFlags.Const); // true

a.referencesOf(def.symbol).map((r) => r.module.path); // ["app.ts", "app.ts"]

// and many more
```

[Read the analyzer documentation →](https://yuku.fyi/analyzer)

## Performance

Yuku prioritizes correctness while delivering top-tier speed and efficiency.

- [Native benchmark (Zig/Rust)](https://github.com/yuku-toolchain/ecmascript-parser-benchmark-native) - competitive with Oxc on raw parsing speed
- [npm benchmark](https://github.com/yuku-toolchain/ecmascript-parser-benchmark-js) - 4-16x faster than alternatives

## 🤝 Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for setup, testing, and playground instructions.

## ⚖️ License

Yuku is free and open-source software licensed under the [MIT License](LICENSE).
