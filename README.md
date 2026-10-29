<!-- markdownlint-disable first-line-h1 -->

<!-- markdownlint-start-capture -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="docs/assets/logo.svg" alt="Logo" width="300" />
  
  <br>
  <br>

[![NPM Version](https://img.shields.io/npm/v/yuku?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku)
[![NPM Downloads](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Farshad-yaseen%2Fstatic%2Fmain%2Fbadges%2Fyuku-downloads.json&logo=npm&logoColor=212121&labelColor=ffc44e&color=212121)](https://npmtrends.com/yuku-analyzer-vs-yuku-codegen-vs-yuku-parser)
[![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

Yuku is a high-performance JavaScript/TypeScript compiler toolchain written in Zig.

[Try it in the playground →](https://playground.yuku.fyi)

</div>

## JavaScript

```bash
npm install yuku
```

```js
import { b, parse, print, walk } from "yuku";

const file = parse(`
  const RATE = 0.2;
  export const tax = (price: number) => price * RATE;
  export const scale = (RATE: number) => RATE * 2;
`, { path: "tax.ts", analyze: true });

const rate = file.rootScope.find("RATE");

walk(file.program, {
  Identifier(node, ctx) {
    if (file.referenceOf(node)?.binding === rate) ctx.replace(b.Literal({ value: 0.2 }));
  },
});

print(file, { strip: true }).code;
// const RATE = 0.2;
// export const tax = (price) => price * 0.2;
// export const scale = (RATE) => RATE * 2;
```

One import parses, analyzes, walks, builds, and prints JavaScript and TypeScript, in Node.js, Bun, Deno, browsers, and edge runtimes. See the [JavaScript guide](https://yuku.fyi/javascript/).

## Zig

```bash
zig fetch --save git+https://github.com/yuku-toolchain/yuku.git
```

The parser, AST, semantic analysis, traversal, and codegen are documented at [yuku.fyi](https://yuku.fyi).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) to set up, test, and try a change. Report vulnerabilities privately, as [SECURITY.md](SECURITY.md) describes.

## License

Yuku is licensed under the [MIT License](LICENSE).
