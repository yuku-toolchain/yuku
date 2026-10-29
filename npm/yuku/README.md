<div align="center">

<img src="https://raw.githubusercontent.com/yuku-toolchain/yuku/main/docs/assets/logo.svg" alt="Yuku" width="260" />

<br />
<br />

**Parse, analyze, walk, build, and print JavaScript and TypeScript.**

One package, no third-party dependencies. Written in Zig, running in Node.js, Bun, Deno, browsers, and edge runtimes.

[Docs](https://yuku.fyi/javascript/) · [Playground](https://playground.yuku.fyi) · [GitHub](https://github.com/yuku-toolchain/yuku)

</div>

```bash
npm install yuku
```

It runs natively where its addon for the platform installs, and as WebAssembly everywhere else. Cloudflare Workers and Vercel Edge run WebAssembly only from a file, so there also install `@yuku-toolchain/wasm`.

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

The [guide](https://yuku.fyi/javascript/) documents how to [parse](https://yuku.fyi/javascript/parse/), [walk and build](https://yuku.fyi/javascript/walk/), [analyze](https://yuku.fyi/javascript/semantic/), and [print](https://yuku.fyi/javascript/print/).

## Performance

`yuku` parses faster than the alternatives on npm, on small and large files.

![Parsing react.js (70 KB) on npm](https://raw.githubusercontent.com/yuku-toolchain/ecmascript-parser-benchmark-js/refs/heads/main/charts/react.svg)

![Parsing typescript.js (7.8 MB) on npm](https://raw.githubusercontent.com/yuku-toolchain/ecmascript-parser-benchmark-js/refs/heads/main/charts/typescript.svg)

Analyzing a whole codebase, it is faster than typescript-eslint and the TypeScript API, and on a single large file too.

![Analyzing three.js (755 files) on npm](https://raw.githubusercontent.com/yuku-toolchain/ecmascript-analyzer-benchmark-js/refs/heads/main/charts/three.svg)

![Analyzing typescript.js (7.8 MB) on npm](https://raw.githubusercontent.com/yuku-toolchain/ecmascript-analyzer-benchmark-js/refs/heads/main/charts/typescript_js.svg)

Measured on an Apple M3, see the [parser](https://github.com/yuku-toolchain/ecmascript-parser-benchmark-js) and [analyzer](https://github.com/yuku-toolchain/ecmascript-analyzer-benchmark-js) benchmarks. Pass one `walk` all your visitors.

## License

MIT
