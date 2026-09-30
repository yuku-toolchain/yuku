# yuku-parser

A fast, spec-compliant JavaScript and TypeScript parser, producing an ESTree / TypeScript-ESTree AST. Part of [Yuku](https://yuku.fyi).

```bash
npm install yuku-parser
```

```js
import { parse } from "yuku-parser";

const { program, comments, diagnostics } = parse("const x: number = 1;", { lang: "ts" });
```

See [yuku.fyi/parser](https://yuku.fyi/parser/) for documentation.

## License

MIT
