# @yuku-parser/wasm

The [`yuku-parser`](https://www.npmjs.com/package/yuku-parser) JavaScript/TypeScript
parser as a single WebAssembly module. Runs anywhere: browsers, bundlers, Deno,
Bun and Node. Same API and same AST as `yuku-parser`. Pairs with [`yuku-ast`](https://www.npmjs.com/package/yuku-ast), which is pure JavaScript and walks the AST anywhere this package runs.

## Install

```sh
npm install @yuku-parser/wasm
```

## Usage

```js
import { parse } from "@yuku-parser/wasm";

const { program, comments, diagnostics } = parse("const x: number = 1", {
  lang: "ts",
});
```

`parse(source, options?)` returns `{ program, comments, diagnostics, scan }`.
Options:

| Option                       | Default    | Description                               |
| ---------------------------- | ---------- | ----------------------------------------- |
| `lang`                       | `"js"`     | `"js" \| "ts" \| "jsx" \| "tsx" \| "dts"` |
| `sourceType`                 | `"module"` | `"module" \| "script" \| "commonjs"`     |
| `preserveParens`             | `true`     | Keep `ParenthesizedExpression` nodes      |
| `semanticErrors`             | `false`    | Also run semantic analysis                |
| `attachComments`             | `false`    | Attach comments to their host node        |
