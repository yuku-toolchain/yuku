# yuku-parser

A high-performance, spec-compliant JavaScript/TypeScript parser for Node.js, powered by [Yuku](https://github.com/yuku-toolchain/yuku).

Yuku's parser is written in Zig. It produces an [ESTree](https://github.com/estree/estree) compatible AST for JavaScript/JSX and a [TypeScript-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) compatible AST for TypeScript.

## Install

```bash
npm install yuku-parser
```

## Usage

```js
import { parse } from "yuku-parser";

const result = parse("const x = 1 + 2;");

console.log(result.program);     // ESTree / TypeScript-ESTree Program node
console.log(result.comments);    // all comments
console.log(result.diagnostics); // errors and warnings
```

## Options

```js
const result = parse(source, {
  sourceType: "module",      // "module" (default) or "script"
  lang: "jsx",               // "js" (default), "ts", "jsx", "tsx", "dts"
  semanticErrors: false,     // include semantic errors (default: false)
});
```

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `sourceType` | `"module"`, `"script"` | `"module"` | Module mode enables `import`/`export`, `import.meta`, top-level `await`, and strict mode. |
| `lang` | `"js"`, `"ts"`, `"jsx"`, `"tsx"`, `"dts"` | `"js"` | Language variant controls which syntax extensions are enabled. |
| `semanticErrors` | `true`, `false` | `false` | Run semantic analysis after parsing and include semantic errors (e.g. duplicate declarations, invalid `break`/`continue` targets) alongside syntax errors. This requires a separate AST pass and may affect performance slightly. |

## Result

`parse` returns a `ParseResult`:

```ts
interface ParseResult {
  program: Program;        // root ESTree / TypeScript-ESTree AST node
  comments: Comment[];     // all comments in source order
  diagnostics: Diagnostic[];
}
```

The parser is error-tolerant, an AST is always produced even when diagnostics are present.

### Diagnostics

Each diagnostic has:

- `severity`: `"error"`, `"warning"`, `"hint"`, or `"info"`
- `message`: description of the issue
- `help`: fix suggestion, or `null`
- `start` / `end`: byte offsets into the source
- `labels`: additional source spans with messages for context

### Comments

Each comment has:

- `type`: `"Line"` or `"Block"`
- `value`: comment text without delimiters
- `start` / `end`: byte offsets

## AST

The AST follows [ESTree](https://github.com/estree/estree) for JavaScript/JSX and [TS-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) for TypeScript. It is identical to [Acorn](https://www.npmjs.com/package/acorn) output for JavaScript.

Extensions beyond the base specs, Stage 3 decorators, import defer, import source, and a `hashbang` field on `Program`.

## License

MIT
