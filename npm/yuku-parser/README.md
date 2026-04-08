# yuku-parser

A high-performance, spec-compliant JavaScript/TypeScript parser written in Zig, powered by [Yuku](https://github.com/yuku-toolchain/yuku).

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

## ESTree / TypeScript-ESTree

For JavaScript and JSX, the AST is fully conformant with the [ESTree](https://github.com/estree/estree) specification, identical to what [Acorn](https://www.npmjs.com/package/acorn) produces.

For TypeScript, the AST conforms to the [TypeScript-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) format used by `@typescript-eslint`.

The only differences from the base ESTree / TypeScript-ESTree specifications are:

- Support for Stage 3 [decorators](https://github.com/tc39/proposal-decorators).
- Support for Stage 3 [import defer](https://github.com/tc39/proposal-defer-import-eval) and [import source](https://github.com/tc39/proposal-source-phase-imports). Dynamic forms (`import.defer(...)`, `import.source(...)`) are represented as an `ImportExpression` with a `phase` field set to `"defer"` or `"source"`, following the ESTree convention.
- A non-standard `hashbang` field on `Program` for `#!/usr/bin/env node` lines.

Any other deviation from Acorn's ESTree or `@typescript-eslint`'s TypeScript-ESTree would be considered a bug.

## AST Types

All AST node types are exported directly from this package:

```ts
import type { Node, Statement, Expression, Identifier } from "yuku-parser";
```

The `Node` union type covers every possible AST node. Individual types like `Statement`, `Expression`, `Declaration`, etc. are also available. See the full list in the [type definitions](https://github.com/yuku-toolchain/yuku/blob/main/npm/yuku-parser/index.d.ts).

## Walking the AST

The AST is standard ESTree, so any ESTree-compatible walker works. For example, with [zimmerframe](https://github.com/sveltejs/zimmerframe):

```js
import { parse } from "yuku-parser";
import { walk } from "zimmerframe";

const { program } = parse(`
  const message = "hello";
  console.log(message);
`);

const state = {};

walk(program, state, {
  Identifier(node, { path }) {
    console.log(node.name);
  },
  VariableDeclaration(node) {
    console.log(node.kind);
  },
});
```

## Options

All options are optional.

```js
const result = parse(source, {
  sourceType: "module",
  lang: "jsx",
  semanticErrors: false,
});
```

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `sourceType` | `"module"`, `"script"` | `"module"` | Module mode enables `import`/`export`, `import.meta`, top-level `await`, and strict mode. |
| `lang` | `"js"`, `"ts"`, `"jsx"`, `"tsx"`, `"dts"` | `"js"` | Language variant controls which syntax extensions are enabled. |
| `semanticErrors` | `true`, `false` | `false` | Run semantic analysis and report semantic errors alongside syntax errors. |

## Result

`parse` returns a `ParseResult`:

```ts
interface ParseResult {
  program: Program;
  comments: Comment[];
  diagnostics: Diagnostic[];
}
```

The parser is error-tolerant, an AST is always produced even when diagnostics are present.

### Diagnostics

Diagnostics cover both syntax errors found during parsing and, when `semanticErrors` is enabled, semantic errors that require scope and binding information (e.g. duplicate `let` declarations, `break` outside a loop, unresolved private fields).

Each diagnostic includes:

- `severity`: `"error"`, `"warning"`, `"hint"`, or `"info"`
- `message`: description of the issue
- `help`: fix suggestion, or `null`
- `start` / `end`: byte offsets into the source
- `labels`: additional source spans with messages for context

### Semantic Errors

By default, the parser only reports syntax errors. Semantic errors require resolving scopes and bindings, which is done in a separate AST pass. Enable this with the `semanticErrors` option:

```js
const result = parse(`let x = 1; let x = 2;`, { semanticErrors: true });
// result.diagnostics will include "Identifier `x` has already been declared", etc.
```

This incurs a very small performance overhead. If your build pipeline already handles semantic validation (e.g. through a linter or type checker), you can leave this off for faster parsing.

### Comments

Each comment includes:

- `type`: `"Line"` or `"Block"`
- `value`: comment text without delimiters (`//`, `/*`, `*/`)
- `start` / `end`: byte offsets

## License

MIT
