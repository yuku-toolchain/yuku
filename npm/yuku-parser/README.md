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
console.log(result.diagnostics); // errors and warnings
```

## ESTree / TypeScript-ESTree

For JavaScript and JSX, the AST is fully conformant with the [ESTree](https://github.com/estree/estree) specification, identical to what [Acorn](https://www.npmjs.com/package/acorn) produces.

For TypeScript, the AST conforms to the [TypeScript-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) format used by `@typescript-eslint`.

Yuku produces exactly the AST that [Oxc](https://oxc.rs) produces, for both JS and TS.

On top of the base specs, the AST also carries:

- Stage 3 [decorators](https://github.com/tc39/proposal-decorators).
- Stage 3 [import defer](https://github.com/tc39/proposal-defer-import-eval) and [import source](https://github.com/tc39/proposal-source-phase-imports). Dynamic forms (`import.defer(...)`, `import.source(...)`) are represented as an `ImportExpression` with a `phase` field set to `"defer"` or `"source"`, following the ESTree convention.
- A non-standard `hashbang` field on `Program` for `#!/usr/bin/env node` lines.

Any other deviation from Acorn's ESTree or `@typescript-eslint`'s TypeScript-ESTree would be considered a bug.

## AST Types

All AST node types are exported directly from this package:

```ts
import type { Node, Statement, Expression, Identifier } from "yuku-parser";
```

The `Node` union type covers every possible AST node. Individual types like `Statement`, `Expression`, `Declaration`, etc. are also available. See the full list in the [type definitions](https://github.com/yuku-toolchain/yuku/blob/main/npm/yuku-parser/index.d.ts).

## Path helpers

Two small helpers are exported for resolving the `lang` and `sourceType` options from a file path:

```ts
import { langFromPath, sourceTypeFromPath } from "yuku-parser";

langFromPath("foo.tsx");           // "tsx"
langFromPath("types.d.ts");        // "dts"
sourceTypeFromPath("foo.cjs");     // "script"
sourceTypeFromPath("foo.mjs");     // "module"
```

## Resolving offsets to `(line, column)`

Nodes and diagnostics carry `start`/`end` offsets (UTF-16 code units). To turn an offset into a `{ line, column }` pair, call `locOf` on the parse result:

```ts
import { parse } from "yuku-parser";

const result = parse(source);
const { line, column } = result.locOf(result.program.body[0].start);
```

Lines are 1-based and columns are 0-based, matching ESTree's `loc` convention. The lookup is an O(log n) binary search: tens of nanoseconds per call, safe to invoke per node during a walk.

When resolving many offsets in roughly source order, `locNear` is faster: it scans from a hint line instead of binary searching. Pass the previously returned `line` as the next `hintLine` so each lookup starts near the answer.

```ts
let hintLine = 1;
for (const node of nodesInSourceOrder) {
  const { line, column } = result.locNear(node.start, hintLine);
  hintLine = line;
}
```

## Walking the AST

[`yuku-ast`](https://www.npmjs.com/package/yuku-ast) is the companion toolkit built for this AST: a typed walker, node builders (`b`), type guards (`is`), and identifier validators.

```ts
import { parse } from "yuku-parser";
import { walk } from "yuku-ast";

const { program } = parse(`
  const message = "hello";
  console.log(message);
`);

walk(program, {
  Identifier(node) {
    console.log(node.name);
  },
  VariableDeclaration(node) {
    console.log(node.kind);
  },
});
```

The AST is also standard ESTree, so any ESTree-compatible walker (e.g. [zimmerframe](https://github.com/sveltejs/zimmerframe)) works just as well.

## Options

All options are optional.

```js
const result = parse(source, {
  sourceType: "module",
  lang: "jsx",
  preserveParens: true,
  allowReturnOutsideFunction: false,
  semanticErrors: false,
  attachComments: false,
});
```

| Option                       | Values                                    | Default    | Description                                                                                                                  |
| ---------------------------- | ----------------------------------------- | ---------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `sourceType`                 | `"module"`, `"script"`                    | `"module"` | Module mode enables `import`/`export`, `import.meta`, top-level `await`, and strict mode.                                    |
| `lang`                       | `"js"`, `"ts"`, `"jsx"`, `"tsx"`, `"dts"` | `"js"`     | Language variant controls which syntax extensions are enabled.                                                               |
| `preserveParens`             | `true`, `false`                           | `true`     | Keep `ParenthesizedExpression` nodes in the AST. When false, parentheses are stripped and only the inner expression is kept. |
| `allowReturnOutsideFunction` | `true`, `false`                           | `false`    | Allow `return` statements outside of functions, at the top level.                                                            |
| `semanticErrors`             | `true`, `false`                           | `false`    | Run semantic analysis and report semantic errors alongside syntax errors.                                                    |
| `attachComments`             | `true`, `false`                           | `false`    | Also attach each comment to its host AST node. The flat `result.comments` list is always present. See [Comments](#comments). |

## Result

`parse` returns a `ParseResult`:

```ts
interface ParseResult {
  program: Program;
  comments: Comment[]; // every comment in source order
  diagnostics: Diagnostic[];
  lineStarts: number[];
  locOf(offset: number): { line: number; column: number };
  locNear(offset: number, hintLine: number): { line: number; column: number };
}
```

The parser is error-tolerant: an AST is always produced even when diagnostics are present.

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

## Comments

Every comment is always in `result.comments`, a flat list in source order with each comment's source span:

```js
const { comments } = parse(`// a line comment\nconst x = 1; /* a block comment */`);

for (const c of comments) {
  console.log(c.type, JSON.stringify(c.value), c.start, c.end);
}
// Line " a line comment" 0 17
// Block " a block comment " 31 52
```

Each entry is:

```ts
interface Comment {
  type: "Line" | "Block";
  value: string; // body without delimiters
  start: number; // byte offset, delimiter included
  end: number;   // byte offset, delimiter included
}
```

The span (`start`/`end`) covers the whole comment, delimiters included, so `source.slice(c.start, c.end)` returns the raw text.

### Attaching comments to nodes

Set `attachComments: true` to also hang each comment on the AST node it sits next to, read off `node.comments`. This is what a codegen pass needs, since attached comments move with their node through transforms.

```js
const { program } = parse(`// header\nfunction foo() {} // trailing`, { attachComments: true });

const fn = program.body[0];
for (const c of fn.comments ?? []) {
  console.log(c.position, c.type, c.value);
}
// before Line " header"
// after  Line " trailing"
```

Each attached comment is:

```ts
interface AttachedComment {
  type: "Line" | "Block";
  position: "before" | "after" | "inside";
  sameLine: boolean;
  value: string; // body without delimiters
}
```

`position` is where the comment sits relative to its host: `"before"` (leading), `"after"` (trailing), or `"inside"` (interior to an otherwise empty host like `function f() { /* hi */ }`). `sameLine` is `true` when the comment shares a source line with the host's adjacent edge.

## License

MIT
