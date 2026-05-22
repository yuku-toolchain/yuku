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

Yuku also matches [Oxc](https://oxc.rs) for both JS and TS, with two intentional deviations from the base specs:

- **Comments are attached to the AST nodes they belong to** rather than exposed as a separate offset-indexed array. See [Comments](#comments) for why.
- **`Identifier` carries a `kind` field** (`"reference" | "binding" | "name" | "label" | "this"`) naming its role in the tree. ESTree collapses these distinct roles onto one `Identifier` type, so consumers normally have to walk the parent context to tell them apart. The parser already knows which is which, so it surfaces the answer as a free field.

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

For bulk work — building your own line/column index, integrating with another source-map or diagnostics library, or skipping the per-call binary search — read `result.lineStarts` directly. It's a sorted array of UTF-16 offsets where each line begins: `lineStarts[i]` is the start of line `i + 1`. Decoded lazily on first access and cached.

## Walking the AST

The AST is standard ESTree, so any ESTree-compatible walker works. For example, with [zimmerframe](https://github.com/sveltejs/zimmerframe):

```ts
import { parse, type Node } from "yuku-parser";
import { walk } from "zimmerframe";

const { program } = parse(`
  const message = "hello";
  console.log(message);
`);

walk(program as Node, null, {
  Identifier(node) {
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
| `attachComments`             | `true`, `false`                           | `false`    | Collect comments and attach them to host AST nodes. See [Comments](#comments).                                               |

## Result

`parse` returns a `ParseResult`:

```ts
interface ParseResult {
  program: Program;
  diagnostics: Diagnostic[];
  lineStarts: number[];
  locOf(offset: number): { line: number; column: number };
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

Comments are attached to the AST node they sit next to. Enable collection with `attachComments`, then read them off any node:

```js
const { program } = parse(
  `// header\nfunction foo() {} // trailing`,
  { attachComments: true },
);

const fn = program.body[0];
for (const c of fn.comments ?? []) {
  console.log(c.position, c.type, c.value);
}
// before Line " header"
// after  Line " trailing"
```

Each comment is:

```ts
interface Comment {
  type: "Line" | "Block";
  position: "before" | "after" | "inside";
  sameLine: boolean;
  value: string; // body without delimiters
}
```

`position` tells you where the comment sits relative to its host:

- `"before"`: leading the host node.
- `"after"`: trailing the host node.
- `"inside"`: interior to an otherwise empty host, like `function f() { /* hi */ }`.

`sameLine` is `true` when the comment shares a source line with the host's adjacent edge (host's start for `before`, host's end for `after`). For `inside` it is always `false`.

When `attachComments` is disabled (the default), comments are skipped like whitespace and no `comments` field appears on any node.

### Why comments are attached to nodes

Comments are part of the source. Any tool that reads or rewrites code needs to know which comments belong with which node, and that link has to survive AST transforms. Yuku puts a single `comments` array on each node, with a `position` field on each comment (`"before"`, `"after"`, `"inside"`). It is the simplest shape that holds up under transforms and stays cheap to decode. Every other approach trades away one of those properties.

**Flat offset-indexed array (ESTree default, Oxc).** A separate `comments: [...]` array on the parse result, sibling to the program, each entry pointing at a source range. Fine for read-only analysis. It falls apart the moment you transform the tree.

- *Transforms desync the array.* Insert, delete, or move a node and every offset downstream of the edit is wrong. The comment that used to sit between `a` and `b` may now sit inside something unrelated, or hang in empty space between two nodes. Re-syncing means walking the entire array and rewriting offsets on every edit, and even then the meaning of "the comment between these two things" cannot really be preserved by offsets alone.
- *Codegen cannot trust the offsets.* A printer takes whatever AST it is handed. With offset-based comments, a transformed AST will either print comments in the wrong places or drop them entirely. With attached comments, when you move a node its comments come with it. The printer reads them off `node.comments` and emits them next to the node, no reconciliation needed.
- *Lookup is awkward.* "Give me the comments belonging to this node" with a flat array is a binary search at best and a linear scan at worst. `node.comments` is O(1) and always exact.

**Parser returns a flat list, you attach on the JS side (Acorn).** Acorn's `onComment` option pushes every comment into an array you provide, and that is all it does. To get them onto AST nodes you have to run a separate JS pass like `escodegen.attachComments`, which walks the tree, scans the comment list alongside it, and tags each comment as leading or trailing on the nearest node. That work is unavoidable if you want to transform the tree, and doing it in JavaScript is slow. Yuku does the same attachment in Zig as part of the parse. On the 7.8 MB `typescript.js` (30k+ comments), turning `attachComments` on adds only around 4ms.

**Three separate fields per node (Babel).** `leadingComments`, `trailingComments`, and `innerComments`. Same idea as Yuku, but split across three arrays. Two costs come with the split.

- *Decode work.* Three fields per node means three slots to check, three potential array allocations, and roughly 3x the work on the JS side for the same information.
- *Duplication.* A comment that sits between two nodes is the `trailingComments` of one and the `leadingComments` of the other. Transforms have to track both copies and keep them in sync.

A single `comments` array with a `position` field carries the same meaning, with one entry per comment and one place to look.

Even with `attachComments` enabled, Yuku still parses the same file roughly 4x faster than Babel does without comments attached at all. When the flag is off (the default), comments are skipped like whitespace and there is no attachment work at all.

## License

MIT
