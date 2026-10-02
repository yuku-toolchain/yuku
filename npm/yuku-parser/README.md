# yuku-parser

A fast, spec-compliant JavaScript and TypeScript parser, part of [Yuku](https://yuku.fyi).

- [Install](#install)
- [Usage](#usage)
- [The AST](#the-ast)
- [Options](#options)
- [Result](#result)
- [Diagnostics](#diagnostics)
- [Comments](#comments)
- [Tokens](#tokens)

## Install

```bash
npm install yuku-parser
```

It runs on Yuku's native core, installed for your platform. In browsers and edge runtimes, load the WebAssembly core from [`@yuku-core/wasm`](https://www.npmjs.com/package/@yuku-core/wasm) and pass it in as `core`.

## Usage

```js
import { parse } from "yuku-parser";

const { program, comments, diagnostics } = parse("const x = 1 + 2;");
```

## The AST

[ESTree](https://github.com/estree/estree) for JavaScript and JSX, identical to [Acorn](https://www.npmjs.com/package/acorn), and [TypeScript-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) for TypeScript, matching [Oxc](https://oxc.rs) for both. On top of the specs, it carries stage 3 [decorators](https://github.com/tc39/proposal-decorators), [import defer](https://github.com/tc39/proposal-defer-import-eval) and [import source](https://github.com/tc39/proposal-source-phase-imports) as `phase` on imports and `ImportExpression`, and the `hashbang` of `Program`.

Every node type is exported, listed in the [type definitions](https://github.com/yuku-toolchain/yuku/blob/main/npm/yuku-types/index.d.ts).

```ts
import type { Expression, Identifier, Node, Statement } from "yuku-parser";
```

To walk, build, and check nodes, use [`yuku-ast`](https://www.npmjs.com/package/yuku-ast). For scopes and bindings, use [`yuku-analyzer`](https://www.npmjs.com/package/yuku-analyzer).

## Options

```js
parse(source, { path: "src/app.tsx" });
```

| Option           | Values                                    | Default    | Description                                                                                        |
| ---------------- | ----------------------------------------- | ---------- | -------------------------------------------------------------------------------------------------- |
| `path`           | a file path                               | none       | Names the file in diagnostics. `lang` and `sourceType` default from its extension.                 |
| `lang`           | `"js"`, `"jsx"`, `"ts"`, `"tsx"`, `"dts"` | `"js"`     | The syntax to parse. `.d.ts`, `.tsx`, `.ts`, and `.jsx` paths select their own.                    |
| `sourceType`     | `"module"`, `"script"`, `"commonjs"`      | `"module"` | `"commonjs"` allows top-level `return`, and `.cjs` and `.cts` paths select it.                     |
| `preserveParens` | `true`, `false`                           | `true`     | Keep `ParenthesizedExpression` nodes.                                                              |
| `semanticErrors` | `true`, `false`                           | `false`    | Also report the early errors that need scopes, such as redeclarations and `break` outside a loop.  |
| `attachComments` | `true`, `false`                           | `false`    | Also attach each comment to its node. See [Comments](#comments).                                   |
| `tokens`         | `true`, `false`                           | `false`    | Keep every token. See [Tokens](#tokens).                                                           |
| `core`           | a loaded core                             | native     | The core that parses, see [`@yuku-core/wasm`](https://www.npmjs.com/package/@yuku-core/wasm).      |

An unknown `lang` or `sourceType` throws a `TypeError`. `langFromPath(path)` and `sourceTypeFromPath(path)` return the values a path implies.

## Result

```ts
interface ParseResult {
  program: Program;
  comments: Comment[];
  tokens?: TokenList; // with tokens: true
  diagnostics: Diagnostic[];
}
```

The parser recovers from errors, so a result with diagnostics still holds a tree of everything it could read.

## Diagnostics

Every Yuku package reports diagnostics in one shape.

```ts
interface Diagnostic {
  severity: "error" | "warning" | "hint" | "info";
  message: string;
  path: string | null; // the path option
  start: number;       // UTF-16 offsets, like nodes
  end: number;
  labels: { start: number; end: number; message: string }[];
  help: string | null;
}
```

## Comments

```js
const { comments } = parse(`// a line comment\nconst x = 1; /* a block comment */`);
// [
//   { type: "Line", value: " a line comment", start: 0, end: 17 },
//   { type: "Block", value: " a block comment ", start: 31, end: 52 },
// ]
```

`attachComments: true` also hangs each comment on the node it sits next to, which [`yuku-codegen`](https://www.npmjs.com/package/yuku-codegen#comments) prints from, so comments move with their nodes.

```js
const { program } = parse(`// header\nfunction foo() {} // trailing`, { attachComments: true });

program.body[0].comments;
// [
//   { type: "Line", position: "before", sameLine: false, value: " header" },
//   { type: "Line", position: "after", sameLine: true, value: " trailing" },
// ]
```

`position` is `"before"`, `"after"`, or `"inside"` an otherwise empty node, such as `function f() { /* hi */ }`.

## Tokens

`tokens: true` keeps every token in a `TokenList`, a view over the parser's token table where a token is an index.

```js
import { parse, TokenKind } from "yuku-parser";

const { tokens } = parse(source, { tokens: true });

for (let i = 0; i < tokens.length; i++) {
  if (tokens.kind(i) === TokenKind.Arrow) console.log(tokens.start(i), tokens.text(i));
}
```

```js
tokens.kind(i)                      // one of TokenKind
tokens.text(i)                      // its source text
tokens.start(i)                     // UTF-16 offsets
tokens.end(i)

tokens.isKeyword(i)
tokens.isReserved(i)                // reserved unconditionally or in strict mode
tokens.isUnconditionallyReserved(i)
tokens.isStrictModeReserved(i)
tokens.isIdentifierLike(i)          // an identifier or any keyword
tokens.isNumericLiteral(i)
tokens.isBinaryOperator(i)
tokens.isLogicalOperator(i)
tokens.isUnaryOperator(i)
tokens.isAssignmentOperator(i)
tokens.precedence(i)                // binary precedence, 0 when none

tokens.newlineBefore(i)             // what ASI reads
tokens.escaped(i)
tokens.invalidEscape(i)             // a template chunk whose cooked value is undefined
tokens.loneSurrogate(i)

tokens.range(node)                  // [from, to) of the tokens inside a node
tokens.first(node)
tokens.last(node)
tokens.before(nodeOrOffset)         // the last token ending at or before it
tokens.after(nodeOrOffset)          // the first token starting at or after it
tokens.at(offset)                   // the token containing an offset
```

The queries answer with an index, or `-1` when there is none. Tokens are as the parser resolved them, so a regex is one `RegexLiteral`, and comments are not tokens. The kinds are listed in [tokens.d.ts](https://github.com/yuku-toolchain/yuku/blob/main/npm/yuku-types/tokens.d.ts).

## License

MIT
