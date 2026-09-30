# yuku-parser

A high-performance, spec-compliant JavaScript and TypeScript parser, part of [Yuku](https://yuku.fyi).

- [Install](#install)
- [Usage](#usage)
- [ESTree / TypeScript-ESTree](#estree--typescript-estree)
- [AST types](#ast-types)
- [Options](#options)
- [Path helpers](#path-helpers)
- [Result](#result)
- [Comments](#comments)
- [Tokens](#tokens)
- [Walking the AST](#walking-the-ast)
- [Semantic analysis](#semantic-analysis)

## Install

```bash
npm install yuku-parser
```

It runs on a native binary for each platform. In browsers, edge runtimes, and on platforms without one, it runs on [`@yuku-engine/wasm`](https://www.npmjs.com/package/@yuku-engine/wasm).

## Usage

```js
import { parse } from "yuku-parser";

const { program, comments, diagnostics } = parse("const x = 1 + 2;");
```

## ESTree / TypeScript-ESTree

For JavaScript and JSX, the AST is fully conformant with the [ESTree](https://github.com/estree/estree) specification, identical to what [Acorn](https://www.npmjs.com/package/acorn) produces. For TypeScript, it conforms to the [TypeScript-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) format used by `@typescript-eslint`. For both, it matches the AST [Oxc](https://oxc.rs) produces.

On top of the base specs, the AST carries:

- Stage 3 [decorators](https://github.com/tc39/proposal-decorators).
- Stage 3 [import defer](https://github.com/tc39/proposal-defer-import-eval) and [import source](https://github.com/tc39/proposal-source-phase-imports). The dynamic forms are an `ImportExpression` with `phase` set to `"defer"` or `"source"`.
- A `hashbang` field on `Program` for `#!/usr/bin/env node` lines.

Any other deviation from Acorn's ESTree or `@typescript-eslint`'s TypeScript-ESTree is a bug.

## AST types

Every node type is exported, from the `Node` union down to individual types, listed in the [type definitions](https://github.com/yuku-toolchain/yuku/blob/main/npm/yuku-types/index.d.ts).

```ts
import type { Expression, Identifier, Node, Statement } from "yuku-parser";
```

## Options

All options are optional.

```js
parse(source, { lang: "tsx", sourceType: "module" });
```

| Option           | Values                                    | Default    | Description                                                                                                                                                                                                                     |
| ---------------- | ----------------------------------------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `sourceType`     | `"module"`, `"script"`, `"commonjs"`      | `"module"` | Module mode enables `import`/`export`, `import.meta`, top-level `await`, and strict mode. CommonJS mode parses script code whose top level behaves like a function body, allowing top-level `return`, `new.target`, and `using`. |
| `lang`           | `"js"`, `"ts"`, `"jsx"`, `"tsx"`, `"dts"` | `"js"`     | The syntax extensions to enable.                                                                                                                                                                                                |
| `preserveParens` | `true`, `false`                           | `true`     | Keep `ParenthesizedExpression` nodes. When off, only the inner expression is kept.                                                                                                                                             |
| `semanticErrors` | `true`, `false`                           | `false`    | Also report semantic errors. See [Semantic errors](#semantic-errors).                                                                                                                                                           |
| `attachComments` | `true`, `false`                           | `false`    | Also attach each comment to its host node. See [Comments](#comments).                                                                                                                                                           |
| `tokens`         | `true`, `false`                           | `false`    | Keep every token in `result.tokens`. See [Tokens](#tokens).                                                                                                                                                                     |

## Path helpers

`lang` and `sourceType` can be inferred from a file path. `.d.ts`, `.tsx`, `.ts`, and `.jsx` select their language, and `.cjs` and `.cts` select CommonJS.

```js
import { langFromPath, sourceTypeFromPath } from "yuku-parser";

langFromPath("app.tsx");        // "tsx"
langFromPath("types.d.ts");     // "dts"
sourceTypeFromPath("app.cjs");  // "commonjs"
sourceTypeFromPath("app.mjs");  // "module"
```

## Result

`parse` returns a `ParseResult`.

```ts
interface ParseResult {
  program: Program;
  comments: Comment[]; // every comment in source order
  tokens?: TokenList;  // with tokens: true
  diagnostics: Diagnostic[];
}
```

The parser recovers from errors, so a parse with diagnostics still returns a tree of everything it could read.

### Diagnostics

```ts
interface Diagnostic {
  severity: "error" | "warning" | "hint" | "info";
  message: string;
  help: string | null; // a fix suggestion
  start: number;       // UTF-16 offsets, like nodes
  end: number;
  labels: { start: number; end: number; message: string }[]; // related code
}
```

Every Yuku package reports diagnostics in this shape.

### Semantic errors

The parser reports syntax errors. Errors that need scopes and bindings, such as duplicate `let` declarations, `break` outside a loop, and unresolved private fields, come from a separate and cheap semantic pass, enabled with `semanticErrors`. Leave it off when a linter or type checker already validates the code.

```js
parse(`let x = 1; let x = 2;`, { semanticErrors: true }).diagnostics;
// includes "Identifier 'x' has already been declared"
```

## Comments

`result.comments` always holds every comment in source order, with its source span.

```js
const { comments } = parse(`// a line comment\nconst x = 1; /* a block comment */`);

for (const c of comments) {
  console.log(c.type, JSON.stringify(c.value), c.start, c.end);
}
// Line " a line comment" 0 17
// Block " a block comment " 31 52
```

```ts
interface Comment {
  type: "Line" | "Block";
  value: string; // body without delimiters
  start: number; // delimiters included
  end: number;
}
```

The span covers the whole comment, so `source.slice(c.start, c.end)` returns the raw text.

### Attaching comments to nodes

`attachComments: true` also hangs each comment on the AST node it sits next to, read off `node.comments`. Attached comments move with their node through transforms, which is what [`yuku-codegen`](https://www.npmjs.com/package/yuku-codegen#comments) prints from.

```js
const { program } = parse(`// header\nfunction foo() {} // trailing`, { attachComments: true });

program.body[0].comments;
// [
//   { type: "Line", position: "before", sameLine: false, value: " header" },
//   { type: "Line", position: "after", sameLine: true, value: " trailing" },
// ]
```

```ts
interface AttachedComment {
  type: "Line" | "Block";
  position: "before" | "after" | "inside";
  sameLine: boolean;
  value: string; // body without delimiters
}
```

`position` is where the comment sits relative to its host: `"before"` leads it, `"after"` trails it, and `"inside"` is interior to an otherwise empty host, like `function f() { /* hi */ }`. `sameLine` is true when the comment shares a source line with the host's adjacent edge, the start for `"before"` and the end for `"after"`, and always false for `"inside"`.

## Tokens

`tokens: true` keeps every token the parser consumed. The result carries a `TokenList`, a view over the parser's token table. Nothing is decoded up front, a token is an index, and each accessor is one typed-array read.

```js
import { parse, TokenKind } from "yuku-parser";

const { tokens } = parse(source, { tokens: true });

for (let i = 0; i < tokens.length; i++) {
  if (tokens.kind(i) === TokenKind.Arrow) console.log(tokens.start(i), tokens.text(i));
}
```

```js
tokens.kind(i)            // one of the 160 kinds in TokenKind
tokens.text(i)            // source text, a string literal keeps its quotes
tokens.start(i)           // UTF-16 offsets, like nodes
tokens.end(i)

tokens.isKeyword(i)       // reserved words and contextual keywords
tokens.isReserved(i)      // reserved unconditionally or in strict mode
tokens.isUnconditionallyReserved(i)   // never an identifier
tokens.isStrictModeReserved(i)        // let, static, implements, ...
tokens.isIdentifierLike(i)            // an identifier or any keyword
tokens.isNumericLiteral(i)
tokens.isBinaryOperator(i)
tokens.isLogicalOperator(i)
tokens.isUnaryOperator(i)
tokens.isAssignmentOperator(i)
tokens.precedence(i)      // binary precedence, 0 when none

tokens.newlineBefore(i)   // a line terminator precedes it, what ASI looks at
tokens.escaped(i)         // async is an async token with this set
tokens.invalidEscape(i)   // a template chunk whose cooked value is undefined
tokens.loneSurrogate(i)   // a string with an unpaired surrogate
```

Every node other than `Program`, `TemplateElement`, and `JSXEmptyExpression` starts on a token start and ends on a token end, so a node's tokens are a contiguous run. The queries take a node and answer with an index, `-1` when there is none. They are binary searches, so they replace a token store without building one.

```js
tokens.range(node)   // [from, to) of the tokens inside the node, empty for a node inside one token
tokens.first(node)
tokens.last(node)
tokens.before(node)  // last token ending at or before it, also takes an offset
tokens.after(node)   // first token starting at or after it, also takes an offset
tokens.at(offset)    // the token containing an offset
```

Tokens are as the parser resolved them: a regex is one `RegexLiteral`, and the `>>` closing a nested generic is two `GreaterThan`. Comments are not tokens. The kinds are listed in [tokens.d.ts](https://github.com/yuku-toolchain/yuku/blob/main/npm/yuku-types/tokens.d.ts).

Why an index and not an array of objects? On a 1 MB file, about 215,000 tokens, `tokens: true` adds 1 ms to the parse and scanning every `kind(i)` another 0.4 ms. An object per token would add 8 ms and 20 to 50 MB of heap, which is what tokens cost in espree, acorn, and Babel, and 70 ms in typescript-estree.

## Walking the AST

[`yuku-ast`](https://www.npmjs.com/package/yuku-ast) walks the AST with typed visitors and in-place mutation, and builds and checks nodes.

```js
import { parse } from "yuku-parser";
import { walk } from "yuku-ast";

walk(parse(`console.log("hello");`).program, {
  Identifier(node) {
    console.log(node.name);
  },
});
```

## Semantic analysis

[`yuku-analyzer`](https://www.npmjs.com/package/yuku-analyzer) adds scopes, symbols, resolved references, closure analysis, and cross-file module linking, computed natively in the same pass as the parse.

## License

MIT
