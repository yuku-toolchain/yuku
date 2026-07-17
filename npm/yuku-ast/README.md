# yuku-ast

A typed, mutating AST walker and syntactic utilities for JavaScript and TypeScript ESTree trees, powered by [Yuku](https://github.com/yuku-toolchain/yuku).

Works with any ESTree / TypeScript-ESTree AST. Traversal order is driven by tables generated from the [yuku-parser](https://www.npmjs.com/package/yuku-parser) AST definition, so it can never drift from the parser, and there is no runtime key discovery.

## Install

```bash
npm install yuku-ast
```

## Walking

Handlers are keyed by node `type`, by alias group, or the universal `enter` / `leave`. Every handler receives the exact node type.

```ts
import { parse } from "yuku-parser";
import { walk } from "yuku-ast";

const { program } = parse(source);

walk(program, {
  Identifier(node) {
    console.log(node.name);
  },
  CallExpression: {
    enter(node, ctx) {},
    leave(node, ctx) {},
  },
  Function(node) {
    // fires for function declarations, expressions, and arrows
  },
  enter(node) {},
});
```

Aliases: `Expression`, `Statement`, `Declaration`, `ModuleDeclaration`, `Function`, `Class`, `Method`, `Loop`, `Pattern`, `JSX`, `TSType`. Per node the order is universal `enter`, alias enters, the typed enter, children, then the mirror for leave.

The context exposes the position (`ctx.parent`, `ctx.key`, `ctx.index`, `ctx.ancestors()`), flow control (`ctx.skip()`, `ctx.stop()`), and in-place mutation: `ctx.replace(node)` continues into the replacement, `ctx.remove()` skips the removed subtree, `ctx.insertBefore(node)` inserts a sibling without visiting it, `ctx.insertAfter(node)` inserts one the walk visits. An optional third argument threads state to every handler as `ctx.state`.

`walkAsync` is the async counterpart: identical traversal and mutation semantics, every handler awaited before the walk moves on.

## Builders

One constructor per node type, its fields derived from the node type itself, so a builder can never drift from the AST. Spans default to 0, which `ctx.replace` fills from the replaced node.

```ts
import { b } from "yuku-ast";

b.Identifier({ name: "x" });
b.CallExpression({ callee: b.Identifier({ name: "f" }), arguments: [], optional: false });
```

## Guards

```ts
import { is } from "yuku-ast";

is.CallExpression(node);
is.Identifier(node, "require");
is.oneOf(node, ["FunctionDeclaration", "ClassDeclaration"]);
is.Expression(node);
is.StringLiteral(node);
is.StaticMemberExpression(node);
is.Directive(node);
```

One guard per concrete node type, per alias group, and for the common shapes ESTree folds into one type: literal kinds, member expression kinds, and directives. Every guard accepts `null` and `undefined` and narrows.

## Modules

```ts
import { collectImports, collectExports } from "yuku-ast";

for (const record of collectImports(program)) {
  record.source;   // "./m"
  record.local;    // the local binding name
  record.imported; // "default", "*", or the export name
  record.typeOnly; // import type / import { type x }
  record.phase;    // "source" | "defer" | null
}

for (const record of collectExports(program)) {
  record.exported; // the exported name, null for bare export *
  record.local;    // the backing local name, when there is one
  record.source;   // the re-export specifier, when there is one
  record.typeOnly;
}
```

Declaration forms expand to one record per bound name, destructuring included. The per-declaration forms `collectImportDeclaration` and `collectExportDeclaration` return the records of a single statement, composing with a walk:

```ts
walk(program, {
  ImportDeclaration(node) {
    records.push(...collectImportDeclaration(node));
  },
});
```

## Utilities

```ts
import {
  nameOf,          // Identifier name or string Literal value
  literalValue,    // string | number | boolean | bigint | RegExp | null
  unwrap,          // strips parens and erased TS assertion wrappers
  isCallOf,        // isCallOf(node, "require")
  bindingIdentifiers, // every binding Identifier a pattern introduces
  findAll,         // findAll(program, "CallExpression")
} from "yuku-ast";
```

## Identifiers

```ts
import { isValidIdentifier, isIdentifierName, isKeyword } from "yuku-ast";

isValidIdentifier("foo");   // true
isValidIdentifier("class"); // false, reserved
isIdentifierName("class");  // true, syntactically an IdentifierName
```

Plus `isIdentifierStart`, `isIdentifierChar`, `isReservedWord`, `isStrictReservedWord`, `isStrictBindReservedWord`, `isStrictBindOnlyReservedWord`.

## Semantic analysis

[`yuku-analyzer`](https://www.npmjs.com/package/yuku-analyzer) builds on this walker and adds full semantics: scopes, symbols, resolved references, closure analysis, and cross-file module linking, computed natively. Its `module.walk` carries the semantic model in context (`ctx.scope`, `ctx.symbol`, `ctx.reference`).
