# yuku-ast

Walk, build, and check any ESTree / TypeScript-ESTree AST, with typed visitors, in-place mutation, builders, guards, and syntactic utilities, part of [Yuku](https://yuku.fyi).

It is plain JavaScript and works on any ESTree AST, whichever parser produced it. Traversal order comes from tables generated from Yuku's AST definition, so it never drifts from the parser, and there is no runtime key discovery.

- [Install](#install)
- [Walking](#walking)
- [Builders](#builders)
- [Guards](#guards)
- [Imports and exports](#imports-and-exports)
- [Utilities](#utilities)
- [Identifier names](#identifier-names)

## Install

```bash
npm install yuku-ast
```

## Walking

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
    // function declarations, expressions, and arrows
  },
  enter(node) {},
});
```

Handlers are keyed by node `type`, by alias group, or by the universal `enter` / `leave`, and receive the exact node type. The alias groups are `Expression`, `Statement`, `Declaration`, `ModuleDeclaration`, `Function`, `Class`, `Method`, `Loop`, `Pattern`, `JSX`, and `TSType`. Per node, the order is the universal `enter`, alias enters, the typed enter, the children, then the same in reverse for leave.

An optional third argument threads state to every handler as `ctx.state`. `walkAsync` is the async counterpart, with the same traversal order and mutation semantics and every handler awaited before the walk moves on.

### The context

One context object is reused across the whole walk, so do not store it.

```js
ctx.node;        // the current node
ctx.parent;      // its parent, or null at the walk root
ctx.key;         // the field on the parent holding this node
ctx.index;       // position in an array field, or null
ctx.ancestors(); // a copy of the ancestor chain, root first
```

### Mutation

Handlers can mutate the AST in place.

| Operation                | Effect                                                                                                         |
| ------------------------ | -------------------------------------------------------------------------------------------------------------- |
| `ctx.skip()`             | Do not descend into this node's children. `leave` still fires.                                                 |
| `ctx.stop()`             | End the walk immediately.                                                                                      |
| `ctx.replace(node)`      | Swap the current node. The walk continues into the replacement's children and `leave` fires for its new type. |
| `ctx.remove()`           | Splice the node out of an array field, or null a plain field. Children are not walked, `leave` does not fire.  |
| `ctx.insertBefore(node)` | Insert a sibling before the current node. The inserted node is not visited.                                    |
| `ctx.insertAfter(node)`  | Insert a sibling after the current node. The walk visits it.                                                   |

A replacement created with `start: 0, end: 0`, such as a [builder](#builders) node, inherits the original node's span, which keeps source maps meaningful through [`yuku-codegen`](https://www.npmjs.com/package/yuku-codegen).

```js
walk(program, {
  DebuggerStatement(node, ctx) {
    ctx.remove();
  },
});
```

### findAll

`findAll` collects every node of the given types, in source order.

```js
import { findAll } from "yuku-ast";

findAll(program, "CallExpression");
findAll(program, ["ClassDeclaration", "TSInterfaceDeclaration"]);
```

`CHILD_KEYS` maps each node type to its child fields in traversal order, for walkers of your own.

## Builders

`b` has one typed constructor per node type, its fields derived from the node type itself, so a builder never drifts from the AST. Spans default to 0, which `ctx.replace` fills from the replaced node.

```ts
import { b } from "yuku-ast";

b.CallExpression({ callee: b.Identifier({ name: "f" }), arguments: [], optional: false });
```

## Guards

`is` has one guard per node type, per alias group, and for the shapes ESTree folds into one type: literal kinds, member expression kinds, and directives. Every guard accepts `null` and `undefined` and narrows.

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

## Imports and exports

`collectImports` and `collectExports` read a module's import and export declarations, one record per bound name, destructuring included.

```ts
import { collectExports, collectImports } from "yuku-ast";

for (const record of collectImports(program)) {
  record.source;   // "./m"
  record.local;    // the local binding name
  record.imported; // "default", "*", or the export name
  record.typeOnly; // import type / import { type x }
  record.phase;    // "source" | "defer" | null
}

for (const record of collectExports(program)) {
  record.exported; // the exported name, null for a bare export *
  record.local;    // the backing local name, when there is one
  record.source;   // the re-export specifier, when there is one
  record.typeOnly;
}
```

`collectImportDeclaration` and `collectExportDeclaration` return the records of a single statement, for use inside a walk.

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
  bindingIdentifiers, // every binding Identifier a pattern introduces
  isCallOf,           // isCallOf(node, "require")
  isWrapper,          // true for the wrappers unwrap strips
  literalValue,       // string | number | boolean | bigint | RegExp | null
  nameOf,             // Identifier name or string Literal value
  unwrap,             // strips parens and erased TypeScript assertion wrappers
} from "yuku-ast";
```

## Identifier names

```ts
import { isIdentifierName, isValidIdentifier } from "yuku-ast";

isValidIdentifier("foo");   // true
isValidIdentifier("class"); // false, reserved
isIdentifierName("class");  // true, syntactically an IdentifierName
```

Plus `isIdentifierStart`, `isIdentifierChar`, `isKeyword`, `isReservedWord`, `isStrictReservedWord`, `isStrictBindReservedWord`, and `isStrictBindOnlyReservedWord`.

## Semantic analysis

[`yuku-analyzer`](https://www.npmjs.com/package/yuku-analyzer) builds on this walker. Its `module.walk` carries the semantic model in the context, as `ctx.scope`, `ctx.symbol`, and `ctx.reference`.

## License

MIT
