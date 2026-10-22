<div align="center">

<img src="https://raw.githubusercontent.com/yuku-toolchain/yuku/main/docs/assets/logo.svg" alt="Yuku" width="260" />

<br />
<br />

**Parse, analyze, walk, build, and print JavaScript and TypeScript.**

One package, no dependencies. Written in Zig, running in Node.js, Bun, Deno, browsers, and edge runtimes.

[Playground](https://playground.yuku.fyi) · [Zig docs](https://yuku.fyi) · [GitHub](https://github.com/yuku-toolchain/yuku)

</div>

```bash
npm install yuku
```

```js
import { js, parse, pattern, print, walk } from "yuku";

const file = parse(`console.log("ready", port);`);
const log = pattern("console.log($$$ARGS)");

walk(file.program, {
  CallExpression(node, ctx) {
    const captures = log.match(node);
    if (captures !== null) ctx.replace(js`logger.info(${captures.ARGS})`);
  },
});

print(file).code; // 'logger.info("ready", port);'
```

```js
const file = parse(`const double = (n) => n * 2;\ndouble(21);`, { semantic: true });

const double = file.rootScope.find("double");
double.references.length; // 1
double.rename("twice");   // [], no conflicts, so it renamed

print(file).code; // "const twice = (n) => n * 2;\ntwice(21);"
```

## Contents

- [Parse](#parse): [options](#options), [diagnostics](#diagnostics), [comments](#comments), [tokens](#tokens)
- [The AST](#the-ast)
- [Walk](#walk): [the context](#the-context), [editing](#editing), [async work](#async-work)
- [Patterns](#patterns)
- [Build](#build): [templates](#templates), [builders](#builders), [guards and helpers](#guards-and-helpers)
- [Semantic analysis](#semantic-analysis): [scopes](#scopes), [bindings](#bindings), [references](#references), [imports and exports](#imports-and-exports)
- [Rename](#rename)
- [Projects](#projects)
- [Print](#print)
- [Performance](#performance)

## Parse

`parse` reads JavaScript, TypeScript, or JSX, from a string or UTF-8 bytes, into a `SourceFile`.

```js
import { parse } from "yuku";

const file = parse(source, { path: "src/app.tsx" });

file.program;     // the ESTree / TypeScript-ESTree Program
file.comments;    // every comment, in source order
file.diagnostics;
file.source;
file.path;
file.lang;        // "tsx", from the path
file.sourceType;  // "module"
```

The parser recovers from errors, so a file with diagnostics still has a program.

### Options

| Option           | Default                                             | Description                                                           |
| ---------------- | --------------------------------------------------- | --------------------------------------------------------------------- |
| `path`           | none                                                | Names the file, and sets `lang` and `sourceType`                      |
| `lang`           | from `path`, else `"js"`                            | `"js"`, `"jsx"`, `"ts"`, `"tsx"`, or `"dts"`                          |
| `sourceType`     | `"commonjs"` for `.cjs` and `.cts`, else `"module"` | `"module"`, `"script"`, or `"commonjs"`                               |
| `preserveParens` | `true`                                              | Keep `ParenthesizedExpression` nodes                                  |
| `attachComments` | `false`                                             | Attach each comment to its node, see [Comments](#comments)            |
| `tokens`         | `false`                                             | Collect `file.tokens`                                                 |
| `semantic`       | `false`                                             | Build the [semantic model](#semantic-analysis)                        |
| `semanticErrors` | `false`                                             | Also report the early errors that need scopes, such as redeclarations |
| `wasm`           | the native addon, else the build `yuku` ships       | A WebAssembly build to run instead                                    |

A file holds what it was parsed with, so reading `file.tokens` or the model of a file parsed without them throws. `langFromPath(path)` and `sourceTypeFromPath(path)` return what a path implies.

### Diagnostics

Every part of Yuku reports problems in the code in one shape, and throws only when called wrongly.

```ts
interface Diagnostic {
  severity: "error" | "warning" | "hint" | "info";
  message: string;
  path: string | null;
  start: number; // UTF-16 offsets, like nodes
  end: number;
  labels: { start: number; end: number; message: string }[];
  help: string | null;
}
```

```js
for (const { start, message } of file.diagnostics) {
  const { line, column } = file.locate(start);
  console.log(`${file.path}:${line}:${column + 1} ${message}`);
}
```

### Comments

`file.comments` lists every comment as `{ type, value, start, end }`. With `attachComments: true`, each also belongs to the node it prints with.

```js
const { program } = parse(`// header\nfunction foo() {} // trailing`, { attachComments: true });

program.body[0].comments;
// [
//   { type: "Line", value: " header", start: 0, end: 9, position: "before", sameLine: false },
//   { type: "Line", value: " trailing", start: 28, end: 39, position: "after", sameLine: true },
// ]
```

[`print`](#print) prints from `node.comments`, so comments move with their nodes.

### Tokens

`file.tokens` is a `TokenList`, where a token is an index.

```js
import { parse, TokenKind } from "yuku";

const { tokens } = parse(source, { tokens: true });

for (let i = 0; i < tokens.length; i++) {
  if (tokens.kind(i) === TokenKind.Arrow) console.log(tokens.start(i), tokens.text(i));
}
```

```js
tokens.kind(i)                      // one of TokenKind
tokens.text(i)
tokens.start(i)                     // UTF-16 offsets
tokens.end(i)

tokens.isKeyword(i)
tokens.isReserved(i)
tokens.isUnconditionallyReserved(i)
tokens.isStrictModeReserved(i)
tokens.isIdentifierLike(i)
tokens.isNumericLiteral(i)
tokens.isBinaryOperator(i)
tokens.isLogicalOperator(i)
tokens.isUnaryOperator(i)
tokens.isAssignmentOperator(i)
tokens.precedence(i)

tokens.newlineBefore(i)
tokens.escaped(i)
tokens.invalidEscape(i)
tokens.loneSurrogate(i)

tokens.range(node)                  // [from, to) of the tokens inside a node
tokens.before(offset)               // the last token ending at or before an offset
tokens.after(offset)                // the first token starting at or after it
tokens.at(offset)                   // the token containing it
```

The queries return an index, or `-1` when there is none.

## The AST

The program is plain objects: [ESTree](https://github.com/estree/estree) for JavaScript and JSX, and [TypeScript-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) for TypeScript, matching [Oxc](https://oxc.rs) down to every field. Every node has `start` and `end` in UTF-16 offsets, and the package exports the type of every node.

```ts
import type { CallExpression, Expression, Node, Program } from "yuku";
```

## Walk

`walk` visits a node and everything under it, depth-first in source order.

```js
import { walk } from "yuku";

walk(file.program, {
  Identifier(node, ctx) {},
  CallExpression: {
    enter(node, ctx) {},
    leave(node, ctx) {},
  },
  Function(node) {}, // declarations, expressions, and arrows
  enter(node) {},    // every node
  leave(node) {},
});
```

Handlers are keyed by node `type`, by alias (`Expression`, `Statement`, `Declaration`, `ModuleDeclaration`, `Function`, `Class`, `Method`, `Loop`, `Pattern`, `JSX`, `TSType`), or by `enter` and `leave`. A list of visitors walks in one pass.

```js
walk(file.program, [noDebugger, noConsole, preferConst]);
```

### The context

```js
ctx.parent;      // null at the walk root
ctx.key;         // the field of the parent holding it
ctx.index;       // its index in an array field, or null
ctx.ancestors(); // from the root down to the parent
```

The [semantic model](#semantic-analysis) answers for any node a walk visits, as `file.scopeOf(node)`, `file.bindingOf(node)`, and `file.referenceOf(node)`.

### Editing

| Call                     | Effect                                                               |
| ------------------------ | -------------------------------------------------------------------- |
| `ctx.skip()`             | Skip the children                                                    |
| `ctx.stop()`             | End the walk                                                         |
| `ctx.replace(node)`      | Put a node, or a list in an array field, in its place, then visit it |
| `ctx.remove()`           | Remove it                                                            |
| `ctx.insertBefore(node)` | Insert a sibling the walk passes over                                |
| `ctx.insertAfter(node)`  | Insert a sibling the walk visits next                                |

```js
walk(file.program, {
  DebuggerStatement(node, ctx) {
    ctx.remove();
  },
});
```

### Async work

A walk runs synchronously. For async work, collect the nodes in one walk, await them together, and edit in a second.

```js
const calls = [];
walk(file.program, { CallExpression: (node) => calls.push(node) });

const results = new Map(await Promise.all(calls.map(async (node) => [node, await rewrite(node)])));

walk(file.program, {
  CallExpression(node, ctx) {
    const result = results.get(node);
    if (result !== undefined) ctx.replace(result);
  },
});
```

## Patterns

`pattern` compiles code with holes into a test of nodes, which runs in a walk.

```js
import { pattern, walk } from "yuku";

const method = pattern("$OBJ.$METHOD($$$ARGS)");

walk(file.program, {
  CallExpression(node) {
    const captures = method.match(node); // or null
    captures?.OBJ;    // one node
    captures?.ARGS;   // a list of nodes
  },
});

method.type; // "CallExpression", the visitor key it matches under
```

| Hole      | Matches                                                      |
| --------- | ------------------------------------------------------------ |
| `$NAME`   | Any one node. A name used twice matches equal nodes.         |
| `$$$NAME` | Any run of items in a list, such as arguments or statements. |
| `$_`      | Any one node, uncaptured.                                    |

```js
pattern("$X === $X");          // a === a, f(x) === f(x)
pattern("f($$$BEFORE, last)"); // f(last), f(1, 2, last)
pattern("if ($TEST) $BODY;");  // any if without an else
```

A pattern ending in `;` matches statements, and anything else matches expressions. A hole stands for the whole node that is nothing but it, so `$S;` matches any statement, `let $D;` any declarator, `x as $T` any type, and `{ $P }` any property. A lone hole such as `$X` would match every node, so it throws. In TypeScript, the captures are typed from the pattern.

Any number of patterns share one walk, each under its type.

```js
const rules = [pattern("console.log($$$ARGS)"), pattern("debugger;")];
const found = [];

walk(
  file.program,
  rules.map((rule) => ({
    [rule.type]: (node) => {
      if (rule.match(node) !== null) found.push(node);
    },
  })),
);
```

## Build

### Templates

`js` and `ts` build nodes from code, an expression unless it ends in `;`.

```js
import { js, ts } from "yuku";

js`console.log(${node})`;
js`return ${value} ?? ${fallback};`;
ts`const total: ${type} = ${initial};`;
```

| A hole takes                         | And becomes                                 |
| ------------------------------------ | ------------------------------------------- |
| a node                               | that node                                   |
| a string                             | a name, which must be a valid identifier    |
| a number, boolean, bigint, or `null` | its literal                                 |
| `undefined`                          | the identifier `undefined`                  |
| an array                             | its items, filling a list such as arguments |

A hole takes a node of the kind it stands for, as a pattern's does, such as a statement in `${s};` or a declarator in `let ${d};`, or a name to fill it, and throws for a node that is neither. A template parses once per call site, and its nodes have no position.

### Builders

`b` has a constructor per node type, defaulting spans to 0 and any field that may be false, null, or empty.

```js
import { b } from "yuku";

b.CallExpression({ callee: b.Identifier({ name: "f" }) });
// { type: "CallExpression", start: 0, end: 0, arguments: [], optional: false, callee: ... }

b.ArrowFunctionExpression({ body: b.Identifier({ name: "x" }), expression: true }); // () => x
```

`clone(node)` deep-copies a node.

### Guards and helpers

```js
import { is } from "yuku";

is.CallExpression(node);
is.Expression(node);              // aliases too
is.oneOf(node, ["FunctionDeclaration", "ClassDeclaration"]);
is.StringLiteral(node);           // also NumericLiteral, BooleanLiteral, NullLiteral,
                                  // BigIntLiteral, and RegExpLiteral
is.StaticMemberExpression(node);  // also ComputedMemberExpression and PrivateFieldExpression
is.Directive(node);
is.Wrapper(node);                 // parentheses or an erased TypeScript assertion
```

```js
import { bindingIdentifiers, lexical, nameOf, unwrap } from "yuku";

nameOf(node);             // an Identifier's name or a string Literal's value
unwrap(node);             // the expression inside every wrapper
bindingIdentifiers(node); // every Identifier a pattern declares

lexical.isValidIdentifier("class");  // false, reserved
lexical.isIdentifierName("class");   // true
```

`lexical` also has `isIdentifierStart`, `isIdentifierChar`, `isKeyword`, `isReservedWord`, `isStrictReservedWord`, and `isEvalOrArguments`.

## Semantic analysis

The semantic model resolves every name in a file, values and types alike, as the TypeScript checker does.

```js
const file = parse(source, { path: "app.ts", semantic: true });

file.scopes;               // by id, the global scope first
file.rootScope;            // the module scope, or the global scope of a script
file.bindings;             // by id
file.references;           // in source order
file.imports;
file.exports;
file.moduleFlags;          // { usesRequire, usesModule, usesExports, usesImportMeta }

file.bindingOf(node);      // the binding it declares or refers to
file.referenceOf(node);
file.scopeOf(node);
file.capturesOf(fn);       // [{ binding, references, isWritten }]
file.parentOf(node);
file.ancestors(node);      // the node, then each parent up to the root
file.nodeAt(offset);
```

### Scopes

```js
scope.kind;        // "global" | "module" | "function" | "functionBody" | "block" | "class"
                   // | "staticBlock" | "expressionName" | "tsModule"
scope.strict;
scope.node;
scope.parent;
scope.hoistTarget; // where a var declared in it lands
scope.bindings;

scope.find("x");           // a binding it declares
scope.lookup("x", "type"); // resolves a name as code in it would, "value" by default
scope.contains(other);
scope.ancestors();
```

### Bindings

```js
binding.name;
binding.scope;
binding.declarations; // each declaration's name node
binding.references;   // its uses in this file
binding.import;       // the import that declares it
binding.flags;        // a BindingFlags bitset

binding.has(BindingFlags.Function);
binding.has(BindingFlags.Class | BindingFlags.Interface); // any of them
binding.visibleIn("type");

binding.definition();     // { file, binding, augmentations }, following imports
binding.findReferences(); // its uses across a project
binding.rename("next");
```

| Flag                     | Set on                                  |
| ------------------------ | --------------------------------------- |
| `FunctionScopedVariable` | `var`, a parameter, or a catch variable |
| `BlockScopedVariable`    | `let`, `const`, `using`, `await using`  |
| `Function`               | a function                              |
| `Class`                  | a class                                 |
| `RegularEnum`            | `enum`                                  |
| `ConstEnum`              | `const enum`                            |
| `ValueModule`            | a namespace with runtime content        |
| `Interface`              | `interface`                             |
| `TypeAlias`              | `type`                                  |
| `TypeParameter`          | `<T>`, `infer T`, a mapped type key     |
| `NamespaceModule`        | a namespace of any kind                 |
| `ValueImport`            | `import x`, `import { x }`              |
| `TypeImport`             | `import type`, `import { type x }`      |
| `Const`                  | `const`, `using`, `await using`         |
| `Ambient`                | `declare`                               |
| `Parameter`              | a parameter                             |
| `CatchVariable`          | `catch (e)`                             |
| `Exported`               | `export`, or implicitly in ambient code |
| `Default`                | `export default <declaration>`          |
| `EnumMember`             | an enum member                          |
| `Variable`               | any variable                            |
| `Import`                 | any import                              |
| `ValueSpace`             | anything visible at runtime             |
| `TypeSpace`              | anything usable as a type               |
| `NamespaceSpace`         | what a dotted type name can start from  |

### References

```js
reference.name;
reference.node;           // the Identifier or JSXIdentifier
reference.scope;
reference.binding;        // null for a global or undeclared name
reference.space;          // "value" | "type" | "namespace" | "typeof" | "any"
reference.inTypePosition;
reference.isWrite;        // x = 1, x++, for (x of xs)
```

### Imports and exports

```js
imp.kind;         // "named" | "namespace" | "sideEffect" | "importEquals" | "dynamic"
                  // | "require" | "augmentation"
imp.name;         // "default" for a default import
imp.local;        // the binding it declares
imp.specifier;
imp.typeOnly;
imp.phase;        // "source" | "defer" | null
imp.node;
imp.resolvedFile; // in a project

exp.kind;         // "named" | "reExport" | "namespace" | "star" | "equals" | "global"
exp.name;         // N for export as namespace N, null for star and equals
exp.local;        // the binding it exports
exp.specifier;
exp.fromName;     // the name a reExport takes
exp.typeOnly;
exp.node;
exp.resolvedFile;
```

## Rename

`binding.rename(name)` renames a binding where it is declared and used, keeping the file's exports.

```js
const file = parse(`export const a = 1;\nlet o = { a };`, { semantic: true });

file.rootScope.find("a").rename("b"); // []
print(file).code;
// const b = 1;
// export { b as a };
// let o = { a: b };
```

When renaming would change what the code means, such as by shadowing or capturing another name, it changes nothing and returns the reasons as [diagnostics](#diagnostics).

## Projects

A `Project` links the imports of many files.

```js
import { Project } from "yuku";

const project = new Project();

project.set("a.ts", `export const value = 1;`);
project.set("b.ts", `export { value as renamed } from "./a.ts";`);
const c = project.set("c.ts", `import { renamed } from "./b.ts"; renamed;`);

c.rootScope.find("renamed").definition().binding.name; // "value"
```

```js
const project = new Project({
  // a path in the project, false for a module outside it, null when it cannot be resolved
  resolve: (specifier, importer) => aliases.get(specifier) ?? null,
});

project.set(path, source, options); // the SourceFile, replacing any at the same path
project.delete(path);
project.files;                      // ReadonlyMap<string, SourceFile>
project.diagnostics;                // every file's, and the links that fail

file.dependencies;       // the files it imports from
file.dependents;         // the files that import it
file.exportedNames();
file.resolveExport("x"); // { file, binding, augmentations }
file.isCurrent;          // false once its path is set again or deleted
```

By default, relative specifiers resolve as TypeScript resolves them, and packages are external.

## Print

`print` turns a file or any node back into code.

```js
import { print } from "yuku";

const { code, map, diagnostics } = print(file, { strip: true, sourceMap: true });
```

| Option      | Default                                 | Description                                                                   |
| ----------- | --------------------------------------- | ----------------------------------------------------------------------------- |
| `strip`     | `false`                                 | Print TypeScript as JavaScript, reporting what has no JavaScript form         |
| `minify`    | `false`                                 | Minify, rewriting syntax to shorter forms such as `!0` and `obj.foo`          |
| `format`    | `"pretty"`, or `"compact"` minifying    | Or `"compact"`                                                                |
| `indent`    | `2`                                     | Spaces per level in the pretty format                                         |
| `quotes`    | `"preserve"`, or `"shortest"` minifying | `"preserve"`, `"double"`, `"single"`, or `"shortest"`                         |
| `comments`  | `"all"`, or `"some"` minifying          | `"all"`, `"some"`, `"none"`, `"line"`, or `"block"`, of the attached comments |
| `sourceMap` | `false`                                 | `true`, or `{ file, sourceRoot, sourcesContent }`, when printing a file       |
| `printers`  | none                                    | Print node types the printer does not know, by `type`                         |

`"some"` keeps legal headers, JSDoc, and `@` or `#` annotations, as bundlers do.

## Performance

`yuku` parses faster than the alternatives on npm, on small and large files.

![Parsing react.js (70 KB) on npm](https://raw.githubusercontent.com/yuku-toolchain/ecmascript-parser-benchmark-js/refs/heads/main/charts/react.png)

![Parsing typescript.js (7.8 MB) on npm](https://raw.githubusercontent.com/yuku-toolchain/ecmascript-parser-benchmark-js/refs/heads/main/charts/typescript.png)

Analyzing a whole codebase, it is faster than typescript-eslint and the TypeScript API, and on a single large file too.

![Analyzing three.js (755 files) on npm](https://raw.githubusercontent.com/yuku-toolchain/ecmascript-analyzer-benchmark-js/refs/heads/main/charts/three.png)

![Analyzing typescript.js (7.8 MB) on npm](https://raw.githubusercontent.com/yuku-toolchain/ecmascript-analyzer-benchmark-js/refs/heads/main/charts/typescript_js.png)

Measured on an Apple M3, see the [parser](https://github.com/yuku-toolchain/ecmascript-parser-benchmark-js) and [analyzer](https://github.com/yuku-toolchain/ecmascript-analyzer-benchmark-js) benchmarks. Pass one `walk` all your visitors and patterns.

## License

MIT
