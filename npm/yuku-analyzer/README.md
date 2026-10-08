# yuku-analyzer

Scopes, bindings, resolved references, closures, and cross-file linking for JavaScript and TypeScript, computed natively, part of [Yuku](https://yuku.fyi).

Getting the same answers otherwise takes a stack of tools, each parsing every file again into its own tree:

- `@typescript-eslint/typescript-estree` for the AST
- `@typescript-eslint/scope-manager` or `eslint-scope` for scopes, bindings, and references
- TypeScript's API for names resolved across files, merged declarations, and exports
- a module resolver, such as `enhanced-resolve`, to find the file behind each import
- the glue that maps one tool's nodes onto another's

`yuku-analyzer` is all of it in one package, from one parse. On [real codebases](https://github.com/yuku-toolchain/ecmascript-analyzer-benchmark-js) it is 5–11× faster than typescript-eslint and 4–7× faster than TypeScript's API, using up to 5× less memory. Each of those compares it with a single tool, so against the whole stack the gap only widens.

It is as accurate as it is fast. [Read how it is tested →](https://yuku.fyi/testing/#semantic-analysis)

- [Install](#install)
- [Usage](#usage)
- [Analyzer](#analyzer)
- [Module](#module)
- [Scope](#scope)
- [Binding](#binding)
- [Reference](#reference)
- [Imports and exports](#imports-and-exports)
- [Walking](#walking)
- [BindingFlags](#bindingflags)

## Install

```bash
npm install yuku-analyzer
```

It runs on Yuku's native core, installed for your platform. In browsers and edge runtimes, load the WebAssembly core from [`@yuku-core/wasm`](https://www.npmjs.com/package/@yuku-core/wasm) and pass it in, with `new Analyzer({ core })` or `analyze(source, { core })`.

## Usage

```js
import { analyze } from "yuku-analyzer";

const module = analyze(`const double = (n: number) => n * 2; double(21);`, { path: "math.ts" });

module.rootScope.find("double").references.length; // 1
```

A project links its files.

```js
import { Analyzer } from "yuku-analyzer";

const project = new Analyzer();
project.setFile("math.ts", `export const add = (x: number, y: number) => x + y;`);
project.setFile("app.ts", `import { add } from "./math"; add(1, 2);`);

const add = project.module("app.ts").rootScope.find("add");
add.definition().module.path; // "math.ts"
```

## Analyzer

```js
const project = new Analyzer({
  // a path in the project, false for a module outside it, null when it cannot be resolved
  resolve: (specifier, importer) => aliases.get(specifier) ?? null,
});

project.setFile(path, source, options); // Module, replacing any at the same path
project.deleteFile(path);               // whether it was in the project
project.module(path);                   // Module | undefined
project.modules;                        // ReadonlyMap<string, Module>
project.diagnostics;                    // every module's diagnostics, and the failed links
project.link();                         // links now, cross-file queries link on demand otherwise
```

The options are `lang` and `sourceType`, inferred from the path, and `preserveParens`, `attachComments`, and `tokens`, as in [`yuku-parser`](https://www.npmjs.com/package/yuku-parser#options). `analyze(source, options)` is a project of one file, with `path` among its options.

The default resolver matches relative specifiers to files in the project, probing extensions and index files as TypeScript does, so `./a.js` finds `a.ts`. A package or an asset such as `./app.css` is external, and a relative specifier with no match is reported.

A diagnostic has the shape of [`yuku-parser`'s](https://www.npmjs.com/package/yuku-parser#diagnostics), with the `path` of its module.

## Module

```js
module.analyzer;    // the Analyzer it belongs to
module.path;
module.source;
module.ast;         // the ESTree / TypeScript-ESTree Program
module.comments;
module.tokens;      // with tokens: true
module.diagnostics;
module.isCurrent;   // false once its path is set again or deleted

module.scopes;               // Scope[], by id
module.rootScope;            // the module scope, or the global scope of a script
module.bindings;             // Binding[], by id
module.references;           // Reference[], in source order
module.unresolvedReferences; // globals and undeclared names
module.imports;              // Import[]
module.exports;              // Export[]
module.moduleFlags;          // { usesRequire, usesModule, usesExports, usesImportMeta }
module.dependencies;         // the modules it imports from
module.dependents;           // the modules that import it

module.bindingOf(node);      // the binding a node declares or refers to
module.referenceOf(node);
module.scopeOf(node);
module.parentOf(node);
module.ancestors(node);      // the node, then each parent up to the root
module.nodeAt(offset);       // the innermost node at a UTF-16 offset
module.lookup("x", { from: scope, space: "value" }); // resolves a name as code there would
module.capturesOf(fn);       // [{ binding, references, isWritten }], the outer bindings it uses
module.exportedNames();      // through export * and module augmentations
module.resolveExport("x");   // { module, binding, augmentations }, the binding behind an export
module.walk(visitors, root);
module.walkAsync(visitors, root);
module.findAll(types);       // every node of the given types, in source order
```

A node is the same object in `module.ast` and in every result, so each query takes the node you hold. The model is a snapshot of the source.

## Scope

```js
scope.id;
scope.module;
scope.kind;        // "global" | "module" | "function" | "functionBody" | "block" | "class"
                   // | "staticBlock" | "expressionName" | "tsModule"
scope.strict;
scope.node;        // the node that creates it
scope.parent;
scope.hoistTarget; // where a var declared in it lands
scope.bindings;

scope.find("x");       // a binding it declares
scope.contains(other); // other is this scope or inside it
scope.ancestors();     // this scope, then each parent
```

## Binding

```js
binding.id;
binding.module;
binding.name;
binding.scope;
binding.declarations; // each declaration's name node, overloads and merges included
binding.references;   // its uses in this module
binding.flags;        // a BindingFlags bitset

binding.has(BindingFlags.Function);
binding.hasAll(BindingFlags.Const | BindingFlags.Exported);
binding.visibleIn("type");

binding.definition();     // { module, binding, augmentations } it imports, followed across modules
binding.findReferences(); // its uses across the project
```

A `definition` with a null `binding` is a whole module namespace, as `import * as ns` binds. Its `augmentations` are the bindings of `declare module "m"` blocks in other modules that merge into it.

## Reference

```js
reference.id;
reference.module;
reference.name;
reference.scope;
reference.node;           // the Identifier or JSXIdentifier
reference.binding;        // null for a global or undeclared name
reference.space;          // "value" | "type" | "namespace" | "typeof" | "any"
reference.inTypePosition; // erased with the types
reference.isWrite;        // x = 1, x++, for (x of xs)
```

Names resolve per space, as in TypeScript.

```ts
type T = string;
function f() {
  const T = 1;
  let x: T; // the outer type T
  T;        // the inner const T
}
```

The blocks of one namespace or enum see each other's exports and members, as one declaration.

## Imports and exports

```js
imp.id;
imp.module;
imp.kind;           // "named" | "namespace" | "sideEffect" | "importEquals" | "dynamic" | "require" | "augmentation"
imp.name;           // the imported name, "default" for a default import
imp.local;          // the binding it declares
imp.isNamespace;    // it binds a whole module, as namespace and importEquals do
imp.specifier;
imp.typeOnly;
imp.phase;          // "source" | "defer" | null
imp.node;
imp.resolvedModule;

exp.id;
exp.module;
exp.kind;           // "named" | "reExport" | "namespace" | "star" | "equals" | "global"
exp.name;           // the exported name, null for star, equals, and global
exp.globalName;     // the N of export as namespace N
exp.local;          // the binding it exports
exp.specifier;      // the module it re-exports from
exp.fromName;       // the name a reExport takes from it
exp.typeOnly;
exp.node;
exp.resolvedModule;
```

`import()` and `require()` with a string specifier are imports too. Linking follows the specification's ResolveExport, so `export *` never forwards `default`, and a name two stars provide differently is reported as ambiguous.

## Walking

`module.walk` is the [walker of `yuku-ast`](https://www.npmjs.com/package/yuku-ast#walking), with the model in its context.

```js
module.walk({
  CallExpression(node, ctx) {
    ctx.binding;   // module.bindingOf(node)
    ctx.reference; // module.referenceOf(node)
    ctx.scope;     // module.scopeOf(node)
    ctx.module;
  },
});
```

## BindingFlags

| Flag                     | Set on                                       |
| ------------------------ | -------------------------------------------- |
| `FunctionScopedVariable` | `var`, a parameter, or a catch variable      |
| `BlockScopedVariable`    | `let`, `const`, `using`, `await using`       |
| `Function`               | a function                                   |
| `Class`                  | a class                                      |
| `RegularEnum`            | `enum`                                       |
| `ConstEnum`              | `const enum`                                 |
| `ValueModule`            | a namespace with runtime content             |
| `Interface`              | `interface`                                  |
| `TypeAlias`              | `type`                                       |
| `TypeParameter`          | `<T>`, `infer T`, a mapped type key          |
| `NamespaceModule`        | a namespace of any kind                      |
| `ValueImport`            | `import x`, `import { x }`                   |
| `TypeImport`             | `import type`, `import { type x }`           |
| `Const`                  | `const`, `using`, `await using`              |
| `Ambient`                | `declare`                                    |
| `Parameter`              | a parameter                                  |
| `CatchVariable`          | `catch (e)`                                  |
| `Exported`               | `export`, or implicitly in ambient code      |
| `Default`                | `export default <declaration>`               |
| `EnumMember`             | an enum member                               |
| `Variable`               | any variable                                 |
| `Import`                 | any import                                   |
| `ValueSpace`             | anything visible at runtime                  |
| `TypeSpace`              | anything usable as a type                    |
| `NamespaceSpace`         | what a dotted type name can start from       |

## License

MIT
