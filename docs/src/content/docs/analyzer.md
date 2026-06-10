---
title: Analyzer
description: Full JavaScript and TypeScript semantic analysis for Node.js. Scopes, symbols, resolved references, closures, and cross-file module linking, computed natively and queried as plain JavaScript objects.
---

`yuku-analyzer` brings full compiler-grade semantic analysis to JavaScript: per-file scopes, symbols, resolved references, closure analysis, and cross-file module linking. One native call per file computes everything. Every query after that is plain JavaScript over compact in-memory tables, with zero per-query FFI cost.

```bash
npm install yuku-analyzer yuku-parser
```

`yuku-parser` is a peer dependency: it provides the AST node types and the shared walk engine.

```js
import { Analyzer } from "yuku-analyzer";

const analyzer = new Analyzer();
const module = analyzer.addFile("app.ts", source);

module.rootScope.find("config")?.references; // every use of `config`
```

## The problem it solves

Tools that need real semantics on the JavaScript side have always faced a bad trade. Walk-and-track libraries give you a scope stack, but you maintain the binding rules yourself: hoisting, catch clauses, named function expressions, TypeScript declaration merging, type space versus value space. Each tool re-implements a subset, each subset has different bugs. The alternative, embedding a full language service, costs hundreds of milliseconds per file and a dependency the size of a compiler.

`yuku-analyzer` takes a third path. The semantics are computed by the same native binder that powers Yuku's ECMAScript spec-compliance checker, validated against the full Test262 corpus and 45,000+ real-world files. JavaScript receives the finished model: not events to track, but answers to query.

## Architecture

The design rests on one observation: a semantic model is mostly integers. Scopes point to parents, symbols point to scopes, references point to symbols, and everything points to AST nodes. Integers serialize for free.

**One native call.** `addFile` parses the source, runs scope construction, binding, and reference resolution in Zig, and serializes the result into a single binary buffer: the AST in Yuku's flat transfer format, followed by the semantic tables as fixed-stride sections. One FFI crossing per file, total.

**Zero-copy decode.** On the JavaScript side, the semantic sections are read through typed-array views directly over the transferred buffer. Nothing is parsed, nothing is copied. A symbol's name, flags, scope, and declaration list are reads at computed offsets.

**Lazy objects, eager answers.** `Scope`, `Symbol`, `Reference`, `Import`, and `Export` are flyweight objects over the tables: tiny, allocated once per row on first access, with getters that read the buffer. Cross-indexes (which references belong to which symbol, which symbols belong to which scope) build lazily on first use and amortize across every later query.

**Node identity.** AST nodes decode lazily and are memoized by node index. The node you reach by walking `module.ast` and the node a semantic query hands back are the same JavaScript object. `symbol.declarations[0] === someNodeYouWalkedTo` is a meaningful comparison, and a `WeakMap` resolves any node back to its index, which is what makes `symbolOf(node)` a lookup instead of a search.

The result: native-code analysis speed, JavaScript-object ergonomics, and a wire format that is provably synchronized with the code that reads it.

## The Analyzer

The `Analyzer` is the project: a set of modules plus the links between them.

```js
import { Analyzer } from "yuku-analyzer";

const analyzer = new Analyzer();

const module = analyzer.addFile("src/app.tsx", source);
analyzer.removeFile("src/app.tsx"); // true if it existed
analyzer.module("src/app.tsx");     // Module | undefined
analyzer.modules;                   // ReadonlyMap<string, Module>
```

`addFile` accepts the same options as `yuku-parser`'s `parse`, with `lang` and `sourceType` defaulting from the file extension:

```js
analyzer.addFile("legacy.cjs", source, {
  // lang: "js"            inferred from the extension
  // sourceType: "script"  inferred from the extension
  preserveParens: true,
  allowReturnOutsideFunction: false,
  attachComments: false,
});
```

Adding a path that already exists replaces the module and marks the graph for relinking.

### Module resolution

Cross-file linking needs to map import specifiers to added files. The default resolver handles relative specifiers with standard extension probing (`./util` matches `util.ts`, `util/index.ts`, and so on). For anything else, supply your own:

```js
const analyzer = new Analyzer({
  resolve(specifier, importerPath) {
    // return the path of an added file, or null for external modules
    return myAliasMap.get(specifier) ?? null;
  },
});
```

Returning `null` marks the import as external: `import.resolvedModule` stays `null` and definition chains stop there, without diagnostics.

## The Module

`addFile` returns a `Module`, the per-file unit of the analysis. Everything on it is local JavaScript: no native calls happen after `addFile` returns.

```js
module.path;        // the path it was added under
module.source;      // the original source text
module.ast;         // ESTree / TS-ESTree Program, lazily decoded
module.diagnostics; // syntax and semantic errors for this file
module.comments;    // every comment in source order
module.lineStarts;  // sorted offsets where each line begins
module.locOf(120);  // { line, column } for an offset
```

The AST is the same ESTree / TypeScript-ESTree output as `yuku-parser`, and nodes are plain mutable objects. Edit them, run them through any ESTree tool, print them with `yuku-codegen`.

The semantic surface:

```js
module.scopes;               // Scope[], index is the scope id
module.rootScope;            // the scope top-level code runs in
module.symbols;              // Symbol[], index is the symbol id
module.references;           // Reference[], in source order
module.unresolvedReferences; // references that resolve to no binding
module.imports;              // Import[], in source order
module.exports;              // Export[], in source order
```

Ids are stable per parse: `(module.path, symbol.id)` is a persistable key, which matters for caches and incremental tooling.

## Scopes

Every lexical environment in the file, as a tree:

```js
const scope = module.scopes[3];

scope.kind;        // "global" | "module" | "function" | "block" | "class"
                   // | "staticBlock" | "expressionName" | "tsModule"
scope.node;        // the AST node that created the scope
scope.parent;      // parent Scope, or null at the global scope
scope.strict;      // strict mode, propagated per spec
scope.hoistTarget; // the scope where a `var` declared here actually lands
scope.bindings;    // symbols declared directly in this scope

scope.find("x");          // direct binding lookup, no chain walk
scope.contains(other);    // is `other` this scope or a descendant?
for (const s of scope.ancestors()) { /* this scope up to global */ }
```

The scope tree is the native binder's exact output, so the spec subtleties are already right.

## Symbols

A `Symbol` is one declared binding:

```js
const sym = module.rootScope.find("render");

sym.name;         // "render"
sym.scope;        // the Scope it is declared in
sym.declarations; // every declarator node, in source order
sym.references;   // every resolved use site in this module
sym.id;           // stable index into module.symbols
```

One symbol can have several declarations when the language merges them: TypeScript function overloads, `class` + `interface` merging, `namespace` + `enum` merging. The analyzer records every declarator, which is exactly what go-to-definition and rename need.

### Flags and predicates

What a symbol is lives in a bitset, with named predicates for every common question:

```js
sym.isVariable;      // var / let / const, parameters and catch bindings included
sym.isFunction;
sym.isClass;
sym.isImported;
sym.isExported;
sym.isConst;         // const or using
sym.isParameter;
sym.isCatchParam;
sym.isTypeOnly;      // import type / import { type x }
sym.isDefaultExport;
sym.inValueSpace;    // visible at runtime
sym.inTypeSpace;     // referencable from TS type positions
```

A `class` satisfies both `inValueSpace` and `inTypeSpace`, which is what makes "use a class as a type" work without special cases.

For anything the predicates do not cover, the raw bitset and the `SymbolFlags` constants are exported:

```js
import { SymbolFlags } from "yuku-analyzer";

sym.has(SymbolFlags.TypeAlias | SymbolFlags.Interface); // any of these
sym.hasAll(SymbolFlags.Function | SymbolFlags.Exported); // all of these
```

The flag values are generated from the native binder's bit layout at build time, so they can never disagree with what the binder wrote.

## References

A `Reference` is one identifier in use position, already resolved:

```js
const ref = module.references[0];

ref.name;    // the identifier text
ref.node;    // the Identifier node, identity-shared with the AST
ref.scope;   // the scope the use occurs in
ref.symbol;  // the resolved Symbol, or null for free names
ref.kind;    // "value" for runtime uses, "type" for TS type positions
ref.isWrite; // true when this use (re)assigns the binding
```

`kind` lets rename and dead-code tools treat a value and a same-named type independently. `isWrite` is computed structurally in the native pass.

`module.unresolvedReferences` is the complement: every name that resolves to no local binding. That list is precisely what a no-undef lint rule or a globals collector wants.

## Node queries

Four methods connect AST nodes to the semantic model. All of them work on node object identity, not positions or names:

```js
module.symbolOf(node);    // the symbol a node declares or references, or null
module.referenceOf(node); // the Reference for an identifier node, or null
module.scopeOf(node);     // the innermost scope whose extent contains the node
module.resolve("fetch");            // scope-chain lookup from the root scope
module.resolve("x", someScope);     // or from any scope, like the engine would
```

`symbolOf` is the workhorse: hand it a declaration identifier and you get the symbol it declares, hand it a reference identifier and you get the symbol it resolves to.

## Walking

`module.walk` is a typed visitor walk with the semantic model in context. Handlers are keyed by node type and receive the exact node type, not a generic node:

```js
module.walk({
  // bare function = enter handler
  CallExpression(node, ctx) {
    if (node.callee.type === "Identifier") {
      const target = ctx.module.symbolOf(node.callee);
      if (target?.isImported) {
        console.log(`calls imported ${node.callee.name}`);
      }
    }
  },

  // or an enter/leave pair
  FunctionDeclaration: {
    enter(node, ctx) { console.log("entering", node.id.name); },
    leave(node, ctx) { console.log("leaving", node.id.name); },
  },

  // universal catch-alls
  enter(node, ctx) {},
  leave(node, ctx) {},
});
```

Per node, the order is: catch-all `enter`, typed enter, children, typed leave, catch-all `leave`. Pass a node as the second argument to walk only a subtree: `module.walk(visitors, someFunction)`.

### The context

One context object is reused across the whole walk (do not store it). It carries the position and the semantics:

```js
ctx.node;      // the current node
ctx.parent;    // its parent, or null at the walk root
ctx.key;       // the field on the parent holding this node
ctx.index;     // position in an array field, or null
ctx.ancestors(); // a copy of the ancestor chain, root first

ctx.scope;     // the innermost Scope at this node
ctx.symbol;    // shorthand for module.symbolOf(node)
ctx.reference; // shorthand for module.referenceOf(node)
ctx.module;    // the module being walked
```

`ctx.scope` is not tracked during the walk. It is replayed from the native scope tree: when the walk enters a node that created a scope, that exact scope is pushed. Non-scope nodes pay a single set-membership test. There is no re-implementation of scoping rules in JavaScript anywhere in the package, which is the point.

### Mutation

The walk mutates the AST in place, with precise semantics:

| Operation               | Effect                                                                                                        |
| ----------------------- | ------------------------------------------------------------------------------------------------------------- |
| `ctx.skip()`            | Do not descend into this node's children. `leave` still fires.                                                |
| `ctx.stop()`            | End the walk immediately.                                                                                      |
| `ctx.replace(node)`     | Swap the current node. The walk continues into the replacement's children and `leave` fires for its new type. |
| `ctx.remove()`          | Splice the node out of an array field, or null a plain field. Children are not walked, `leave` does not fire.  |
| `ctx.insertBefore(node)`| Insert a sibling before the current node. The inserted node is not visited.                                    |
| `ctx.insertAfter(node)` | Insert a sibling after the current node. The walk visits it.                                                   |

A replacement node created with `start: 0, end: 0` inherits the original node's span, which keeps source maps meaningful through `yuku-codegen`.

```js
module.walk({
  DebuggerStatement(node, ctx) {
    ctx.remove();
  },
  Identifier(node, ctx) {
    if (ctx.symbol === legacyName) node.name = "modernName";
  },
});
```

One rule to remember: the semantic tables are a snapshot of the parsed source. Nodes you create have no symbols or references of their own. Analyze, transform, print, and re-analyze the output if you need fresh semantics for the transformed code.

## Scanning

`module.scan` is the second traversal tier: a readonly pass over the parsed node records in the binary buffer, without materializing AST objects at all.

```js
module.scan({
  CallExpression(cursor) {
    console.log(cursor.start, cursor.end); // spans, straight off the buffer
  },
  enter(cursor) {
    // runs for every node
  },
});
```

The engine iterates fixed-stride records, dispatches on integer tags through a dense per-tag handler table, and calls your handlers only for registered types. The cursor exposes:

```js
cursor.type;      // the node's type string
cursor.start;     // span start
cursor.end;       // span end
cursor.index;     // the node's index in the buffer, stable per parse
cursor.node();    // materialize this one node, on demand
cursor.skip();    // do not descend into children
cursor.stop();    // end the scan

cursor.module;    // the module being scanned
cursor.symbol;    // semantic lookup, no node materialization
cursor.reference; // semantic lookup, no node materialization
```

The last two are the interesting ones. The semantic tables are keyed by node index, and the scan operates in index space, so `cursor.reference` resolves an identifier to its reference record without ever building a node object. A query like "every write to an imported binding across the project" runs over thousands of files without constructing a single AST node:

```js
for (const module of analyzer.modules.values()) {
  module.scan({
    Identifier(cursor) {
      const ref = cursor.reference;
      if (ref?.isWrite && ref.symbol?.isImported) {
        console.log(module.path, module.locOf(cursor.start));
      }
    },
  });
}
```

When a handler does need the node, `cursor.node()` materializes exactly that one, and it is the same memoized object the walked AST produces.

Scanning is readonly by design. The rule of thumb: scan to find, walk to change. For a semantic query like the one above, scanning runs about 4x faster than the equivalent walk, because it never builds the AST and resolves symbols and references straight from the buffer. The gap widens across many files, since nothing is allocated per node.

`yuku-parser` ships the same two tiers without semantics: `walk(program, visitors, state?)` and `parseResult.scan(visitors)`.

### findAll

For the simplest queries there is a one-liner:

```js
module.findAll("FunctionDeclaration");          // FunctionDeclaration[]
module.findAll(["ClassDeclaration", "TSInterfaceDeclaration"]);
```

## Closure analysis

`capturesOf` computes the free variables of a function: every binding referenced inside it (nested closures included) that is declared outside it.

```js
const source = `
  let count = 0;
  const step = 2;
  export function tick() {
    count += step;
    return () => count;
  }
`;

const module = analyzer.addFile("counter.ts", source);
const [tick] = module.findAll("FunctionDeclaration");

for (const capture of module.capturesOf(tick)) {
  console.log(capture.symbol.name, capture.isWritten);
}
// count true     (tick writes to it)
// step  false    (read only)
```

Each `Capture` carries the outer `symbol`, the capturing `references` inside the function, and `isWritten`. Type-only references are excluded, since they do not exist at runtime.

Because the computation rides the resolved reference table, it is shadowing-correct and alias-correct by construction. A local `count` declared inside the function does not produce a false capture, and a reference is attributed to the binding it actually resolves to, not to the nearest matching name.

## Cross-file analysis

### Import and export records

Each module carries spec-true records of its module surface, computed natively:

```js
for (const imp of module.imports) {
  imp.specifier;      // "./lib.ts"
  imp.name;           // imported export name, "default" for default imports,
                      // null for namespace and side-effect imports
  imp.local;          // the local binding Symbol, or null for side effects
  imp.isNamespace;    // import * as ns
  imp.isSideEffect;   // import "m"
  imp.typeOnly;       // import type / import { type x }
  imp.phase;          // "source" | "defer" | null (stage 3 phase imports)
  imp.resolvedModule; // the defining Module, or null when external
}

for (const exp of module.exports) {
  exp.name;                // exported name, "default" included, null for export *
  exp.local;               // backing local Symbol, when there is one
  exp.isStar;              // export * from "m"
  exp.specifier;           // re-export source, or null for local exports
  exp.fromName;            // the name taken from the source module
  exp.isNamespaceReexport; // export * as ns from "m"
  exp.typeOnly;            // export type
  exp.resolvedModule;      // the source Module for re-exports
}
```

Following the specification, `default` is modeled as an export *name*, not a separate kind, and `export *` never forwards `default`. Tools built on these records inherit the spec behavior instead of approximating it.

### Linking

`analyzer.link()` joins the graph: resolves every specifier through the resolver, populates `resolvedModule` on imports and re-exports, builds `dependencies` / `dependents`, and reports missing exports as diagnostics.

Calling it is optional. Every cross-file surface links on demand after files change, so reading `import.resolvedModule` or `module.dependencies` is always correct. Call `link()` explicitly when you want to control when the work happens and collect the diagnostics at a known point:

```js
analyzer.link();

for (const d of analyzer.diagnostics) {
  console.log(`${d.module}: ${d.message}`);
  // "main.ts: Module './lib.ts' has no export 'helpr'"
}
```

### Definitions across modules

`definitionOf` follows import, re-export, and `export *` chains to the place a binding is actually defined, however many files away:

```js
// a.ts:  export const value = 1;
// b.ts:  export { value as renamed } from "./a.ts";
// c.ts:  import { renamed } from "./b.ts";

const c = analyzer.module("c.ts");
const sym = c.rootScope.find("renamed");

const def = analyzer.definitionOf(sym);
def.module.path;  // "a.ts"
def.symbol.name;  // "value"
```

`symbol.definition()` is the instance-method shorthand. A result with `symbol: null` means the definition is a whole module namespace (`import * as ns`). A `null` result means the chain leaves the added file set: an external package, by design not an error.

Chains with cycles terminate safely, and ambiguous `export *` names resolve first-match in declaration order.

### References across modules

The inverse direction: every use of a symbol anywhere in the graph, with imports followed back to the definition:

```js
const uses = analyzer.referencesOf(def.symbol);
for (const { module, reference } of uses) {
  console.log(module.path, reference.name, reference.isWrite);
}
```

This is find-all-references as a compiler primitive: rename across files, unused-export detection, impact analysis.

```js
// unused exports, whole project
for (const module of analyzer.modules.values()) {
  for (const exp of module.exports) {
    if (exp.local && analyzer.referencesOf(exp.local).length === 0) {
      console.log(`${module.path}: '${exp.name}' is exported but never used`);
    }
  }
}
```

## SymbolFlags reference

The full bitset, generated from the native binder's layout:

| Flag                    | Meaning                                          |
| ----------------------- | ------------------------------------------------ |
| `FunctionScopedVariable`| `var`, parameter, or catch variable              |
| `BlockScopedVariable`   | `let`, `const`, `using`, `await using`           |
| `Function`              | function declaration or expression               |
| `Class`                 | class declaration or expression                  |
| `RegularEnum`           | TS `enum`                                        |
| `ConstEnum`             | TS `const enum`                                  |
| `ValueModule`           | TS namespace with runtime content                |
| `Interface`             | TS `interface`                                   |
| `TypeAlias`             | TS `type` alias                                  |
| `TypeParameter`         | TS `<T>`, `infer T`, mapped-type key             |
| `NamespaceModule`       | TS namespace of any kind                         |
| `Import`                | value (or unspecified-kind) import binding       |
| `TypeImport`            | `import type` / `import { type x }` binding      |
| `Const`                 | `const` or `using` binding                       |
| `Ambient`               | TS `declare`                                     |
| `Parameter`             | function or method parameter                     |
| `CatchVariable`         | `catch (e)` binding                              |
| `Exported`              | exported from its module                         |
| `Default`               | the default export                               |

## Performance

Numbers from an Apple M-series machine, release builds:

- Parsing plus complete semantic analysis of a typical source file lands well under a millisecond. The semantic pass adds roughly half of parse time on top of the parse itself.
- The walk sustains tens of millions of nodes per second. For semantic queries, scanning runs about 4x faster than walking and allocates nothing.
- Linking a 2,000-module graph takes about a millisecond. `capturesOf` on a typical function takes tens of microseconds.
- The implementation is validated against 45,000+ real-world files with zero failures.

The structural reasons: one FFI crossing per file, zero-copy typed-array reads, lazy materialization with index memoization, and cross-indexes that build once and amortize.

## TypeScript types

Everything is precisely typed against `yuku-parser`'s AST types. Visitor handlers receive exact node types and the full semantic surface is exported:

```ts
import type {
  Module, Scope, Symbol, Reference, Import, Export,
  Capture, Definition, ModuleReference,
  Visitors, WalkContext, ScanCursor, ScanVisitors,
} from "yuku-analyzer";
```

```ts
module.walk({
  ArrowFunctionExpression(node, ctx) {
    node.params;     // typed as the arrow's params
    node.superClass; // type error: arrows have no superClass
  },
});
```
