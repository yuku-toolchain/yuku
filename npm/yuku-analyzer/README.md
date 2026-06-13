# yuku-analyzer

Full semantic analysis for JavaScript and TypeScript: scopes, symbols, resolved references, closures, and cross-file module linking. Powered by [Yuku](https://github.com/yuku-toolchain/yuku), written in Zig.

The usual options are a hand-rolled scope tracker (fragile, per-file, re-bugged in every tool) or the TypeScript compiler (correct, but hundreds of milliseconds per file and a compiler-sized dependency). This is the fast path between them. One native call per file, then every query is plain JavaScript with zero per-query FFI cost, sub-millisecond on a typical file.

```bash
npm install yuku-analyzer yuku-parser
```

`yuku-parser` is a peer dependency.

## Quick start

```js
import { Analyzer, SymbolFlags } from "yuku-analyzer";

const analyzer = new Analyzer();

analyzer.addFile("lib.ts", `export const helper = (x: number) => x * 2;`);
const main = analyzer.addFile(
  "main.ts",
  `import { helper } from "./lib.ts";
   export const out = helper(21);`,
);

// per-file semantics
const helperSym = main.rootScope.find("helper");
console.log(helperSym.has(SymbolFlags.Import)); // true
console.log(helperSym.references.length); // 1, the call site

// cross-file: follow the import to where helper is actually defined
const def = helperSym.definition();
console.log(def.module.path); // "lib.ts"
console.log(def.symbol.has(SymbolFlags.Const)); // true
```

## Why this exists

JavaScript tooling that needs real semantics has had two options: track scopes by hand during a walk (fragile, incomplete, and re-implemented in every tool), or embed a full language service (heavy). `yuku-analyzer` is the missing middle: the exact scope and binding model of a production compiler, exposed to JavaScript as a small, fast object graph.

Nothing semantic is reimplemented in JavaScript. The binder, scope tree, reference resolution, and module records are computed by the same well-tested native analyzer that powers the rest of Yuku, then shipped to JavaScript as one compact buffer that the JS side only decodes, through lazy zero-copy views. That handoff is usually where native tooling stalls: either every query pays an FFI round trip, or the semantics get a hand-written JS twin that slowly drifts. Here there is one implementation and one crossing, so it cannot drift, and the corner cases arrive already correct: catch-clause scope sharing, named function expression scopes, `var` hoist targets, TS declaration merging, value space versus type space, and write detection through destructuring patterns.

## What one `addFile` gives you

```js
const module = analyzer.addFile("app.tsx", source);

module.ast                  // ESTree / TS-ESTree program
module.scopes               // every lexical scope, as a tree
module.symbols              // every declared binding
module.references           // every identifier use, resolved to its symbol
module.unresolvedReferences // free names and globals
module.imports              // spec-true import records
module.exports              // spec-true export records
module.diagnostics          // syntax + semantic errors
```

And node-level queries that work directly on AST nodes:

```js
module.symbolOf(node)    // the symbol a node declares or references
module.referenceOf(node) // the reference recorded for an identifier
module.scopeOf(node)     // the innermost scope containing the node
module.parentOf(node)    // the node that structurally contains it, or null
module.resolve("name")   // scope-chain lookup, like the engine does at runtime
```

Node identity is exact: the node you reach by walking `module.ast` and the node a semantic query returns are the same JavaScript object, so `===` always works.

## Walking with semantic context

`module.walk` is a typed visitor walk where every handler also receives the current scope, symbol, and reference. No manual scope tracking, ever:

```js
module.walk({
  Identifier(node, ctx) {
    if (ctx.reference?.isWrite && ctx.symbol?.has(SymbolFlags.Import)) {
      console.log(`${node.name} assigns to an import`);
    }
  },
  FunctionDeclaration: {
    enter(node, ctx) {
      console.log(node.id.name, "declared in a", ctx.scope.kind, "scope");
    },
  },
});
```

The walk mutates in place: `ctx.replace(node)`, `ctx.remove()`, `ctx.insertBefore(node)`, `ctx.insertAfter(node)`, plus `ctx.skip()` and `ctx.stop()`. Transform the AST during the walk and print it with [`yuku-codegen`](https://www.npmjs.com/package/yuku-codegen).

## Closure analysis

`capturesOf` reports the free variables of any function: every outer binding it closes over, with the capturing reference sites and whether the function writes to the binding. Shadowing and aliasing are handled by the resolved reference table, not by name matching:

```js
const [fn] = main.findAll("FunctionDeclaration");
for (const capture of main.capturesOf(fn)) {
  console.log(capture.symbol.name, capture.isWritten ? "(written)" : "");
}
```

## Cross-file analysis

The analyzer joins imports to exports across every added file with the spec's ResolveExport semantics: re-export and `export *` chains are followed per name, `default` never travels through `export *`, and a name supplied by multiple `export *` declarations through different bindings is reported as ambiguous.

```js
// where is this binding actually defined?
analyzer.definitionOf(symbol); // { module, symbol } or null for external modules

// every use across the whole graph, imports followed back
analyzer.referencesOf(symbol); // [{ module, reference }, ...]

module.exportedNames(); // every exported name, `export *` chains included
module.dependencies;    // modules this file imports from
module.dependents;      // modules that import this file
analyzer.diagnostics;   // e.g. "Module './lib.ts' has no export 'helpr'"
```

Linking is automatic: every cross-file surface relinks on demand after files change. Call `analyzer.link()` explicitly if you want to control when the work happens.

Module resolution is pluggable. The default resolves relative specifiers among added files with standard extension and index probing. Pass your own resolver for anything else:

```js
const analyzer = new Analyzer({
  resolve: (specifier, importerPath) => myResolver(specifier, importerPath),
});
```

## Performance

Analysis runs in the native parser pass, so full semantics cost roughly half of parsing time on top of the parse itself. Validated against 55,000+ real-world files.

Concretely, on an Apple M-series machine: parsing plus complete semantic analysis of a typical source file lands well under a millisecond, walking sustains tens of millions of nodes per second, and linking a 2,000-module graph takes about a millisecond.

## TypeScript

Everything is fully typed. Visitor handlers receive exact node types, AST types come from `yuku-parser`, and the semantic surface (`Module`, `Scope`, `Symbol`, `Reference`, `Import`, `Export`, `Capture`) is exported:

```ts
import type { Module, Symbol, Capture } from "yuku-analyzer";
```

## Documentation

The full documentation, including the architecture, every type, and the design decisions: [yuku.fyi/analyzer](https://yuku.fyi/analyzer).

## License

MIT
