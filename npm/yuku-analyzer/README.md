# yuku-analyzer

Full semantic analysis for JavaScript and TypeScript: scopes, symbols, resolved references, closures, and cross-file module linking, computed natively in Zig and queried as plain JavaScript objects. Powered by [Yuku](https://github.com/yuku-toolchain/yuku).

**No single library gives you all of this.** Scopes and resolved references mean `eslint-scope` or `@typescript-eslint/scope-manager`. Cross-file go-to-definition means the TypeScript compiler or `ts-morph`. A parser sits under both. `yuku-analyzer` is all of them in one native pass behind one API.

**At native speed.** Up to ~15× faster per file than `eslint-scope`, `@typescript-eslint/scope-manager`, and `@babel/traverse`, with zero per-query cost after the single native call. Stitch those separate tools together yourself and the gap only widens: each re-walks the AST, you re-parse to resolve across files, and you keep the indexes between them in sync by hand. `yuku-analyzer` pays all of that once, in Zig.

```bash
npm install yuku-analyzer
```

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

## How it works

Nothing semantic is reimplemented in JavaScript. The binder, scope tree, reference resolution, and module records are computed by the same well-tested native analyzer that powers the rest of Yuku, then shipped to JavaScript as one compact buffer that the JS side only decodes, through lazy zero-copy views. That handoff is usually where native tooling stalls: either every query pays an FFI round trip, or the semantics get a hand-written JS twin that slowly drifts. Here there is one implementation and one crossing, so it cannot drift, and the corner cases arrive already correct: catch-clause scope sharing, named function expression scopes, `var` hoist targets, TS declaration merging, space-aware resolution (a `const T` never captures a type-position `T` away from an outer `type T`), and write detection through destructuring patterns.

## What one `addFile` gives you

```js
const module = analyzer.addFile("app.tsx", source);

module.ast                  // ESTree / TS-ESTree program
module.scopes               // every lexical scope, as a tree
module.symbols              // every declared binding
module.references           // every identifier use, resolved to its symbol
module.unresolvedReferences // free names and globals
module.imports              // import records, dynamic import() and require() included
module.exports              // spec-true export records
module.moduleFlags          // CommonJS classification signals
module.diagnostics          // syntax + semantic errors
```

And node-level queries that work directly on AST nodes:

```js
module.symbolOf(node)    // the symbol a node declares or references
module.referenceOf(node) // the reference recorded for an identifier
module.scopeOf(node)     // the innermost scope containing the node
module.parentOf(node)    // the node that structurally contains it, or null
module.resolve("name")             // scope-chain lookup, like the engine at runtime
module.resolve("T", scope, "type") // or in another declaration space
```

Every reference carries the declaration space its position resolves in (`ref.space`: `"value"`, `"type"`, `"namespace"`, `"typeof"`, or `"any"`), and resolution is space-aware the way TypeScript's is: an inner `const T` does not capture a type annotation's `T` away from an outer `type T`.

Node identity is exact: the node you reach by walking `module.ast` and the node a semantic query returns are the same JavaScript object, so `===` always works.

## Editing the AST

That identity is the payoff. Because the `node` on a symbol or reference _is_ the AST node, a refactor is a plain assignment, and [`yuku-codegen`](https://www.npmjs.com/package/yuku-codegen) prints the mutated tree back to source. No visitor to register, no separate model to translate back.

```js
import { print } from "yuku-codegen";

const m = analyzer.addFile("util.ts", `const tmp = load();\nexport const data = tmp.value + tmp.size;`);
const tmp = m.rootScope.find("tmp");
tmp.declarations[0] === m.ast.body[0].declarations[0].id; // true, literally the same node

tmp.declarations[0].name = "raw"; // rename the binding
for (const ref of tmp.references) ref.node.name = "raw"; // and every resolved use

// across files, analyzer.referencesOf(symbol) hands back these same live nodes for every use
print(m.ast).code;
// const raw = load();
// export const data = raw.value + raw.size;
```

The uses come from resolved references, not a name search, so a shadowing inner `tmp` is left untouched: the rename only reaches the identifiers that actually bind to this symbol.

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

Linking is automatic: every cross-file surface relinks on demand after files change. Re-adding a path replaces its module with a new object and relinks, and removing it relinks too. Call `analyzer.link()` explicitly if you want to control when the work happens.

Module records and linking model ECMAScript and TypeScript module syntax, so CommonJS `require` and `module.exports` are ordinary code rather than records and take no part in linking. Per-file scopes, symbols, and references are still computed for CommonJS sources.

Module resolution is pluggable. The default resolves relative specifiers among added files with standard extension and index probing. Pass your own resolver for anything else:

```js
const analyzer = new Analyzer({
  resolve: (specifier, importerPath) => myResolver(specifier, importerPath),
});
```

## Performance

Analysis runs in the native parser pass, so full semantics cost roughly half of parsing time on top of the parse itself. Validated against every file in the [parser test corpus](https://yuku.fyi/testing/).

Concretely, on an Apple M-series machine: parsing plus complete semantic analysis of a typical source file lands well under a millisecond, walking sustains tens of millions of nodes per second, and linking a 2,000-module graph takes about a millisecond.

## TypeScript

Everything is fully typed. Visitor handlers receive exact node types, and the semantic surface (`Module`, `Scope`, `Symbol`, `Reference`, `Import`, `Export`, `Capture`) is exported:

```ts
import type { Module, Symbol, Capture } from "yuku-analyzer";
```

## Documentation

The full documentation, including the architecture, every type, and the design decisions: [yuku.fyi/analyzer](https://yuku.fyi/analyzer).

## License

MIT
