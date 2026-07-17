# yuku-analyzer

Full semantic analysis for JavaScript and TypeScript: scopes, symbols, resolved references, closures, and cross-file module linking, computed natively in Zig and queried as plain JavaScript objects. Powered by [Yuku](https://github.com/yuku-toolchain/yuku).

**No single library gives you all of this.** Scopes and resolved references mean `eslint-scope` or `@typescript-eslint/scope-manager`. Cross-file go-to-definition means the TypeScript compiler or `ts-morph`. A parser sits under both. `yuku-analyzer` is all of them in one native pass behind one API.

**At native speed.** Up to ~15× faster per file than `eslint-scope`, `@typescript-eslint/scope-manager`, and `@babel/traverse`, with zero per-query cost after the single native call. Stitch those separate tools together yourself and the gap only widens: each re-walks the AST, you re-parse to resolve across files, and you keep the indexes between them in sync by hand. `yuku-analyzer` pays all of that once, in Zig.

## Install

```bash
npm install yuku-analyzer
```

## Quick start

For one file, `analyze` returns the full per-file semantics in a call:

```js
import { analyze } from "yuku-analyzer";

const module = analyze(`const double = (n: number) => n * 2; double(21);`, { lang: "ts" });

module.rootScope.find("double").references.length; // 1
module.walk({
  Identifier(node, ctx) {
    console.log(node.name, ctx.scope.kind, ctx.symbol, ctx.reference);
  },
});
```

For a project, `Analyzer` adds files and links them:

```js
import { Analyzer, SymbolFlags } from "yuku-analyzer";

const analyzer = new Analyzer();

analyzer.addFile("lib.ts", `export const helper = (x: number) => x * 2;`);
const main = analyzer.addFile("main.ts", `import { helper } from "./lib.ts"; helper(21);`);

const helper = main.rootScope.find("helper");
helper.has(SymbolFlags.Import); // true

const def = helper.definition(); // follow the import across files
def.module.path; // "lib.ts"
```

## What you get

- **Scopes** as a tree, with `var` hoist targets, catch-clause sharing, and named-expression scopes exact.
- **Symbols** with TypeScript declaration merging, queried through one `SymbolFlags` bitset.
- **References** resolved and space-aware, so a value never captures a same-named type.
- **Node queries** on object identity: `symbolOf`, `referenceOf`, `scopeOf`, `parentOf`, `resolve`.
- **A semantic walk** and `walkAsync`, every handler receiving the current scope, symbol, and reference.
- **Closure analysis** via `capturesOf`, shadowing- and alias-correct.
- **Cross-file linking**: `definition()`, `referencesOf`, `exportedNames`, and spec-true `ResolveExport` across the graph.

Node identity is exact throughout: the node a semantic query returns is the same object you reach by walking `module.ast`, so a rename is a plain assignment and [`yuku-codegen`](https://www.npmjs.com/package/yuku-codegen) prints it back.

## Documentation

The full guide, with the architecture, every API, and the design decisions: **[yuku.fyi/analyzer](https://yuku.fyi/analyzer)**.

## License

MIT
