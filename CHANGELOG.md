# Changelog

What changed in each release of Yuku, newest first. Releases up to 0.14.0 are listed on [GitHub](https://github.com/yuku-toolchain/yuku/releases).

## 0.24.0

### Breaking

- `pattern` is removed, so test nodes in a `walk` visitor with the `is` guards (#295 by @arshad-yaseen)
- The `b` builders are removed, so build nodes with `js` and `ts`, and build a node that only exists inside another, such as a property, from a template of its container (#295 by @arshad-yaseen)
- `is`, `unwrap`, and the identifier checks import from `yuku/utils`, where the checks are plain exports, so `lexical.isValidIdentifier` is `isValidIdentifier` (#295 by @arshad-yaseen)
- `nameOf`, `bindingIdentifiers`, `clone`, `langFromPath`, and `sourceTypeFromPath` are removed. Read `node.name` and `binding.declarations`, copy a node with `structuredClone`, and read `file.lang` and `file.sourceType` instead (#295 by @arshad-yaseen)
- On Cloudflare Workers and Vercel Edge, also install `@yuku-toolchain/wasm`, and import `yuku/yuku.wasm` as `@yuku-toolchain/wasm/yuku.wasm` (#295 by @arshad-yaseen)

### Changes

- Installing `yuku` fetches only the addon for its platform, from `@yuku-toolchain/<platform>`, so an install takes about 2.4 MB instead of 7.5 MB (#295 by @arshad-yaseen)
- Templates take a string or a number inside quotes, as in ``js`import x from "${path}";` ``, build `yield` as a generator would, close a JSX tag with its own hole, keep the type of a name such as `(${p}: T)` and the comment before an expression, and copy a node used twice (#295 by @arshad-yaseen)
- Templates with holes build 3 to 7 times faster (#295 by @arshad-yaseen)
- A regular expression on the line after an expression that ends in an arrow function or `yield` parses, as in `x = () => {}` followed by `/=/g.test(y)`, and `x = () => {} + 1` is an error (#296 by @arshad-yaseen)

## 0.23.0

### Breaking

- The `wasm` option of `parse` and `new Project` is `core`, so pass `core: new Core({ wasm: module })` where you passed `wasm: module`

### Changes

- A dialect runs natively in Node.js, Bun, and Deno when its package ships addons in its core, as `new Core({ wasm, addons })`, and `addAddons` in `build.zig` builds them for every platform

## 0.22.2

### Changes

- A regular expression with an unknown, repeated, or conflicting flag keeps its node and the code after it, and the error points at the flag (#284 by @arshad-yaseen)
- Enum member names follow TypeScript, so `["D"]` and `` [`D`] `` are valid and resolve, a name that is a number such as `"1"` is an error, and a computed member starts at its `[` (#284 by @arshad-yaseen)

## 0.22.1

### Changes

- Node.js, Bun, and Deno with `--allow-ffi` run Yuku natively on macOS, Linux, and Windows, for x64 and arm64, parsing a whole project 27 to 34% faster with up to 35% less peak memory, and everything else keeps running WebAssembly (#283 by @arshad-yaseen)

## 0.22.0

### Breaking

- Everything imports from `yuku`. `yuku/analyzer`, `yuku/ast`, `yuku/codegen`, and `yuku/types` are removed, and their exports keep their names unless a line below says otherwise (#282 by @arshad-yaseen)
- `parse` returns a `SourceFile` in place of `ParseResult`, and reading `tokens` without `tokens: true` throws instead of returning `undefined` (#282 by @arshad-yaseen)
- `analyze(source)` is `parse(source, { semantic: true })`, which puts scopes, bindings, references, imports, and exports on the same `SourceFile`. `Module` is `SourceFile`, `module.ast` is `file.program`, and reading the semantic model without `semantic: true` throws (#282 by @arshad-yaseen)
- `Analyzer` is `Project`, with `set` in place of `setFile`, `delete` in place of `deleteFile`, and `files` in place of `modules` and `module(path)`. `link()` is removed, since queries relink as needed, and `AnalyzerOptions` and `SetFileOptions` are `ProjectOptions` and `FileOptions` (#282 by @arshad-yaseen)
- Scopes, bindings, references, imports, exports, and definitions have `file` in place of `module`, and imports and exports have `resolvedFile` in place of `resolvedModule` (#282 by @arshad-yaseen)
- `module.lookup` is `scope.lookup`, and `module.walk` and `module.findAll` are removed, so use `walk` with `file.scopeOf`, `file.bindingOf`, and `file.referenceOf` (#282 by @arshad-yaseen)
- `unresolvedReferences`, `binding.hasAll`, `import.isNamespace`, and `export.globalName` are removed. Filter `file.references` by `binding === null`, test `binding.flags`, and read `import.kind` and `export.name` instead (#282 by @arshad-yaseen)
- `walk` takes no state argument and returns nothing, and its context has no `node` or `state`, so keep state in your handlers' scope (#282 by @arshad-yaseen)
- `walkAsync` is removed. Collect the nodes in one walk, await the work, then edit in a second walk, as the README shows (#282 by @arshad-yaseen)
- `findAll`, `isCallOf`, `isWrapper`, and `literalValue` are removed. Use `walk` with a typed handler, `nameOf(unwrap(node.callee))` on a `CallExpression`, `is.Wrapper`, and `node.value` (#282 by @arshad-yaseen)
- `collectImports`, `collectExports`, `collectImportDeclaration`, and `collectExportDeclaration` are removed, so read `imports` and `exports` from `parse(source, { semantic: true })` (#282 by @arshad-yaseen)
- The identifier checks are under `lexical`, as in `lexical.isIdentifierName`. `isStrictBindOnlyReservedWord` is `lexical.isEvalOrArguments`, and `isStrictBindReservedWord` is `lexical.isStrictReservedWord` or `lexical.isEvalOrArguments` (#282 by @arshad-yaseen)
- `generate` is `print`, with `PrintOptions` and `PrintResult` in place of `GenerateOptions` and `GenerateResult`, and its `print` option is `printers` (#282 by @arshad-yaseen)
- `minify` takes only `true` or `false`, and also picks the compact format, the shortest quotes, and `"some"` comments, which `format`, `quotes`, and `comments` still override. Write `minify: true, format: "pretty"` to minify syntax alone (#282 by @arshad-yaseen)
- `comments` takes `"all"` and `"none"` in place of `true` and `false`, and `print` throws on an option it does not know (#282 by @arshad-yaseen)
- Zig: `tree.setIdentifierName` is `tree.setName`, `Tree.initEmpty(allocator)` is `Tree.init(allocator, "")`, `tree.hasDiagnostics()` is removed in favor of `tree.diagnostics.items.len`, and `ast.StringPool` is removed (#282 by @arshad-yaseen)
- Zig: `Semantic`, `Symbol`, `SymbolId`, `Reference`, `ReferenceId`, `Scope`, `ScopeId`, and `ScopeTree` come from `parser.semantic`, since `traverser.semantic` and `traverser.scoped` no longer re-export them (#282 by @arshad-yaseen)
- Zig: `semantic.iterSymbols`, `iterScopes`, and `iterReferences` are removed, so loop over `semantic.symbols`, `semantic.scopes.list`, and `semantic.references`. `semantic.scope(id)` is `semantic.scopes.get(id)`, and `flags.inValueSpace()` is `flags.intersects(Symbol.value_space)`, and likewise for types and namespaces (#282 by @arshad-yaseen)
- Zig: a scoped or semantic traverser reads `ctx.scope.current` and `ctx.scope.get(id)` in place of `currentScope()`, `hoistTarget()`, and `isStrict()` (#282 by @arshad-yaseen)
- Zig: `semantic.AnalysisError`, `codegen.Error`, and `dialect.Error` are `std.mem.Allocator.Error`, and `codegen.Diagnostic` is `ast.Diagnostic` (#282 by @arshad-yaseen)
- Zig: a dialect calls `recordComment` in place of `addComment`, and reads `p.tree().source` in place of `p.source()` (#282 by @arshad-yaseen)

### Changes

- `pattern` matches nodes against code such as `$A.then($$$ARGS)` and returns what each hole captured (#282 by @arshad-yaseen)
- `js` and `ts` build nodes from code with values in its holes, as in ``js`console.log(${node})` `` (#282 by @arshad-yaseen)
- `clone` makes a deep copy of a node (#282 by @arshad-yaseen)
- `binding.rename` renames a binding where it is declared and used, keeps the file's exports, and changes nothing when the rename would change what the code means, returning the reasons instead (#282 by @arshad-yaseen)
- `file.locate(offset)` gives the line and column of an offset (#282 by @arshad-yaseen)
- `walk` takes a list of visitors and runs them in one pass, and walks 43 to 48% faster (#282 by @arshad-yaseen)
- `print` takes a `SourceFile` as well as a node, and `sourceMap: true` adds a source map with the default options (#282 by @arshad-yaseen)
- Parsing is up to 24% faster on large files, and parsing a whole project takes 11 to 15% less time and 19 to 37% less peak memory (#282 by @arshad-yaseen)
- Reading one node from the semantic model, such as a reference's node, builds only that node instead of the whole AST (#282 by @arshad-yaseen)
- Zig: `NumericLiteral.value` is exact for hexadecimal, octal, and binary literals above 2^64, and reads decimal literals of any length (#282 by @arshad-yaseen)

## 0.21.0

### Breaking

- `analyze` and `Analyzer` report the early errors that need scopes, such as redeclarations, only with `semanticErrors: true`, as `parse` does, so pass it to keep them (#273 by @arshad-yaseen)
- `yuku/types` no longer exports `AttachedComment`. A node's `comments` holds the same `Comment` objects as the `comments` list, so use `Comment`, which now also has `start` and `end` (#272 by @arshad-yaseen)
- Zig: the `comments` parse option takes `.none`, `.collect`, or `.attach` in place of `.flat`, `.attached`, and `.both`, and `tree.commentsOf` returns an iterator of `Comment` (#272 by @arshad-yaseen)
- Zig: `semantic.analyze` takes options, and reports early errors only with `.check = true` (#273 by @arshad-yaseen)
- Zig: a custom traverser context passes its visitor straight to `traverser.walk` with a `.whole_tree` option, in place of `traverser.Layer`, and its `enter` and `exit` take the node tag first (#275 by @arshad-yaseen)
- Zig: `ast.JSXText.value` is removed, so decode `raw` with `lexical.decodeJSXEntities` (#276 by @arshad-yaseen)

### Changes

- `analyze` is 17 to 21% faster on large files, or 12 to 16% with `semanticErrors` (#273, #275 by @arshad-yaseen)
- Identifiers that escape `$` or `_`, such as `$a`, parse (#270 by @arshad-yaseen)
- Identifier checks in `yuku/ast`, such as `isIdentifierStart`, follow the same Unicode version as the parser on every runtime, instead of the runtime's own (#270 by @arshad-yaseen)
- A decorator with type arguments parses before `export`, as in `@dec<T> export class A {}` (#277 by @arshad-yaseen)
- `(): (void) => {}` reads `(void)` as the return type, and `type F = (this) => void` parses as a function type (#278 by @arshad-yaseen)
- Zig: semantic analysis is 14 to 20% faster (#275 by @arshad-yaseen)
- Zig: `parser.lexical` gives the lexer's rules for identifiers, keywords, line breaks, and escapes, including `isEvalOrArguments` (#270, #276 by @arshad-yaseen)
- Zig: `tree.addComment` and `tree.moveComments` add and move a node's comments, `tree.hasComments` checks for them, and `tree.commentsIn` and `tree.tokensIn` return those within a span (#272, #276 by @arshad-yaseen)
- Zig: `semantic.member` finds a member of a namespace or enum across its merged bodies (#275 by @arshad-yaseen)

## 0.20.1

### Changes

- Yuku runs in Vercel's Edge Runtime, which loads it through the `edge-light` condition (#264 by @arshad-yaseen)
- `generate` is 12 to 17% faster with a source map, and 7 to 13% faster without one (#263 by @arshad-yaseen)
- Parsing works on big-endian hosts such as s390x (#265 by @arshad-yaseen)
- A `for (init;;)` head parses `in` correctly inside parentheses, literals, templates, functions, and classes (#265 by @arshad-yaseen)
- JSX text after a closing fragment parses, so `<p><>a</> — b</p>` works (#265 by @arshad-yaseen)
- TypeScript overloads of a private method no longer report a duplicate private name (#265 by @arshad-yaseen)
- `super()` in a computed key of a class inside a derived constructor no longer reports an error (#265 by @arshad-yaseen)
- `module.imports` lists `export {} from "m"` as a side-effect import, and `import()` or `require()` of a parenthesized string or a template literal without substitutions (#265 by @arshad-yaseen)
- `generate` keeps the braces in `import type {} from "m"`, which printed as invalid `import type "m"` (#265 by @arshad-yaseen)
- `generate` keeps the parentheses around a `++` or `--` operand followed by a comment that breaks the line (#265 by @arshad-yaseen)
- Source maps map a JSX attribute's string value, and a string key that minifying prints as a name, to its own column (#265 by @arshad-yaseen)
- `unwrap` from `yuku/ast` accepts any node, `null`, or `undefined`, and returns anything it does not unwrap as it is (#265 by @arshad-yaseen)

## 0.20.0

### Breaking

- Yuku now ships as one package, `yuku`, in place of `yuku-parser`, `yuku-analyzer`, `yuku-ast`, `yuku-codegen`, and `@yuku-toolchain/types`. Install `yuku` and import from `yuku`, `yuku/analyzer`, `yuku/ast`, `yuku/codegen`, and `yuku/types` (#225 by @arshad-yaseen)
- `yuku-core`, `@yuku-core/wasm`, and the `@yuku-core/binding-*` native packages are removed, since everything runs on the WebAssembly module inside `yuku` (#225 by @arshad-yaseen)
- Node.js 16.4 or newer is required, and browsers need WebAssembly, such as Safari 16.4 or newer (#225 by @arshad-yaseen)
- `yuku/types` no longer exports `Core` (#225 by @arshad-yaseen)
- Zig: `-Dparser-extension` is replaced by `-Ddialect`, the semantic traverser's `Ctx.init` and `SymbolTracker.init` take a mode, and `Ctx.parentOf` is removed (#225 by @thejackshelton and @arshad-yaseen)

### Changes

- Dialects let you add your own syntax to the parser in Zig, matched to files by extension and passed to `parse` and `analyze` through the `wasm` option (#225 by @thejackshelton and @arshad-yaseen)
- Yuku runs unchanged in Node.js, Bun, Deno, browsers, and Workers, with no setup before the first parse (#225 by @arshad-yaseen)
- Parsing is 13 to 22% faster (#225 by @arshad-yaseen)
- `semanticErrors` is 29 to 49% cheaper, since it skips the full semantic model (#225 by @arshad-yaseen)
- Repeated calls reuse memory, so batches that mix large and small files run much faster (#250, #225 by @arshad-yaseen)

## 0.19.0

### Breaking

- Using Yuku as a Zig package now needs Zig 0.17.0 or later, so upgrade Zig before you update (#242 by @arshad-yaseen)

### Changes

- `generate` in `yuku-codegen` prints any single node, such as an object property, not only a whole `Program` (#245 by @arshad-yaseen)
- `@yuku-core/wasm` parses an empty source correctly in runtimes like WebContainers, where it could read leftover bytes as input (#246 by @arshad-yaseen)

## 0.18.1

### Changes

- parser: join a surrogate pair in a string however it is written (#239 by @arshad-yaseen)
- parser: span a type predicate's type without its stripped parentheses (#239 by @arshad-yaseen)
- codegen: print every attached comment (#238 by @arshad-yaseen)
- codegen: strip an uninitialized `const` as ambient (#239 by @arshad-yaseen)
- codegen: reprint a hashbang verbatim (#239 by @arshad-yaseen)

## 0.18.0

### Breaking

- all: pass the core strings (#233 by @arshad-yaseen)
  - `Core.parse` and `Core.analyze` take a string. Decode UTF-8 bytes before passing them to a core. `parse` and `setFile` still accept bytes.

### Changes

- all: decode and walk trees of any depth (#228 by @arshad-yaseen)
- parser: attach comments at any depth (#233 by @arshad-yaseen)
- parser: reject `import` and `export` in a script or below the top level (#234 by @arshad-yaseen)
- parser: reject decorators on both sides of `export` (#234 by @arshad-yaseen)
- parser: decode entities in JSX text and attribute values (#234 by @arshad-yaseen)
- parser: report duplicate exports and redeclarations as TypeScript does (#234 by @arshad-yaseen)
- analyzer: merge `declare module "m"` augmentations into `m` (#234 by @arshad-yaseen)
- analyzer: resolve `ns` in `import x = ns.T` in the namespace space (#234 by @arshad-yaseen)
- analyzer: mark the members of a declare enum ambient (#233 by @arshad-yaseen)
- codegen: keep decorators before `export` (#234 by @arshad-yaseen)
- types: correct pattern unions and the `findAll` parameter

## 0.17.0

### Breaking

- all: pass the core explicitly and rename the engine to yuku-core (#227 by @arshad-yaseen)
  - `yuku-engine` is `yuku-core`, and `@yuku-engine/wasm` is `@yuku-core/wasm`.
  - `init()` is gone. Load the WebAssembly core with `load()` or `loadSync()` from `@yuku-core/wasm` and pass it as `core` to `parse`, `analyze`, or `new Analyzer`.
  - Installing `@yuku-core/wasm` no longer switches the packages to it, and a platform without a native build no longer falls back to it. Pass it in as `core` instead.

### Changes

- wasm: build the WebAssembly core for speed (#227 by @arshad-yaseen)

## 0.15.1

### Changes

- analyzer: memoize export resolution for each link
- analyzer: index nodes on the first node query

## 0.15.0

### Breaking

- all: tidy the analyzer API and share file options and diagnostics
  - `addFile` and `removeFile` are `setFile` and `deleteFile`.
  - `Symbol` and `SymbolFlags` are `Binding` and `BindingFlags`, and `symbols`, `symbolOf`, and `.symbol` are `bindings`, `bindingOf`, and `.binding`.
  - `module.resolve(name, scope, space)` is `module.lookup(name, { from, space })`.
  - `analyzer.definitionOf` and `analyzer.referencesOf` are `binding.definition()` and `binding.findReferences()`.
  - The `is*` kind predicates are gone, except `imp.isNamespace`. Compare `kind` instead.
  - A resolver returns `false` for a module outside the project. `null` means unresolved.
  - Link diagnostics have the shared diagnostic shape, with `path` in place of `module`.
  - An unknown `lang` or `sourceType` throws a `TypeError`.

### Changes

- all: resolve names, merges, and exports as tsc does (#223 by @arshad-yaseen)
- analyzer: probe module paths in TypeScript's order (#223 by @arshad-yaseen)
- analyzer: add `module.ancestors` (#223 by @arshad-yaseen)
- all: build decoder caches on demand (#223 by @arshad-yaseen)
- all: add a `path` option to `parse`, and `module.isCurrent`, `module.nodeAt`, and `module.resolveExport`
