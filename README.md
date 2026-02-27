<!-- markdownlint-disable first-line-h1 -->

<!-- markdownlint-start-capture -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="/docs/public/logo.svg" alt="Logo" width="300" />
  
  <br>
  <br>

  [![NPM Version](https://img.shields.io/npm/v/yuku?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku)
  [![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

A high-performance JavaScript/TypeScript toolchain written in Zig, bringing modern JavaScript tooling infrastructure to the Zig ecosystem.

</div>

> **Early stage, under active development.** The JavaScript and JSX parser is complete with full spec compliance and thorough testing. A visitor/traverser API and TypeScript support are currently in progress. See the [Roadmap](#roadmap) for details.

## Parser

- **Correctness**: Full ECMAScript spec compliance. Passes all 40000+ files from [Test262](https://github.com/tc39/test262) with AST matching. See [test results](/test/results.txt).
- **Performance**: Exceptionally fast through meticulous performance engineering and data-oriented design. Competitive with leading parsers like Oxc. [See benchmarks](https://github.com/yuku-toolchain/parser-benchmark).
- **Modern**: Supports modern and experimental JavaScript features, including decorators, source and defer imports, and more.

### AST

Yuku produces the same AST as [Oxc](https://oxc.rs):

- **JavaScript / JSX** — Fully conformant with the [ESTree](https://github.com/estree/estree) standard, identical to the AST produced by [Acorn](https://www.npmjs.com/package/acorn).
- **TypeScript** — Conforms to the [TS-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) format used by `@typescript-eslint`.

The only extensions beyond the base specs are support for Stage 3 [decorators](https://github.com/tc39/proposal-decorators), [import defer](https://github.com/tc39/proposal-defer-import-eval), [import source](https://github.com/tc39/proposal-source-phase-imports), and a non-standard `hashbang` field on `Program`.

## Usage

The parser can be used from Zig or from JavaScript/TypeScript via WASM.

### Zig

```zig
const std = @import("std");
const parser = @import("parser");

const tree = try parser.parse(allocator, "const x = 5;", .{
    .lang = .js,       // .js, .jsx, .ts, .tsx, .dts
    .source_type = .module, // .module, .script
});
defer tree.deinit();
```

The parser uses an `ArenaAllocator` internally. The allocator you pass to `parse` is the backing (child) allocator for that arena. The returned `ParseTree` owns the arena, which holds all memory the parser allocated:

- `program` - root node (always a `Program`)
- `nodes` - all AST nodes
- `extra` - extra data for variadic node children
- `diagnostics` - errors and warnings from parsing
- `comments` - comments found in source
- `arena` - the arena allocator owning all the memory

Calling `tree.deinit()` frees everything at once.

To serialize the AST to JSON (useful for quick inspection):

```zig
const json = try parser.estree.toJSON(&tree, allocator, .{});
defer allocator.free(json);

std.debug.print("{s}\n", .{json});
```

> A visitor/traverser API is in progress and will be the recommended way to work with the AST. JSON serialization is available now for quick testing.

### JavaScript / TypeScript (WASM)

```bash
npm install yuku-parser-wasm
```

```ts
import { parse } from "yuku-parser-wasm";

const ast = await parse("const x = 5;", {
  sourceType: "module",
  lang: "js",
});
```

A native Node.js module (N-API) will be available in the future.

## Contributing

Yuku is pure Zig with no external dependencies.

```bash
git clone https://github.com/yuku-toolchain/yuku.git
cd yuku
zig build
```

> Requires the latest Zig nightly build. Yuku stays up to date with Zig's latest development version.

`src/main.zig` parses `test.js` and prints the AST as JSON. Edit `test.js` and run:

```bash
zig build run
```

### Testing

Run the full test suite (40,000+ files from Test262 and others) with AST matching:

```bash
bun run test
```

The first run will download the test suite (wait for it to finish). After the run completes, check `test/results.txt` for results.

## Roadmap

- [x] JavaScript Parser
- [x] WASM
- [x] JSX Support
- [ ] Visitor/Traverser (In Progress)
- [ ] TypeScript Support
- [ ] Documentation
- [ ] Module Resolver
- [ ] TypeScript Declaration Transpiler, Minifier, and Bundler
