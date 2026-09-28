# Contributing to Yuku

Yuku is a JavaScript/TypeScript toolchain written in pure Zig: a parser, a
codegen (print, strip, minify) with source maps, and a semantic analyzer.

## Prerequisites

- [Zig](https://ziglang.org/) 0.16.0 or later.
- [Bun](https://bun.sh/) for the test suites and workspace tooling.

## Setup

```bash
git clone https://github.com/yuku-toolchain/yuku.git
cd yuku
bun install
zig build
```

## Project layout

```
src/
  main.zig            playground (zig build run)
  parser/
    lexer.zig         tokenizer
    parser.zig        parser entry point and state
    ast.zig           node definitions and the Tree API
    syntax/           the grammar, by construct (class.zig, modules.zig, ts/, jsx/, ...)
    traverser/        AST visitors
    codegen/          print / strip / minify, plus source maps
    semantic/         scopes, symbols, references
    ffi/              N-API and WebAssembly bindings, and the AST transfer format
    testing/          Zig-side tests and the fuzzer (zig build test, zig build fuzz)
tools/                code generators (JS decoders and encoder, walk tables, token types)
test/                 the product test suites, run through the published JS packages
npm/                  published JS packages (the native bridges the tests import)
docs/                 the website
```

## Playground

`src/main.zig` runs the toolchain on a snippet: parse the source, traverse the
AST with a visitor, then codegen it back. It is the quickest way to try a change:

```bash
zig build run
```

Edit the source, the parse options, the visitor hooks, or the codegen options.
For a tight loop, use watch mode:

```bash
zig build run --watch -fincremental
```

`bun run playground` serves the web playground instead. Every pull request
publishes preview packages, and `https://playground.yuku.fyi/?pr=<commit>` loads
them, so a change can be tried in the playground before it merges.

## Testing

Run the full suite:

```bash
bun run test
```

It downloads the parser corpus (over 55,000 files from Test262, TypeScript, and
Babel, fetched again only when upstream changes), builds the native and
WebAssembly packages from your Zig, typechecks, then runs every suite. For what
the suites verify, see [how Yuku is tested](https://yuku.fyi/testing/).

> The JS suites import packages compiled from your Zig. `bun run test` rebuilds
> them first. If you run a suite on its own after editing Zig, run
> `bun run build:local` first (and `bun run build:wasm` for the WebAssembly
> suite), or it tests the previous build. `build:local` builds only for your
> machine, `build:npm` builds every platform for publishing.

### Parser

```bash
bun run test:parser
```

The corpus under `test/parser/suite/` checks the parser against the wider
ecosystem (you don't edit these). Your own cases go in `test/parser/misc/`. The
runner writes per-suite results to `test/parser/results/` and exits non-zero on
any failure.

To add one:

1. Drop a source file in `test/parser/misc/<group>/`, where `<group>` is `js`,
   `ts`, `jsx`, or `comments`. A `.module.ts` / `.module.js` name parses as a
   module, otherwise as a script. The `js/semantic`, `js/commonjs`, and
   `js/preserve-parens-disabled` folders run with those parse options.
2. Run `bun run test:parser`. A new fixture auto-generates its snapshot at
   `snapshots/<name>.snapshot.json`, capturing the AST, comments, and diagnostics.
3. Check the snapshot, then commit it with the fixture. Errors are allowed, so a
   failing case records its diagnostic in the snapshot.

When a change updates existing snapshots, re-run with `--update-snapshots` and
review the diff:

```bash
bun test/parser/run.ts --update-snapshots
```

### Codegen

```bash
bun run test:codegen
```

Inline-snapshot tests via the `gen(source, options?, path?, parseOptions?)`
helper, which parses `source` as the language of `path` (default `input.ts`) and
returns the generated code:

```ts
test("strip drops type annotations", () => {
  expect(gen(`let x: number = 1;`, { strip: true })).toMatchInlineSnapshot(`"let x = 1;"`);
});
```

Add a test in any `test/codegen/*.test.ts`, leave the snapshot empty, and fill it
with `bun run test:codegen --update-snapshots`.

### Analyzer

```bash
bun run test:analyzer
```

Inline-snapshot tests for scopes, symbols, and references via a `summary()`
helper. Update with `bun run test:analyzer --update-snapshots`.

### Source maps

```bash
bun run test:sourcemap
```

Round-trips every corpus file through `generate` with source maps and checks
each identifier traces back to the right name. No snapshots to maintain.

### Other suites

- `bun run test:ast` tests the `yuku-ast` walker and utilities.
- `bun run test:wasm` tests the WebAssembly packages.
- `zig build test` runs the Zig-side tests, and `zig build fuzz` the fuzzer.

## Formatting

```bash
bun run format
```

## Documentation

The docs live in `docs/`, built with [Zine](https://zine-ssg.io) 0.11.3. Pages
are Markdown (`.smd`) files in `docs/content/`. Install the
[Zine binary](https://github.com/kristoff-it/zine/releases), then:

```bash
cd docs
zine            # preview at http://localhost:1990
zine release    # build into docs/public/
```

After editing pages, regenerate `docs/assets/llms.txt` and `llms-full.txt`:

```bash
bun run docs:llms
```
