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
    testing/          Zig-side tests and the fuzzer (zig build test, zig build fuzz)
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

Edit `source`, change the parse `options`, add an `enter_*` / `exit_*` visitor
hook, or swap `strip` for `print` / `minify`. For a tight loop, use watch mode:

```bash
zig build run --watch -fincremental
```

`bun run playground` serves the web playground instead.

## Testing

Run the full suite:

```bash
bun run test
```

On first run it downloads the parser corpus (tens of thousands of files from
Test262, TypeScript, Babel, and others, cached for a day), builds the native
addons from your Zig, then runs the parser, codegen, analyzer, and source map
suites. For what these suites verify and how conformance is tracked, see
[how Yuku is tested](https://yuku.fyi/testing/).

> The JS suites import native addons (`yuku-parser`, `yuku-codegen`,
> `yuku-analyzer`) compiled from your Zig. `bun run test` rebuilds them first via
> `build:local`, which builds only for your machine. If you run a suite on its own
> after editing Zig, run `bun run build:local` first, or it tests the previously
> built addon. (Plain `build:npm` builds every platform, for publishing.)

### Parser

```bash
bun run test:parser
```

The corpus under `test/parser/suite/` checks the parser against the wider
ecosystem (you don't edit these). Your own cases go in `test/parser/misc/`.

To add one:

1. Drop a source file in `test/parser/misc/<group>/`, where `<group>` is `js`,
   `ts`, `jsx`, or `comments`. A `.module.ts` / `.module.js` name parses as a
   module, otherwise as a script.
2. Run `bun run test:parser`. A new fixture auto-generates its snapshot at
   `snapshots/<name>.snapshot.json`, capturing the AST, comments, and diagnostics.
3. Check the snapshot, then commit it with the fixture. Errors are allowed, so a
   failing case records its diagnostic in the snapshot.

When a change updates existing snapshots, re-run with `--update-snapshots` and
review the diff:

```bash
bun run test:parser --update-snapshots
```

### Codegen

```bash
bun run test:codegen
```

Inline-snapshot tests for `print`, `strip`, and `minify` via the `gen()` helper:

```ts
test("strip drops type annotations", () => {
  expect(gen("strip", `let x: number = 1;`)).toMatchInlineSnapshot(`"let x = 1;"`);
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

Round-trips every corpus file through `print` with source maps and checks each
identifier traces back to the right name. No snapshots to maintain.

## Formatting

```bash
bun run format
```

## Documentation

The docs live in `docs/`, built with [Zine](https://zine-ssg.io). Pages are
Markdown (`.smd`) files in `docs/content/`. Install the
[Zine binary](https://github.com/kristoff-it/zine/releases), then from `docs/`:

```bash
cd docs
zine            # preview at http://localhost:1990
zine release    # build into docs/public/
```

After editing pages, regenerate `docs/assets/llms.txt` and `llms-full.txt`:

```bash
bun run docs:llms
```
