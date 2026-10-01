# Contributing to Yuku

Yuku is a JavaScript and TypeScript toolchain written in pure Zig: a parser, a
code generator with source maps, and a semantic analyzer.

## Set up

You need [Zig](https://ziglang.org/) 0.16 and [Bun](https://bun.sh/).

```bash
git clone https://github.com/yuku-toolchain/yuku.git
cd yuku
bun install
```

## Try a change

`src/main.zig` parses a snippet, walks the AST, and prints it back. Change the
source or the options and run it:

```bash
zig build run --watch -fincremental
```

It reruns on every save. For a browser playground, run `bun run playground`.

## Test it

```bash
bun run test
```

This builds everything from your Zig and runs every suite, fetching the parser
test suite and a few pinned open-source projects on the first run. To run a
single one, use `test:parser`, `test:codegen`, `test:analyzer`,
`test:sourcemap`, `test:ast`, `test:wasm`, or `test:runtime`, which runs every
package with the `node` on your path. CI runs it on the minimum runtime, see
AGENTS.md. After editing Zig, run `bun run build:local` and `bun run build:wasm`
first so the suites see your change.

### Add a test

- **Parser**: drop a file into `test/parser/misc/` under `js`, `ts`, `jsx`, or
  `comments`, then run `bun run test:parser`. Its snapshot is written for you.
- **Codegen and analyzer**: add a test with an empty inline snapshot to any file
  in `test/codegen/` or `test/analyzer/`, then run the suite with
  `--update-snapshots`.

Review the snapshot and commit it alongside your test. To learn what each suite
checks, see [how Yuku is tested](https://yuku.fyi/testing/).

### Change the codegen

The codegen has a Zig implementation, `src/parser/codegen/printer.zig`, and a
JavaScript one, `npm/yuku-codegen/src/printer.ts`. They mirror each other
function by function, so a change lands in both. `bun run test:codegen` prints
the test corpus with each and fails on any difference.

## Find your way around

```
src/parser/
  lexer.zig      tokenizer
  parser.zig     parser entry point
  ast.zig        node definitions
  syntax/        grammar by construct, with ts/ and jsx/
  codegen/       print, strip, minify, source maps
  semantic/      scopes, symbols, references, early errors
  traverser/     AST visitors
  ffi/           N-API and WebAssembly bindings
  testing/       Zig tests and the fuzzer
npm/             published packages
test/            test suites
docs/            the website
```

## Edit the docs

Pages are Markdown files in `docs/content/`. Preview them with
[Zine](https://github.com/kristoff-it/zine/releases) 0.11.3:

```bash
cd docs && zine
```

Then open http://localhost:1990. When you're done, run `bun run docs:llms`.

## Open a pull request

Run `bun run format` and `bun run test`, then open your PR. Title it like a
commit subject, `area: what changed`, for example
`parser: accept legal line breaks in TypeScript declarations`.
