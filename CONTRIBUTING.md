# Contributing to Yuku

## Set up

You need [Zig](https://ziglang.org/) 0.17 and [Bun](https://bun.sh/).

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

This builds everything and runs every suite, fetching the parser test suite and
a few pinned open-source projects on the first run. To run one suite, use
`test:parser`, `test:codegen`, `test:analyzer`, `test:sourcemap`, `test:ast`,
or `test:runtime`, which runs the package with the `node` on
your path, and in CI on the minimum runtime (see AGENTS.md). After editing Zig,
run `bun run build` first so the suites see your change.

### Add a test

- **Parser**: drop a file into `test/parser/misc/` under `js`, `ts`, `jsx`, or
  `comments`, then run `bun run test:parser`. Its snapshot is written for you.
- **Codegen and analyzer**: add a test with an empty inline snapshot to any file
  in `test/codegen/` or `test/analyzer/`, then run the suite with
  `--update-snapshots`.

Review the snapshot and commit it with your test. For what each suite checks,
see [how Yuku is tested](https://yuku.fyi/testing/).

### Change the codegen

The printer is `src/parser/codegen/printer.zig`, and `print` runs it on the tree
that `npm/yuku/core/encode.js` writes from ESTree. `bun run test:codegen` prints
the test corpus from both the parsed and the encoded tree and fails on any
difference.

## Find your way around

```
src/spec/        what a specification defines, each linked to its section
src/util/        generic helpers that no specification defines
src/parser/
  lexer.zig      tokenizer
  parser.zig     parser entry point
  ast.zig        node definitions
  syntax/        grammar by construct, with ts/ and jsx/
  codegen/       print, strip, minify, source maps
  semantic/      scopes, symbols, references, early errors
  traverser/     AST visitors
  ffi/           Node-API and WebAssembly bindings
  testing/       Zig tests and the fuzzer
npm/yuku/        the published package, one entry in index.ts
  core/          the addon and WebAssembly loaders, the decoder, and the encoder
  ast/           walk, builders, and utils
  file/          SourceFile, Project, the semantic model, and rename
  print/         print and its options
  types/         the AST types
npm/wasm/        the core as WebAssembly, for edge runtimes
npm/*-*/         the core as a Node-API addon, one package per platform
test/            test suites
profile/         benchmarks CodSpeed runs on every pull request
docs/            the website
```

## Edit the docs

[yuku.fyi](https://yuku.fyi) documents the JavaScript package and the Zig API,
from the Markdown files in `docs/content/`. Preview them with
[Zine](https://github.com/kristoff-it/zine/releases) 0.11.3:

```bash
cd docs && zine
```

Then open http://localhost:1990. The JavaScript API lives in the package's
guide, `npm/yuku/README.md`, which npm and GitHub show. When you're done with either, run
`bun run docs:llms`, which regenerates `llms.txt` and `llms-full.txt`.

## Open a pull request

Run `bun run format` and `bun run test`, then open your PR. Title it like a
commit subject, lowercase and imperative with no area prefix, for example
`accept legal line breaks in TypeScript declarations`.
