# Contributing to Yuku

Yuku is pure Zig with no external dependencies.

## Prerequisites

- [Zig](https://ziglang.org/) **nightly/development build** (build + compile). Yuku always keeps up to date with the latest Zig development version.
- [Bun](https://bun.sh/) (test runner, playground, workspace dependencies)

## Setup

```bash
git clone https://github.com/yuku-toolchain/yuku.git
cd yuku
bun install
zig build
```

## Testing

Run the full test suite (45,000+ files from Test262 and others) with AST matching:

```bash
bun run test
```

The first run will download the test suite (wait for it to finish). After the run completes, check `test/results` for results.

## Playground

A scratch space for quickly testing the parser:

```bash
bun play
```

This recompiles the project and rebuilds the npm package before running `test/playground.ts`, so any changes you make to the source (e.g. the parser) will be reflected immediately. Edit it however you want. Change the input file, swap parser options, log different parts of the AST. By default it parses `test/index.js`, but nothing is fixed.
