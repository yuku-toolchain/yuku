# Contributing to Yuku

Yuku is pure Zig with no external dependencies.

## Setup

```bash
git clone https://github.com/yuku-toolchain/yuku.git
cd yuku
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

This rebuilds the npm package and runs `test/playground.ts`. Edit it however you want. Change the input file, swap parser options, log different parts of the AST. By default it parses `test/index.js`, but nothing is fixed.
