# Contributing to Yuku

Yuku is pure Zig with no external dependencies.

## Prerequisites

- [Zig](https://ziglang.org/) **nightly/development build** (build + compile). Yuku always keeps up to date with the latest Zig development version.
- [Bun](https://bun.sh/) (test runner, workspace dependencies)

## Setup

```bash
git clone https://github.com/yuku-toolchain/yuku.git
cd yuku
bun install
zig build
```

## Testing

Run the full test suite (55,000+ files from Test262, Babel, TypeScript and others) with AST matching:

```bash
bun run test
```

The first run will download the test suite (wait for it to finish). After the run completes, check `test/results` for results.

## Playground

`src/main.zig` is a scratch space for quickly testing the toolchain from Zig:

```bash
zig build run
```

Edit it however you want. Change the source, swap parse options, log different parts of the tree, etc.

For a fast edit-run loop, use Zig's watch mode with incremental compilation:

```bash
zig build run --watch -fincremental
```

Prefer a browser-based playground? Run `bun run playground` to build and serve it locally.
