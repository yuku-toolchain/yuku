# Contributing to Yuku

Yuku is pure Zig with no external dependencies.

## Prerequisites

- [Zig](https://ziglang.org/) **0.16.0 or later** (build + compile). Yuku always keeps up to date with the latest stable Zig version.
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

## Documentation

The docs live in `docs/`, built with [Zine](https://zine-ssg.io). Pages are Markdown (`.smd`) files in `docs/content/` that you can edit directly. You only need Zine installed to preview changes locally.

To preview, download the Zine binary for your OS from the [releases page](https://github.com/kristoff-it/zine/releases) and put `zine` on your `PATH`, then run from `docs/`:

```bash
cd docs
zine            # preview at http://localhost:1990
zine release    # build into docs/public/
```
