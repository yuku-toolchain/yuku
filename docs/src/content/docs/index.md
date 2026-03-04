---
title: Yuku
description: High-performance JavaScript/TypeScript toolchain in Zig
head:
  - tag: style
    content: |
      .content-panel:has(h1#_top:not([data-page-title])) {
          display: none;
      }
---

<div align="center">

<h1 style="font-size: 5rem;margin-bottom: 2rem;color: var(--sl-color-accent);">yuku</h1>

A high-performance JavaScript/TypeScript toolchain written in Zig, bringing modern JavaScript tooling infrastructure to the Zig ecosystem.

<br />

</div>

:::note
**Early stage, under active development.** The JavaScript and JSX parser is complete with full spec compliance and thorough testing. TypeScript parsing is supported. The visitor/traverser API is functional and actively being improved. See the [Roadmap](#roadmap) for details.
:::

## Why Yuku

Yuku is a JavaScript/TypeScript toolchain built from the ground up in Zig. It is designed for correctness, performance, and clarity.

**Correctness first.** Yuku is 100% ECMAScript spec compliant. It passes all 45,000+ tests from [Test262](https://github.com/tc39/test262) with full AST matching, covering every edge case in the specification. Zero failures, zero AST mismatches.

**Fast by design.** The parser is built using data-oriented design principles and generous performance engineering. It is competitive with leading parsers like [Oxc](https://oxc.rs). [See benchmarks](https://github.com/yuku-toolchain/parser-benchmark).

**Pure Zig, zero dependencies.** The entire toolchain is written in Zig with no external C libraries or runtime dependencies. This makes it easy to build, embed, and cross-compile.

**Modern JavaScript.** Full support for modern and experimental features including decorators, source phase imports, deferred imports, `using`/`await using` declarations, and more.

## What's included

| Component | Status |
|-----------|--------|
| [JavaScript/JSX Parser](/parser) | Complete |
| [AST](/parser/ast) | Complete |
| TypeScript Parsing | In Progress |
| [Visitor/Traverser](/parser/traverse) | Planned |
| Module Resolver | Planned |

## Quick start

Add Yuku as a dependency to your Zig project:

```sh
zig fetch --save git+https://github.com/yuku-toolchain/yuku.git
```

Then in your `build.zig`:

```zig
const yuku = b.dependency("yuku", .{
    .target = target,
    .optimize = optimize,
});

exe.root_module.addImport("parser", yuku.module("parser"));
```

Parse some JavaScript:

```zig
const parser = @import("parser");

const tree = try parser.parse(allocator, "const x = 5;", .{});
defer tree.deinit();
```

See the [Parser documentation](/parser) for the full guide.
