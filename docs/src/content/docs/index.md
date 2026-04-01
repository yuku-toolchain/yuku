---
title: Yuku — JavaScript/TypeScript Toolchain in Zig
description: Yuku is a high-performance JavaScript and TypeScript parser and toolchain written in Zig. Spec-compliant, zero dependencies, fast by design.
head:
  - tag: style
    content: |
      .content-panel:has(h1#_top:not([data-page-title])) {
          display: none;
      }
---

<div align="center">

<h1 style="font-size: 5rem;margin-bottom: 2rem;color: var(--sl-color-accent);">yuku</h1>

A high-performance JavaScript/TypeScript parser and toolchain written in Zig, bringing modern JavaScript tooling infrastructure to the Zig ecosystem.

</div>

## Why Yuku

Yuku is a JavaScript/TypeScript toolchain built from the ground up in Zig. It is designed for correctness, performance, and clarity.

**Correctness first.** Yuku is 100% ECMAScript spec compliant. It passes all 45,000+ tests from [Test262](https://github.com/tc39/test262) with full AST matching, covering every edge case in the specification. Zero failures, zero AST mismatches. See the [test results](https://github.com/yuku-toolchain/yuku/tree/main/test/results).

**Fast by design.** The parser is built using data-oriented design principles and generous performance engineering.

<div align="center" style="margin: 1.5rem 0;">

![Benchmark: Parsing TypeScript source to JavaScript](https://raw.githubusercontent.com/yuku-toolchain/parser-benchmark/refs/heads/main/charts/typescript.png)

<span style="font-size: 0.8rem; color: var(--sl-color-gray-3);">Parsing the bundled TypeScript compiler source (7.8 MB) · macOS (ARM) | Apple M4 Pro · [Source](https://github.com/yuku-toolchain/parser-benchmark)</span>

</div>

**Pure Zig, zero dependencies.** The entire toolchain is written in Zig with no external C libraries or runtime dependencies. This makes it easy to build, embed, and cross-compile.

**Modern JavaScript.** Full support for modern and experimental features including decorators, source phase imports, deferred imports, `using`/`await using` declarations, and more.

## Current Status

Yuku currently provides a 100% spec-compliant, reliable, and fast JavaScript/JSX parser ready for use in Zig projects, along with a [Traverser](/parser/traverse). TypeScript parsing support is actively in development.

More tooling is planned (a resolver, transpiler including `.d.ts` generation, minifier, and more), built incrementally, one component at a time. Contributions are welcome and will help accelerate development.

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

var tree = try parser.parse(allocator, "const x = 5;", .{});
defer tree.deinit();
```

See the [Parser documentation](/parser) for the full guide.
