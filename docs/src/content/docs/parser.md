---
title: Parser
description: A fast, spec-compliant JavaScript and TypeScript parser written in Zig.
---

Yuku's parser turns JavaScript and TypeScript source code into an Abstract Syntax Tree (AST).

## Installation

```bash
zig fetch --save git+https://github.com/yuku-toolchain/yuku.git
```

In your `build.zig`:

```zig
const yuku_dep = b.dependency("yuku", .{
    .target = target,
    .optimize = optimize,
});
my_module.addImport("parser", yuku_dep.module("parser"));
```

:::note
Yuku requires the latest Zig nightly build.
:::

## Quick Start

```zig
const std = @import("std");
const parser = @import("parser");

pub fn main() !void {
    var tree = try parser.parse(std.heap.page_allocator, "const x = 5;", .{});
    defer tree.deinit();

    for (tree.diagnostics.items) |d| {
        std.debug.print("{s}\n", .{d.message});
    }
}
```
