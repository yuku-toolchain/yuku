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

## Options

### `lang`

| Value | Description |
|-------|-------------|
| `.js` | Plain JavaScript (default) |
| `.jsx` | JavaScript with JSX |
| `.ts` | TypeScript |
| `.tsx` | TypeScript with JSX |
| `.dts` | TypeScript declaration files |

### `source_type`

| Value | Description |
|-------|-------------|
| `.module` | ES module semantics, strict mode (default) |
| `.script` | Classic script, sloppy mode |

## The Tree

`parser.parse()` returns a `Tree`. One type, one `deinit()`. Everything lives in the tree's arena: AST nodes, diagnostics, scope data, symbol data, strings. When you call `deinit()`, all of it is freed at once.

```zig
var tree = try parser.parse(allocator, source, .{});
defer tree.deinit();
```

### Reading

```zig
const data = tree.getData(node_index);     // node's data (tagged union)
const span = tree.getSpan(node_index);     // source location { .start, .end }
const children = tree.getExtra(range);     // child list for IndexRange fields
const text = tree.getString(string_id);    // resolve a StringId to text
```

### Diagnostics

```zig
if (tree.hasErrors()) { ... }

for (tree.diagnostics.items) |d| {
    // d.severity  .error, .warning, .hint, .info
    // d.message   human-readable description
    // d.span      { .start, .end } byte offsets
    // d.help      optional fix suggestion
    // d.labels    additional labeled spans
}
```

### JSON Serialization

```zig
const json = try parser.estree.toJSON(&tree, allocator, .{});
defer allocator.free(json);
```

Serializes to ESTree/typescript-estree compatible JSON. Useful for testing and interoperability.

## Semantic Analysis

`semantic.analyze()` runs semantic checks on the tree and builds scope and symbol data:

```zig
const semantic = parser.semantic;

var tree = try parser.parse(allocator, source, .{});
defer tree.deinit();

const result = try semantic.analyze(&tree);

// result.scope_tree   -- scope hierarchy
// result.symbol_table -- all symbols and references
```

Semantic diagnostics (like redeclaration errors) are appended directly to `tree.diagnostics` alongside parse errors. All allocations use the tree's arena, so `tree.deinit()` frees everything.

If you only need the scope tree and symbol table without semantic error checks, use the [semantic traverser](/parser/traverse#semantic-traverser) directly with your own visitor.

## Traversal

Yuku provides a traverser system with four modes for walking, analyzing, and transforming the AST. Every mode gives full context at every node: the tree, path from root, and any scope or symbol information accumulated so far.

See the [Traverse documentation](/parser/traverse) for the complete guide.

## Error Recovery

The parser does not stop at the first error. It reports a diagnostic and continues parsing to find as many issues as possible in a single pass. You always get a usable AST for the valid portions, plus all errors in `tree.diagnostics`.
