---
title: Parser
description: Yuku's JavaScript/TypeScript parser
---

Yuku's parser turns JavaScript and TypeScript source code into an Abstract Syntax Tree (AST). It is a single-pass, recursive descent parser with full ECMAScript spec compliance and comprehensive error recovery.

## Installation

Add Yuku as a dependency using `zig fetch`:

```bash
zig fetch --save git+https://github.com/yuku-toolchain/yuku.git
```

This adds Yuku to your `build.zig.zon` dependencies. Then, in your `build.zig`, import the parser module:

```zig
const yuku_dep = b.dependency("yuku", .{
    .target = target,
    .optimize = optimize,
});

// Add the parser module to your own module
my_module.addImport("parser", yuku_dep.module("parser"));
```

Now you can `@import("parser")` anywhere in your Zig code.

:::note
Yuku requires the latest Zig nightly build. It stays up to date with Zig's latest development version.
:::

## Quick Start

```zig
const std = @import("std");
const parser = @import("parser");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var tree = try parser.parse(allocator, "const x = 5;", .{});
    defer tree.deinit(); // frees everything at once

    if (tree.hasErrors()) {
        for (tree.diagnostics.items) |d| {
            std.debug.print("{s}\n", .{d.message});
        }
    }
}
```

## Options

The parser accepts two options:

### `lang`

Determines which syntax features are enabled.

| Value | Description |
|-------|-------------|
| `.js` | Plain JavaScript (default) |
| `.jsx` | JavaScript with JSX |
| `.ts` | TypeScript |
| `.tsx` | TypeScript with JSX |
| `.dts` | TypeScript declaration files |

### `source_type`

Determines how the code is parsed and evaluated, following the [ECMAScript source type specification](https://tc39.es/ecma262/#sec-types-of-source-code).

| Value | Description |
|-------|-------------|
| `.module` | ES module semantics with strict mode enabled (default) |
| `.script` | Classic script semantics with sloppy mode |

## The `Tree`

`parser.parse()` returns a `Tree`. It's the AST, backed by an arena allocator. All memory (nodes, diagnostics, scopes, symbols, strings) lives in the arena, and `deinit()` frees everything at once.

```zig
var tree = try parser.parse(allocator, source, .{});
defer tree.deinit();
```

The tree is readable immediately after parsing. It can also be enriched with semantic analysis or transforms:

```zig
var tree = try parser.parse(allocator, source, .{});
defer tree.deinit();

// Semantic analysis appends diagnostics and builds scope/symbol data
const result = try parser.semantic.analyze(&tree);

// All diagnostics (parse + semantic) in one place
for (tree.diagnostics.items) |d| {
    std.debug.print("{s}\n", .{d.message});
}
```

### Reading Nodes

```zig
const data = tree.getData(some_node_index);
const span = tree.getSpan(some_node_index);
const children = tree.getExtra(some_index_range);
const text = tree.getString(some_string_id);
```

### Checking for Errors

```zig
if (tree.hasErrors()) { ... }
if (tree.hasDiagnostics()) { ... }

for (tree.diagnostics.items) |d| {
    // d.severity  -- .error, .warning, .hint, .info
    // d.message   -- human-readable description
    // d.span      -- { .start, .end } byte offsets
    // d.help      -- optional suggestion for fixing
    // d.labels    -- additional labeled spans for context
}
```

### Mutation Methods

For transforms and programmatic AST construction:

| Method | Description |
|--------|-------------|
| `replaceData(index, data)` | Replace a node's data in place |
| `replaceSpan(index, span)` | Replace a node's source span |
| `createNode(data, span)` | Append a new node, returns its `NodeIndex` |
| `createExtra(children)` | Allocate a child list, returns an `IndexRange` |
| `addString(str)` | Add a new string to the pool, returns a `StringId` |
| `appendDiagnostic(diag)` | Add a diagnostic (error, warning, etc.) |

:::tip[Traversal and AST Construction]
Yuku provides a **traverser system** with four modes:

| Mode | Description | Result |
|------|-------------|--------|
| **Basic** | Walk with parent and ancestor access. | |
| **Scoped** | Automatic lexical scope tracking. | `ScopeTree` |
| **Semantic** | Full scope and symbol tracking. | `ScopeTree` + `SymbolTable` |
| **Transform** | Mutate the AST during traversal. | |

Read-only traversers (basic, scoped, semantic) access the tree through `*const Tree`, so they cannot accidentally mutate the AST. The transform traverser gets `*Tree` for full mutation access.

See the [Traverse documentation](/parser/traverse) for the complete guide.
:::

### JSON Serialization

```zig
const json = try parser.estree.toJSON(&tree, allocator, .{});
defer allocator.free(json);
```

## Error Recovery

The parser does not stop at the first error. When it encounters invalid syntax, it reports a diagnostic and continues parsing to find as many issues as possible in a single pass. You can parse a file with syntax errors and still get a usable AST for the valid portions.
