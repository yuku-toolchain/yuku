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

const semantic = parser.semantic;

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Parse source into a TreeBuilder
    var tree = try parser.parse(allocator, "const x = 5;", .{});

    // Run semantic analysis (appends errors to tree, builds scope/symbol data)
    const result = try semantic.analyze(&tree);
    _ = result; // result.scope_tree, result.symbol_table

    // Finalize into an immutable ParseTree
    var pt = tree.finalize();
    defer pt.deinit(); // frees everything: AST, diagnostics, scopes, symbols

    // Check for errors (parse + semantic errors are unified)
    for (pt.diagnostics) |d| {
        std.debug.print("{s}\n", .{d.message});
    }
}
```

If you only need to parse without semantic analysis or transforms, use `parseTree()` as a shorthand:

```zig
var tree = try parser.parseTree(allocator, "const x = 5;", .{});
defer tree.deinit();
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

## How It Works

The parser pipeline has three phases:

```
source  ->  parser.parse()  ->  TreeBuilder  ->  .finalize()  ->  ParseTree
                                     |                            |
                              analyze / transform           finalized, immutable
                              append diagnostics            single deinit() frees all
                              allocate on arena
```

**`parser.parse()`** parses source into a `TreeBuilder`. The tree is complete and readable, but still mutable: you can run semantic analysis (which appends diagnostics and builds scope/symbol data on the tree's arena), run transforms (which mutate nodes), or do both.

**`.finalize()`** finalizes the builder into an immutable `ParseTree`. After this, the builder should not be used. The `ParseTree` owns all the memory (AST nodes, diagnostics, scopes, symbols, strings) and `deinit()` frees everything at once.

### Memory Model

All memory lives in an `ArenaAllocator`. The allocator you pass to `parse()` is the backing allocator for that arena. Everything the parser, semantic analysis, and transforms allocate goes into this arena: nodes, diagnostics, scope data, symbol data, strings.

When you call `deinit()` on the `ParseTree` (or `TreeBuilder` if you skip finalization), the entire arena is freed in one operation. No individual frees needed.

## The `TreeBuilder`

`parser.parse()` returns a `TreeBuilder`. It supports all read methods (`getData`, `getSpan`, `getExtra`, `getString`), plus mutation and diagnostic methods:

| Method | Description |
|--------|-------------|
| `getData(index)` | Read a node's data |
| `getSpan(index)` | Read a node's source span |
| `getExtra(range)` | Read a child list |
| `getString(id)` | Resolve a `StringId` to text |
| `replaceData(index, data)` | Replace a node's data in place |
| `replaceSpan(index, span)` | Replace a node's source span |
| `createNode(data, span)` | Append a new node, returns its `NodeIndex` |
| `createExtra(children)` | Allocate a child list, returns an `IndexRange` |
| `addString(str)` | Add a new string to the pool, returns a `StringId` |
| `appendDiagnostic(diag)` | Add a diagnostic (error, warning, etc.) |
| `finalize()` | Finalize into an immutable `ParseTree` |
| `deinit()` | Free all memory without finalizing |

Call `finalize()` to finalize into an immutable `ParseTree`. The builder should not be used after this.

## The `ParseTree`

The immutable result of `finalize()` (or `parser.parseTree()` for the shorthand). All data is finalized into compact slices.

| Field | Type | Description |
|-------|------|-------------|
| `program` | `NodeIndex` | Root node (always a `Program`) |
| `nodes` | `NodeList.Slice` | All AST nodes |
| `extra` | `[]const NodeIndex` | Extra data for variadic children |
| `diagnostics` | `[]const Diagnostic` | All errors and warnings (parse + semantic) |
| `comments` | `[]const Comment` | All comments found in the source |
| `strings` | `StringPool` | All strings. Access `strings.source` for the original source text. |
| `source_type` | `SourceType` | Whether parsed as module or script |
| `lang` | `Lang` | Language variant used |

### Reading Nodes

```zig
const data = tree.getData(some_node_index);
const span = tree.getSpan(some_node_index);
const children = tree.getExtra(some_index_range);
const text = tree.getString(some_string_id);
```

### Checking for Errors

```zig
// Does the tree have any errors?
if (tree.hasErrors()) { ... }

// Does it have any diagnostics at all (errors, warnings, hints)?
if (tree.hasDiagnostics()) { ... }

// Iterate diagnostics
for (tree.diagnostics) |d| {
    // d.severity  -- .error, .warning, .hint, .info
    // d.message   -- human-readable description
    // d.span      -- { .start, .end } byte offsets
    // d.help      -- optional suggestion for fixing
    // d.labels    -- additional labeled spans for context
}
```

:::tip[Traversal and AST Construction]
Yuku provides a **traverser system** with four modes for walking, analyzing, and transforming the AST:

| Mode | Description | Result |
|------|-------------|--------|
| **Basic** | Walk with parent and ancestor access. No allocator needed. | |
| **Scoped** | Automatic lexical scope tracking. Query scopes, walk ancestors, check strict mode, and more. | `ScopeTree` |
| **Semantic** | Full scope and symbol tracking. Iterate declarations, resolve bindings, navigate the scope-symbol hierarchy. | `ScopeTree` + `SymbolTable` |
| **Transform** | Mutate the AST during traversal. Replace nodes, create new ones, add strings, and restructure the tree safely. | |

All traversers operate on a `TreeBuilder`. Read-only traversers (basic, scoped, semantic) access the tree through `*const TreeBuilder`, so they cannot accidentally mutate the AST. The transform traverser gets `*TreeBuilder` for full mutation access.

See the [Traverse documentation](/parser/traverse) for the complete guide.
:::

### JSON Serialization

To serialize the AST to JSON (ESTree format), useful for inspection and testing:

```zig
const json = try parser.estree.toJSON(&tree, allocator, .{});
defer allocator.free(json);

std.debug.print("{s}\n", .{json});
```

:::note
The visitor/traverser API (see [Traverse](/parser/traverse)) is the recommended way to work with the AST programmatically. JSON serialization is available for quick testing and interoperability.
:::

## Error Recovery

The parser does not stop at the first error. When it encounters invalid syntax, it reports a diagnostic and continues parsing to find as many issues as possible in a single pass.

This means you can parse a file with syntax errors and still get a usable AST for the valid portions, along with all the errors collected in `tree.diagnostics`.
