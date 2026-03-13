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

    const tree = try parser.parse(allocator, "const x = 5;", .{
        .lang = .js, // or .fromPath(file_path)
        .source_type = .module, // .fromPath(file_path)
    });
    
    defer tree.deinit();

    // Check for errors
    if (tree.hasErrors()) {
        for (tree.diagnostics) |d| {
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

## The `ParseTree`

Calling `parser.parse()` returns a `ParseTree`. This struct owns all the memory the parser allocated and provides access to every part of the result.

```zig
const tree = try parser.parse(allocator, source, .{});
defer tree.deinit(); // frees everything at once
```

| Field | Type | Description |
|-------|------|-------------|
| `program` | `NodeIndex` | Root node (always a `Program`) |
| `nodes` | `NodeList.Slice` | All AST nodes |
| `extra` | `[]const NodeIndex` | Extra data for variadic children (arrays of child nodes) |
| `diagnostics` | `[]const Diagnostic` | Errors and warnings from parsing |
| `comments` | `[]const Comment` | All comments found in the source |
| `strings` | `StringPool` | All strings referenced by AST nodes. Access `strings.source` for the original source text. |
| `source_type` | `SourceType` | Whether parsed as module or script |
| `lang` | `Lang` | Language variant used |

### Memory Model

The parser uses an `ArenaAllocator` internally. The allocator you pass to `parse()` is the backing allocator for that arena. All memory the parser allocates (nodes, diagnostics, comments, extra data) lives in this arena.

When you call `tree.deinit()`, the entire arena is freed in one operation. There is no need to free individual nodes or arrays.

### Reading Nodes

```zig
// Get the data (what kind of node, with its fields) for a node
const data = tree.getData(some_node_index);

// Get the source location of a node
const span = tree.getSpan(some_node_index);

// Get the children stored in an IndexRange
const children = tree.getExtra(some_index_range);

// Get string content from a StringId
const text = tree.getString(some_string_id);
```

## The `TreeBuilder`

If you need to modify the AST after parsing (e.g. with the transform traverser), use `parser.build()` instead of `parser.parse()`. It returns a mutable `TreeBuilder`:

```zig
var builder = try parser.build(allocator, source, .{});

// Run transforms on the mutable tree...
// var t = MyTransform{};
// try transform.traverse(MyTransform, &builder, &t);

// Convert to an immutable ParseTree when done
var tree = builder.toTree(.{});
defer tree.deinit();
```

The `TreeBuilder` supports all the same read methods as `ParseTree` (`getData`, `getSpan`, `getExtra`, `getString`), plus mutation methods:

| Method | Description |
|--------|-------------|
| `replaceData(index, data)` | Replace a node's data in place |
| `replaceSpan(index, span)` | Replace a node's source span |
| `createNode(data, span)` | Append a new node, returns its `NodeIndex` |
| `createExtra(children)` | Allocate a child list, returns an `IndexRange` |
| `addString(str)` | Add a new string to the pool, returns a `StringId` |

Call `builder.toTree(meta)` to finalize into an immutable `ParseTree`. The builder should not be used after this.

:::tip[Traversal and AST Construction]
Yuku provides a rich **traverser system** with four modes for walking, analyzing, and transforming the AST:

| Mode | Description | Result |
|------|-------------|--------|
| **Basic** | Walk with parent and ancestor access. No allocator needed. | |
| **Scoped** | Automatic lexical scope tracking. Query scopes, walk ancestors, check strict mode, and more. | `ScopeTree` |
| **Semantic** | Symbol and reference tracking on top of scopes. Iterate declarations, resolve bindings, navigate the scope-symbol hierarchy. | `ScopeTree` + `SymbolTable` |
| **Transform** | Operates on a mutable `TreeBuilder`. Replace nodes, create new ones, add strings, and restructure the tree during traversal safely. | |

Every mode gives you full context at every node: parents, ancestors, children, siblings, and any scope or symbol information accumulated so far. You can also use `TreeBuilder.initEmpty()` to build an AST from scratch, either standalone or from within a visitor hook on another tree with full access to the traversal context.

See the [Traverse documentation](/parser/traverse) for the complete guide.
:::

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
