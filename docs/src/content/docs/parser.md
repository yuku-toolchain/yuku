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

`parse` takes an `Options` struct to configure the parsing mode:

```zig
const tree = try parser.parse(allocator, source, .{
    .source_type = .module,
    .lang = .jsx,
});
```

| Field | Values | Default | Description |
|-------|--------|---------|-------------|
| `source_type` | `.script`, `.module` | `.module` | Script mode or ES module mode (strict mode) |
| `lang` | `.js`, `.ts`, `.jsx`, `.tsx`, `.dts` | `.js` | Language variant and syntax features to enable |

Both fields can be inferred from a file path:

```zig
const tree = try parser.parse(allocator, source, .{
    .source_type = parser.ast.SourceType.fromPath("app.cjs"), // .script
    .lang = parser.ast.Lang.fromPath("app.tsx"),               // .tsx
});
```

## The Tree

`parse` returns a `Tree` containing the full AST, diagnostics, and source metadata. All memory is owned by the tree's arena allocator. `tree.deinit()` frees everything at once.

```zig
var tree = try parser.parse(allocator, source, .{});
defer tree.deinit();

// read the root program node
const program = tree.getData(tree.program);

// read a node's source location
const span = tree.getSpan(tree.program);

// read string content from a node
const name = tree.getString(some_identifier.name);

// iterate variable-length children (e.g. program body)
for (tree.getExtra(program.program.body)) |child_index| {
    const child = tree.getData(child_index);
    // ...
}
```

See the [AST reference](/parser/ast) for the full node type catalog and memory model.

## Diagnostics

The parser recovers from errors and continues, so a single parse produces the full AST alongside all diagnostics:

```zig
for (tree.diagnostics.items) |d| {
    std.debug.print("[{s}] {s} at {d}..{d}\n", .{
        d.severity.toString(), d.message, d.span.start, d.span.end,
    });

    for (d.labels) |label| {
        std.debug.print("  {d}..{d}: {s}\n", .{ label.span.start, label.span.end, label.message });
    }

    if (d.help) |help| {
        std.debug.print("  help: {s}\n", .{help});
    }
}
```

Each diagnostic has a `severity` (`.error`, `.warning`, `.hint`, `.info`), a `message`, a source `span`, optional `labels` pointing to related code regions, and optional `help` text.

## Traversal

The [traverser system](/parser/traverse) walks the AST and calls visitor hooks at every node. There are four modes with increasing context:

| Mode | Context | Result |
|------|---------|--------|
| **Basic** | Path (parents, ancestors, depth) | |
| **Scoped** | Path + lexical scopes | `ScopeTree` |
| **Semantic** | Path + scopes + symbols/references | `ScopeTree` + `SymbolTable` |
| **Transform** | Path + mutable tree | |

```zig
const traverser = parser.traverser;

const MyVisitor = struct {
    pub fn enter_function(self: *MyVisitor, func: parser.ast.Function, index: parser.ast.NodeIndex, ctx: *traverser.basic.Ctx) traverser.Action {
        // called when entering any function node
        return .proceed;
    }
};

var visitor = MyVisitor{};
try traverser.basic.traverse(MyVisitor, &tree, &visitor);
```

See [Traverse](/parser/traverse) for the full API.

## Semantic Analysis

The ECMAScript specification defines a set of [early errors](https://tc39.es/ecma262/#early-error) that conformant implementations must report before execution. Some are detectable during parsing from local context alone: `return` outside a function, `yield` outside a generator, invalid destructuring. Others require knowledge of the program's scope structure and bindings: redeclarations, unresolved exports, private fields used outside their class, and more.

Yuku defers these scope-dependent checks to a separate semantic analysis pass. This keeps parsing fast and lets each consumer opt in only to the work it actually needs. A formatter, for example, only needs the AST and should not pay the cost of scope resolution.

`semantic.analyze` builds a scope tree and symbol table, resolves identifier references to their declarations, and reports the remaining early errors. Together, parsing and semantic analysis cover the full set of early errors required by the specification.

```zig
var tree = try parser.parse(allocator, source, .{});
defer tree.deinit();

// run semantic analysis
const result = try parser.semantic.analyze(&tree);

// result.scope_tree   - all lexical scopes
// result.symbol_table - all symbols and references
```

Semantic diagnostics are appended directly to `tree.diagnostics` alongside parse errors. After analysis, `tree.hasErrors()` reflects both.

All allocations (scope tree, symbol table) use the tree's arena, so they are valid for the lifetime of the tree and freed by `tree.deinit()`.

:::note
If you need the scope tree and symbol table without checking for semantic early errors, use the [semantic traverser](/parser/traverse#semantic-traverser) directly with an empty visitor (or your own visitor hooks). This is what `semantic.analyze` uses [internally](https://github.com/yuku-toolchain/yuku/blob/main/src/parser/semantic_checker.zig).

```zig
const sem = parser.traverser.semantic;

var visitor = struct {}{}; // empty visitor, no error checking
const result = try sem.traverse(@TypeOf(visitor), &tree, &visitor);

// result.scope_tree and result.symbol_table are ready to use
```
:::

## JSON Serialization

The AST can be serialized to [ESTree](https://github.com/estree/estree)-compatible JSON. JavaScript/JSX output matches [Acorn](https://www.npmjs.com/package/acorn). TypeScript output conforms to [TS-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree).

```zig
const json = try parser.estree.toJSON(&tree, allocator, .{});
defer allocator.free(json);

std.debug.print("{s}\n", .{json});
```

The output includes the program AST, all comments, and diagnostics. Source positions are converted to UTF-16 offsets for JavaScript interoperability.
