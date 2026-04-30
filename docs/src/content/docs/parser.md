---
title: Parser
description: A fast, spec-compliant JavaScript and TypeScript parser written in Zig.
---

Yuku's parser turns JavaScript and TypeScript source code into an [Abstract Syntax Tree](/parser/ast).

## Node.js

```bash
npm install yuku-parser
```

```js
import { parse } from "yuku-parser";

const { program, comments, diagnostics } = parse("const x = 1 + 2;");
```

Outputs an [ESTree](https://github.com/estree/estree) / [TypeScript-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree)-compatible AST matching [Oxc](https://oxc.rs), and runs 3–5x faster than alternatives on npm. See [yuku-parser on npm](https://www.npmjs.com/package/yuku-parser) for the full API.

## Zig

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
    // the page allocator is used as the backing allocator for the tree's internal arena
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

| Field                           | Values                               | Default   | Description                                     |
| ------------------------------- | ------------------------------------ | --------- | ----------------------------------------------- |
| `source_type`                   | `.script`, `.module`                 | `.module` | Script mode or ES module mode (strict mode)     |
| `lang`                          | `.js`, `.ts`, `.jsx`, `.tsx`, `.dts` | `.js`     | Language variant and syntax features to enable  |
| `preserve_parens`               | `true`, `false`                      | `true`    | Keep `ParenthesizedExpression` nodes in the AST |
| `allow_return_outside_function` | `true`, `false`                      | `false`   | Allow `return` statements at the top level      |

Both fields can be inferred from a file path:

```zig
const tree = try parser.parse(allocator, source, .{
    .source_type = .fromPath("app.cjs"), // .script
    .lang = .fromPath("app.tsx"),               // .tsx
});
```

## The Tree

`parse` returns a `Tree` containing the full AST, diagnostics, and source metadata. The allocator passed to `parse` is used as the backing allocator for the tree's internal arena, so `tree.deinit()` frees everything at once.

The AST is a flat array of nodes referenced by integer index. `tree.root` is always a `program` node, so unpack it directly. Everything below it is a tagged union you `switch` on:

```zig
var tree = try parser.parse(allocator, source, .{});
defer tree.deinit();

const program = tree.data(tree.root).program;

for (tree.extra(program.body)) |child_idx| {
    switch (tree.data(child_idx)) {
        .variable_declaration => |decl| {
            for (tree.extra(decl.declarators)) |d| {
                _ = d;
            }
        },
        .function => |func| {
            _ = func;
        },
        else => {},
    }
}
```

The four read primitives are `tree.data(idx)` for a node's typed payload, `tree.span(idx)` for its source range, `tree.extra(range)` for a variadic child list, and `tree.string(handle)` for string content. See the [AST reference](/parser/ast) for the full node catalog, the field conventions, and the seven categorical predicates on `NodeData`.

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

| Mode          | Context                            | Result                      |
| ------------- | ---------------------------------- | --------------------------- |
| **Basic**     | Path (parents, ancestors, depth)   |                             |
| **Scoped**    | Path + lexical scopes              | `ScopeTree`                 |
| **Semantic**  | Path + scopes + symbols/references | `ScopeTree` + `SymbolTable` |
| **Transform** | Path + mutable tree                |                             |

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
:::
