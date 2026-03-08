---
title: Traverse
description: Visiting and traversing the AST
---

:::caution
The traverser API is under active development and may change as new features are added.
:::

Yuku provides a traversal system for walking the AST and reacting to nodes. It is built on three layers:

1. **Walk engine** (`traverser.walk`): the generic depth-first walker that drives everything.
2. **Contexts**: pluggable state that the walker threads through every node. Two are built-in: `BasicCtx` (lightweight) and `ScopedCtx` (scope-aware). You can also build your own.
3. **Visitors**: your code. You declare typed hooks that fire at the nodes you care about.

No code generation, no macros, no runtime dispatch. Zig's comptime resolves all hook lookups at compile time, producing a walker that only contains the code paths you actually use. Hooks that don't exist are eliminated entirely, they cost nothing.

## Quick Start

A minimal example that counts all functions in a source file using scoped traversal:

```zig
const parser = @import("parser");
const ast = parser.ast;
const traverser = parser.traverser;

// Parse source code
const tree = try parser.parse(allocator, source, .{
    .lang = .javascript,
    .source_type = .module,
});
defer tree.deinit();

const scoped = traverser.scoped;

// Define a visitor
const Visitor = struct {
    count: u32 = 0,

    pub fn enter_function(self: *@This(), _: ast.Function, _: ast.NodeIndex, _: *scoped.ScopedCtx) traverser.Action {
        self.count += 1;
        return .proceed;
    }
};

var visitor = Visitor{};
const scope_tree = try scoped.traverse(Visitor, &tree, &visitor, allocator);

std.debug.print("found {} functions\n", .{visitor.count});
```

## Visitor Hooks

A visitor is any struct with `enter_` and/or `exit_` methods named after AST node types. The naming convention is:

```
enter_{node_type}   called before a node's children are walked
exit_{node_type}    called after a node's children are walked
```

For example, `enter_variable_declaration` fires when entering a `variable_declaration` node. The names must exactly match the tags in `ast.NodeData`. If you misspell one, you get a compile error, not a silent bug:

```zig
pub fn enter_functoin(...) { ... }  // typo
```

```
error: Invalid visitor hook 'enter_functoin': no field 'functoin' exists in ast.NodeData
```

To see all available node types, check the [AST page](/parser/ast/#node-types).

### Hook Signatures

Enter hooks receive the node payload, the node index, and the context, and return an `Action`:

```zig
pub fn enter_function(self: *@This(), node: ast.Function, index: ast.NodeIndex, ctx: *ScopedCtx) traverser.Action {
    // ...
    return .proceed;
}
```

Exit hooks have the same parameters but return nothing:

```zig
pub fn exit_function(self: *@This(), node: ast.Function, index: ast.NodeIndex, ctx: *ScopedCtx) void {
    // ...
}
```

The payload type (`ast.Function` above) is checked at compile time. If it doesn't match the node type, you get a compile error telling you the expected type:

```zig
pub fn enter_function(self: *@This(), node: ast.CallExpression, ...) { ... }
```

```
error: Visitor hook 'enter_function': expected payload type 'ast.Function', found 'ast.CallExpression'
```

### Catch-All Hooks

Two special hooks fire for every node:

```zig
pub fn enter_node(self: *@This(), data: ast.NodeData, index: ast.NodeIndex, ctx: *C) traverser.Action {
    // called before any typed enter hook
}

pub fn exit_node(self: *@This(), data: ast.NodeData, index: ast.NodeIndex, ctx: *C) void {
    // called after any typed exit hook
}
```

These receive the full `NodeData` tagged union instead of an unwrapped payload. When both `enter_node` and a typed hook exist, `enter_node` fires first. If it returns `.skip` or `.stop`, the typed hook is not called.

### Actions

Enter hooks return an `Action` to control traversal flow:

| Action | Effect |
|--------|--------|
| `.proceed` | Continue into this node's children (default). |
| `.skip` | Skip this node's children, continue to the next sibling. |
| `.stop` | Stop the entire traversal immediately. |

## Traversal Modes

Two built-in traversal modes are provided. Choose basic for lightweight walks, or scoped when you need scope information.

### Basic Traversal

The simplest way to walk an AST. No scope tracking, no allocator needed:

```zig
const basic = parser.traverser.basic;

const Counter = struct {
    calls: u32 = 0,

    pub fn enter_call_expression(self: *@This(), _: ast.CallExpression, _: ast.NodeIndex, _: *basic.BasicCtx) traverser.Action {
        self.calls += 1;
        return .proceed;
    }
};

var counter = Counter{};
try basic.traverse(Counter, &tree, &counter);
```

`BasicCtx` provides:

| Field / Method | Description |
|----------------|-------------|
| `ctx.tree` | Pointer to the `ParseTree`. Access any node via `ctx.tree.getData(index)`. |
| `ctx.path` | `NodePath` tracking ancestors from root to the current node. |

### Scoped Traversal

Extends basic traversal with automatic scope tracking. Every time the walker enters a scope-creating node (function, block, class, etc.), a new scope is pushed and popped on exit. Your visitor receives a `ScopedCtx` with full scope information at every node:

```zig
const scoped = parser.traverser.scoped;

const StrictChecker = struct {
    pub fn enter_identifier_reference(_: *@This(), id: ast.IdentifierReference, _: ast.NodeIndex, ctx: *scoped.ScopedCtx) traverser.Action {
        if (ctx.isStrict()) {
            const name = ctx.tree.getSourceText(id.name_start, id.name_len);
            // check strict mode rules...
        }
        return .proceed;
    }
};

var checker = StrictChecker{};
const scope_tree = try scoped.traverse(StrictChecker, &tree, &checker, allocator);
```

`scoped.traverse` returns a `ScopeTree`, the immutable result of all scopes discovered during the walk. You can use it after traversal for downstream analysis.

## Navigating the Tree

Inside any visitor hook, you have full access to the parse tree in every direction: up to parents, down to children, and sideways to siblings.

### Accessing Node Data

From any hook, `ctx.tree` gives you the full parse tree. You can read any node by index:

```zig
pub fn enter_call_expression(self: *@This(), call: ast.CallExpression, index: ast.NodeIndex, ctx: *C) traverser.Action {
    // read the callee node
    const callee_data = ctx.tree.getData(call.callee);

    // read variable-length children
    const args = ctx.tree.getExtra(call.arguments);
    for (args) |arg| {
        const arg_data = ctx.tree.getData(arg);
        // ...
    }

    // get source location
    const span = ctx.tree.getSpan(index);
    // span.start, span.end are byte offsets

    return .proceed;
}
```

### Walking Up: Parent and Ancestor Access

Both `BasicCtx` and `ScopedCtx` include a `path` field that tracks the node path from the root to the current position:

```zig
pub fn enter_identifier_reference(self: *@This(), id: ast.IdentifierReference, index: ast.NodeIndex, ctx: *C) traverser.Action {
    // immediate parent
    if (ctx.path.parent()) |parent_index| {
        const parent_data = ctx.tree.getData(parent_index);
        switch (parent_data) {
            .member_expression => |m| {
                // this identifier is part of a member expression
            },
            else => {},
        }
    }

    // grandparent
    if (ctx.path.ancestor(2)) |gp_index| {
        // ...
    }

    // current nesting depth
    const depth = ctx.path.depth();

    return .proceed;
}
```

| Method | Description |
|--------|-------------|
| `path.parent()` | Returns the parent `NodeIndex`, or `null` at the root. |
| `path.ancestor(n)` | Returns the ancestor `n` levels up (0 = self, 1 = parent, 2 = grandparent). |
| `path.depth()` | Current nesting depth from the root. |

### Walking Down and Sideways

There is no explicit "next sibling" API because you don't need one. To inspect siblings, read the parent's children:

```zig
if (ctx.path.parent()) |parent_idx| {
    const parent = ctx.tree.getData(parent_idx);
    switch (parent) {
        .block_statement => |block| {
            const stmts = ctx.tree.getExtra(block.body);
            // stmts contains all siblings, including the current node
        },
        else => {},
    }
}
```

You already have full downward access through each node's fields. `getData` lets you read any child at any depth.

## Scope Tracking

The scoped traverser automatically tracks JavaScript's lexical scope rules as it walks the AST. Scopes are pushed on enter and popped on exit, giving you accurate scope information at every node.

### Scope Kinds

The scoped traverser recognizes these scope-creating constructs:

| Kind | Created by | `var` hoists here? |
|------|-----------|-------------------|
| `global` | Program root | Yes |
| `module` | ES module (pushed automatically for `.module` source type) | Yes |
| `function_params` | Function / arrow function parameter list | No |
| `function_body` | Function / arrow function body | Yes |
| `block` | Block statements, `for`, `for-in`, `for-of`, `catch`, `switch` | No |
| `class` | Class declaration / expression | No |
| `static_block` | `static { ... }` | Yes |

### ScopedCtx API

These methods are available on the `ScopedCtx` passed to your visitor hooks:

| Method | Returns | Description |
|--------|---------|-------------|
| `currentScopeId()` | `ScopeId` | ID of the currently active scope. |
| `currentScope()` | `Scope` | The currently active scope (by value). |
| `currentScopePtr()` | `*Scope` | Mutable pointer to the current scope. |
| `currentHoistScopeId()` | `ScopeId` | ID of the nearest scope where `var` declarations hoist to. O(1) lookup. |
| `getScope(id)` | `Scope` | Look up any scope by ID. |
| `getScopePtr(id)` | `*Scope` | Mutable pointer to any scope by ID. |
| `isStrict()` | `bool` | Whether the current scope is in strict mode. |
| `ancestors(start)` | `AncestorIterator` | Iterate ancestor scopes from `start` up to the root. |
| `toScopeTree()` | `ScopeTree` | Snapshot the scope tree (call after traversal). |

### The Scope Struct

Each scope entry contains the following fields:

```zig
pub const Scope = struct {
    /// The AST node that introduced this scope.
    node: ast.NodeIndex,
    /// The enclosing parent scope, or `.none` for the root.
    parent: ScopeId,
    /// Nearest ancestor (or self) where `var` declarations hoist to.
    hoist_target: ScopeId,
    /// What kind of syntactic construct created this scope.
    kind: Kind,
    /// Inherited and local flags (e.g. strict mode).
    flags: Flags,
};
```

The `hoist_target` is precomputed when each scope is created, so looking up where a `var` hoists to is always O(1) with no need to walk up the scope chain.

### Strict Mode Detection

`"use strict"` directives are automatically detected. When one is encountered, the current scope's `strict` flag is set, and all child scopes inherit it:

```zig
pub fn enter_variable_declaration(self: *@This(), decl: ast.VariableDeclaration, _: ast.NodeIndex, ctx: *scoped.ScopedCtx) traverser.Action {
    if (ctx.isStrict() and decl.kind == .var) {
        // var in strict mode...
    }
    return .proceed;
}
```

ES modules are strict by default. The module scope is created with `strict: true`.

### ScopeTree: Post-Traversal Access

`scoped.traverse` returns a `ScopeTree`, which is the immutable result of all scopes discovered during the walk. Use it after traversal for downstream consumers like linters, bundlers, or code generators:

```zig
const scope_tree = try scoped.traverse(MyVisitor, &tree, &visitor, allocator);

// look up any scope
const scope = scope_tree.getScope(some_scope_id);

// walk ancestors
var it = scope_tree.ancestors(some_scope_id);
while (it.next()) |id| {
    const s = scope_tree.getScope(id);
    // ...
}

// each scope stores the AST node that created it,
// so you can read its children directly from the tree
const func_scope = scope_tree.getScope(some_scope_id);
const node_data = tree.getData(func_scope.node);
switch (node_data) {
    .function => |func| {
        const body_data = tree.getData(func.body);
        // access all statements in the function body...
    },
    else => {},
}
```

The `ScopeTree` is a flat array of `Scope` structs linked by parent pointers. No tree allocation, just a slice. Each scope stores the `node` that created it, so you always have a direct link back to the AST to read that scope's contents.

### Var Hoisting Example

Shows how a symbol table builder can use `currentHoistScopeId()` to resolve where `var` declarations land:

```zig
const SymbolCollector = struct {
    pub fn enter_variable_declaration(self: *@This(), decl: ast.VariableDeclaration, _: ast.NodeIndex, ctx: *scoped.ScopedCtx) traverser.Action {
        const target_scope = switch (decl.kind) {
            .var => ctx.currentHoistScopeId(),  // hoists to function/global
            else => ctx.currentScopeId(),        // let/const stay in block
        };
        // register declaration in target_scope...
        return .proceed;
    }
};
```

## Custom Contexts

The built-in `BasicCtx` and `ScopedCtx` cover most use cases, but you can create your own context type and use the walk engine directly. A context is any struct with a `tree` field:

```zig
const MyCtx = struct {
    tree: *const ast.ParseTree,
    path: traverser.NodePath = .{},  // opt-in: add this field to enable parent tracking
    my_data: MyAnalysisState,

    // Optional hooks called by the walker at every node:
    pub fn onEnter(self: *MyCtx, index: ast.NodeIndex, tag: NodeTag) !void {
        // called after visitor enter hooks, before walking children
    }

    pub fn onExit(self: *MyCtx, index: ast.NodeIndex, tag: NodeTag) void {
        // called after visitor exit hooks
    }
};
```

Then drive the walk yourself:

```zig
var ctx = MyCtx{ .tree = &tree, .my_data = .{} };
try traverser.walk(MyCtx, MyVisitor, &visitor, &ctx);
```

### Context Contract

The walker detects these fields and declarations at comptime. Only `tree` is required; everything else is opt-in.

| Requirement | Description |
|-------------|-------------|
| `tree: *const ast.ParseTree` | **Required.** The walker reads the AST through this field. |
| `path: NodePath` | **Optional.** If present, the walker automatically pushes/pops node indices as it walks, enabling `path.parent()` and `path.ancestor(n)`. |
| `onEnter(self, index, tag) !void` | **Optional.** Called when entering a node, after visitor enter hooks. May return an error (e.g. `Allocator.Error`). |
| `onExit(self, index, tag) void` | **Optional.** Called when exiting a node, after visitor exit hooks. |

All optional fields are detected via `@hasField` / `@hasDecl` at comptime. If they don't exist, the walker generates no code for them.
