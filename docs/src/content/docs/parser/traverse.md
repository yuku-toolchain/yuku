---
title: Traverse
description: Walk, analyze, and transform JavaScript and TypeScript ASTs with Yuku's traverser system.
---

Yuku's traverser system walks the AST and calls your visitor hooks at every node. There are four modes, each adding more context on top of the previous:

| Mode | Context | Result |
|------|---------|--------|
| **Basic** | Path (parents, ancestors, depth) | |
| **Scoped** | Path + lexical scopes | `ScopeTree` |
| **Semantic** | Path + scopes + symbols/references | `ScopeTree` + `SymbolTable` |
| **Transform** | Path + mutable tree | |

Every mode gives you the full tree, the current path from root, and whatever tracking that mode provides. Read-only traversers (basic, scoped, semantic) access the tree through `*const Tree`, so they cannot accidentally mutate the AST. The transform traverser gets `*Tree` for full mutation access.

## Visitor Hooks

A visitor is any struct with `enter_*` and `exit_*` methods. Hooks are named after node types:

```zig
const MyVisitor = struct {
    pub fn enter_function(self: *MyVisitor, func: ast.Function, index: ast.NodeIndex, ctx: *Ctx) traverser.Action {
        // Called when entering any function node
        return .proceed;
    }

    pub fn exit_function(self: *MyVisitor, func: ast.Function, index: ast.NodeIndex, ctx: *Ctx) void {
        // Called when exiting any function node
    }
};
```

The hook name must match a field in `ast.NodeData`. All hooks are validated at compile time: a misspelled hook name (e.g. `enter_funciton`) or a wrong payload type will produce a clear compile error. No silent mismatches.

### Catch-All Hooks

`enter_node` and `exit_node` fire for every node, regardless of type:

```zig
pub fn enter_node(self: *V, data: ast.NodeData, index: ast.NodeIndex, ctx: *Ctx) traverser.Action {
    // Called for every node
    return .proceed;
}
```

When both exist, the order is:
- **Enter**: `enter_node` fires first, then `enter_<type>`
- **Exit**: `exit_<type>` fires first, then `exit_node`

### Actions

Enter hooks return an `Action` to control traversal:

| Action | Effect |
|--------|--------|
| `.proceed` | Walk into this node's children |
| `.skip` | Skip children, move to next sibling |
| `.stop` | Stop the entire traversal |

Enter hooks can return either `Action` or `Allocator.Error!Action`. Both work. Exit hooks return `void`.

### Full Context

Every hook receives `ctx`, which gives you full access depending on the traverser mode:

```zig
// Always available:
ctx.tree          // the tree (*const Tree or *Tree in transform mode)
ctx.path          // path from root to current node

// In scoped and semantic modes:
ctx.scope         // scope tracker (current scope, flags, strict mode, ancestors)

// In semantic mode:
ctx.symbols       // symbol tracker (declarations, references, binding context)
```

### Reading Node Data in Hooks

Each hook receives its node's payload already unpacked as the second argument, so no switching is needed:

```zig
pub fn enter_binary_expression(
    self: *V,
    expr: ast.BinaryExpression, // payload, unpacked from NodeData
    index: ast.NodeIndex,
    ctx: *Ctx,
) traverser.Action {
    // expr.left, expr.right, expr.operator are ready to use directly
    return .proceed;
}
```

When you need to inspect a child node eagerly without waiting for the traverser to visit it, switch on `ctx.tree.getData`:

```zig
pub fn enter_call_expression(self: *V, call: ast.CallExpression, index: ast.NodeIndex, ctx: *Ctx) traverser.Action {
    switch (ctx.tree.getData(call.callee)) {
        .member_expression => |mem| {
            // callee is obj.method
        },
        .identifier_reference => |id| {
            const name = ctx.tree.getString(id.name);
            // callee is a plain identifier
        },
        else => {},
    }
    return .proceed;
}
```

See the [AST reference](/parser/ast) for all node types and their fields.

## Basic Traverser

The simplest mode. Tracks only the path from root to the current node.

```zig
const traverser = parser.traverser;
const basic = traverser.basic;

var visitor = MyVisitor{};
try basic.traverse(MyVisitor, &tree, &visitor);
```

No allocator needed. No result returned.

### Path Navigation

The path is a stack of `NodeIndex` values from root to the current node. Combined with the full tree reference, you can navigate in any direction: up to ancestors, down into children, or sideways to siblings.

```zig
pub fn enter_node(self: *V, data: ast.NodeData, index: ast.NodeIndex, ctx: *basic.Ctx) traverser.Action {
    // Navigate up
    const parent = ctx.path.parent();          // parent NodeIndex, or null at root
    const grandparent = ctx.path.ancestor(2);  // 0 = current, 1 = parent, 2 = grandparent
    const depth = ctx.path.depth();            // nesting depth (0 at root)

    // Walk from current node up to root
    var it = ctx.path.ancestors();
    while (it.next()) |ancestor_index| {
        const ancestor_data = ctx.tree.getData(ancestor_index);
        // ...
    }

    // Navigate sideways or down (the full tree is always accessible)
    if (ctx.path.parent()) |p| {
        const parent_data = ctx.tree.getData(p);
        // inspect siblings via parent's children, or descend into any subtree
    }

    return .proceed;
}
```

## Scoped Traverser

Adds automatic lexical scope tracking on top of path navigation.

```zig
const scoped = traverser.scoped;

var visitor = MyVisitor{};
const scope_tree = try scoped.traverse(MyVisitor, &tree, &visitor);
```

Returns a `ScopeTree` containing all scopes created during the walk.

### Scope Tracking

The scope tracker automatically pushes and pops scopes as the walker enters and exits scope-creating nodes:

| Node | Scope Kind |
|------|------------|
| Program | `global` (+ `module` if source_type is module) |
| Function declaration/expression | `function` |
| Arrow function | `function` |
| Block statement | `block` |
| For / for-in / for-of | `block` |
| Catch clause | `block` |
| Switch statement | `block` |
| Class declaration/expression | `class` |
| Static block | `static_block` |
| Named function/class expression | `expression_name` + `function`/`class` |

Named function and class expressions create two scopes. For `const x = function foo() { ... }`:

```
outer scope         (x lives here)
  expression_name   (foo lives here, immutable binding)
    function scope  (body bindings live here)
```

This is per ECMAScript spec (Section 15.2.5 and 15.7.14). Without this, `const foo = 1` inside the body would conflict with the expression name.

### Querying Scopes in Hooks

```zig
pub fn enter_node(self: *V, data: ast.NodeData, index: ast.NodeIndex, ctx: *scoped.Ctx) traverser.Action {
    const scope_id = ctx.scope.currentScopeId();
    const scope = ctx.scope.currentScope();

    // Check strict mode
    if (ctx.scope.isStrict()) { ... }

    // Get the scope kind
    if (scope.kind == .function) { ... }

    // Where var declarations would hoist to
    const hoist_target = ctx.scope.currentHoistScopeId();

    // Walk up scope ancestors
    var it = ctx.scope.ancestors(scope_id);
    while (it.next()) |ancestor_id| {
        const ancestor = ctx.scope.getScope(ancestor_id);
        // ...
    }

    return .proceed;
}
```

### Strict Mode

Strict mode is tracked automatically:
- Module scopes are always strict
- `"use strict"` directives set the flag on the current scope
- Child scopes inherit strict mode from their parent
- Functions with a `"use strict"` directive in their body are detected eagerly when the function scope is created, so the flag is correct when parameter hooks fire

### Using the ScopeTree

After traversal, the `ScopeTree` contains all scopes as an immutable slice:

```zig
const scope_tree = try scoped.traverse(MyVisitor, &tree, &visitor);

const root = scope_tree.getScope(.root);

// Walk ancestors from any scope
var it = scope_tree.ancestors(some_scope_id);
while (it.next()) |id| {
    const scope = scope_tree.getScope(id);
    // ...
}
```

## Semantic Traverser

The full-power mode. Tracks path, scopes, and symbols/references.

```zig
const sem = traverser.semantic;

var visitor = MyVisitor{};
const result = try sem.traverse(MyVisitor, &tree, &visitor);
// result.scope_tree   contains all scopes
// result.symbol_table contains all symbols and references
```

### Two-Phase Binding

The semantic traverser uses a two-phase approach for symbol declaration:

1. **Phase 1 (enter)**: When entering a `variable_declaration`, `function`, `class`, `import_declaration`, etc., the tracker records what kind of binding is coming next (`let`, `var`, `function`, etc.) and where it should land.

2. **Phase 2 (post_enter)**: After your enter hooks run but before children are walked, the tracker creates the actual symbol or reference for `binding_identifier` and `identifier_reference` nodes.

:::note
Your enter hooks see the scope state *before* the current node's bindings are declared. This lets you inspect what is about to be declared before it happens.
:::

```zig
pub fn enter_binding_identifier(
    self: *V,
    id: ast.BindingIdentifier,
    index: ast.NodeIndex,
    ctx: *sem.Ctx,
) !traverser.Action {
    // What kind of binding is this?
    const kind = ctx.symbols.currentBindingKind();
    // .lexical, .hoisted, .function, .class, .parameter, .import

    // Which scope will it land in?
    const target = ctx.symbols.resolveTargetScope(&ctx.scope);

    // Is there already a symbol with this name in that scope?
    const name = ctx.tree.getString(id.name);
    if (ctx.symbols.findInScope(target, name)) |existing_id| {
        const existing = ctx.symbols.getSymbol(existing_id);
        // Handle duplicate...
    }

    return .proceed;
}
```

### Symbol Flags

Each symbol carries flags:

```zig
symbol.flags.exported     // exported from module
symbol.flags.is_default   // default export
symbol.flags.is_const     // const or using binding
symbol.flags.is_ambient   // TypeScript declare
```

### Iterating Symbols in a Scope

```zig
// During traversal (on the tracker):
var it = ctx.symbols.scopeSymbols(scope_id);
while (it.next()) |sym_id_ptr| {
    const sym = ctx.symbols.getSymbol(sym_id_ptr.*);
    const name = ctx.symbols.getName(sym);
    // ...
}

// After traversal (on the table):
var it = result.symbol_table.scopeSymbols(scope_id);
while (it.next()) |sym_id_ptr| {
    const sym = result.symbol_table.getSymbol(sym_id_ptr.*);
    // ...
}
```

### References and Resolution

Every `identifier_reference` node creates a `Reference` with `resolved = .none`. After traversal, resolve all references by walking up scope chains:

```zig
const result = try sem.traverse(MyVisitor, &tree, &visitor);

// Resolve all references
var table = result.symbol_table;
table.resolveAll(result.scope_tree);

// Now check a specific reference
const ref = table.getReference(some_ref_id);
if (ref.resolved != .none) {
    const sym = table.getSymbol(ref.resolved);
    // ref refers to sym
}

// Or resolve manually
if (table.resolve(scope_id, "myVar", result.scope_tree)) |sym_id| {
    const sym = table.getSymbol(sym_id);
}
```

`resolve()` walks up from the given scope through all ancestor scopes until it finds a matching symbol, just like JavaScript's scope chain lookup. It also checks hoisted `var` declarations that pass through intermediate block scopes. Use `findInScopeOrHoisted()` for the same behavior when looking up names directly.

## Transform Traverser

The transform traverser gives your visitor a mutable `*Tree`, so you can modify the AST during traversal.

```zig
const transform = traverser.transform;

var visitor = MyTransform{};
try transform.traverse(MyTransform, &tree, &visitor);
```

### Replacing Nodes

Replace a node's data in its enter hook. The walker automatically re-reads the node after your hook returns, so the replacement's children are walked:

```zig
pub fn enter_binary_expression(
    _: *MyTransform,
    expr: ast.BinaryExpression,
    index: ast.NodeIndex,
    ctx: *transform.Ctx,
) traverser.Action {
    if (expr.operator == .add) {
        ctx.tree.replaceData(index, .{ .binary_expression = .{
            .left = expr.left,
            .right = expr.right,
            .operator = .multiply,
        } });
    }
    return .proceed;
}
```

### Creating New Nodes

Use `createNode` to append a new node and `createExtra` to allocate child lists:

```zig
// Create a new node
const new_node = try ctx.tree.createNode(
    .{ .numeric_literal = .{ .value = 42 } },
    .none,
);

// Create a child list for nodes with IndexRange fields
const children = try ctx.tree.createExtra(&.{ child1, child2, child3 });
```

### Wrapping Nodes

A common pattern is wrapping a node: copy the original to a new node, then replace the current node with a wrapper:

```zig
const span = ctx.tree.getSpan(index);

// Move original data to a new node
const inner = try ctx.tree.createNode(
    .{ .binary_expression = expr },
    span,
);

// Replace current node with wrapper
ctx.tree.replaceData(index, .{ .parenthesized_expression = .{
    .expression = inner,
} });

// Skip so the walker doesn't re-descend into the moved inner node
return .skip;
```

### Self-Reference Safety

:::caution
Never point a node's child back to its own index. The walker re-reads node data after every enter hook, so a self-referential node causes infinite recursion.
:::

```zig
// WRONG: creates a cycle
const wrapper = try ctx.tree.createNode(.{ .parenthesized_expression = .{ .expression = index } }, span);
ctx.tree.replaceData(index, ctx.tree.getData(wrapper));

// RIGHT: move original to new node, point wrapper to it
const inner = try ctx.tree.createNode(original_data, span);
ctx.tree.replaceData(index, .{ .parenthesized_expression = .{ .expression = inner } });
```

## Building ASTs from Scratch

`Tree.initEmpty()` creates a tree with no source text for programmatic AST construction. Since there is no source, all strings must be created with `addString()`. A valid tree starts from a `program` root node:

```zig
const ast = parser.ast;

const Span = ast.Span;

var out = ast.Tree.initEmpty(allocator);
defer out.deinit();

// Create a string literal node: "hello"
const hello_str = try out.addString("hello");
const hello = try out.createNode(
    .{ .string_literal = .{ .value = hello_str } },
    .none,
);

// Create an expression statement wrapping the literal
const stmt = try out.createNode(
    .{ .expression_statement = .{ .expression = hello } },
    .none,
);

// Build the program body (list of statements)
const body = try out.createExtra(&.{stmt});

// Create the root program node
out.program = try out.createNode(
    .{ .program = .{ .source_type = .module, .body = body } },
    .none,
);
```

### Building a New AST While Traversing Another

A powerful pattern is traversing one tree (with full context: scopes, symbols, path) while building a completely separate output tree. This is how transpilers work:

```zig
const sem = traverser.semantic;

const Transpiler = struct {
    out: *ast.Tree,

    pub fn enter_function(
        self: *Transpiler,
        func: ast.Function,
        index: ast.NodeIndex,
        ctx: *sem.Ctx,
    ) !traverser.Action {
        // Full context from the source tree:
        const is_strict = ctx.scope.isStrict();
        const source_span = ctx.tree.getSpan(index);

        // Build nodes in the output tree:
        const name_str = try self.out.addString("transpiledFn");
        const name_node = try self.out.createNode(
            .{ .binding_identifier = .{ .name = name_str } },
            source_span,
        );
        // ... continue building output AST using source context ...

        return .proceed;
    }
};

var source_tree = try parser.parse(allocator, source, .{});
defer source_tree.deinit();

var out = ast.Tree.initEmpty(allocator);
defer out.deinit();

var transpiler = Transpiler{ .out = &out };
const result = try sem.traverse(Transpiler, &source_tree, &transpiler);

// out now contains a new AST built with full knowledge of the source tree's
// scopes, symbols, and structure
```

You get the full power of any traverser mode (scopes, symbols, path navigation) on the source tree while constructing an entirely separate output tree. The two trees have independent arenas and lifetimes.

## Combining Modes

The traverser modes are composable. You can run multiple passes:

```zig
var tree = try parser.parse(allocator, source, .{});
defer tree.deinit();

// Pass 1: Transform
try transform.traverse(MyTransform, &tree, &my_transform);

// Pass 2: Semantic analysis on the transformed tree
const result = try parser.semantic.analyze(&tree);
```

Since transforms and scope/symbol tracking cannot safely run in the same pass (mutations would invalidate tracked state), the design enforces this separation at the type level: read-only traversers get `*const Tree`, transform gets `*Tree`.
