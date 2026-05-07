---
title: Traverse
description: Walk, analyze, and transform JavaScript and TypeScript ASTs with Yuku's traverser system.
---

The traverser is how you do real work on a Yuku AST. It walks the tree for you, calls your visitor hooks at every node, and (depending on the mode you pick) hands you the surrounding lexical scopes, the symbol table, or a mutable handle on the tree itself.

| Mode          | What you get                                          | Returns                       |
| ------------- | ----------------------------------------------------- | ----------------------------- |
| **Basic**     | Path from root, plus the full immutable tree          | nothing                       |
| **Scoped**    | Path + lexical scopes (with strict-mode tracking)     | `ScopeTree`                   |
| **Semantic**  | Path + scopes + symbols and references                | `ScopeTree` + `SymbolTable`   |
| **Transform** | Path + a mutable `*Tree` for in-place rewrites        | nothing                       |

The three read-only modes (basic, scoped, semantic) hand your visitor a `*const Tree`, so the type system guarantees you cannot accidentally mutate the AST while analysing it. Transform is the only mode that gives you `*Tree`. This is intentional: tracked state (scopes, symbols) and tree mutation cannot safely coexist in a single pass, so the API splits them.

:::note
The traverser is stable and powers Yuku's semantic checker in production. The surface may still grow as we build more tools on top of it (minifier, bundler, formatter). Breaking changes will be called out in release notes.
:::

## Your First Visitor

A visitor is just a struct with `enter_*` and `exit_*` methods. Pick a node type, write a method named after it, and the walker will call you when it gets there.

```zig
const std = @import("std");
const parser = @import("parser");
const ast = parser.ast;
const traverser = parser.traverser;
const basic = traverser.basic;

const Counter = struct {
    functions: u32 = 0,

    pub fn enter_function(
        self: *Counter,
        _: ast.Function,
        _: ast.NodeIndex,
        _: *basic.Ctx,
    ) traverser.Action {
        self.functions += 1;
        return .proceed;
    }
};

var tree = try parser.parse(allocator, source, .{});
defer tree.deinit();

var counter = Counter{};
try basic.traverse(Counter, &tree, &counter);

std.debug.print("found {d} functions\n", .{counter.functions});
```

That is the entire shape of every traverser-based tool you will write. The rest of this page is "what else you can do inside a hook".

## Hooks

Every hook follows the same four-argument shape:

```zig
pub fn enter_<node_type>(
    self: *V,                     // your visitor
    payload: ast.<NodeType>,      // the node's unpacked payload
    index: ast.NodeIndex,         // the node's index in the tree
    ctx: *<Mode>.Ctx,             // mode-specific context
) traverser.Action { ... }

pub fn exit_<node_type>(
    self: *V,
    payload: ast.<NodeType>,
    index: ast.NodeIndex,
    ctx: *<Mode>.Ctx,
) void { ... }
```

The hook name must match a field in `ast.NodeData`. Misspell it (`enter_funciton`) or use the wrong payload type and Zig produces a clear compile error at the `traverse` call site. There are no silent mismatches, and no runtime "method not found" surprises.

Enter hooks may return either `traverser.Action` or `Allocator.Error!traverser.Action`. Both are accepted, so a hook that never allocates can write `return .proceed;` directly, while one that calls `addNode` writes `return .proceed;` from inside a `try` block.

Exit hooks always return `void`.

### Receiving the Payload Already Unpacked

Each typed hook receives its node's payload pre-extracted from the tagged union, so you never write a `switch` to "unwrap" the node you just matched on:

```zig
pub fn enter_binary_expression(
    _: *V,
    expr: ast.BinaryExpression, // already unpacked, no switch needed
    _: ast.NodeIndex,
    _: *basic.Ctx,
) traverser.Action {
    if (expr.operator == .add) { ... }
    return .proceed;
}
```

When you do need to peek at a child node before the walker reaches it, switch on `ctx.tree.data(child_index)`:

```zig
pub fn enter_call_expression(
    _: *V,
    call: ast.CallExpression,
    _: ast.NodeIndex,
    ctx: *basic.Ctx,
) traverser.Action {
    switch (ctx.tree.data(call.callee)) {
        .member_expression => |mem| {
            // callee looks like obj.method(...)
            _ = mem;
        },
        .identifier_reference => |id| {
            const name = ctx.tree.string(id.name);
            // callee is a bare identifier
            _ = name;
        },
        else => {},
    }
    return .proceed;
}
```

See the [AST reference](/parser/ast) for every node type and its fields.

### Catch-All Hooks

If you want one hook that fires for every node regardless of type, define `enter_node` and/or `exit_node`. The payload is `ast.NodeData` (the full union):

```zig
pub fn enter_node(
    self: *V,
    data: ast.NodeData,
    index: ast.NodeIndex,
    ctx: *basic.Ctx,
) traverser.Action {
    return .proceed;
}

pub fn exit_node(
    self: *V,
    data: ast.NodeData,
    index: ast.NodeIndex,
    ctx: *basic.Ctx,
) void {}
```

When both a typed hook and a catch-all are defined, the order is:

- **Enter**: `enter_node` first, then `enter_<type>`
- **Exit**: `exit_<type>` first, then `exit_node`

That ordering lets a catch-all enter "gate" a subtree (return `.skip` to opt out before the typed hooks run) and a catch-all exit "summarise" what just happened.

### Actions

Every enter hook returns one of three actions:

| Action     | Effect                                                    |
| ---------- | --------------------------------------------------------- |
| `.proceed` | Walk into this node's children                            |
| `.skip`    | Do not descend, move to the next sibling                  |
| `.stop`    | End the entire traversal immediately                      |

`.skip` is what you return after hand-walking a subtree yourself, or after a transform that should not be re-entered. `.stop` is how you implement "find the first X and exit".

### Running Without Hooks

Sometimes the only thing you want from the traverser is its output: a `ScopeTree` from the scoped mode, or a `ScopeTree` + `SymbolTable` from the semantic mode. In that case you do not need to define any hooks. Pass an empty struct as the visitor and the walker still runs end-to-end, the trackers still produce their result, and nothing fires in between.

```zig
// Just the scope tree:
var noop = struct {}{};
const scope_tree = try scoped.traverse(@TypeOf(noop), &tree, &noop);

// Just the scope tree + symbol table:
var result = try sem.traverse(@TypeOf(noop), &tree, &noop);
try result.symbol_table.resolveAll(result.scope_tree);
```

The empty struct makes the absence of hooks visible in the code, which is why there is no hidden `analyze`-style helper inside the traverser. If a tool wants the result of a walk without any per-node logic, the caller spells that out exactly.

## The Path

Every mode tracks the path from the root down to the current node. The path is a small fixed-capacity stack of `NodeIndex` values you can read at any time through `ctx.path`.

```zig
ctx.path.parent()        // immediate parent NodeIndex, or null at root
ctx.path.ancestor(0)     // current node
ctx.path.ancestor(1)     // parent (same as parent())
ctx.path.ancestor(2)     // grandparent
ctx.path.depth()         // 0 at root, grows as you descend

var it = ctx.path.ancestors();
while (it.next()) |idx| {
    // walks from current node up to root
}
```

Combined with the full tree (always available as `ctx.tree`), the path lets you navigate freely:

```zig
pub fn enter_identifier_reference(
    _: *V,
    id: ast.IdentifierReference,
    _: ast.NodeIndex,
    ctx: *basic.Ctx,
) traverser.Action {
    // is this identifier the callee of a call expression?
    if (ctx.path.parent()) |parent_idx| {
        if (ctx.tree.data(parent_idx) == .call_expression) {
            const name = ctx.tree.string(id.name);
            _ = name;
        }
    }
    return .proceed;
}
```

The path has a hard cap of 256 entries. In practice this is far beyond any realistic ECMAScript nesting depth, so you can treat it as effectively unbounded.

## Basic Traverser

The minimum: path tracking plus the full immutable tree. No allocator needed, nothing returned.

Use it for tools that only need structural pattern matching:

- `eslint`-style rules that look at "this node and its parent"
- counters and statistics
- AST-shape assertions in tests
- pretty-printers that read but never write the tree

```zig
const basic = traverser.basic;

var visitor = MyVisitor{};
try basic.traverse(MyVisitor, &tree, &visitor);
```

`basic.Ctx` carries:

```zig
ctx.tree    // *const ast.Tree, full read access
ctx.path    // wk.NodePath, the current path stack
```

That is it. Step up to scoped or semantic when you need to know what bindings are in scope.

## Scoped Traverser

Scoped mode adds automatic lexical scope tracking. Whenever the walker enters a scope-creating node, the tracker pushes a new scope; on exit, it pops. Your hooks see `ctx.scope.currentScope()` already pointing at the right place.

```zig
const scoped = traverser.scoped;

var visitor = MyVisitor{};
const scope_tree = try scoped.traverse(MyVisitor, &tree, &visitor);
// scope_tree contains every scope the walk produced
```

### What Creates a Scope

The tracker recognises every construct in the spec that introduces a new lexical environment, plus the TypeScript-specific ones that scope type parameters and infer bindings.

| Node                                                          | Scope kind                                              |
| ------------------------------------------------------------- | ------------------------------------------------------- |
| Program                                                       | `global` (plus a child `module` if `source_type` is `module`) |
| Function declaration / expression                             | `function`                                              |
| Arrow function                                                | `function`                                              |
| Block statement                                               | `block`                                                 |
| `for` / `for...in` / `for...of`                               | `block`                                                 |
| `catch` clause                                                | `block` (the body block reuses this scope)              |
| `switch` statement                                            | `block`                                                 |
| Class declaration / expression                                | `class` (always strict per spec)                        |
| Class static block                                            | `static_block`                                          |
| Named function or class expression                            | `expression_name` wrapping a `function` / `class` scope |
| TS interface / type alias                                     | `block`                                                 |
| TS function/constructor type, call/construct/index signature  | `block`                                                 |
| TS method signature                                           | `block`                                                 |
| TS mapped / conditional type                                  | `block` (conditional isolates `infer T` per branch)     |
| TS namespace body                                             | `ts_module` (its own kind, see below)                   |

A few details worth pinning down because they trip up first-time readers:

**Catch clauses share their body block.** Per spec section 14.15.2 the catch parameter and the block body live in the same `catchEnv`. The tracker pushes one `block` scope on `catch_clause`, and the body's `block_statement` reuses it instead of pushing a second one. This is what lets `findInScopeOrHoisted` naturally detect the early-error case where a `var` inside the body collides with the parameter.

**Named function and class expressions create two scopes.** For `const x = function foo() { ... }`, ECMAScript section 15.2.5 wraps the body in an extra environment that holds an immutable binding for `foo`:

```
outer scope          (x lives here)
  expression_name    (foo lives here, immutable)
    function scope   (body bindings live here)
```

Without this, `const foo = 1` inside the body would conflict with the expression name. Same pattern for `const C = class D { ... }` per section 15.7.14.

**`ts_module` is its own scope kind, not a block.** TypeScript namespace bodies act as a `var`-hoist target, so a `var` inside a namespace stays inside the namespace instead of escaping to the surrounding scope. That difference is encoded as a separate `Scope.Kind` so `Kind.isHoistTarget()` returns true for it.

### Strict Mode

Strict mode propagates automatically:

- Module scopes are always strict.
- Class scopes are always strict.
- A `"use strict"` directive at the top of any scope sets `flags.strict` on that scope.
- Child scopes inherit strict mode from their parent.
- Functions whose body opens with `"use strict"` get `strict = true` set **at function-scope creation time**, before any parameter hooks fire. This is needed because the directive applies retroactively to the parameter list, where rules like "no duplicate parameters" only kick in under strict mode.

### Querying the Tracker

Inside a hook, `ctx.scope` is a live `ScopeTracker` you can interrogate:

```zig
pub fn enter_node(
    _: *V,
    _: ast.NodeData,
    _: ast.NodeIndex,
    ctx: *scoped.Ctx,
) traverser.Action {
    const id = ctx.scope.currentScopeId();      // ScopeId of current scope
    const cur = ctx.scope.currentScope();       // Scope value (kind, flags, parent, ...)
    const hoist = ctx.scope.currentHoistScopeId(); // where `var` declarations would land
    const strict = ctx.scope.isStrict();

    if (cur.kind == .function and !strict) { ... }

    var it = ctx.scope.ancestors(id);
    while (it.next()) |ancestor_id| {
        const ancestor = ctx.scope.getScope(ancestor_id);
        _ = ancestor;
    }

    return .proceed;
}
```

`getScope(id)` and `getScopeMut(id)` look up any scope by id. `currentScopeMut()` is there if you need to flip a flag on the active scope yourself (rarely needed; the tracker handles `"use strict"` automatically).

### Using the ScopeTree After Traversal

`scoped.traverse` returns an immutable `ScopeTree` containing every scope that was created:

```zig
const scope_tree = try scoped.traverse(MyVisitor, &tree, &visitor);

const root = scope_tree.getScope(.root);          // ScopeId.root is always 0
_ = root;

var it = scope_tree.ancestors(some_scope_id);
while (it.next()) |id| {
    const scope = scope_tree.getScope(id);
    _ = scope;
}
```

The tree is backed by the parser's arena, so it lives as long as the source `Tree` does. Calling `tree.deinit()` invalidates it.

## Semantic Traverser

Semantic mode is the full-power one: path, scopes, symbols, references, redeclaration handling, TypeScript context tracking.

```zig
const sem = traverser.semantic;

var visitor = MyVisitor{};
const result = try sem.traverse(MyVisitor, &tree, &visitor);
// result.scope_tree   - every scope
// result.symbol_table - every symbol and reference
```

`sem.Ctx` exposes:

```zig
ctx.tree          // *const ast.Tree
ctx.path          // wk.NodePath
ctx.scope         // ScopeTracker (same API as scoped mode)
ctx.symbols       // SymbolTracker (the new one)
ctx.inTypePosition()  // true inside a TS type-only subtree
ctx.inTsNamespace()   // true inside a TS `namespace` body
```

### Two-Phase Binding

Symbol declaration is split across two phases per node:

1. **Phase 1, on enter**: when entering a parent declaration node (`variable_declaration`, `function`, `class`, `import_declaration`, `formal_parameters`, `catch_clause`, `ts_interface_declaration`, etc.), the tracker records *what kind* of binding the next `binding_identifier` should produce: its flags, its redeclaration excludes, and its target scope. This happens **before** your enter hook runs.

2. **Phase 2, on `post_enter`**: after your enter hook returns, but before the walker descends into children, the tracker materialises the actual symbol or reference. `binding_identifier` becomes a `Symbol`; `identifier_reference` becomes a `Reference`.

Why the split? It guarantees a useful invariant for your visitor:

:::note
Your enter hook on a `binding_identifier` sees the scope state **before** that binding has been declared. You can inspect what is *about to* be declared, look up whether something with the same name already exists, and decide what to do, before the tracker commits the new symbol.
:::

```zig
pub fn enter_binding_identifier(
    _: *V,
    id: ast.BindingIdentifier,
    _: ast.NodeIndex,
    ctx: *sem.Ctx,
) !traverser.Action {
    const flags    = ctx.symbols.currentBindingFlags();    // what the new symbol will be
    const excludes = ctx.symbols.currentBindingExcludes(); // what it conflicts with
    const target   = ctx.symbols.currentTarget();          // which scope it lands in

    const name = ctx.tree.string(id.name);
    if (ctx.symbols.findInScopeOrHoisted(target, name)) |existing_id| {
        const existing = ctx.symbols.getSymbol(existing_id);
        if (existing.flags.intersects(excludes)) {
            // genuine conflict: emit a redeclaration diagnostic
        } else {
            // compatible merge (e.g. function overload, class + interface,
            // namespace + value). The tracker will merge them automatically
            // in post_enter.
        }
    }

    return .proceed;
}
```

`currentBindingFlags`, `currentBindingExcludes`, and `currentTarget` are the three readers for the pending state. Reading them inside an enter hook on a `binding_identifier` is always safe; reading them at any other node is undefined.

### Symbol Flags

`Symbol.Flags` describes everything the tracker knows about a binding. A single symbol can carry several flags at once: a `class` lives in both value and type space, an exported `var` is both `function_scoped_var` and `exported`, and an interface and a class of the same name merge into a single symbol that satisfies both kinds.

The flags group into three categories.

**Declaration kind** (what created the binding):

```zig
symbol.flags.function_scoped_var  // var, parameter, catch_var
symbol.flags.block_scoped_var     // let, const, using, await_using
symbol.flags.function             // function declaration / expression
symbol.flags.class                // class declaration / expression
symbol.flags.interface            // TS interface
symbol.flags.type_alias           // TS type alias
symbol.flags.type_parameter       // TS <T>, infer T, mapped key
symbol.flags.regular_enum         // TS enum
symbol.flags.const_enum           // TS const enum
symbol.flags.value_module         // TS namespace whose body has runtime content
symbol.flags.namespace_module     // TS namespace (any kind)
symbol.flags.import               // value or unspecified-kind import
symbol.flags.type_import          // `import type ...` or `import { type x }`
```

**Modifiers** (qualifiers on the binding):

```zig
symbol.flags.const_var   // const or using binding
symbol.flags.parameter   // function/method parameter
symbol.flags.catch_var   // catch (e) binding
symbol.flags.ambient     // TS `declare`
symbol.flags.exported    // exported from a module
symbol.flags.is_default  // default export
```

**Helpers** on the flag struct itself:

```zig
flags.intersects(other)     // true if `flags` and `other` share at least one flag
flags.merge(other)          // union of two flag sets (used when merging compatible declarations)
flags.isHoistingVar()       // true for a real `var` (not a parameter, not a catch_var)
flags.toString()            // human-readable category for diagnostics
```

**Space predicates.** A symbol can occupy JS value space (visible at runtime), TS type space (referenced from annotations), or both (a `class` straddles them by design). These are common enough questions that they have direct predicates, no manual flag combinations needed:

```zig
flags.inValueSpace()        // var, let, const, function, class, enum, value namespace
flags.inTypeSpace()         // class, enum, interface, type alias, type parameter
flags.isBlockScopedLike()   // names a hoisting `var` cannot pass through
                            // (block_scoped_var, class, function)
```

`class` and `regular_enum` deliberately satisfy both `inValueSpace` and `inTypeSpace`. That is what makes "use a class as a type" work without special-casing.

### Per-Kind Redeclaration Excludes

For each declaration kind, the tracker has a precomputed `Symbol.Excludes.X` flag set. The rule is uniform:

> A new declaration with `Excludes.X` conflicts with any existing symbol whose flags **intersect** `Excludes.X`. Otherwise the two declarations merge into a single symbol with the union of their flags.

```zig
Symbol.Excludes.block_var        // let / const / using
Symbol.Excludes.function_var     // var
Symbol.Excludes.function         // function (allows overloads in TS, var-merge in sloppy)
Symbol.Excludes.class
Symbol.Excludes.interface
Symbol.Excludes.type_alias
Symbol.Excludes.regular_enum
Symbol.Excludes.const_enum
Symbol.Excludes.value_module
Symbol.Excludes.namespace_module
Symbol.Excludes.import_binding
Symbol.Excludes.parameter
Symbol.Excludes.catch_param
Symbol.Excludes.type_parameter
```

This single mechanism handles function overloads, `class` + `interface` declaration merging, `namespace` + `enum` merging, and ambient module patterns without any per-construct branching. If you ever need to teach the tracker a new merging rule, you change one flag set, not a tangle of `if` statements.

### TypeScript Context Flags

Two booleans on `sem.Ctx` track whether the walker is currently inside TS-only territory:

```zig
pub fn enter_identifier_reference(
    _: *V,
    id: ast.IdentifierReference,
    _: ast.NodeIndex,
    ctx: *sem.Ctx,
) traverser.Action {
    if (ctx.inTypePosition()) {
        // Inside a type annotation, type reference, type parameter,
        // type literal, mapped/conditional type, etc.
        // References here are tagged as `.type` automatically.
    }
    if (ctx.inTsNamespace()) {
        // Inside a TS `namespace` body.
    }
    _ = id;
    return .proceed;
}
```

`inTypePosition()` is also what the tracker uses internally to decide that a `binding_identifier` inside a function-type or index signature is a parameter *label* (not a real declaration). Only `type_parameter` bindings are real in type position.

### References and Their Kind

Every `identifier_reference` node produces one `Reference`. Each reference carries a `kind`:

```zig
pub const Reference = struct {
    name: String,
    scope: ScopeId,
    node: ast.NodeIndex,
    kind: Kind,  // .value or .type
};
```

`.value` means the reference is a runtime use (the receiver of a property access, an argument, the LHS of an assignment, etc.). `.type` means it appears inside a type annotation, an `extends`/`implements` clause, a type argument, or any other TS type-position context. Rename-aware tooling distinguishes the two so it can change a value without touching a same-named type, and vice versa.

### Resolving References

During traversal, references are recorded but not yet linked to their declarations. After traversal, call `resolveAll` with the scope tree to walk every reference up its chain and build the cross-index:

```zig
var result = try sem.traverse(MyVisitor, &tree, &visitor);
try result.symbol_table.resolveAll(result.scope_tree);
```

Once resolved you have the full bidirectional map:

```zig
const table = result.symbol_table;

// Forward: what does this reference point to?
const sym_id = table.referenceSymbol(some_ref_id);
if (sym_id != .none) {
    const sym = table.getSymbol(sym_id);
    _ = sym;
}

// Reverse: who references this symbol?
for (table.symbolReferences(my_sym_id)) |ref_id| {
    const ref = table.getReference(ref_id);
    _ = ref;
}

// Quick check: is this binding used at all?
if (!table.isReferenced(my_sym_id)) {
    // candidate for an "unused variable" diagnostic
}

// References that did not resolve to any local binding.
// These are globals, undeclared names, or free variables.
var it = table.iterUnresolved();
while (it.next()) |entry| {
    // entry.id, entry.reference
    _ = entry;
}
```

`iterUnresolved` is exactly what a "no-undef" linter wants: every name the parser saw that is not bound anywhere in the tree.

You can also resolve a name manually from any starting scope. `resolve` takes the scope tree alongside the starting scope:

```zig
if (table.resolve(result.scope_tree, scope_id, "myVar")) |found| {
    const sym = table.getSymbol(found);
    _ = sym;
}
```

`resolve` walks up the scope chain just like JavaScript does at runtime, also matching hoisted `var`s passing through intermediate block scopes.

### Single-Scope Lookups

For tools that do *not* want a full chain walk (a minifier checking "is this name shadowed in this exact scope"), both the tracker and the table expose tight single-scope lookups:

```zig
findInScope(scope, name)           // bindings declared directly in `scope`
findInScopeOrHoisted(scope, name)  // also matches a `var` passing through `scope`
```

`findInScopeOrHoisted` is the same lookup the tracker uses internally to detect block-scoped redeclarations of names that a hoisting `var` is travelling through.

### Iterating the Table

Three iterators walk the whole table; each yields a `(id, value)` entry so you never reach back through the table for a lookup you were just handed:

```zig
var syms = table.iterSymbols();
while (syms.next()) |entry| {
    // entry.id, entry.symbol
    _ = entry;
}

var refs = table.iterReferences();
while (refs.next()) |entry| {
    // entry.id, entry.reference
    _ = entry;
}

var unresolved = table.iterUnresolved();
while (unresolved.next()) |entry| {
    // entry.id, entry.reference (only refs that did not resolve)
    _ = entry;
}
```

For tight per-scope loops (a minifier checking shadowing in one scope, a renamer enumerating bindings in a function body), `scopeSymbols(scope_id)` yields raw `*SymbolId`s straight out of the per-scope hash map, so you avoid copying full `Symbol` structs:

```zig
// During traversal (live tracker):
var it = ctx.symbols.scopeSymbols(scope_id);
while (it.next()) |sym_id_ptr| {
    const sym = ctx.symbols.getSymbol(sym_id_ptr.*);
    const name = ctx.tree.string(sym.name);
    _ = name;
}

// After traversal (immutable table):
var it2 = result.symbol_table.scopeSymbols(scope_id);
while (it2.next()) |sym_id_ptr| {
    const sym = result.symbol_table.getSymbol(sym_id_ptr.*);
    _ = sym;
}
```

### SymbolTable Reference

The table's public surface, in one place:

```zig
table.symbols                          // []const Symbol, in declaration order
table.references                       // []const Reference, in source order

table.string(handle)                   // []const u8 from a String handle
table.getSymbol(sym_id)                // Symbol by id
table.getReference(ref_id)             // Reference by id

table.iterSymbols()                    // (id, symbol) entries
table.iterReferences()                 // (id, reference) entries
table.iterUnresolved()                 // unresolved (id, reference) entries
table.scopeSymbols(scope_id)           // *SymbolId per binding in the scope

table.findInScope(scope, name)         // single-scope lookup
table.findInScopeOrHoisted(scope, name)// + hoisting var passing through
table.resolve(scope_tree, scope, name) // scope-chain lookup

try table.resolveAll(scope_tree)       // build the cross-index
table.referenceSymbol(ref_id)          // forward (ref -> sym) after resolveAll
table.symbolReferences(sym_id)         // reverse (sym -> []ref) after resolveAll
table.isReferenced(sym_id)             // shorthand for "any references?"
```

## Transform Traverser

Transform mode is for rewrites: codemods, desugaring passes, AST-level optimisations. Your visitor receives `*Tree` (mutable) and can call `setData`, `addNode`, `addExtra`, and `setSpan` from inside any hook.

```zig
const transform = traverser.transform;

var visitor = MyTransform{};
try transform.traverse(MyTransform, &tree, &visitor);
```

`transform.Ctx` is intentionally minimal:

```zig
ctx.tree    // *ast.Tree, full read AND write access
ctx.path    // wk.NodePath
```

There is no scope or symbol tracking in this mode. Mutating the tree would invalidate any tracked state mid-walk, so the design splits "analyse" from "rewrite" at the type level. If you need both, run two passes (see [Combining Modes](#combining-modes) below).

### Replacing a Node In Place

The simplest transform replaces a node's data inside its enter hook. The walker re-reads the node after every enter, so the replacement's children are walked automatically:

```zig
pub fn enter_binary_expression(
    _: *MyTransform,
    expr: ast.BinaryExpression,
    index: ast.NodeIndex,
    ctx: *transform.Ctx,
) traverser.Action {
    if (expr.operator == .add) {
        ctx.tree.setData(index, .{ .binary_expression = .{
            .left = expr.left,
            .right = expr.right,
            .operator = .multiply,
        }});
    }
    return .proceed;
}
```

### Creating New Nodes

Use `addNode` to append a brand-new node and get its index. Use `addExtra` to allocate variable-length child lists for fields typed as `IndexRange`:

```zig
const lit = try ctx.tree.addNode(
    .{ .numeric_literal = .{ .raw = "42" } },
    .none,                              // span: .none if it has no source location
);

const args = try ctx.tree.addExtra(&.{ child1, child2, child3 });
```

Both are safe to call during traversal and use the tree's arena, so there is nothing to free.

### Wrapping a Node

A common pattern: "take this node, move it to a fresh node, replace this one with a wrapper pointing at the moved copy". Useful for parenthesising expressions, wrapping in `await`, etc.

```zig
pub fn enter_binary_expression(
    _: *MyTransform,
    expr: ast.BinaryExpression,
    index: ast.NodeIndex,
    ctx: *transform.Ctx,
) !traverser.Action {
    const span = ctx.tree.span(index);

    // 1. Move the original data into a new node, keeping its span.
    const inner = try ctx.tree.addNode(
        .{ .binary_expression = expr },
        span,
    );

    // 2. Replace the current node with a wrapper that points at the moved copy.
    ctx.tree.setData(index, .{ .parenthesized_expression = .{ .expression = inner } });

    // 3. Skip so the walker does not re-enter and re-wrap the moved node.
    return .skip;
}
```

Returning `.skip` here is essential: if you let the walker descend, it will re-read the new wrapper, find its child (the moved copy), and call `enter_binary_expression` again on the same data, infinitely.

### Self-Reference Safety

:::caution
Never set a node's child to its own index. The walker re-reads node data after every enter hook, so a self-referential node causes infinite recursion.
:::

```zig
// WRONG: cycle. The wrapper points to its own index.
const wrapper = try ctx.tree.addNode(
    .{ .parenthesized_expression = .{ .expression = index } },
    span,
);
ctx.tree.setData(index, ctx.tree.data(wrapper));

// RIGHT: move original data to a fresh node, wrap that.
const inner = try ctx.tree.addNode(original_data, span);
ctx.tree.setData(index, .{ .parenthesized_expression = .{ .expression = inner } });
```

## Building ASTs From Scratch

`Tree.initEmpty(allocator)` creates a tree with no source text, intended for programmatic AST construction. Because there is no source backing it, every string must be created with `tree.addString(...)`.

A valid tree starts from a `program` root node:

```zig
var out = ast.Tree.initEmpty(allocator);
defer out.deinit();

const hello = try out.addString("hello");
const lit = try out.addNode(
    .{ .string_literal = .{ .value = hello } },
    .none,
);

const stmt = try out.addNode(
    .{ .expression_statement = .{ .expression = lit } },
    .none,
);

const body = try out.addExtra(&.{stmt});

out.root = try out.addNode(
    .{ .program = .{ .source_type = .module, .body = body } },
    .none,
);
```

That is enough for a tree that any read-only consumer (a printer, an emitter, another traverser pass) will accept.

### Building One Tree While Walking Another

A particularly powerful pattern, central to transpilers and source-to-source compilers, is walking an *input* tree with full semantic context (scopes, symbols, path) while building a completely separate *output* tree:

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
        // Read context from the *source* tree:
        const is_strict = ctx.scope.isStrict();
        const span = ctx.tree.span(index);
        _ = is_strict;
        _ = func;

        // Build into the *output* tree:
        const name = try self.out.addString("transpiledFn");
        const id = try self.out.addNode(
            .{ .binding_identifier = .{ .name = name } },
            span,
        );
        _ = id;

        return .proceed;
    }
};

var source_tree = try parser.parse(allocator, source, .{});
defer source_tree.deinit();

var out = ast.Tree.initEmpty(allocator);
defer out.deinit();

var transpiler = Transpiler{ .out = &out };
_ = try sem.traverse(Transpiler, &source_tree, &transpiler);

// `out` is a fresh AST you built using full knowledge of the source's
// scopes, symbols, and structure. The two trees have independent arenas.
```

The two trees never touch each other's storage, and either can be freed independently. This is the recommended structure for any tool that produces a transformed AST from an input AST without mutating the input.

## Combining Modes

The four modes compose via multiple passes. A typical pipeline looks like:

```zig
var tree = try parser.parse(allocator, source, .{});
defer tree.deinit();

// Pass 1: rewrite syntax (sugar lowering, JSX transform, etc.)
var rewriter = MyTransform{};
try transform.traverse(MyTransform, &tree, &rewriter);

// Pass 2: semantic analysis on the rewritten tree
var analyser = MyAnalyser{};
var result = try sem.traverse(MyAnalyser, &tree, &analyser);
try result.symbol_table.resolveAll(result.scope_tree);

// Pass 3: emit, lint, minify, etc.
```

Because read-only modes take `*const Tree` and transform takes `*Tree`, the type system enforces that you cannot accidentally smuggle a tracking pass into a mutation pass. If a function compiles with `*const Tree` you have a mathematical guarantee it will not change the AST.
