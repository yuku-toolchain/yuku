---
title: AST
description: Internal AST structure, node types, and the data model for Yuku's parser.
---

Yuku's internal AST is a flat, arena-allocated structure optimized for sequential access. When serialized to JSON or exposed through Node.js bindings, it is converted to an [ESTree](https://github.com/estree/estree)-compatible format matching [Oxc](https://oxc.rs):

- **JavaScript / JSX**: Fully conformant with [ESTree](https://github.com/estree/estree), identical to [Acorn](https://www.npmjs.com/package/acorn).
- **TypeScript**: Conforms to [TS-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) used by `@typescript-eslint`.

Extensions beyond the base specs: Stage 3 [decorators](https://github.com/tc39/proposal-decorators), [import defer](https://github.com/tc39/proposal-defer-import-eval), [import source](https://github.com/tc39/proposal-source-phase-imports), and a `hashbang` field on `Program`.

This page covers the internal Zig AST: its memory model, core types, and the complete node reference.

## Memory Model

The AST is not a graph of heap-allocated structs. All nodes live in a single flat array (`Tree.nodes`), and children reference each other by integer index. This gives linear memory layout and predictable cache behavior during traversal.

```
Tree
 nodes   NodeList        flat array of all nodes (data + span, struct-of-arrays)
 extra   []NodeIndex     variable-length child lists (IndexRange points here)
 strings StringPool      all string content (source refs + interned extras)
```

`NodeList` is a `MultiArrayList(Node)`, meaning `data` and `span` are stored in separate parallel arrays. Reading only spans, or only data, stays within a single array.

All memory is owned by a single `ArenaAllocator`. `tree.deinit()` frees everything at once.

## The Tree

`Tree` is the root container returned by `parser.parse()`.

| Field | Type | Description |
|-------|------|-------------|
| `program` | `NodeIndex` | Root node (always a `program`) |
| `nodes` | `NodeList` | All AST nodes |
| `extra` | `ArrayList(NodeIndex)` | Variable-length child index lists |
| `diagnostics` | `ArrayList(Diagnostic)` | Parse errors, warnings, hints |
| `comments` | `[]const Comment` | All comments found in source |
| `source` | `[]const u8` | Original source text |
| `source_type` | `SourceType` | `.script` or `.module` |
| `lang` | `Lang` | `.js`, `.ts`, `.jsx`, `.tsx`, or `.dts` |

Key read methods:

```zig
tree.getData(index)           // NodeData for a node
tree.getSpan(index)           // Span (source byte range) for a node
tree.getExtra(range)          // []const NodeIndex for an IndexRange
tree.getString(handle)        // []const u8 from a String handle

tree.hasErrors()              // true if any diagnostic has severity .error
tree.isTs()                   // true for .ts, .tsx, .dts
tree.isJsx()                  // true for .jsx, .tsx
tree.isModule()               // true for source_type .module
```

## Core Types

Four types appear in nearly every node definition. Understanding them once makes the entire node reference readable.

### NodeIndex

```zig
pub const NodeIndex = enum(u32) { null = std.math.maxInt(u32), _ };
```

Every node is identified by its position in `Tree.nodes`. Optional child fields use `.null` to indicate absence:

```zig
// if_statement.alternate is .null when there is no else branch
if (node.alternate != .null) {
    const else_data = tree.getData(node.alternate);
}
```

### IndexRange

```zig
pub const IndexRange = struct { start: u32, len: u32 };
```

Nodes with a variable number of children store them as a contiguous slice in `Tree.extra`. An `IndexRange` is a `(start, len)` window into that array.

```zig
const children = tree.getExtra(node.body); // []const NodeIndex
for (children) |child_index| {
    const child_data = tree.getData(child_index);
}
```

`IndexRange.empty` (`{ .start = 0, .len = 0 }`) represents an empty list.

### String

```zig
pub const String = struct { start: u32, end: u32 };
```

A `String` is a lightweight handle to string content . It points into one of two backing stores:

- **Source slice (zero-copy)**: most identifiers and string literals parsed from source. The bytes live inside the original `source` slice .
- **Pool entry**: programmatically added strings (`tree.addString()`), transformed names, or escaped identifiers. These live in the string pool's extra buffer.

`tree.getString(handle)` resolves both cases transparently:

```zig
const name = tree.getString(node.name); // always returns []const u8
```

`String.empty` (zero value `{ 0, 0 }`) represents an empty string. This design means reading any identifier during traversal is just a bounds check . No allocation, no hash lookup.

### Span

```zig
pub const Span = struct { start: u32, end: u32 };
```

Byte offsets into the source text. `start` is inclusive, `end` is exclusive:

```zig
const span = tree.getSpan(index);
const source_text = tree.source[span.start..span.end];
```

## NodeData

`NodeData` is a tagged union with a variant for every node type. `tree.getData(index)` returns one, and you switch on the tag to determine the type and unpack its fields:

```zig
const data = tree.getData(index);
switch (data) {
    .binary_expression => |expr| {
        // expr.left and expr.right are NodeIndex (recurse with getData)
        // expr.operator is a BinaryOperator enum
        const left_data = tree.getData(expr.left);
    },
    .variable_declaration => |decl| {
        // decl.kind is VariableKind (.var, .let, .const, .using, .await_using)
        // decl.declarators is IndexRange (read with getExtra)
        const declarators = tree.getExtra(decl.declarators);
    },
    .identifier_reference => |id| {
        // id.name is a String (resolve with getString)
        const name = tree.getString(id.name);
    },
    else => {},
}
```

## Node Reference

Every field of `ast.NodeData` is a distinct node type. The field name is the exact hook name used in visitor structs:

```zig
// NodeData field:  binary_expression: BinaryExpression
// Visitor hook:    enter_binary_expression / exit_binary_expression
```

The full `NodeData` union is what the traverser's compile-time validation checks against. Any `enter_*` or `exit_*` method on your visitor must match a field name here exactly.

Optional child fields (`.null`) and optional child lists (`IndexRange.empty`) are noted where they apply.

---

### Program

The root of every tree. There is always exactly one `program` node at `tree.program`.

```zig
pub const Program = struct {
    source_type: SourceType,    // .script or .module
    body: IndexRange,           // (Statement | Directive)[]
    hashbang: ?Hashbang,        // non-null for #!/usr/bin/env node lines
};
```

---

### Statements

| Node | JS Syntax | Notes |
|------|-----------|-------|
| `expression_statement` | `expr;` | Wraps any expression used as a statement |
| `block_statement` | `{ ... }` | `body` is a statement/directive list |
| `empty_statement` | `;` | No fields |
| `debugger_statement` | `debugger;` | No fields |
| `if_statement` | `if (test) cons else alt` | `alternate` is `.null` when no else branch |
| `switch_statement` | `switch (d) { cases }` | `cases` is a list of `switch_case` nodes |
| `switch_case` | `case x: ...` / `default: ...` | `test` is `.null` for the default case |
| `for_statement` | `for (init; test; update) body` | `init`, `test`, and `update` are all optional (`.null`) |
| `for_in_statement` | `for (x in y) body` | `left` is a declaration or assignment target |
| `for_of_statement` | `for (x of y) body` | `await: bool` for `for await (...of...)` |
| `while_statement` | `while (test) body` | |
| `do_while_statement` | `do body while (test)` | |
| `break_statement` | `break;` / `break label;` | `label` is `.null` for unlabeled |
| `continue_statement` | `continue;` / `continue label;` | `label` is `.null` for unlabeled |
| `labeled_statement` | `label: stmt` | |
| `return_statement` | `return;` / `return expr;` | `argument` is `.null` for bare return |
| `throw_statement` | `throw expr;` | |
| `try_statement` | `try {} catch {} finally {}` | `handler` and `finalizer` are `.null` when absent |
| `catch_clause` | `catch (e) { body }` | `param` is `.null` for `catch {}` without binding |
| `with_statement` | `with (obj) body` | |

---

### Declarations

| Node | JS Syntax | Notes |
|------|-----------|-------|
| `variable_declaration` | `var/let/const/using x = ...` | `kind` is a `VariableKind` enum; `declarators` is a list of `variable_declarator` nodes |
| `variable_declarator` | `x = init` | `id` is a binding pattern; `init` is `.null` for `let x;` |
| `directive` | `"use strict";` | Only appears at the start of a function or module body, before regular statements |
| `function` | `function foo() {}` | Covers all function forms; check the `type` field |
| `class` | `class Foo {}` | Covers both declarations and expressions; check the `type` field |

`function` and `class` are dual-purpose nodes. The `type` field distinguishes the form:

```zig
// FunctionType
function_declaration          // function foo() {}
function_expression           // const x = function() {}
ts_declare_function           // declare function foo(): void
ts_empty_body_function_expression  // abstract methods, interface methods

// ClassType
class_declaration             // class Foo {}
class_expression              // const x = class {}
```

---

### Expressions

| Node | JS Syntax | Notes |
|------|-----------|-------|
| `binary_expression` | `a + b`, `a === b`, `a instanceof b` | `operator` is a `BinaryOperator` enum |
| `logical_expression` | `a && b`, `a \|\| b`, `a ?? b` | `operator` is a `LogicalOperator` enum |
| `unary_expression` | `!x`, `typeof x`, `void x`, `delete x` | `operator` is a `UnaryOperator` enum |
| `update_expression` | `x++`, `++x`, `x--` | `operator` is `UpdateOperator`; `prefix: bool` distinguishes pre/post |
| `assignment_expression` | `x = y`, `x += y`, `x ??= y` | `operator` is an `AssignmentOperator` enum |
| `conditional_expression` | `test ? a : b` | |
| `sequence_expression` | `a, b, c` | `expressions` is a node list |
| `parenthesized_expression` | `(expr)` | Wraps an expression to preserve explicit parentheses in the tree |
| `member_expression` | `obj.prop`, `obj[x]`, `obj.#priv` | `computed: bool` for bracket access; `optional: bool` for `?.` |
| `call_expression` | `fn(args)`, `fn?.()` | `optional: bool` for `?.()` |
| `new_expression` | `new Foo(args)` | |
| `chain_expression` | `a?.b`, `a?.()` | Wrapper around an optional chain; the inner node carries `optional: true` |
| `tagged_template_expression` | `` tag`hello` `` | `tag` is the function; `quasi` is the template literal |
| `await_expression` | `await expr` | |
| `yield_expression` | `yield expr`, `yield* expr` | `delegate: bool` for `yield*`; `argument` may be `.null` |
| `meta_property` | `import.meta`, `new.target` | `meta` and `property` are `identifier_name` nodes |
| `array_expression` | `[a, , b, ...c]` | `elements` may contain `.null` entries for holes |
| `object_expression` | `{a: 1, b, ...c}` | `properties` contains `object_property` and `spread_element` nodes |
| `object_property` | `key: value`, getters, setters, methods | `kind` (`PropertyKind`), `method`, `shorthand`, `computed` |
| `spread_element` | `...expr` | Used in arrays, calls, and object literals |
| `import_expression` | `import(src)`, `import.source(src)` | Dynamic import; `phase` may be `.source`, `.defer`, or `null` |
| `this_expression` | `this` | No fields |

---

### Literals

| Node | JS Syntax | Notes |
|------|-----------|-------|
| `string_literal` | `"hello"`, `'world'` | `raw` includes surrounding quotes; `value` is the decoded content without quotes (escape sequences resolved) |
| `numeric_literal` | `42`, `0xFF`, `0o7`, `0b1010` | `raw` is the source text; `kind` distinguishes decimal / hex / octal / binary; `value` is the parsed `f64` |
| `bigint_literal` | `42n` | `raw` is the full source text including the `n` suffix; `value` is the digits without it |
| `boolean_literal` | `true`, `false` | `value: bool` |
| `null_literal` | `null` | No fields |
| `regexp_literal` | `/pattern/flags` | `pattern` and `flags` are separate `String` handles |
| `template_literal` | `` `hello ${name}` `` | `quasis` (list of `template_element`) and `expressions` are interleaved; always `quasis.len == expressions.len + 1` |
| `template_element` | the text parts between `${}` | `raw` is the source text; `cooked` is the escape-decoded content (empty when `is_cooked_undefined`); `tail: bool` marks the last segment |

---

### Identifiers

Four distinct identifier node types all carry a single `name: String` field:

| Node | Used For |
|------|----------|
| `identifier_reference` | A name used as a value: `x`, `console`, `Math` |
| `binding_identifier` | A name being declared: `const x`, `function foo`, `import { x }` |
| `identifier_name` | A bare name in non-expression position: object keys (`{foo: 1}`), member access right-hand side (`obj.foo`), `import.meta` |
| `label_identifier` | A label name in `break label`, `continue label`, or `label: stmt` |
| `private_identifier` | A private class member: `#field` (the `#` is not part of `name`) |

:::note
This distinction matters in the semantic traverser. `identifier_reference` nodes are recorded as references, `binding_identifier` nodes are recorded as declarations, and `identifier_name` nodes are never resolved against the scope chain.

```js
const foo = bar.baz;
//    ^^^   ^^^ ^^^
//    |     |   IdentifierName (property, never resolved)
//    |     IdentifierReference (variable use, resolved by scope chain)
//    BindingIdentifier (declaration, recorded as a symbol)
```

:::

---

### Patterns (Destructuring)

| Node | JS Syntax | Notes |
|------|-----------|-------|
| `array_pattern` | `[a, , b, ...rest]` | `elements` may include `.null` for holes; `rest` is `.null` if absent |
| `object_pattern` | `{a, b: c, ...rest}` | `properties` contains `binding_property` nodes; `rest` is `.null` if absent |
| `binding_property` | `key: value` or shorthand `key` | `shorthand: bool`, `computed: bool` |
| `assignment_pattern` | `x = default` | Used for default values in destructuring and function parameters |
| `binding_rest_element` | `...rest` | `argument` is the binding pattern the rest collects into |
| `formal_parameters` | `(a, b = 1, ...rest)` | `items` contains `formal_parameter` nodes; `rest` is `.null` if absent |
| `formal_parameter` | a single parameter slot | `pattern` is the binding (may be any binding pattern, with optional default via `assignment_pattern`) |

---

### Functions

```zig
pub const Function = struct {
    type: FunctionType,    // declaration, expression, or TS forms
    id: NodeIndex,         // BindingIdentifier (.null for anonymous functions)
    generator: bool,       // true for function*
    async: bool,           // true for async function
    params: NodeIndex,     // FormalParameters
    body: NodeIndex,       // FunctionBody (.null for TS overloads/abstract methods)
};
```

| Node | Description |
|------|-------------|
| `function` | All named and anonymous function forms. Use `type`, `generator`, and `async` to distinguish them. |
| `function_body` | The `{ ... }` body of a function. Contains directives and statements in `body: IndexRange`. |
| `arrow_function_expression` | Arrow functions. `expression: bool` is `true` when the body is an expression (not a block). `async: bool` for async arrows. |

---

### Classes

```zig
pub const Class = struct {
    type: ClassType,        // class_declaration or class_expression
    decorators: IndexRange, // Decorator[] (empty if none)
    id: NodeIndex,          // BindingIdentifier (.null for anonymous expressions)
    super_class: NodeIndex, // Expression (.null if no extends clause)
    body: NodeIndex,        // ClassBody
};
```

| Node | Description |
|------|-------------|
| `class` | Both `class Foo {}` and `const x = class {}`. Check `type`. |
| `class_body` | The `{ members }` block. `body` contains `method_definition`, `property_definition`, and `static_block` nodes. |
| `method_definition` | A method, getter, setter, or constructor. `kind` is a `MethodDefinitionKind` enum (`constructor`, `method`, `get`, `set`). `static: bool`, `computed: bool`. |
| `property_definition` | A class field (`x = 1`). `value` is `.null` for fields without an initializer. `accessor: bool` for auto-accessors. `static: bool`, `computed: bool`. |
| `static_block` | `static { ... }`. `body` is a list of statements. |
| `decorator` | `@expr`. `expression` is the decorator expression node. |
| `super` | The `super` keyword. No fields. |

---

### Modules

| Node | JS Syntax | Notes |
|------|-----------|-------|
| `import_declaration` | `import x from 'y'` | `specifiers` is empty for side-effect-only imports; `phase` is `.source` or `.defer` for staged imports, `null` for regular |
| `import_specifier` | `{ imported as local }` | |
| `import_default_specifier` | `import x from ...` | `local` is the binding |
| `import_namespace_specifier` | `import * as x from ...` | `local` is the binding |
| `import_attribute` | `{ type: "json" }` | Import attributes / assertions |
| `export_named_declaration` | `export { x }`, `export var x` | `declaration` is `.null` for specifier-only; `source` is `.null` for local (non-re-export) |
| `export_default_declaration` | `export default expr` | `declaration` is an expression, function, or class |
| `export_all_declaration` | `export * from 'y'` | `exported` is `.null` for `export *`; non-null for `export * as name` |
| `export_specifier` | `{ local as exported }` | |

---

### JSX

JSX nodes are only present in `.jsx` and `.tsx` trees.

| Node | JSX Syntax | Notes |
|------|------------|-------|
| `jsx_element` | `<Foo>...</Foo>` | `closing_element` is `.null` for self-closing tags |
| `jsx_opening_element` | `<Foo ...>` | `self_closing: bool` for `<Foo />`; `name` is a JSX name node |
| `jsx_closing_element` | `</Foo>` | |
| `jsx_fragment` | `<>...</>` | |
| `jsx_opening_fragment` | `<>` | No fields |
| `jsx_closing_fragment` | `</>` | No fields |
| `jsx_identifier` | `Foo` in JSX position | `name: String` |
| `jsx_namespaced_name` | `namespace:name` | |
| `jsx_member_expression` | `Foo.Bar.Baz` | |
| `jsx_attribute` | `foo="bar"` or `foo={expr}` | `value` is `.null` for boolean attributes like `disabled` |
| `jsx_spread_attribute` | `{...props}` | `argument` is the spread expression |
| `jsx_expression_container` | `{expression}` | `expression` is a `jsx_empty_expression` node for `{}` |
| `jsx_empty_expression` | `{}` | No fields |
| `jsx_text` | Text content between tags | `raw: String` |
| `jsx_spread_child` | `{...children}` | `expression` is the spread expression |

---

### TypeScript

| Node | TS Syntax | Notes |
|------|-----------|-------|
| `ts_export_assignment` | `export = expr` | CommonJS-style TypeScript module export |
| `ts_namespace_export_declaration` | `export as namespace name` | UMD global namespace declaration |
