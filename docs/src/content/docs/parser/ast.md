---
title: AST
description: The Abstract Syntax Tree produced by Yuku's parser
---

In Node.js, Yuku produces the same AST as [Oxc](https://oxc.rs):

- **JavaScript / JSX** — Fully conformant with the [ESTree](https://github.com/estree/estree) standard, identical to the AST produced by [Acorn](https://www.npmjs.com/package/acorn).
- **TypeScript** — Conforms to the [TS-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) format used by `@typescript-eslint`.

The only extensions beyond the base specs are support for Stage 3 [decorators](https://github.com/tc39/proposal-decorators), [import defer](https://github.com/tc39/proposal-defer-import-eval), [import source](https://github.com/tc39/proposal-source-phase-imports), and a non-standard `hashbang` field on `Program`.

This page covers the structure of the AST, all node types, and how to work with them.

## Node Storage

All AST nodes are stored in a single flat array (`tree.nodes`). Instead of heap-allocated tree nodes connected by pointers, every node is identified by a `NodeIndex`, a simple `u32` that indexes into this array.

```
NodeIndex  -->  nodes[i]  -->  { data: NodeData, span: Span }
```

Each node has two parts:

- **`data`**: A tagged union (`NodeData`) that tells you what kind of node it is and holds its fields.
- **`span`**: The byte range `{ start: u32, end: u32 }` in the original source code.

This flat layout gives excellent cache locality during traversals, because all node data is packed contiguously in memory.

### Node Data

`NodeData` is a Zig tagged union with a variant for every possible AST node type. When you read a node's data, you switch on the tag to determine the node type and access its fields:

```zig
const data = tree.getData(node_index);
switch (data) {
    .variable_declaration => |decl| {
        // decl.kind is .var, .let, .const, .using, or .await_using
        // decl.declarators is an IndexRange into tree.extra
    },
    .function => |func| {
        // func.id, func.params, func.body are NodeIndex values
        // func.async, func.generator are booleans
    },
    // ... other node types
    else => {},
}
```

### Variable-Length Children

Some nodes have a variable number of children (e.g., function arguments, array elements, block statements). These are stored using `IndexRange`, which points to a contiguous slice in the `extra` array:

```zig
// IndexRange = { start: u32, len: u32 }
// Points into tree.extra, a flat array of NodeIndex values

const block_data = tree.getData(some_block).block_statement;
const statements = tree.getExtra(block_data.body);

for (statements) |stmt| {
    const stmt_data = tree.getData(stmt);
    // process each statement...
}
```

### Optional Nodes

Optional child nodes use `null_node` (the maximum `u32` value) to indicate "not present":

```zig
const ast = parser.ast;

const if_stmt = tree.getData(node).if_statement;

// The else clause is optional
if (!ast.isNull(if_stmt.alternate)) {
    const else_body = tree.getData(if_stmt.alternate);
    // process else...
}
```

## Zero-Copy Source References

String values in the AST (identifiers, string literals, numeric literals, etc.) are not copied. Instead, each node stores a `_start` (u32) and `_len` (u16) that reference directly into the original source buffer:

```zig
const id = tree.getData(node).identifier_reference;
const name = tree.getSourceText(id.name_start, id.name_len);
// "name" is a slice of the original source -- no allocation
```

This applies to all text-carrying nodes: identifiers, string literals, numeric literals, BigInt literals, regex patterns, template elements, and more. The original source bytes are reused, avoiding any string allocation or copying during parsing.

## Node Types

Below is a complete reference of every AST node type, grouped by category.

### Program

The root node of every AST.

| Node | Fields | Description |
|------|--------|-------------|
| `program` | `source_type`, `body: IndexRange`, `hashbang: ?Hashbang` | Root node. `body` contains all top-level statements. |
| `directive` | `expression: NodeIndex`, `value_start`, `value_len` | String directives like `"use strict"`. |

### Statements

| Node | Fields | Description |
|------|--------|-------------|
| `block_statement` | `body: IndexRange` | `{ ... }` block |
| `expression_statement` | `expression: NodeIndex` | `expr;` |
| `empty_statement` | (none) | `;` |
| `debugger_statement` | (none) | `debugger;` |
| `return_statement` | `argument: NodeIndex` | `return expr;` (argument may be null_node) |
| `throw_statement` | `argument: NodeIndex` | `throw expr;` |
| `break_statement` | `label: NodeIndex` | `break;` or `break label;` |
| `continue_statement` | `label: NodeIndex` | `continue;` or `continue label;` |
| `labeled_statement` | `label: NodeIndex`, `body: NodeIndex` | `label: stmt` |
| `if_statement` | `test`, `consequent`, `alternate: NodeIndex` | `if (test) consequent else alternate` |
| `switch_statement` | `discriminant: NodeIndex`, `cases: IndexRange` | `switch (x) { ... }` |
| `switch_case` | `test: NodeIndex`, `consequent: IndexRange` | `case x:` or `default:` (test is null_node for default) |
| `for_statement` | `init`, `test`, `update`, `body: NodeIndex` | `for (init; test; update) body` |
| `for_in_statement` | `left`, `right`, `body: NodeIndex` | `for (left in right) body` |
| `for_of_statement` | `left`, `right`, `body: NodeIndex`, `await: bool` | `for (left of right) body` |
| `while_statement` | `test`, `body: NodeIndex` | `while (test) body` |
| `do_while_statement` | `body`, `test: NodeIndex` | `do body while (test)` |
| `try_statement` | `block`, `handler`, `finalizer: NodeIndex` | `try { } catch { } finally { }` |
| `catch_clause` | `param: NodeIndex`, `body: NodeIndex` | `catch (param) { body }` |
| `with_statement` | `object`, `body: NodeIndex` | `with (object) body` |

### Declarations

| Node | Fields | Description |
|------|--------|-------------|
| `variable_declaration` | `kind: VariableKind`, `declarators: IndexRange` | `var`/`let`/`const`/`using`/`await using` |
| `variable_declarator` | `id: NodeIndex`, `init: NodeIndex` | `pattern = initializer` |
| `function` | `type`, `id`, `params`, `body: NodeIndex`, `generator`, `async: bool` | Function declaration or expression |
| `class` | `type`, `decorators: IndexRange`, `id`, `super_class`, `body: NodeIndex` | Class declaration or expression |
| `class_body` | `body: IndexRange` | Contents of a class |
| `method_definition` | `decorators: IndexRange`, `key`, `value: NodeIndex`, `kind`, `computed`, `static: bool` | Class method |
| `property_definition` | `decorators: IndexRange`, `key`, `value: NodeIndex`, `computed`, `static`, `accessor: bool` | Class field |
| `static_block` | `body: IndexRange` | `static { ... }` |
| `decorator` | `expression: NodeIndex` | `@expression` |

### Expressions

| Node | Fields | Description |
|------|--------|-------------|
| `identifier_reference` | `name_start`, `name_len` | Variable reference (`foo`) |
| `this_expression` | (none) | `this` |
| `super` | (none) | `super` |
| `null_literal` | (none) | `null` |
| `boolean_literal` | `value: bool` | `true` or `false` |
| `numeric_literal` | `raw_start`, `raw_len`, `kind` | `42`, `0xFF`, `0o77`, `0b10` |
| `bigint_literal` | `raw_start`, `raw_len` | `42n` |
| `string_literal` | `raw_start`, `raw_len` | `"hello"` or `'hello'` |
| `regexp_literal` | `pattern_start`, `pattern_len`, `flags_start`, `flags_len` | `/pattern/flags` |
| `template_literal` | `quasis: IndexRange`, `expressions: IndexRange` | `` `hello ${name}` `` |
| `template_element` | `raw_start`, `raw_len`, `tail`, `is_cooked_undefined` | Text part of a template |
| `tagged_template_expression` | `tag`, `quasi: NodeIndex` | `` tag`template` `` |
| `array_expression` | `elements: IndexRange` | `[a, b, c]` |
| `object_expression` | `properties: IndexRange` | `{a: 1, b}` |
| `object_property` | `key`, `value: NodeIndex`, `kind`, `method`, `shorthand`, `computed: bool` | Property in object literal |
| `spread_element` | `argument: NodeIndex` | `...expr` |
| `unary_expression` | `argument: NodeIndex`, `operator` | `!x`, `typeof x`, `-x` |
| `update_expression` | `argument: NodeIndex`, `operator`, `prefix: bool` | `++x` or `x++` |
| `binary_expression` | `left`, `right: NodeIndex`, `operator` | `a + b`, `a === b` |
| `logical_expression` | `left`, `right: NodeIndex`, `operator` | `a && b`, `a \|\| b`, `a ?? b` |
| `assignment_expression` | `left`, `right: NodeIndex`, `operator` | `a = b`, `a += b` |
| `conditional_expression` | `test`, `consequent`, `alternate: NodeIndex` | `a ? b : c` |
| `sequence_expression` | `expressions: IndexRange` | `a, b, c` |
| `call_expression` | `callee: NodeIndex`, `arguments: IndexRange`, `optional: bool` | `f(a, b)` or `f?.(a)` |
| `new_expression` | `callee: NodeIndex`, `arguments: IndexRange` | `new Foo(a)` |
| `member_expression` | `object`, `property: NodeIndex`, `computed`, `optional: bool` | `a.b`, `a[b]`, `a?.b` |
| `chain_expression` | `expression: NodeIndex` | Wrapper for optional chaining |
| `parenthesized_expression` | `expression: NodeIndex` | `(expr)` |
| `arrow_function_expression` | `expression`, `async: bool`, `params`, `body: NodeIndex` | `(a) => b` |
| `yield_expression` | `argument: NodeIndex`, `delegate: bool` | `yield x` or `yield* x` |
| `await_expression` | `argument: NodeIndex` | `await x` |
| `import_expression` | `source`, `options: NodeIndex`, `phase: ?ImportPhase` | `import("x")` |
| `meta_property` | `meta`, `property: NodeIndex` | `import.meta` or `new.target` |

### Functions and Parameters

| Node | Fields | Description |
|------|--------|-------------|
| `function` | `type`, `id`, `params`, `body: NodeIndex`, `generator`, `async: bool` | Shared for declarations and expressions |
| `function_body` | `body: IndexRange` | Statements inside a function |
| `formal_parameters` | `items: IndexRange`, `rest: NodeIndex`, `kind` | Parameter list `(a, b, ...rest)` |
| `formal_parameter` | `pattern: NodeIndex` | Single parameter wrapping a binding pattern |

`FormalParameterKind` distinguishes between:
- `formal_parameters` -- regular function params
- `unique_formal_parameters` -- params in methods, no duplicate names allowed
- `arrow_formal_parameters` -- arrow function params

### Patterns (Destructuring)

Patterns appear in variable declarations, function parameters, assignment targets, and catch clauses.

| Node | Fields | Description |
|------|--------|-------------|
| `binding_identifier` | `name_start`, `name_len` | Simple identifier binding (`x`) |
| `array_pattern` | `elements: IndexRange`, `rest: NodeIndex` | `[a, b, ...rest]` |
| `object_pattern` | `properties: IndexRange`, `rest: NodeIndex` | `{a, b: c, ...rest}` |
| `binding_property` | `key`, `value: NodeIndex`, `shorthand`, `computed: bool` | Property in object pattern |
| `assignment_pattern` | `left`, `right: NodeIndex` | `pattern = default` |
| `binding_rest_element` | `argument: NodeIndex` | `...rest` |

Array pattern elements can be `null_node` to represent holes: `[a, , b]`.

### Identifiers

Yuku distinguishes between different identifier roles:

| Node | Fields | Description |
|------|--------|-------------|
| `identifier_reference` | `name_start`, `name_len` | Expression position (reading a variable) |
| `binding_identifier` | `name_start`, `name_len` | Declaration position (declaring a variable) |
| `identifier_name` | `name_start`, `name_len` | Property keys, labels |
| `label_identifier` | `name_start`, `name_len` | Labels in `break`/`continue` |
| `private_identifier` | `name_start`, `name_len` | `#name` (the stored name excludes the `#` prefix) |

All identifier types store source position and length, referencing the original source text directly.

### Imports and Exports

| Node | Fields | Description |
|------|--------|-------------|
| `import_declaration` | `specifiers: IndexRange`, `source`, `attributes: IndexRange`, `phase: ?ImportPhase` | `import ... from "x"` |
| `import_specifier` | `imported`, `local: NodeIndex` | `{imported as local}` |
| `import_default_specifier` | `local: NodeIndex` | `import local from "x"` |
| `import_namespace_specifier` | `local: NodeIndex` | `import * as local from "x"` |
| `import_attribute` | `key`, `value: NodeIndex` | `with { type: "json" }` |
| `export_named_declaration` | `declaration`, `source: NodeIndex`, `specifiers`, `attributes: IndexRange` | `export { a, b }` |
| `export_default_declaration` | `declaration: NodeIndex` | `export default expr` |
| `export_all_declaration` | `exported`, `source: NodeIndex`, `attributes: IndexRange` | `export * from "x"` |
| `export_specifier` | `local`, `exported: NodeIndex` | `{local as exported}` |

Import phases support source phase imports (`import source x from "x"`) and deferred imports (`import defer * as x from "x"`).

### JSX

| Node | Fields | Description |
|------|--------|-------------|
| `jsx_element` | `opening_element`, `closing_element: NodeIndex`, `children: IndexRange` | `<Foo>...</Foo>` |
| `jsx_opening_element` | `name: NodeIndex`, `attributes: IndexRange`, `self_closing: bool` | `<Foo bar={x}>` |
| `jsx_closing_element` | `name: NodeIndex` | `</Foo>` |
| `jsx_fragment` | `opening_fragment`, `closing_fragment: NodeIndex`, `children: IndexRange` | `<>...</>` |
| `jsx_identifier` | `name_start`, `name_len` | Tag and attribute names |
| `jsx_namespaced_name` | `namespace`, `name: NodeIndex` | `ns:name` |
| `jsx_member_expression` | `object`, `property: NodeIndex` | `Foo.Bar` |
| `jsx_attribute` | `name`, `value: NodeIndex` | `name={value}` or `name="str"` |
| `jsx_spread_attribute` | `argument: NodeIndex` | `{...props}` |
| `jsx_expression_container` | `expression: NodeIndex` | `{expr}` |
| `jsx_empty_expression` | (none) | `{}` (empty expression container) |
| `jsx_text` | `raw_start`, `raw_len` | Text content between tags |

## Operators

The AST uses dedicated enums for all operator types:

### BinaryOperator
`equal`, `not_equal`, `strict_equal`, `strict_not_equal`, `less_than`, `less_than_or_equal`, `greater_than`, `greater_than_or_equal`, `add`, `subtract`, `multiply`, `divide`, `modulo`, `exponent`, `bitwise_or`, `bitwise_xor`, `bitwise_and`, `left_shift`, `right_shift`, `unsigned_right_shift`, `in`, `instanceof`

### LogicalOperator
`and` (`&&`), `or` (`||`), `nullish_coalescing` (`??`)

### UnaryOperator
`negate` (`-`), `positive` (`+`), `logical_not` (`!`), `bitwise_not` (`~`), `typeof`, `void`, `delete`

### UpdateOperator
`increment` (`++`), `decrement` (`--`)

### AssignmentOperator
`assign`, `add_assign`, `subtract_assign`, `multiply_assign`, `divide_assign`, `modulo_assign`, `exponent_assign`, `left_shift_assign`, `right_shift_assign`, `unsigned_right_shift_assign`, `bitwise_or_assign`, `bitwise_xor_assign`, `bitwise_and_assign`, `logical_or_assign`, `logical_and_assign`, `nullish_assign`

## Comments

Comments are collected separately from the AST during parsing:

```zig
for (tree.comments) |comment| {
    // comment.type is .line or .block
    // comment.start, comment.end are byte positions
    const text = comment.getValue(tree.source);
    // For line comments: text excludes the "//" prefix
    // For block comments: text excludes "/*" and "*/"
}
```

## Diagnostics

Diagnostics provide detailed error reporting:

```zig
for (tree.diagnostics) |d| {
    // d.severity: .error, .warning, .hint, .info
    // d.message: human-readable description
    // d.span: { .start, .end } byte offsets in source
    // d.help: optional suggestion text
    // d.labels: additional highlighted regions with messages
}
```

Labels allow a single diagnostic to highlight multiple source locations. For example, an "unterminated string" error might label where the string was opened.
