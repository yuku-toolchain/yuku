---
title: AST
description: The flat, data-oriented AST returned by Yuku's parser, with the full node reference.
---

The AST that comes out of `parser.parse()` is a flat array of nodes that reference each other by integer index. Reading and walking it is fast, predictable, and explicit. There are no boxed structs, no virtual dispatch, no surprise allocations. Every operation is a tagged-union switch and a slice index away.

The same tree, when exposed through the [`yuku-parser`](https://www.npmjs.com/package/yuku-parser) npm package, becomes [ESTree](https://github.com/estree/estree)-compatible output matching [Oxc](https://oxc.rs):

- **JavaScript / JSX**: fully conformant with [ESTree](https://github.com/estree/estree), identical to [Acorn](https://www.npmjs.com/package/acorn).
- **TypeScript**: conforms to [TS-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) used by `@typescript-eslint`.

Beyond the base specs the AST also carries Stage 3 [decorators](https://github.com/tc39/proposal-decorators), [import defer](https://github.com/tc39/proposal-defer-import-eval), [import source](https://github.com/tc39/proposal-source-phase-imports), and a `hashbang` field on `program`.

:::tip[Building tools on the AST?]
The [traverser](/parser/traverse) is the recommended way to work with the AST. It gives you ergonomic visitor hooks, scopes, symbols, and transforms, everything you need to walk, analyze, and rewrite the tree without managing indices yourself. Reach for it first when building lints, codemods, or any pass over the tree.

This page covers the AST itself, the node types, their fields, and how to read them directly when you need to.
:::

## Memory model

The AST is not a graph of heap-allocated structs. Every node lives in a single flat array (`Tree.nodes`), and child references are indices into that array. Variable-length child lists live in a second flat array (`Tree.extras`), and string content lives in a string pool. Three arrays, one arena.

```
Tree
 nodes    NodeList         flat array of all nodes (data + span, struct-of-arrays)
 extras   []NodeIndex      variable-length child lists (IndexRange points here)
 strings  StringPool       all string content (source refs + interned extras)
```

`NodeList` is a `MultiArrayList(Node)`, so `data` and `span` are stored in two separate parallel arrays. Code that only reads spans, or only reads data, touches one array.

All memory is owned by a single `ArenaAllocator`. `tree.deinit()` frees the entire tree at once.

## The Tree

`Tree` is the root container returned by `parser.parse()`.

| Field         | Type                    | Description                             |
| ------------- | ----------------------- | --------------------------------------- |
| `root`        | `NodeIndex`             | Index of the root node (a `program`)    |
| `nodes`       | `NodeList`              | All AST nodes                           |
| `extras`      | `ArrayList(NodeIndex)`  | Variable-length child index lists       |
| `diagnostics` | `ArrayList(Diagnostic)` | Parse errors, warnings, hints           |
| `comments`    | `[]const Comment`       | All comments found in source            |
| `source`      | `[]const u8`            | Original source text                    |
| `source_type` | `SourceType`            | `.script` or `.module`                  |
| `lang`        | `Lang`                  | `.js`, `.ts`, `.jsx`, `.tsx`, or `.dts` |

```zig
tree.data(idx)           // NodeData for the node at idx
tree.span(idx)           // Span (source byte range) for the node at idx
tree.extra(range)        // []const NodeIndex for an IndexRange
tree.string(handle)      // []const u8 for a String handle

tree.isTs()              // language is .ts, .tsx, or .dts
tree.isJsx()             // language is .jsx or .tsx
tree.isModule()          // source_type is .module
tree.hasErrors()         // any diagnostic with severity .error
```

## Core types

Four small types carry every reference inside the AST.

### NodeIndex

```zig
pub const NodeIndex = enum(u32) { null = std.math.maxInt(u32), _ };
```

Every node is identified by its position in `Tree.nodes`. Optional child slots use `.null` to signal absence.

```zig
// if_statement.alternate is .null when there is no else branch
if (node.alternate != .null) {
    const else_data = tree.data(node.alternate);
}
```

### IndexRange

```zig
pub const IndexRange = struct { start: u32, len: u32 };
```

Variable-length children are stored as a contiguous slice in `Tree.extras`. An `IndexRange` is a `(start, len)` window into that array. Resolve it with `tree.extra(range)`:

```zig
const children = tree.extra(node.body); // []const NodeIndex
for (children) |child| {
    const child_data = tree.data(child);
}
```

`IndexRange.empty` is the zero-length range.

### String

```zig
pub const String = struct { start: u32, end: u32 };
```

`String` is a lightweight handle to text. It points into one of two backing stores:

- **Source slice (zero-copy)**: most identifiers and string literals parsed from input. The bytes live inside `tree.source` directly.
- **Pool entry**: interned strings such as escaped identifiers and names produced by transforms. These live in the string pool's extra buffer.

`tree.string(handle)` resolves both transparently and always returns `[]const u8`:

```zig
const name = tree.string(node.name);
```

### Span

```zig
pub const Span = struct { start: u32, end: u32 };
```

Byte offsets into the source text. `start` is inclusive, `end` is exclusive.

```zig
const span = tree.span(idx);
const text = tree.source[span.start..span.end];
```

## Reading a node

`NodeData` is a tagged union with one variant per node type. The variant tags are snake_case (`binary_expression`, `if_statement`, `ts_type_alias_declaration`, ...). `tree.data(idx)` returns one. `switch` on the tag and unpack:

```zig
switch (tree.data(idx)) {
    .binary_expression => |expr| {
        // expr.left and expr.right are NodeIndex (recurse with data)
        // expr.operator is a BinaryOperator enum
        const left = tree.data(expr.left);
    },
    .variable_declaration => |decl| {
        // decl.kind is VariableKind (.var, .let, .const, .using, .await_using)
        // decl.declarators is IndexRange (read with extra)
        for (tree.extra(decl.declarators)) |d| { /* ... */ }
    },
    .identifier_reference => |id| {
        // id.name is a String (resolve with string)
        const text = tree.string(id.name);
    },
    else => {},
}
```

The same snake_case tag names are used as visitor hook names: a method called `enter_binary_expression` on your visitor struct fires when the [traverser](/parser/traverse) enters that node kind.

### Reading children

A node's children sit in two kinds of fields:

- **Single child**: `NodeIndex`. Either a real index, or `.null` for an absent optional slot. Read with `tree.data(field)`.
- **Variadic children**: `IndexRange`. Resolve with `tree.extra(range)` to get `[]const NodeIndex`, then iterate.

```zig
switch (tree.data(idx)) {
    .function => |func| {
        // single child
        const body = tree.data(func.body);

        // variadic children: function params -> formal_parameters -> items
        const params = tree.data(func.params).formal_parameters;
        for (tree.extra(params.items)) |param| { /* ... */ }
    },
    else => {},
}
```

For tree-wide walks, use the [traverser](/parser/traverse) instead of writing recursion by hand. It handles every node kind correctly without per-tag bookkeeping.

## Predicates

Seven methods on `NodeData` answer the categorical questions linters and analyzers ask most often. They collapse a family of tags into a single boolean.

| Method            | True for                                                                                                                                                                                                                                                                                                  |
| ----------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `isExpression()`  | Any node that produces a value at runtime: literals, identifier references, operator expressions, member access, calls, function and class expression forms, JSX elements, and the TypeScript value-position wrappers.                                                                                  |
| `isStatement()`   | Any node valid at statement position: control flow, structural statements, declarations, imports, exports, and TypeScript top-level declarations. Function and class declaration forms are included; expression forms are not.                                                                          |
| `isLiteral()`     | `string_literal`, `numeric_literal`, `bigint_literal`, `boolean_literal`, `null_literal`, `regexp_literal`, `template_literal`.                                                                                                                                                                            |
| `isCallable()`    | `function` (any form) and `arrow_function_expression`. Excludes `method_definition`, which wraps a `function` in its `value` field.                                                                                                                                                                       |
| `isPattern()`     | `binding_identifier`, `array_pattern`, `object_pattern`, `assignment_pattern`.                                                                                                                                                                                                                            |
| `isDeclaration()` | `variable_declaration`, function and class declaration forms, `import_declaration`, `export_named_declaration`, `export_default_declaration`, `export_all_declaration`, `ts_type_alias_declaration`, `ts_interface_declaration`, `ts_enum_declaration`, `ts_module_declaration`, `ts_global_declaration`, `ts_import_equals_declaration`. |
| `isIteration()`   | `for_statement`, `for_in_statement`, `for_of_statement`, `while_statement`, `do_while_statement`. Useful for `break` and `continue` scope checks.                                                                                                                                                         |

For dual-purpose nodes (`function` and `class`) the predicates consult the `type` field internally, so `isExpression()` returns true only for the expression forms and `isStatement()` / `isDeclaration()` only for the declaration forms.

```zig
const data = tree.data(idx);

if (data.isExpression()) {
    // any value-producing node
}

if (data.isCallable()) {
    // function or arrow_function_expression
    // The body, params, etc. are still type-specific,
    // so switch on the tag to access them.
}
```

For anything narrower than these seven, `switch` directly:

```zig
switch (data) {
    .arrow_function_expression => |arrow| { /* ... */ },
    else => {},
}
```

## Node reference

Every entry in `NodeData` is a distinct node tag. The tag name is the exact name used in `tree.data()` switches and visitor hooks (`enter_<tag>`).

Optional child fields are noted with `.null`. Optional child lists are noted with `.empty`.

---

### Program

The root of every tree. There is always exactly one `program` node at `tree.root`.

```zig
pub const Program = struct {
    source_type: SourceType,    // .script or .module
    body: IndexRange,           // (any statement | directive)[]
    hashbang: ?Hashbang,        // non-null for #!/usr/bin/env node lines
};
```

`directive` nodes (such as `"use strict";`) appear at the start of the body. Imports and exports appear in source order alongside other statements.

---

### Statements

| Tag                    | Syntax                          | Description                                                                       |
| ---------------------- | ------------------------------- | --------------------------------------------------------------------------------- |
| `expression_statement` | `expr;`                         | An expression used as a statement.                                                |
| `block_statement`      | `{ ... }`                       | A braced block.                                                                   |
| `empty_statement`      | `;`                             | A standalone semicolon.                                                           |
| `debugger_statement`   | `debugger;`                     | A debugger breakpoint.                                                            |
| `if_statement`         | `if (test) cons else alt`       | An `if` / `else` branch.                                                          |
| `switch_statement`     | `switch (d) { cases }`          | A `switch` with one or more `case` and `default` clauses.                         |
| `switch_case`          | `case x: ...` / `default: ...`  | A single clause inside a `switch`.                                                |
| `for_statement`        | `for (init; test; update) body` | A C-style `for` loop.                                                             |
| `for_in_statement`     | `for (x in y) body`             | A `for ... in` loop iterating over enumerable property keys.                      |
| `for_of_statement`     | `for (x of y) body`             | A `for ... of` or `for await ... of` loop iterating over an iterable.             |
| `while_statement`      | `while (test) body`             | A `while` loop.                                                                   |
| `do_while_statement`   | `do body while (test)`          | A `do ... while` loop.                                                            |
| `break_statement`      | `break;` / `break label;`       | A `break` exiting the nearest loop, switch, or labeled statement.                 |
| `continue_statement`   | `continue;` / `continue label;` | A `continue` jumping to the next iteration of the nearest or labeled loop.        |
| `labeled_statement`    | `label: stmt`                   | A statement prefixed with a label that `break` and `continue` can target.         |
| `return_statement`     | `return;` / `return expr;`      | A `return` from the enclosing function.                                           |
| `throw_statement`      | `throw expr;`                   | A `throw` raising an exception.                                                   |
| `try_statement`        | `try {} catch {} finally {}`    | A `try` with optional `catch` and `finally` clauses.                              |
| `catch_clause`         | `catch (e) { body }`            | The `catch` clause of a `try`, with an optional binding.                          |
| `with_statement`       | `with (obj) body`               | A `with` block. Forbidden in strict mode.                                         |

---

### Declarations

| Tag                    | Syntax                        | Description                                                                       |
| ---------------------- | ----------------------------- | --------------------------------------------------------------------------------- |
| `variable_declaration` | `var/let/const/using x = ...` | A `var`, `let`, `const`, `using`, or `await using` declaration.                   |
| `variable_declarator`  | `x = init`                    | A single binding inside a variable declaration.                                   |
| `directive`            | `"use strict";`               | A directive prologue, only valid at the top of a function or module body.        |
| `function`             | `function foo() {}`           | Every function form (declaration, expression, ambient, body-less signature).      |
| `class`                | `class Foo {}`                | Both class declarations and class expressions.                                    |

`function` and `class` are dual-purpose nodes. The `type` field distinguishes the form:

```zig
// FunctionType
function_declaration                // function foo() {}
function_expression                 // const x = function () {}
ts_declare_function                 // declare function foo(): void
                                    // also: plain overload signatures
ts_empty_body_function_expression   // body-less class methods (overloads,
                                    // abstract, ambient)

// ClassType
class_declaration                   // class Foo {}
class_expression                    // const x = class {}
```

---

### Expressions

| Tag                          | Syntax                                  | Description                                                                          |
| ---------------------------- | --------------------------------------- | ------------------------------------------------------------------------------------ |
| `binary_expression`          | `a + b`, `a === b`, `a instanceof b`    | A non-logical, non-assignment binary operation.                                      |
| `logical_expression`         | `a && b`, `a \|\| b`, `a ?? b`          | A short-circuiting logical operation.                                                |
| `unary_expression`           | `!x`, `typeof x`, `void x`, `delete x`  | A unary prefix operation.                                                            |
| `update_expression`          | `x++`, `++x`, `x--`                     | A prefix or postfix increment or decrement.                                          |
| `assignment_expression`      | `x = y`, `x += y`, `x ??= y`            | An assignment or compound assignment.                                                |
| `conditional_expression`     | `test ? a : b`                          | A ternary expression.                                                                |
| `sequence_expression`        | `a, b, c`                               | A comma-separated sequence of expressions.                                           |
| `parenthesized_expression`   | `(expr)`                                | An expression wrapped in parentheses, preserved in the tree.                         |
| `member_expression`          | `obj.prop`, `obj[x]`, `obj.#priv`       | Property access, in static, computed, or optional form.                              |
| `call_expression`            | `fn(args)`, `fn?.()`                    | A function call, optionally with type arguments or optional invocation.              |
| `new_expression`             | `new Foo(args)`                         | A `new` constructor invocation.                                                      |
| `chain_expression`           | `a?.b`, `a?.()`                         | A wrapper that scopes optional-chain short-circuiting to a member or call chain.     |
| `tagged_template_expression` | `` tag`hello` ``                        | A template literal preceded by a tag function.                                       |
| `await_expression`           | `await expr`                            | An `await` of a promise inside an async context.                                     |
| `yield_expression`           | `yield expr`, `yield* expr`             | A `yield` or delegating `yield*` inside a generator.                                 |
| `meta_property`              | `import.meta`, `new.target`             | A meta property reference such as `import.meta` or `new.target`.                     |
| `array_expression`           | `[a, , b, ...c]`                        | An array literal, including holes and spread elements.                               |
| `object_expression`          | `{a: 1, b, ...c}`                       | An object literal, including spread elements.                                        |
| `object_property`            | `key: value`, getters, setters, methods | A property entry inside an object literal.                                           |
| `spread_element`             | `...expr`                               | A spread element used in arrays, calls, and object literals.                         |
| `import_expression`          | `import(src)`, `import.source(src)`     | A dynamic `import()` call or phased import.                                          |
| `this_expression`            | `this`                                  | The `this` keyword used as an expression.                                            |
| `super`                      | `super`                                 | The `super` keyword used as an expression head.                                      |

---

### Literals

| Tag                | Syntax                        | Description                                                                       |
| ------------------ | ----------------------------- | --------------------------------------------------------------------------------- |
| `string_literal`   | `"hello"`, `'world'`          | A string literal with escape sequences resolved.                                  |
| `numeric_literal`  | `42`, `0xFF`, `0o7`, `0b1010` | A numeric literal in decimal, hex, octal, or binary.                              |
| `bigint_literal`   | `42n`                         | A BigInt literal.                                                                 |
| `boolean_literal`  | `true`, `false`               | The `true` or `false` keyword as a value.                                         |
| `null_literal`     | `null`                        | The `null` literal.                                                               |
| `regexp_literal`   | `/pattern/flags`              | A regular expression literal.                                                     |
| `template_literal` | `` `hello ${name}` ``         | A template literal with zero or more interpolations.                              |
| `template_element` | text part between `${...}`    | A static text span inside a template literal.                                     |

---

### Identifiers

Five tags, all carrying a single `name: String` field. They are structurally identical but appear in different syntactic positions and resolve differently.

| Tag                    | Used for                                                                                                                   |
| ---------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| `identifier_reference` | A name used as a value: `x`, `console`, `Math`                                                                             |
| `binding_identifier`   | A name being declared: `const x`, `function foo`, `import { x }`                                                           |
| `identifier_name`      | A bare name in non-expression position: object keys (`{foo: 1}`), member access right-hand side (`obj.foo`), `import.meta` |
| `label_identifier`     | A label name in `break label`, `continue label`, or `label: stmt`                                                          |
| `private_identifier`   | A private class member: `#field` (the `#` is not part of `name`)                                                           |

```js
const foo = bar.baz;
//    ^^^   ^^^ ^^^
//    |     |   identifier_name (property, never resolved)
//    |     identifier_reference (variable use, resolved by scope chain)
//    binding_identifier (declaration, recorded as a symbol)
```

`binding_identifier` additionally carries decorators, an optional type annotation, and an optional `?` flag when it appears in a parameter position.

---

### Patterns (destructuring)

| Tag                    | Syntax                          | Description                                                                       |
| ---------------------- | ------------------------------- | --------------------------------------------------------------------------------- |
| `array_pattern`        | `[a, , b, ...rest]`             | An array destructuring pattern.                                                   |
| `object_pattern`       | `{a, b: c, ...rest}`            | An object destructuring pattern.                                                  |
| `binding_property`     | `key: value` or shorthand `key` | A single property inside an object pattern.                                       |
| `assignment_pattern`   | `x = default`                   | A binding pattern with a default value, used in destructuring and parameter lists.|
| `binding_rest_element` | `...rest`                       | A `...rest` element inside a binding pattern or parameter list.                   |
| `formal_parameters`    | `(a, b = 1, ...rest)`           | The parameter list of a function.                                                 |
| `formal_parameter`     | a single parameter slot         | A single parameter slot wrapping a binding pattern.                               |

---

### Functions

```zig
pub const Function = struct {
    type: FunctionType,    // declaration, expression, or TS forms
    id: NodeIndex,         // binding_identifier (.null for anonymous)
    generator: bool,       // true for function*
    async: bool,           // true for async function
    declare: bool,         // true for declare function
    params: NodeIndex,     // formal_parameters
    body: NodeIndex,       // function_body (.null for TS overloads / abstract)
    type_parameters: NodeIndex,  // ts_type_parameter_declaration or .null
    return_type: NodeIndex,      // ts_type_annotation or .null
};
```

| Tag                         | Description                                                                       |
| --------------------------- | --------------------------------------------------------------------------------- |
| `function`                  | Every named and anonymous function form, including ambient and body-less ones.    |
| `function_body`             | The braced body of a function.                                                    |
| `arrow_function_expression` | An arrow function, with either an expression or a block body.                     |

---

### Classes

```zig
pub const Class = struct {
    type: ClassType,           // class_declaration or class_expression
    decorators: IndexRange,    // decorator[] (empty if none)
    id: NodeIndex,             // binding_identifier (.null for anonymous expressions)
    super_class: NodeIndex,    // any expression (.null if no extends clause)
    body: NodeIndex,           // class_body
    type_parameters: NodeIndex,    // ts_type_parameter_declaration or .null
    super_type_arguments: NodeIndex, // ts_type_parameter_instantiation or .null
    implements: IndexRange,    // ts_class_implements[] (empty if none)
    abstract: bool,            // true for abstract class
    declare: bool,             // true for declare class
};
```

| Tag                   | Description                                                                       |
| --------------------- | --------------------------------------------------------------------------------- |
| `class`               | Both class declarations and class expressions.                                    |
| `class_body`          | The braced body of a class, holding its members.                                  |
| `method_definition`   | A method, getter, setter, or constructor inside a class.                          |
| `property_definition` | A class field or auto-accessor declaration.                                       |
| `static_block`        | A `static { ... }` initialization block inside a class.                           |
| `decorator`           | A decorator (`@expr`) applied to a class or class member.                         |
| `super`               | The `super` keyword used as an expression head.                                   |

---

### Modules

| Tag                          | Syntax                         | Description                                                                       |
| ---------------------------- | ------------------------------ | --------------------------------------------------------------------------------- |
| `import_declaration`         | `import x from 'y'`            | A static `import` declaration, including side-effect and phased forms.            |
| `import_specifier`           | `{ imported as local }`        | A named binding specifier in an import declaration.                               |
| `import_default_specifier`   | `import x from ...`            | The default-binding specifier in an import declaration.                           |
| `import_namespace_specifier` | `import * as x from ...`       | A `* as local` namespace import specifier.                                        |
| `import_attribute`           | `{ type: "json" }`             | A single attribute in a `with { ... }` clause on an import or export.             |
| `export_named_declaration`   | `export { x }`, `export var x` | An `export { ... }` or `export <decl>` declaration, with optional re-export.      |
| `export_default_declaration` | `export default expr`          | An `export default` declaration.                                                  |
| `export_all_declaration`     | `export * from 'y'`            | An `export * from "m"` or `export * as ns from "m"` declaration.                  |
| `export_specifier`           | `{ local as exported }`        | A named binding specifier in an export declaration.                               |

---

### JSX

JSX nodes are only present in `.jsx` and `.tsx` trees.

| Tag                        | Syntax                      | Description                                                                       |
| -------------------------- | --------------------------- | --------------------------------------------------------------------------------- |
| `jsx_element`              | `<Foo>...</Foo>`            | A JSX element, possibly self-closing.                                             |
| `jsx_opening_element`      | `<Foo ...>`                 | The opening tag of a JSX element.                                                 |
| `jsx_closing_element`      | `</Foo>`                    | The closing tag of a JSX element.                                                 |
| `jsx_fragment`             | `<>...</>`                  | A JSX fragment.                                                                   |
| `jsx_opening_fragment`     | `<>`                        | The opening `<>` of a JSX fragment.                                               |
| `jsx_closing_fragment`     | `</>`                       | The closing `</>` of a JSX fragment.                                              |
| `jsx_identifier`           | `Foo` in JSX position       | An identifier used as a JSX tag or attribute name.                                |
| `jsx_namespaced_name`      | `namespace:name`            | A namespaced JSX name.                                                            |
| `jsx_member_expression`    | `Foo.Bar.Baz`               | A dotted JSX tag name.                                                            |
| `jsx_attribute`            | `foo="bar"` or `foo={expr}` | A single JSX attribute, including boolean-only forms.                             |
| `jsx_spread_attribute`     | `{...props}`                | A spread attribute on a JSX element.                                              |
| `jsx_expression_container` | `{expression}`              | An `{ expression }` slot inside JSX.                                              |
| `jsx_empty_expression`     | `{}`                        | The empty `{}` placeholder inside a JSX expression slot.                          |
| `jsx_text`                 | text content between tags   | A span of raw text inside a JSX element or fragment.                              |
| `jsx_spread_child`         | `{...children}`             | A spread child inside a JSX element.                                              |

A JSX tag name (the `name` field on `jsx_opening_element`, `jsx_closing_element`, and one form of `jsx_attribute`) is one of `jsx_identifier`, `jsx_namespaced_name`, or `jsx_member_expression`.

A JSX child (entries in the `children` list on `jsx_element` and `jsx_fragment`) is one of `jsx_text`, `jsx_expression_container`, `jsx_spread_child`, `jsx_element`, or `jsx_fragment`.

---

## TypeScript

TypeScript nodes are present in `.ts`, `.tsx`, and `.dts` trees.

### Type wrapper

| Tag                  | Syntax | Description                                                                       |
| -------------------- | ------ | --------------------------------------------------------------------------------- |
| `ts_type_annotation` | `: T`  | A `: T` annotation wrapping an inner type. The span starts at the `:` token.      |

### Keyword types

Each keyword is its own zero-field node.

| Tag                     | Syntax       |
| ----------------------- | ------------ |
| `ts_any_keyword`        | `any`        |
| `ts_unknown_keyword`    | `unknown`    |
| `ts_never_keyword`      | `never`      |
| `ts_void_keyword`       | `void`       |
| `ts_null_keyword`       | `null`       |
| `ts_undefined_keyword`  | `undefined`  |
| `ts_string_keyword`     | `string`     |
| `ts_number_keyword`     | `number`     |
| `ts_bigint_keyword`     | `bigint`     |
| `ts_boolean_keyword`    | `boolean`    |
| `ts_symbol_keyword`     | `symbol`     |
| `ts_object_keyword`     | `object`     |
| `ts_intrinsic_keyword`  | `intrinsic`  |
| `ts_this_type`          | `this`       |

### Type references

| Tag                  | Syntax                | Description                                                                       |
| -------------------- | --------------------- | --------------------------------------------------------------------------------- |
| `ts_type_reference`  | `Foo`, `Promise<T>`   | A reference to a named type, optionally with type arguments.                      |
| `ts_qualified_name`  | `A.B.C`               | A left-associative dotted type name.                                              |
| `ts_type_query`      | `typeof console.log`  | The `typeof` type operator applied to a value reference.                          |
| `ts_import_type`     | `import("m").Foo<T>`  | A reference to a type imported from a module path, written in type position.      |

### Type parameters and arguments

| Tag                                | Syntax                  | Description                                                                       |
| ---------------------------------- | ----------------------- | --------------------------------------------------------------------------------- |
| `ts_type_parameter`                | `T`, `T extends U = V`  | A single type parameter introduced by a generic declaration.                      |
| `ts_type_parameter_declaration`    | `<T, U>`                | The `<...>` parameter list introduced by a generic declaration.                   |
| `ts_type_parameter_instantiation`  | `<number, string>`      | The `<...>` argument list applied at a call site, reference, or instantiation.    |

### Literal and template types

| Tag                          | Syntax                          | Description                                                                       |
| ---------------------------- | ------------------------------- | --------------------------------------------------------------------------------- |
| `ts_literal_type`            | `"hello"`, `42`, `true`, `-1`   | A literal value used in type position.                                            |
| `ts_template_literal_type`   | `` `Hello, ${N}!` ``            | A template literal in type position with one or more interpolations.              |

### Composite types

| Tag                       | Syntax                  | Description                                                                       |
| ------------------------- | ----------------------- | --------------------------------------------------------------------------------- |
| `ts_array_type`           | `T[]`                   | A postfix array type.                                                             |
| `ts_indexed_access_type`  | `T[K]`                  | An indexed access type that looks up a property type.                             |
| `ts_tuple_type`           | `[T, U?, ...V[]]`       | A fixed-length tuple type with positional, optional, rest, or named entries.      |
| `ts_named_tuple_member`   | `label: T`, `label?: T` | A labeled element inside a tuple type.                                            |
| `ts_optional_type`        | `T?` (in tuple slot)    | An optional element inside a tuple type.                                          |
| `ts_rest_type`            | `...T` (in tuple slot)  | A rest element inside a tuple type.                                               |

### Set-operation types

| Tag                     | Syntax                              | Description                                                                       |
| ----------------------- | ----------------------------------- | --------------------------------------------------------------------------------- |
| `ts_union_type`         | `A \| B \| C`                       | A union of two or more types.                                                     |
| `ts_intersection_type`  | `A & B & C`                         | An intersection of two or more types.                                             |
| `ts_conditional_type`   | `T extends U ? X : Y`               | A conditional type selecting between two branches.                                |
| `ts_infer_type`         | `infer R`, `infer R extends string` | An `infer` placeholder inside a conditional type's extends branch.                |

### Type operators

| Tag                       | Syntax                                     | Description                                                                       |
| ------------------------- | ------------------------------------------ | --------------------------------------------------------------------------------- |
| `ts_type_operator`        | `keyof T`, `unique symbol`, `readonly T[]` | A `keyof`, `unique`, or `readonly` prefix on an inner type.                       |
| `ts_parenthesized_type`   | `(T)`                                      | A parenthesized type used for grouping or precedence.                             |

### Callable types

| Tag                   | Syntax                                  | Description                                                                       |
| --------------------- | --------------------------------------- | --------------------------------------------------------------------------------- |
| `ts_function_type`    | `(x: T) => U`                           | A callable signature in type position.                                            |
| `ts_constructor_type` | `new (x: T) => U`, `abstract new ...`   | A constructor signature in type position, optionally `abstract`.                  |
| `ts_type_predicate`   | `x is T`, `asserts x is T`, `asserts x` | A type predicate that narrows a parameter or `this` in control-flow analysis.     |

### Object-shape types

| Tag                | Syntax              | Description                                                                       |
| ------------------ | ------------------- | --------------------------------------------------------------------------------- |
| `ts_type_literal`  | `{ x: T; y: U }`    | An anonymous object type holding a list of signatures.                            |
| `ts_mapped_type`   | `{ [K in T]: V }`   | A mapped type that projects every key in a union to a new property type.          |

### JSDoc types

| Tag                          | Syntax            | Description                                                                       |
| ---------------------------- | ----------------- | --------------------------------------------------------------------------------- |
| `ts_jsdoc_nullable_type`     | `?T` or `T?`      | A JSDoc-style nullable type marker.                                               |
| `ts_jsdoc_non_nullable_type` | `!T` or `T!`      | A JSDoc-style non-nullable type marker.                                           |
| `ts_jsdoc_unknown_type`      | `?` (in `Foo<?>`) | A JSDoc-style unknown type, valid only in a type argument slot.                   |

### Signature members

These appear inside `ts_type_literal.members` and `ts_interface_body.body`.

| Tag                                   | Syntax                                    | Description                                                                       |
| ------------------------------------- | ----------------------------------------- | --------------------------------------------------------------------------------- |
| `ts_property_signature`               | `key: T`, `readonly key?: T`              | A property declaration inside a type literal or interface body.                   |
| `ts_method_signature`                 | `m(x: T): U`, `get x(): T`, `set x(v: T)` | A method, getter, or setter declaration inside a type literal or interface body.  |
| `ts_call_signature_declaration`       | `(x: T): U`                               | A bare call signature inside a type literal or interface body.                    |
| `ts_construct_signature_declaration`  | `new (x: T): U`                           | A bare construct signature inside a type literal or interface body.               |
| `ts_index_signature`                  | `[k: K]: V`, `readonly [...]: V`          | An index signature inside a type literal, interface body, or class body.          |

### Type, interface, and enum declarations

| Tag                            | Syntax                                    | Description                                                                       |
| ------------------------------ | ----------------------------------------- | --------------------------------------------------------------------------------- |
| `ts_type_alias_declaration`    | `type Maybe<T> = T \| null`               | A `type` alias declaration, optionally generic and optionally ambient.            |
| `ts_interface_declaration`     | `interface Foo<T> extends Bar { ... }`    | An `interface` declaration, optionally generic and optionally ambient.            |
| `ts_interface_body`            | `{ ... }` of an interface                 | The body of an interface, holding its signature members.                          |
| `ts_interface_heritage`        | one entry of an `extends` clause          | A single parent listed in an interface's `extends` clause.                        |
| `ts_class_implements`          | one entry of an `implements` clause       | A single interface listed in a class's `implements` clause.                       |
| `ts_enum_declaration`          | `enum Color { ... }`                      | An `enum` declaration, optionally `const` and optionally ambient.                 |
| `ts_enum_body`                 | `{ ... }` of an enum                      | The body of an enum, holding its members in source order.                         |
| `ts_enum_member`               | `A = 1` inside an enum body               | A single member of an enum body, with an optional initializer.                    |

### Module and namespace declarations

| Tag                            | Syntax                                    | Description                                                                       |
| ------------------------------ | ----------------------------------------- | --------------------------------------------------------------------------------- |
| `ts_module_declaration`        | `namespace Foo { ... }`, `module "x" {}`  | A `namespace` or `module` declaration, optionally ambient.                        |
| `ts_module_block`              | the `{ ... }` of a module                 | The body of a `namespace`, `module`, or `declare global` declaration.             |
| `ts_global_declaration`        | `declare global { ... }`                  | A `declare global` augmentation block.                                            |

### Parameters and `this`

| Tag                       | Syntax                          | Description                                                                       |
| ------------------------- | ------------------------------- | --------------------------------------------------------------------------------- |
| `ts_parameter_property`   | `constructor(public x: T) {}`   | A constructor parameter that implicitly declares a class field.                   |
| `ts_this_parameter`       | `function f(this: T)`           | An explicit `this` parameter declaring the type of `this` in the function body.   |

### TypeScript expressions

| Tag                            | Syntax              | Description                                                                       |
| ------------------------------ | ------------------- | --------------------------------------------------------------------------------- |
| `ts_as_expression`             | `expr as T`         | A postfix `as` type assertion.                                                    |
| `ts_satisfies_expression`      | `expr satisfies T`  | A postfix `satisfies` constraint check.                                           |
| `ts_type_assertion`            | `<T>expr`           | A prefix `<T>` type assertion. Forbidden in `.tsx`.                               |
| `ts_non_null_expression`       | `expr!`             | A postfix non-null assertion.                                                     |
| `ts_instantiation_expression`  | `expr<T>`           | A type instantiation expression without call parentheses.                         |

### TypeScript module forms

| Tag                                | Syntax                                          | Description                                                                       |
| ---------------------------------- | ----------------------------------------------- | --------------------------------------------------------------------------------- |
| `ts_export_assignment`             | `export = expr`                                 | A CommonJS-style ambient export.                                                  |
| `ts_namespace_export_declaration`  | `export as namespace Name`                      | A UMD ambient namespace export.                                                   |
| `ts_import_equals_declaration`     | `import x = require("m")`, `import x = Foo.Bar` | An `import =` declaration binding to a require or entity name.                    |
| `ts_external_module_reference`     | `require("m")`                                  | The `require("module")` form on the right-hand side of `import x = require(...)`. |
