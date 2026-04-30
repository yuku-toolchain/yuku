---
title: AST
description: Internal AST structure, node types, and the data model for Yuku's parser.
---

Yuku's internal AST is a flat, arena-allocated structure optimized for sequential access. When serialized to JSON or exposed through Node.js bindings, it is converted to an [ESTree](https://github.com/estree/estree)-compatible format matching [Oxc](https://oxc.rs):

- **JavaScript / JSX**: Fully conformant with [ESTree](https://github.com/estree/estree), identical to [Acorn](https://www.npmjs.com/package/acorn).
- **TypeScript**: Conforms to [TS-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) used by `@typescript-eslint`.

Extensions beyond the base specs: Stage 3 [decorators](https://github.com/tc39/proposal-decorators), [import defer](https://github.com/tc39/proposal-defer-import-eval), [import source](https://github.com/tc39/proposal-source-phase-imports), and a `hashbang` field on `program`.

This page covers the internal Zig AST, its memory model, core types, and the complete node reference.

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

| Field         | Type                    | Description                             |
| ------------- | ----------------------- | --------------------------------------- |
| `program`     | `NodeIndex`             | Root node (always a `program`)          |
| `nodes`       | `NodeList`              | All AST nodes                           |
| `extra`       | `ArrayList(NodeIndex)`  | Variable-length child index lists       |
| `diagnostics` | `ArrayList(Diagnostic)` | Parse errors, warnings, hints           |
| `comments`    | `[]const Comment`       | All comments found in source            |
| `source`      | `[]const u8`            | Original source text                    |
| `source_type` | `SourceType`            | `.script` or `.module`                  |
| `lang`        | `Lang`                  | `.js`, `.ts`, `.jsx`, `.tsx`, or `.dts` |

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

Four types appear in nearly every node definition.

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

A `String` is a lightweight handle to string content. It points into one of two backing stores:

- **Source slice (zero-copy)**: most identifiers and string literals parsed from source. The bytes live inside the original `source` slice.
- **Pool entry**: programmatically added strings (`tree.addString()`), transformed names, or escaped identifiers. These live in the string pool's extra buffer.

`tree.getString(handle)` resolves both cases transparently:

```zig
const name = tree.getString(node.name); // always returns []const u8
```

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

`NodeData` is a tagged union with one variant per node type. The variant tags are snake_case (`binary_expression`, `if_statement`, `ts_type_alias_declaration`, ...). `tree.getData(index)` returns one. Switch on the tag to determine the type and unpack its fields:

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

The same snake_case names are used for visitor hooks: a method named `enter_binary_expression` on your visitor struct fires when the traverser enters that node kind.

## Node reference

Every entry in `NodeData` is a distinct node tag. The tag name is the exact name used in `tree.getData()` switches and visitor hooks (`enter_<tag>`).

Optional child fields are noted with `.null`. Optional child lists are noted with `.empty`.

---

### Program

The root of every tree. There is always exactly one `program` node at `tree.program`.

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

| Tag                    | Syntax                          | Notes                                                   |
| ---------------------- | ------------------------------- | ------------------------------------------------------- |
| `expression_statement` | `expr;`                         | Wraps any expression used as a statement                |
| `block_statement`      | `{ ... }`                       | `body` is a statement list                              |
| `empty_statement`      | `;`                             | No fields                                               |
| `debugger_statement`   | `debugger;`                     | No fields                                               |
| `if_statement`         | `if (test) cons else alt`       | `alternate` is `.null` when no else branch              |
| `switch_statement`     | `switch (d) { cases }`          | `cases` is a list of `switch_case` nodes                |
| `switch_case`          | `case x: ...` / `default: ...`  | `test` is `.null` for the default case                  |
| `for_statement`        | `for (init; test; update) body` | `init`, `test`, and `update` are all optional (`.null`) |
| `for_in_statement`     | `for (x in y) body`             | `left` is a declaration or assignment target            |
| `for_of_statement`     | `for (x of y) body`             | `await: bool` for `for await (...of...)`                |
| `while_statement`      | `while (test) body`             |                                                         |
| `do_while_statement`   | `do body while (test)`          |                                                         |
| `break_statement`      | `break;` / `break label;`       | `label` is `.null` for unlabeled                        |
| `continue_statement`   | `continue;` / `continue label;` | `label` is `.null` for unlabeled                        |
| `labeled_statement`    | `label: stmt`                   |                                                         |
| `return_statement`     | `return;` / `return expr;`      | `argument` is `.null` for bare return                   |
| `throw_statement`      | `throw expr;`                   |                                                         |
| `try_statement`        | `try {} catch {} finally {}`    | `handler` and `finalizer` are `.null` when absent       |
| `catch_clause`         | `catch (e) { body }`            | `param` is `.null` for `catch {}` without binding       |
| `with_statement`       | `with (obj) body`               |                                                         |

---

### Declarations

| Tag                    | Syntax                        | Notes                                                                                   |
| ---------------------- | ----------------------------- | --------------------------------------------------------------------------------------- |
| `variable_declaration` | `var/let/const/using x = ...` | `kind` is a `VariableKind` enum; `declarators` is a list of `variable_declarator` nodes |
| `variable_declarator`  | `x = init`                    | `id` is any binding pattern; `init` is `.null` for `let x;`                             |
| `directive`            | `"use strict";`               | Only appears at the start of a function or module body, before regular statements       |
| `function`             | `function foo() {}`           | Covers all function forms; check the `type` field                                       |
| `class`                | `class Foo {}`                | Covers both declarations and expressions; check the `type` field                        |

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

| Tag                          | Syntax                                  | Notes                                                                     |
| ---------------------------- | --------------------------------------- | ------------------------------------------------------------------------- |
| `binary_expression`          | `a + b`, `a === b`, `a instanceof b`    | `operator` is a `BinaryOperator` enum                                     |
| `logical_expression`         | `a && b`, `a \|\| b`, `a ?? b`          | `operator` is a `LogicalOperator` enum                                    |
| `unary_expression`           | `!x`, `typeof x`, `void x`, `delete x`  | `operator` is a `UnaryOperator` enum                                      |
| `update_expression`          | `x++`, `++x`, `x--`                     | `operator` is `UpdateOperator`; `prefix: bool` distinguishes pre / post   |
| `assignment_expression`      | `x = y`, `x += y`, `x ??= y`            | `operator` is an `AssignmentOperator` enum                                |
| `conditional_expression`     | `test ? a : b`                          |                                                                           |
| `sequence_expression`        | `a, b, c`                               | `expressions` is a node list                                              |
| `parenthesized_expression`   | `(expr)`                                | Wraps an expression to preserve explicit parentheses in the tree          |
| `member_expression`          | `obj.prop`, `obj[x]`, `obj.#priv`       | `computed: bool` for bracket access; `optional: bool` for `?.`            |
| `call_expression`            | `fn(args)`, `fn?.()`                    | `optional: bool` for `?.()`; `type_arguments` for `fn<T>(args)`           |
| `new_expression`             | `new Foo(args)`                         | `type_arguments` for `new Foo<T>(args)`                                   |
| `chain_expression`           | `a?.b`, `a?.()`                         | Wrapper around an optional chain; the inner node carries `optional: true` |
| `tagged_template_expression` | `` tag`hello` ``                        | `tag` is the function; `quasi` is the template literal                    |
| `await_expression`           | `await expr`                            |                                                                           |
| `yield_expression`           | `yield expr`, `yield* expr`             | `delegate: bool` for `yield*`; `argument` may be `.null`                  |
| `meta_property`              | `import.meta`, `new.target`             | `meta` and `property` are `identifier_name` nodes                         |
| `array_expression`           | `[a, , b, ...c]`                        | `elements` may contain `.null` entries for holes                          |
| `object_expression`          | `{a: 1, b, ...c}`                       | `properties` contains `object_property` and `spread_element` nodes        |
| `object_property`            | `key: value`, getters, setters, methods | `kind` (`PropertyKind`), `method`, `shorthand`, `computed`                |
| `spread_element`             | `...expr`                               | Used in arrays, calls, and object literals                                |
| `import_expression`          | `import(src)`, `import.source(src)`     | Dynamic import; `phase` may be `.source`, `.defer`, or `null`             |
| `this_expression`            | `this`                                  | No fields                                                                 |
| `super`                      | `super`                                 | The `super` keyword. No fields                                            |

---

### Literals

| Tag                | Syntax                        | Notes                                                                                                                                |
| ------------------ | ----------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| `string_literal`   | `"hello"`, `'world'`          | `value` is the decoded content without quotes (escape sequences resolved). Raw source text is available via the span.                |
| `numeric_literal`  | `42`, `0xFF`, `0o7`, `0b1010` | `value()` is the parsed `f64`; `kind` distinguishes decimal / hex / octal / binary; `raw` is the lexeme                              |
| `bigint_literal`   | `42n`                         | `raw` is the digits without the trailing `n`                                                                                         |
| `boolean_literal`  | `true`, `false`               | `value: bool`                                                                                                                        |
| `null_literal`     | `null`                        | No fields                                                                                                                            |
| `regexp_literal`   | `/pattern/flags`              | `pattern` and `flags` are separate `String` handles                                                                                  |
| `template_literal` | `` `hello ${name}` ``         | `quasis` (list of `template_element`) and `expressions` are interleaved; always `quasis.len == expressions.len + 1`                  |
| `template_element` | text part between `${...}`    | `cooked` is the escape-decoded content (empty when `is_cooked_undefined`); `tail: bool` marks the last segment                       |

---

### Identifiers

Five distinct identifier tags, all carrying a single `name: String` field. They look identical structurally but are used in different positions and are typically resolved differently.

| Tag                    | Used For                                                                                                                   |
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

| Tag                    | Syntax                          | Notes                                                                                                 |
| ---------------------- | ------------------------------- | ----------------------------------------------------------------------------------------------------- |
| `array_pattern`        | `[a, , b, ...rest]`             | `elements` may include `.null` for holes; `rest` is `.null` if absent                                 |
| `object_pattern`       | `{a, b: c, ...rest}`            | `properties` contains `binding_property` nodes; `rest` is `.null` if absent                           |
| `binding_property`     | `key: value` or shorthand `key` | `shorthand: bool`, `computed: bool`                                                                   |
| `assignment_pattern`   | `x = default`                   | Used for default values in destructuring and function parameters                                      |
| `binding_rest_element` | `...rest`                       | `argument` is the binding pattern the rest collects into                                              |
| `formal_parameters`    | `(a, b = 1, ...rest)`           | `items` contains `formal_parameter` (and possibly `ts_parameter_property`); `rest` is `.null` if absent |
| `formal_parameter`     | a single parameter slot         | `pattern` is the binding (any binding pattern, with optional default via `assignment_pattern`)        |

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

| Tag                         | Description                                                                                                                 |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `function`                  | All named and anonymous function forms. Use `type`, `generator`, and `async` to distinguish them.                           |
| `function_body`             | The `{ ... }` body of a function. Contains directives and statements in `body: IndexRange`.                                 |
| `arrow_function_expression` | Arrow functions. `expression: bool` is `true` when the body is an expression (not a block). `async: bool` for async arrows. |

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

| Tag                   | Description                                                                                                                                                  |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `class`               | Both `class Foo {}` and `const x = class {}`. Check `type`.                                                                                                  |
| `class_body`          | The `{ members }` block. `body` contains `method_definition`, `property_definition`, `static_block`, and `ts_index_signature` nodes.                         |
| `method_definition`   | A method, getter, setter, or constructor. `kind` is a `MethodDefinitionKind` enum (`constructor`, `method`, `get`, `set`). `static`, `computed`, `override`, `optional`, `abstract`, and `accessibility` carry the modifiers. |
| `property_definition` | A class field (`x = 1`). `value` is `.null` for fields without an initializer. `accessor: bool` for auto-accessors. `static`, `computed`, `declare`, `override`, `optional`, `definite`, `readonly`, `abstract`, and `accessibility` carry the modifiers. |
| `static_block`        | `static { ... }`. `body` is a list of statements.                                                                                                            |
| `decorator`           | `@expr`. `expression` is the decorator expression node.                                                                                                      |
| `super`               | The `super` keyword. No fields.                                                                                                                              |

---

### Modules

| Tag                          | Syntax                         | Notes                                                                                                                       |
| ---------------------------- | ------------------------------ | --------------------------------------------------------------------------------------------------------------------------- |
| `import_declaration`         | `import x from 'y'`            | `specifiers` is empty for side-effect-only imports; `phase` is `.source` or `.defer` for staged imports, `null` for regular |
| `import_specifier`           | `{ imported as local }`        | `imported` is `identifier_name` or `string_literal`; `local` is `binding_identifier`                                        |
| `import_default_specifier`   | `import x from ...`            | `local` is the binding                                                                                                      |
| `import_namespace_specifier` | `import * as x from ...`       | `local` is the binding                                                                                                      |
| `import_attribute`           | `{ type: "json" }`             | Import attributes / assertions                                                                                              |
| `export_named_declaration`   | `export { x }`, `export var x` | `declaration` is `.null` for specifier-only; `source` is `.null` for local (non-re-export)                                  |
| `export_default_declaration` | `export default expr`          | `declaration` is any expression, `function`, or `class`                                                                     |
| `export_all_declaration`     | `export * from 'y'`            | `exported` is `.null` for `export *`; non-null for `export * as name`                                                       |
| `export_specifier`           | `{ local as exported }`        |                                                                                                                             |

---

### JSX

JSX nodes are only present in `.jsx` and `.tsx` trees.

| Tag                        | JSX Syntax                  | Notes                                                                                  |
| -------------------------- | --------------------------- | -------------------------------------------------------------------------------------- |
| `jsx_element`              | `<Foo>...</Foo>`            | `closing_element` is `.null` for self-closing tags                                     |
| `jsx_opening_element`      | `<Foo ...>`                 | `self_closing: bool` for `<Foo />`; `name` is a JSX tag name; `type_arguments` for `<Foo<T>>` |
| `jsx_closing_element`      | `</Foo>`                    |                                                                                        |
| `jsx_fragment`             | `<>...</>`                  |                                                                                        |
| `jsx_opening_fragment`     | `<>`                        | No fields                                                                              |
| `jsx_closing_fragment`     | `</>`                       | No fields                                                                              |
| `jsx_identifier`           | `Foo` in JSX position       | `name: String`                                                                         |
| `jsx_namespaced_name`      | `namespace:name`            |                                                                                        |
| `jsx_member_expression`    | `Foo.Bar.Baz`               |                                                                                        |
| `jsx_attribute`            | `foo="bar"` or `foo={expr}` | `value` is `.null` for boolean attributes like `disabled`                              |
| `jsx_spread_attribute`     | `{...props}`                | `argument` is the spread expression                                                    |
| `jsx_expression_container` | `{expression}`              | `expression` is a `jsx_empty_expression` node for `{}`                                 |
| `jsx_empty_expression`     | `{}`                        | No fields                                                                              |
| `jsx_text`                 | text content between tags   | `value: String` is the text content                                                    |
| `jsx_spread_child`         | `{...children}`             | `expression` is the spread expression                                                  |

JSX tag names (the `name` field on `jsx_opening_element`, `jsx_closing_element`, and one form of `jsx_attribute`) are one of `jsx_identifier`, `jsx_namespaced_name`, or `jsx_member_expression`.

JSX children (entries in the `children` list on `jsx_element` and `jsx_fragment`) are one of `jsx_text`, `jsx_expression_container`, `jsx_spread_child`, `jsx_element`, or `jsx_fragment`.

---

## TypeScript

TypeScript nodes are present in `.ts`, `.tsx`, and `.dts` trees.

### Type wrapper

| Tag                 | Syntax            | Notes                                                                                                  |
| ------------------- | ----------------- | ------------------------------------------------------------------------------------------------------ |
| `ts_type_annotation` | `: T`             | Wrapper around a TS type. The span starts at `:`. `type_annotation` field holds the inner type.        |

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

| Tag                  | TS Syntax                    | Notes                                                                                                                                  |
| -------------------- | ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ts_type_reference`  | `Foo`, `Promise<T>`          | `type_name` is `identifier_reference`, `ts_qualified_name`, or `this_expression`. `type_arguments` is `.null` when none.               |
| `ts_qualified_name`  | `A.B.C`                      | Left-associative. `left` may be a nested `ts_qualified_name`. `right` is `identifier_name`.                                            |
| `ts_type_query`      | `typeof console.log`         | `expr_name` is `identifier_reference`, `ts_qualified_name`, or `ts_import_type`. `type_arguments` for `typeof Err<number>`.            |
| `ts_import_type`     | `import("m").Foo<T>`         | `source` (string), `options` (object expression or `.null`), `qualifier` (identifier name or qualified chain or `.null`), `type_arguments`. |

### Type parameters and arguments

| Tag                                | TS Syntax            | Notes                                                                                                       |
| ---------------------------------- | -------------------- | ----------------------------------------------------------------------------------------------------------- |
| `ts_type_parameter`                | `T`, `T extends U = V` | Carries `name`, optional `constraint`, optional `default`, plus `in`, `out`, `const` modifier flags.         |
| `ts_type_parameter_declaration`    | `<T, U>`             | The `<...>` declaration list on a generic. `params` is a `ts_type_parameter[]`.                              |
| `ts_type_parameter_instantiation`  | `<number, string>`   | The `<...>` argument list at a call, instantiation, or reference. `params` is a list of any ts type.        |

### Literal and template types

| Tag                          | TS Syntax           | Notes                                                                                                                                  |
| ---------------------------- | ------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ts_literal_type`            | `"hello"`, `42`, `true`, `-1` | `literal` is one of `string_literal`, `numeric_literal`, `bigint_literal`, `boolean_literal`, `template_literal` (no interpolations), or a `unary_expression` wrapping a numeric form. |
| `ts_template_literal_type`   | `` `Hello, ${N}!` `` | `quasis` is `template_element[]`. `types` is the list of interpolated type nodes. Always `quasis.len == types.len + 1`.               |

### Composite types

| Tag                       | TS Syntax              | Notes                                                                                                                                  |
| ------------------------- | ---------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ts_array_type`           | `T[]`                  | `element_type` is the inner type. Stacks for `T[][]`.                                                                                  |
| `ts_indexed_access_type`  | `T[K]`                 | `object_type` and `index_type`. Stacks for `T[K][L]`.                                                                                  |
| `ts_tuple_type`           | `[T, U?, ...V[]]`      | `element_types` may contain plain types, `ts_optional_type`, `ts_rest_type`, or `ts_named_tuple_member`.                               |
| `ts_named_tuple_member`   | `label: T`, `label?: T` | `label` is `identifier_name`, `element_type` is the type, `optional: bool`. Wrapped in `ts_rest_type` for `[...rest: T[]]`.            |
| `ts_optional_type`        | `T?` (in tuple slot)   | Only valid as a direct tuple element. `type_annotation` is the inner type.                                                             |
| `ts_rest_type`            | `...T` (in tuple slot) | `type_annotation` is the inner type or a `ts_named_tuple_member`.                                                                      |

### Set-operation types

| Tag                     | TS Syntax                    | Notes                                                                                                                                  |
| ----------------------- | ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ts_union_type`         | `A \| B \| C`                | `types` is a list. A leading `\|` is permitted and preserved in the span.                                                              |
| `ts_intersection_type`  | `A & B & C`                  | `types` is a list. A leading `&` is permitted and preserved in the span. Binds tighter than union.                                     |
| `ts_conditional_type`   | `T extends U ? X : Y`        | `check_type`, `extends_type`, `true_type`, `false_type`.                                                                               |
| `ts_infer_type`         | `infer R`, `infer R extends string` | `type_parameter` is a `ts_type_parameter` carrying the name and optional constraint.                                            |

### Type operators

| Tag                       | TS Syntax                | Notes                                                                                                                                  |
| ------------------------- | ------------------------ | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ts_type_operator`        | `keyof T`, `unique symbol`, `readonly T[]` | `operator` is `TSTypeOperatorKind` (`keyof`, `unique`, `readonly`). `type_annotation` is the inner type.            |
| `ts_parenthesized_type`   | `(T)`                    | Wraps an inner type in parentheses.                                                                                                    |

### Callable types

| Tag                  | TS Syntax                   | Notes                                                                                                                                  |
| -------------------- | --------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ts_function_type`   | `(x: T) => U`               | `params` is `formal_parameters`. `return_type` is a `ts_type_annotation`. `type_parameters` for generic forms.                         |
| `ts_constructor_type` | `new (x: T) => U`, `abstract new ...` | Same shape as `ts_function_type`, plus `abstract: bool`.                                                                       |
| `ts_type_predicate`  | `x is T`, `asserts x is T`, `asserts x` | `parameter_name` is `identifier_name` or `ts_this_type`. `type_annotation` is `.null` for bare `asserts x`. `asserts: bool`.   |

### Object-shape types

| Tag                | TS Syntax              | Notes                                                                                                                                  |
| ------------------ | ---------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ts_type_literal`  | `{ x: T; y: U }`       | `members` is a list of `ts_property_signature`, `ts_method_signature`, `ts_call_signature_declaration`, `ts_construct_signature_declaration`, or `ts_index_signature`. |
| `ts_mapped_type`   | `{ [K in T]: V }`      | `key`, `constraint`, optional `name_type` (for `as` clause), `type_annotation`, plus `optional` and `readonly` modifiers (`TSMappedTypeModifier`). |

### JSDoc types

| Tag                          | TS Syntax       | Notes                                                                |
| ---------------------------- | --------------- | -------------------------------------------------------------------- |
| `ts_jsdoc_nullable_type`     | `?T` or `T?`    | `type_annotation` is the inner type. `postfix: bool`.                |
| `ts_jsdoc_non_nullable_type` | `!T` or `T!`    | Same shape as nullable.                                              |
| `ts_jsdoc_unknown_type`      | `?` (in `Foo<?>`) | No fields. Valid only in a type argument slot.                       |

### Signature members

These appear inside `ts_type_literal.members` and `ts_interface_body.body`.

| Tag                                   | TS Syntax                            | Notes                                                                                                                                  |
| ------------------------------------- | ------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ts_property_signature`               | `key: T`, `readonly key?: T`         | `key` is a property key. `type_annotation` is `.null` when omitted. `readonly`, `optional`, `computed`.                                |
| `ts_method_signature`                 | `m(x: T): U`, `get x(): T`, `set x(v: T)` | `kind` is `TSMethodSignatureKind` (`method`, `get`, `set`). `type_parameters`, `params`, `return_type`. `optional`, `computed`.       |
| `ts_call_signature_declaration`       | `(x: T): U`                          | Bare call signature. `type_parameters`, `params`, `return_type`.                                                                       |
| `ts_construct_signature_declaration`  | `new (x: T): U`                      | Bare construct signature. Same shape as call signature.                                                                                |
| `ts_index_signature`                  | `[k: K]: V`, `readonly [...]: V`     | `parameters` is a `binding_identifier[]` carrying the index parameter type. `type_annotation` is the value type. `readonly`, `static` (in class bodies). |

### Type, interface, and enum declarations

| Tag                            | TS Syntax                        | Notes                                                                                                                                  |
| ------------------------------ | -------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ts_type_alias_declaration`    | `type Maybe<T> = T \| null`      | `id`, optional `type_parameters`, `type_annotation` (a bare type, no `ts_type_annotation` wrapper), `declare: bool`.                   |
| `ts_interface_declaration`     | `interface Foo<T> extends Bar { ... }` | `id`, optional `type_parameters`, `extends` is `ts_interface_heritage[]`, `body` is `ts_interface_body`, `declare: bool`.        |
| `ts_interface_body`            | `{ ... }` of an interface        | `body` is a list of signature members.                                                                                                 |
| `ts_interface_heritage`        | one entry of an `extends` clause | `expression` is `identifier_reference` or a `member_expression` chain. `type_arguments` for `<T>`.                                     |
| `ts_class_implements`          | one entry of an `implements` clause | Same shape as `ts_interface_heritage`.                                                                                              |
| `ts_enum_declaration`          | `enum Color { ... }`             | `id`, `body`, `is_const: bool`, `declare: bool`.                                                                                       |
| `ts_enum_body`                 | `{ ... }` of an enum             | `members` is a list of `ts_enum_member`.                                                                                               |
| `ts_enum_member`               | `A = 1` inside an enum body      | `id` is `identifier_name`, `string_literal`, or `template_literal`. `initializer` is `.null` when absent. `computed: bool`.            |

### Module and namespace declarations

| Tag                            | TS Syntax                                 | Notes                                                                                                                                  |
| ------------------------------ | ----------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ts_module_declaration`        | `namespace Foo { ... }`, `module "x" {}`  | `id` is `binding_identifier`, `string_literal`, or `ts_qualified_name`. `body` is `ts_module_block` or `.null` for body-less forwards. `kind` is `TSModuleDeclarationKind`. `declare: bool`. |
| `ts_module_block`              | the `{ ... }` of a module                 | `body` is a statement list.                                                                                                            |
| `ts_global_declaration`        | `declare global { ... }`                  | `id` is `identifier_name` (the `global` keyword span). `body` is `ts_module_block`. `declare: bool`.                                   |

### Parameters and `this`

| Tag                       | TS Syntax                                | Notes                                                                                                                                  |
| ------------------------- | ---------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ts_parameter_property`   | `constructor(public x: T) {}`            | `parameter` is `binding_identifier` or `assignment_pattern`. Carries `decorators`, `accessibility`, `readonly`, `override`.            |
| `ts_this_parameter`       | `function f(this: T)`                    | `type_annotation` is `.null` when written as bare `this`. Erased at emit time.                                                         |

### TypeScript expressions

| Tag                            | TS Syntax           | Notes                                                            |
| ------------------------------ | ------------------- | ---------------------------------------------------------------- |
| `ts_as_expression`             | `expr as T`         | `expression`, `type_annotation`.                                 |
| `ts_satisfies_expression`      | `expr satisfies T`  | `expression`, `type_annotation`.                                 |
| `ts_type_assertion`            | `<T>expr`           | `type_annotation`, `expression`. Forbidden in `.tsx`.            |
| `ts_non_null_expression`       | `expr!`             | `expression`.                                                    |
| `ts_instantiation_expression`  | `expr<T>`           | `expression`, `type_arguments`.                                  |

### TypeScript module forms

| Tag                                | TS Syntax                          | Notes                                                                                                            |
| ---------------------------------- | ---------------------------------- | ---------------------------------------------------------------------------------------------------------------- |
| `ts_export_assignment`             | `export = expr`                    | `expression`. CommonJS-style ambient export.                                                                     |
| `ts_namespace_export_declaration`  | `export as namespace Name`         | `id` is `binding_identifier`. UMD ambient namespace export.                                                      |
| `ts_import_equals_declaration`     | `import x = require("m")`, `import x = Foo.Bar` | `id`, `module_reference` is `ts_external_module_reference`, `identifier_reference`, or `ts_qualified_name`. `import_kind`. |
| `ts_external_module_reference`     | `require("m")`                     | `expression` is the `string_literal`.                                                                            |
