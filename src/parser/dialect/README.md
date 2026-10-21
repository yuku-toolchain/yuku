# Dialects

A dialect adds your own syntax to JavaScript, TypeScript, or JSX, from a single operator to a whole language such as [TSRX](https://tsrx.dev). You write only the new syntax, in one Zig file, compiled into Yuku's parser. Yuku parses, analyzes, walks, and prints the rest of the language to the spec, and your nodes with it, from Zig and from the [`yuku` package](https://github.com/yuku-toolchain/yuku/tree/main/packages/yuku#readme).

- [Example](#example)
- [The dialect file](#the-dialect-file): [files](#files), [positions](#positions), [the parser](#the-parser), [nodes](#nodes), [printing](#printing)
- [Analysis, walking, and lowering](#analysis-walking-and-lowering)
- [Building](#building)
- [From JavaScript](#from-javascript)

## Example

A pipeline operator, `|>`, with `%` as the value piped in.

```zig
// src/pipeline.zig
const std = @import("std");
const yuku = @import("parser");

const ast = yuku.ast;
const dialect = yuku.dialect;
const Error = std.mem.Allocator.Error;

pub const files: []const dialect.File = &.{.{ .extension = ".pipe.js", .lang = .js }};

pub const Node = enum {
    pipeline,
    topic,

    pub fn shape(node: Node) dialect.Shape {
        return switch (node) {
            .pipeline => .{ .type = "PipelineExpression", .fields = &.{ "left", "right" } },
            .topic => .{ .type = "TopicReference" },
        };
    }
};

pub fn operator(p: *dialect.Parser, left: ast.NodeIndex, precedence: u8) Error!dialect.Claim {
    if (!std.mem.startsWith(u8, p.tree().source[p.token().span.start..], "|>")) return .decline;
    if (precedence > ast.Precedence.Assignment) return .end;

    if (!try p.advance() or !try p.advance()) return .failed;
    const right = try p.parseExpression(ast.Precedence.Assignment + 1) orelse return .failed;
    const span: ast.Span = .{ .start = p.tree().span(left).start, .end = p.tree().span(right).end };
    return .{ .node = try p.addNode(.pipeline, &.{ left, right }, span) };
}

pub fn expression(p: *dialect.Parser) Error!dialect.Claim {
    const token = p.token();
    if (token.tag != .percent) return .decline;

    if (!try p.advance()) return .failed;
    return .{ .node = try p.addNode(.topic, &.{}, token.span) };
}

pub fn print(w: *dialect.Printer, kind: Node, children: []const ast.NodeIndex) Error!void {
    switch (kind) {
        .pipeline => {
            try w.node(children[0]);
            try w.write(" |> ");
            try w.node(children[1]);
        },
        .topic => try w.write("%"),
    }
}
```

In `prices |> sum(%) |> round(%, 2)`, the pipelines group left to right, the calls inside them are standard nodes, and each pipeline decodes in JavaScript as `{ type: "PipelineExpression", start, end, left, right }`.

## The dialect file

A dialect is a Zig file of public declarations, which imports the parser it is compiled into as `@import("parser")`.

| Declaration        | Required            | Purpose                                |
| ------------------ | ------------------- | -------------------------------------- |
| `files`            | yes                 | The [files](#files) it reads           |
| `Node`             | to add nodes        | Your [node kinds](#nodes)              |
| Position functions | at least one        | Where your syntax [starts](#positions) |
| `print`            | to print your nodes | How codegen [prints](#printing) them   |

A missing `files`, a function named after no position, a wrong signature, a `Node` without `shape`, or an invalid shape is a compile error. Signatures return exactly `std.mem.Allocator.Error`, not an inferred error set.

### Files

```zig
pub const files: []const dialect.File = &.{
    .{ .extension = ".pipe.js", .lang = .js },
    .{ .extension = ".pipe.ts", .lang = .ts },
};
```

Each entry is a file extension and the standard `lang` it extends. In Zig, a parse opts in with `.dialect = true`, which needs a build with a dialect compiled in, as `dialect.bound` tells. In JavaScript, the path picks the dialect, see [From JavaScript](#from-javascript).

### Positions

A position is a point in the grammar where the parser asks your dialect first, through the `pub fn` of that name.

| Position           | Called at                                                                                   |
| ------------------ | ------------------------------------------------------------------------------------------- |
| `statement`        | The start of every statement                                                                |
| `expression`       | The start of every operand, before any prefix operator, and at a `@` after `export default` |
| `operator`         | After the operand `left`, where an infix or postfix operator could follow                   |
| `function_body`    | Where a function, method, or arrow function body starts                                     |
| `jsx_child`        | Each JSX child, at byte `offset` in `parent`                                                |
| `jsx_attribute`    | A `{` in a JSX tag, where standard JSX reads a spread attribute                             |
| `jsx_element_name` | The start of an element name, in an opening or closing tag                                  |
| `module_specifier` | A module specifier that is not a string                                                     |

```zig
pub fn statement(p: *dialect.Parser) std.mem.Allocator.Error!dialect.Claim
pub fn expression(p: *dialect.Parser) std.mem.Allocator.Error!dialect.Claim
pub fn operator(p: *dialect.Parser, left: ast.NodeIndex, precedence: u8) std.mem.Allocator.Error!dialect.Claim
pub fn function_body(p: *dialect.Parser) std.mem.Allocator.Error!dialect.Claim
pub fn jsx_child(p: *dialect.Parser, offset: u32, parent: ast.NodeIndex) std.mem.Allocator.Error!dialect.Claim
pub fn jsx_attribute(p: *dialect.Parser) std.mem.Allocator.Error!dialect.Claim
pub fn jsx_element_name(p: *dialect.Parser) std.mem.Allocator.Error!dialect.Claim
pub fn module_specifier(p: *dialect.Parser) std.mem.Allocator.Error!dialect.Claim
```

| `dialect.Claim`      | Means                                                          |
| -------------------- | -------------------------------------------------------------- |
| `.decline`           | Not your syntax. Consume no tokens and add no nodes.           |
| `.{ .node = index }` | The node you added, with the current token just past it.       |
| `.failed`            | A diagnostic was reported, by you or by a parse call.          |
| `.end`               | `operator` only. The expression ends before the current token. |

- Positions apply inside your own nodes too, so your syntax and standard syntax nest.
- Decline anything the standard grammar accepts, unless you mean to read it differently.
- `operator` receives the lowest `precedence` that continues the current expression. Answer `.end` when your operator binds more loosely, as the example does.
- `function_body` allows `return` and a directive prologue inside.
- A `jsx_child` node starts at `offset` and ends on its last token, which stays current. `parent` is the opening element or fragment. Call `seek(offset)` first when the child starts with tokens.

### The parser

| `*dialect.Parser`                    | Does                                                                                                       |
| ------------------------------------ | ---------------------------------------------------------------------------------------------------------- |
| `token()`                            | The current `ast.Token`                                                                                    |
| `advance()`                          | Moves to the next token, or returns `false` after a lexical error                                          |
| `expect(tag, message)`               | Consumes a token of `tag`, or reports `message` and returns `false`                                        |
| `tokenAt(offset)`                    | The token at or after a byte offset, without moving                                                        |
| `seek(offset)`                       | Makes the token at a byte offset current, at or after the current one                                      |
| `checkpoint()`, `rewind(checkpoint)` | Saves the position as a `dialect.Checkpoint`, and returns to it                                            |
| `semicolon(end)`                     | Ends a statement, with `;` or ASI, returning its end, or null after a diagnostic                           |
| `report(span, message)`              | Reports an error                                                                                           |
| `parseStatement()`                   | Parses a statement                                                                                         |
| `parseExpression(precedence)`        | Parses an expression binding at `precedence` or tighter, `ast.Precedence.Lowest` for a whole one           |
| `parseBindingPattern()`              | Parses an identifier, array, or object pattern, with its type annotation                                   |
| `parseJsxExpressionContainer()`      | Parses the `{ expression }` at the current `{` in a tag                                                    |
| `parseJsxElement()`                  | Parses the element or fragment at the current `<`, leaving its last `>` current                            |
| `addNode(kind, children, span)`      | Adds one of your [nodes](#nodes)                                                                           |
| `list()`                             | A `dialect.List` that collects children of unknown count                                                   |
| `addJsxText(span)`                   | Adds the source text in a non-empty `span` as JSX text                                                     |
| `recordComment(type, span)`          | Records a comment where the standard grammar reads none, in source order                                   |
| `tree()`                             | The `ast.Tree`, to read nodes and [build](https://yuku.fyi/parser/ast/#building-and-editing) standard ones |

A parse call returns `null` after it reports a diagnostic, and then you answer `.failed`.

```zig
var children = p.list();
defer children.reset();
while (p.token().tag != .right_brace) {
    try children.append(try p.parseStatement() orelse return .failed);
}
const node = try p.addNode(.block, children.items(), span);
```

### Nodes

`Node` is an enum of your node kinds, counting up from 0, whose `shape` describes each one.

```zig
pub const Node = enum {
    each,

    pub fn shape(node: Node) dialect.Shape {
        return switch (node) {
            .each => .{
                .type = "EachStatement",
                .fields = &.{ "item", "list", "body[]" },
                .block_scope = true,
                .lexical_bindings = true,
            },
        };
    }
};
```

| `dialect.Shape`    | Default | Means                                                                             |
| ------------------ | ------- | --------------------------------------------------------------------------------- |
| `type`             |         | The node's `type` in JavaScript                                                   |
| `fields`           | `&.{}`  | Its children in order, where one name ending in `[]` takes the rest as an array   |
| `properties`       | `"{}"`  | A JSON object of constant properties every node of the kind has in JavaScript     |
| `block_scope`      | `false` | The node opens a block scope                                                      |
| `lexical_bindings` | `false` | Each binding identifier inside declares a `let`-like binding in the current scope |
| `catch_bindings`   | `false` | Each binding identifier inside declares a catch parameter                         |
| `iteration`        | `false` | The node is a loop, which `break` and `continue` end or skip                      |

`addNode` takes children in `fields` order, `.null` for an absent one, lying inside `span`. In the tree, the node is an `ast.Dialect`, its `kind` a `dialect.Kind` and its `children` a range for `tree.extra`. In JavaScript it is a plain object of its `type`, `start`, `end`, fields, and `properties`, so `each (item of list) { ... }` decodes as `{ type: "EachStatement", start, end, item, list, body: [...] }`.

### Printing

```zig
pub fn print(w: *dialect.Printer, kind: Node, children: []const ast.NodeIndex) std.mem.Allocator.Error!void
```

| `*dialect.Printer` | Does                                             |
| ------------------ | ------------------------------------------------ |
| `write(code)`      | Writes code, with a space before it where needed |
| `space()`          | Writes a space in the pretty format              |
| `node(index)`      | Prints any node, standard or yours               |
| `block(items)`     | Prints a list of statements between braces       |
| `tree()`           | The tree, to read nodes                          |

Without `print`, codegen reports a diagnostic for each of your nodes.

## Analysis, walking, and lowering

Semantic analysis walks into your nodes. Standard code inside them declares, resolves, and reports early errors as usual, and the [shape](#nodes) decides the scope and bindings your node adds. Every [traverser](https://yuku.fyi/parser/traverse/) mode reaches your nodes through `enter_dialect` and `exit_dialect`.

```zig
pub fn enter_dialect(self: *Lower, node: ast.Dialect, index: ast.NodeIndex, ctx: *transform.Ctx) traverser.Action
```

To compile your syntax to standard JavaScript, replace each of your nodes with standard ones in a [transform](https://yuku.fyi/parser/traverse/#transform) pass. Every tool then reads plain JavaScript.

## Building

Pass the dialect file to the `yuku` dependency, which then holds that one dialect.

```zig
// build.zig
const yuku = b.dependency("yuku", .{
    .target = target,
    .optimize = optimize,
    .dialect = b.path("src/pipeline.zig"),
});
my_module.addImport("parser", yuku.module("parser"));

b.installArtifact(@import("yuku").addWasm(b, yuku, "pipeline")); // zig-out/bin/pipeline.wasm
```

`addWasm(b, yuku, name)` builds the WebAssembly module the `yuku` package runs: wasm32 with SIMD, optimized for speed, and stripped. Inside the Yuku repository, `zig build wasm -Ddialect=src/pipeline.zig` builds it as `zig-out/bin/yuku.wasm`.

A build without a dialect has no dialect code. With one, a standard file pays one flag check per position, and a dialect file one call.

## From JavaScript

Pass the build as the `wasm` option of `parse` or `new Project`, and every part of the package works with your nodes in the tree.

```js
import { readFileSync } from "node:fs";
import { parse, print } from "yuku";

const wasm = new WebAssembly.Module(readFileSync("zig-out/bin/pipeline.wasm"));
const file = parse("const total = prices |> sum(%);", { path: "app.pipe.js", wasm });

print(file, {
  printers: {
    PipelineExpression(node, p) {
      p.node(node.left);
      p.write(" |> ");
      p.node(node.right);
    },
    TopicReference(node, p) {
      p.write("%");
    },
  },
});
```

A path ending in one of your extensions parses with the dialect, a source with no path uses the first, and any other path, or an explicit `lang`, reads as standard syntax. `printers` takes the same calls as `print` in Zig.

Declare your nodes in `DialectNodes` to type them in TypeScript.

```ts
import type { BaseNode, Expression } from "yuku";

declare module "yuku" {
  interface DialectNodes {
    PipelineExpression: BaseNode & { type: "PipelineExpression"; left: Expression; right: Expression };
    TopicReference: BaseNode & { type: "TopicReference" };
  }
}
```

A dialect's own package depends on `yuku`, ships the build, and passes it for its users.

```ts
import { parse as parseYuku, type ParseOptions, type SourceFile } from "yuku";
import { pipeline } from "#module";

export function parse(source: string, options: Omit<ParseOptions, "wasm"> = {}): SourceFile {
  return parseYuku(source, { ...options, wasm: pipeline });
}
```

To load the build in every runtime, follow `yuku`'s own [loaders](https://github.com/yuku-toolchain/yuku/tree/main/packages/yuku/core/module): inline the bytes by default, and import the `.wasm` file in Cloudflare Workers and Vercel Edge, which cannot compile WebAssembly from bytes.
