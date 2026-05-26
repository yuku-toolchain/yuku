---
title: Codegen
description: Print a parsed Tree back to JavaScript or TypeScript source, with configurable formatting, type stripping, minification, and Source Map V3 output.
---

The codegen takes a `Tree` and writes it back as source code. It walks the AST directly and emits each node from its tag, so the output is always syntactically valid and faithful to the tree's structure.

## Node.js

```bash
npm install yuku-codegen
```

```js
import { parse } from "yuku-parser";
import { print } from "yuku-codegen";

const { program } = parse("const x = 1 + 2;");
const { code } = print(program);
```

`print`, `strip`, and `minify` take the `Program` node off the `ParseResult` returned by [yuku-parser](https://www.npmjs.com/package/yuku-parser). For source maps, pass the parser's `lineStarts` via `sourceMaps: { lineStarts }`. See [yuku-codegen on npm](https://www.npmjs.com/package/yuku-codegen) for the full API.

## Zig

```bash
zig fetch --save git+https://github.com/yuku-toolchain/yuku.git
```

```zig
const std = @import("std");
const parser = @import("parser");

pub fn main() !void {
    const allocator = std.heap.smp_allocator;

    var tree = try parser.parse(allocator, "const x = 1 + 2;", .{});
    defer tree.deinit();

    const result = try parser.codegen.print(allocator, &tree, .{});
    defer result.deinit(allocator);

    std.debug.print("{s}", .{result.code});
}
```

`print` reads from the tree, writes a fresh buffer, and never mutates anything. The `Tree` remains valid after the call. The returned `Result` owns its buffers, freed via `result.deinit(allocator)`.

## Result

```zig
pub const Result = struct {
    code: []const u8,
    errors: []const Diagnostic,
    map: ?SourceMap = null,
};
```

| Field    | Type           | Description                                         |
| -------- | -------------- | --------------------------------------------------- |
| `code`   | `[]const u8`   | Generated source                                    |
| `errors` | `[]Diagnostic` | Codegen-detected problems, empty for a clean print  |
| `map`    | `?SourceMap`   | Source Map V3 when `source_maps` was set, else null |

The only return-value error from `print` is allocation failure. Codegen-detected problems are reported in `errors` and do not abort the run. For a plain `print`, `errors` is always empty. They appear only when stripping (see [Type stripping](#type-stripping)).

A `Diagnostic` carries:

```zig
pub const Diagnostic = struct {
    message: []const u8,
    start: u32,
    end: u32,
};
```

## Options

```zig
const result = try parser.codegen.print(allocator, &tree, .{
    .format = .pretty,
    .indent = 2,
    .quotes = .double,
    .comments = .some,
    .source_maps = null,
});
```

| Field         | Type                | Default   | Description                                                |
| ------------- | ------------------- | --------- | ---------------------------------------------------------- |
| `format`      | `Format`            | `.pretty` | `.pretty` (indented) or `.compact` (no extra whitespace)   |
| `indent`      | `u8`                | `2`       | Spaces per level when `format == .pretty`                  |
| `quotes`      | `Quotes`            | `.double` | `.double` or `.single` for emitted string literals         |
| `comments`    | `Comments`          | `.some`   | Comment passthrough filter. See [Comments](#comments).     |
| `source_maps` | `?SourceMapOptions` | `null`    | Set to emit a Source Map V3 alongside the code             |

`Format` controls only discretionary whitespace. Grammar-required separators (semicolons, commas, parentheses) are always emitted regardless of mode.

## Source maps

Set `source_maps` to emit a Source Map V3 alongside the generated code.

```zig
var tree = try parser.parse(allocator, source, .{});
defer tree.deinit();

const result = try parser.codegen.print(allocator, &tree, .{
    .source_maps = .{
        .file = "out.js",
        .source_file_name = "in.js",
        .sources_content = source,
    },
});
defer result.deinit(allocator);

// result.map is non-null when source_maps was set.
const map = result.map.?;
```

### `SourceMapOptions`

```zig
pub const SourceMapOptions = struct {
    file: ?[]const u8 = null,
    source_file_name: ?[]const u8 = null,
    source_root: ?[]const u8 = null,
    sources_content: ?[]const u8 = null,
};
```

| Field              | Type          | Description                                                          |
| ------------------ | ------------- | -------------------------------------------------------------------- |
| `file`             | `?[]const u8` | Output filename, embedded as the map's `file`                        |
| `source_file_name` | `?[]const u8` | Source filename, embedded as the single entry of `sources`           |
| `source_root`      | `?[]const u8` | Prefix embedded as `sourceRoot`                                      |
| `sources_content`  | `?[]const u8` | When set, embedded as the single entry of the map's `sourcesContent` |

### Result shape

`SourceMap` is the Source Map V3 wire format, ready to serialize:

```zig
pub const SourceMap = struct {
    version: u8 = 3,
    file: ?[]const u8,
    source_root: ?[]const u8,
    sources: []const []const u8,
    sources_content: ?[]const ?[]const u8,
    names: []const []const u8,
    mappings: []const u8,
};
```

Columns are 0-indexed UTF-16 code units, matching the convention used by Chrome DevTools and consumer-side libraries (`@jridgewell/trace-mapping`, `source-map`, etc.).

## Comments

Comments live on the AST nodes they were attached to during parsing (see [Comments](/parser/ast#comments) in the AST reference). For codegen to print them, the tree must have been parsed with `comments = .attached` (or `.both`).

The `comments` option selects which attached comments are emitted. The default is `.some`, matching the bundler convention of preserving legal banners, JSDoc, and tree-shaking annotations while dropping plain noise.

```zig
pub const Comments = enum {
    none,    // drop every comment
    all,     // emit every comment
    some,    // legal banners, jsdoc, and tree-shaking annotations
    line,    // emit `// ...` only
    block,   // emit `/* ... */` only
};
```

Because comments are attached to nodes, they survive AST transforms: move or replace a node and its comments come with it.

## Type stripping

Strip TypeScript syntax from a `Tree`, leaving JavaScript. Same codegen, same options, with TypeScript-only nodes and fields removed from the output.

```zig
var tree = try parser.parse(allocator, source, .{ .lang = .ts });
defer tree.deinit();

const result = try parser.codegen.strip(allocator, &tree, .{});
defer result.deinit(allocator);

std.debug.print("{s}", .{result.code});
```

### How it works

Stripping is not regex, not a separate transform pass, not a whitespace overlay on top of the original source. It is the codegen with one extra rule per node visit: skip nodes that are TypeScript-only, and skip TypeScript-only fields on shared nodes.

That makes it always accurate and reliable. Nothing is parsed by hand a second time. The parser already classified every byte, and the codegen reads that classification directly. Comments, whitespace, nested template literal types, generic call expressions, conditional types, and every other awkward boundary case are tree nodes like any other, not regex edge cases. And it is extremely fast, one traversal of the parsed tree, writing directly into an output buffer.

### What stripping does not do

A few TypeScript features (`enum`, `namespace`, `module`, `export =`, `import = require()`, parameter properties) emit JavaScript runtime values. Converting them to JavaScript equivalents is a transpilation step, not a syntax-stripping step. The stripper does exactly what its name says, it strips TypeScript syntax. When a runtime-emitting construct is encountered, it is reported as a `Diagnostic` and skipped, and the rest of the file is still emitted.

The ambient forms of these constructs (`declare enum`, `declare namespace`, `declare module`, `import type X = require(...)`) carry no runtime, and are stripped silently along with the rest of the type system.

A future Yuku transpiler will fill that role. It will lower runtime-emitting constructs to JavaScript and drive the codegen with stripping enabled in the same pass, giving fast, clean TypeScript-to-JavaScript output in a single pipeline.

All other TypeScript syntax (types, interfaces, type aliases, generics, type assertions, `satisfies`, non-null `!`, `declare`, `abstract`) strips cleanly today.

## Minification

`minify` applies size-reducing rewrites at print time:

```zig
const result = try parser.codegen.minify(allocator, &tree, .{ .format = .compact });
defer result.deinit(allocator);
```

The substitutions:

- `true` / `false` → `!0` / `!1`
- `undefined` → `void 0` (in expression position)
- `Infinity` → `1/0`
- numeric literals shortened to their shortest form (`1000000` → `1e6`, `0.5` → `.5`, etc.)
- `obj["foo"]` → `obj.foo` when the key is a valid identifier
- `{ "foo": x }` → `{ foo: x }` when safe

Combine with `format = .compact` for full minification.
