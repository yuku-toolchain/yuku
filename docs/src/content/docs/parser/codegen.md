---
title: Codegen
description: Print a parsed Tree back to JavaScript source. Configurable formatting, quote style, and indentation.
---

The codegen takes a `Tree` and writes it back as JavaScript source. It walks the AST directly and emits each node from its tag, so the output is always syntactically valid and faithful to the tree's structure. There is no source rewriting, no whitespace overlay, no second pass. One traversal, writing into a single output buffer.

:::note
Codegen does not yet emit sourcemaps and there is no sourcemap option on `Options`. Sourcemap support is on the roadmap.
:::

## Zig

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
};
```

| Field    | Type             | Description                                          |
| -------- | ---------------- | ---------------------------------------------------- |
| `code`   | `[]const u8`     | Generated source                                     |
| `errors` | `[]Diagnostic`   | Codegen-detected problems, empty for a clean print   |

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
});
```

| Field           | Type     | Default   | Description                                                  |
| --------------- | -------- | --------- | ------------------------------------------------------------ |
| `format`        | `Format` | `.pretty` | `.pretty` (indented) or `.compact` (no extra whitespace)     |
| `indent`        | `u8`     | `2`       | Spaces per level when `format == .pretty`                    |
| `quotes`        | `Quotes` | `.double` | `.double` or `.single` for emitted string literals           |

`Format` controls only discretionary whitespace. Grammar-required separators (semicolons, commas, parentheses) are always emitted regardless of mode.

## Type stripping

Strip TypeScript syntax from a `Tree`, leaving JavaScript. Same codegen, same options, with TypeScript-only nodes and fields removed from the output.

### Zig

```zig
var tree = try parser.parse(allocator, source, .{ .lang = .ts });
defer tree.deinit();

const result = try parser.codegen.strip(allocator, &tree, .{});
defer result.deinit(allocator);

std.debug.print("{s}", .{result.code});
```

### Node.js

```bash
npm install yuku-strip
```

```js
import { strip } from "yuku-strip";
import { readFileSync } from "node:fs";

const { code, errors } = strip(readFileSync("example.ts", "utf8"));
console.log(code);
```

```ts
// example.ts

import { run, type Awaitable, type Result as Res } from "./api"

export type Unwrap<T> = T extends Awaitable<infer U> ? Unwrap<U> : T
export type Tagged<T> = { readonly [K in keyof T as `${string & K}$`]: T[K] }

class Store<T extends { id: `id_${string}` }> implements Iterable<T> {
  declare readonly index!: ReadonlyMap<T["id"], T>

  async load<R extends T = T>(id: T["id"]): Promise<R | null> {
    return (await run<R>(id))! satisfies R | null
  }

  *[Symbol.iterator](): IterableIterator<T> {
    for (const [, v] of this.index!) yield v
  }
}

function ensure<T, U extends T>(
  xs: readonly T[],
  is: (x: T) => x is U,
): asserts xs is readonly U[] {
  if (!xs.every(is)) throw new Error()
}

const fns = {
  ok: <const T>(v: T) => ({ tag: "ok" as const, v }),
  err: (r: string) => ({ tag: "err" as const, r }),
} satisfies Record<string, (...a: never[]) => Res<unknown>>
```

`yuku-strip` produces:

```js
import { run } from "./api";
class Store {
  async load(id) {
    return (await run(id));
  }
  *[Symbol.iterator]() {
    for (const [, v] of this.index) yield v;
  }
}
function ensure(xs, is) {
  if (!xs.every(is)) throw new Error();
}
const fns = { ok: (v) => ({ tag: "ok", v }), err: (r) => ({ tag: "err", r }) };
```

The same options are accepted, in camelCase:

| Option         | Values                                    | Default    |
| -------------- | ----------------------------------------- | ---------- |
| `sourceType`   | `"module"`, `"script"`                    | `"module"` |
| `lang`         | `"ts"`, `"tsx"`, `"dts"`, `"js"`, `"jsx"` | `"ts"`     |
| `format`       | `"pretty"`, `"compact"`                   | `"pretty"` |
| `indent`       | `number`                                  | `2`        |
| `quotes`       | `"double"`, `"single"`                    | `"double"` |

### How it works

Stripping is not regex, not a separate transform pass, not a whitespace overlay on top of the original source. It is the codegen with one extra rule per node visit: skip nodes that are TypeScript-only, and skip TypeScript-only fields on shared nodes.

That makes it always accurate and reliable. Nothing is parsed by hand a second time. The parser already classified every byte, and the codegen reads that classification directly. Comments, whitespace, nested template literal types, generic call expressions, conditional types, and every other awkward boundary case are tree nodes like any other, not regex edge cases. And it is extremely fast, one traversal of the parsed tree, writing directly into an output buffer.

### What stripping does not do

A few TypeScript features (`enum`, `namespace`, `module`, `export =`, `import = require()`, parameter properties) emit JavaScript runtime values. Converting them to JavaScript equivalents is a transpilation step, not a syntax-stripping step. The stripper does exactly what its name says, it strips TypeScript syntax. When a runtime-emitting construct is encountered, it is reported as a `Diagnostic` and skipped, and the rest of the file is still emitted.

The ambient forms of these constructs (`declare enum`, `declare namespace`, `declare module`, `import type X = require(...)`) carry no runtime, and are stripped silently along with the rest of the type system.

A future Yuku transpiler will fill that role. It will lower runtime-emitting constructs to JavaScript and drive the codegen with stripping enabled in the same pass, giving fast, clean TypeScript-to-JavaScript output in a single pipeline.

All other TypeScript syntax (types, interfaces, type aliases, generics, type assertions, `satisfies`, non-null `!`, `declare`, `abstract`) strips cleanly today.
