# yuku-strip

A high-performance TypeScript stripper. Removes type syntax from JavaScript and TypeScript source, returning runnable JavaScript. Powered by [Yuku](https://github.com/yuku-toolchain/yuku).

## Install

```bash
npm install yuku-strip
```

## Usage

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

## How it works

Stripping is AST-based. The source is parsed once into a structured tree, and the codegen prints it back to JavaScript while skipping TypeScript-only nodes and fields. There is no regex, no whitespace overlay, no second pass.

That makes it always accurate and reliable. Every byte was already classified by the parser, and the stripper reads that classification directly. Nested template literal types, generic call expressions, conditional types, and every other awkward case are tree nodes like any other, not regex edge cases. And because parsing, stripping, and codegen all happen in native Zig in a single traversal, it is extremely fast. The JavaScript side only hands over the source string and reads back the result.

## Options

```js
const { code, errors } = strip(source, {
  sourceType: "module",
  lang: "ts",
  format: "pretty",
  indent: 2,
  quotes: "double",
  finalNewline: true,
});
```

| Option         | Values                                    | Default    | Description                                                |
| -------------- | ----------------------------------------- | ---------- | ---------------------------------------------------------- |
| `sourceType`   | `"module"`, `"script"`                    | `"module"` | Module mode enables `import`/`export` and strict mode      |
| `lang`         | `"ts"`, `"tsx"`, `"dts"`, `"js"`, `"jsx"` | `"ts"`     | Language variant of the input source                       |
| `format`       | `"pretty"`, `"compact"`                   | `"pretty"` | Whitespace mode for the output                             |
| `indent`       | `number`                                  | `2`        | Spaces per level when `format === "pretty"`                |
| `quotes`       | `"double"`, `"single"`                    | `"double"` | Quote style for emitted string literals                    |
| `finalNewline` | `boolean`                                 | `true`     | Append a trailing newline to the output if missing         |

## Result

```ts
interface StripResult {
  code: string;
  errors: Diagnostic[];
}

interface Diagnostic {
  message: string;
  start: number;
  end: number;
}
```

`code` is the stripped JavaScript output. `errors` is empty on a clean strip.

> **Note:** `yuku-strip` does not yet return sourcemaps and has no sourcemap option. Sourcemap support is on the roadmap.

## What stripping does not do

A few TypeScript features (`enum`, `namespace`, `module`, `export =`, `import = require()`, parameter properties) emit JavaScript runtime values. Converting them to JavaScript equivalents is a transpilation step, not a syntax-stripping step. `yuku-strip` does exactly what its name says, it strips TypeScript syntax. When a runtime-emitting construct is encountered, it is reported as a `Diagnostic` and skipped, and the rest of the file is still emitted.

A future Yuku transpiler will fill that role. It will lower these constructs to JavaScript and drive the codegen with stripping enabled in the same pass, giving fast, clean TypeScript-to-JavaScript output in a single pipeline.

All other TypeScript syntax (types, interfaces, type aliases, generics, type assertions, `satisfies`, non-null `!`, `declare`, `abstract`) strips cleanly today.

## License

MIT
