# yuku-codegen

A high-performance JavaScript/TypeScript code generator written in Zig, powered by [Yuku](https://github.com/yuku-toolchain/yuku).

Renders an ESTree / TypeScript-ESTree AST back to source code, with optional Source Map V3 output. The input AST is exactly what [`yuku-parser`](https://www.npmjs.com/package/yuku-parser) produces.

## Install

```bash
npm install yuku-codegen
```

## Usage

```js
import { parse } from "yuku-parser";
import { print } from "yuku-codegen";

const { program } = parse("const x = 1 + 2;");
const { code } = print(program);

console.log(code); // "const x = 1 + 2;"
```

## API

Three entry points share the same options shape:

- `print(ast, options?)` renders verbatim, preserving TypeScript syntax.
- `strip(ast, options?)` drops type-only syntax and emits plain JavaScript.
- `minify(ast, options?)` applies size-reducing rewrites such as `true → !0` and shortest-numeric-form. Combine with `format: "compact"` for full minification.

All return the same `CodegenResult`:

```ts
interface CodegenResult {
  code: string;
  errors: Diagnostic[];
  map: SourceMap | null;
}
```

`errors` is empty when codegen succeeded cleanly. `map` is populated only when `sourceMaps` is enabled.

## Options

```js
const result = print(ast, {
  format: "pretty",
  indent: 2,
  quotes: "double",
  sourceMaps: { source },
});
```

| Option       | Values                  | Default    | Description                                                                                |
| ------------ | ----------------------- | ---------- | ------------------------------------------------------------------------------------------ |
| `format`     | `"pretty"`, `"compact"` | `"pretty"` | Whitespace mode. `"compact"` emits only the separators the grammar requires.               |
| `indent`     | `number`                | `2`        | Spaces per indentation level. Used only when `format` is `"pretty"`.                       |
| `quotes`     | `"double"`, `"single"`  | `"double"` | Quote style for emitted string literals.                                                   |
| `sourceMaps` | `SourceMapOptions`      | _disabled_ | Pass an object to emit a Source Map V3 alongside the code. Omit (or set to `null`) to skip. |

## Source maps

Pass `sourceMaps` to emit a Source Map V3 alongside the code. The original source text is required.

```js
import { parse } from "yuku-parser";
import { print } from "yuku-codegen";

const source = `const greet = (name) => "Hello, " + name;`;
const { program } = parse(source);

const { code, map } = print(program, {
  sourceMaps: {
    source,
    file: "out.js",
    sourceFileName: "in.js",
    sourcesContent: true,
  },
});

await Bun.write("out.js", code + `\n//# sourceMappingURL=out.js.map`);
await Bun.write("out.js.map", JSON.stringify(map));
```

### `SourceMapOptions`

| Field             | Type      | Required | Description                                                       |
| ----------------- | --------- | -------- | ----------------------------------------------------------------- |
| `source`          | `string`  | yes      | Original source the AST was parsed from.                          |
| `file`            | `string`  | no       | Output filename, embedded as the map's `file`.                    |
| `sourceFileName`  | `string`  | no       | Source filename, embedded as the single entry of `sources`.       |
| `sourceRoot`      | `string`  | no       | Prefix embedded as `sourceRoot`.                                  |
| `sourcesContent`  | `boolean` | no       | When `true`, embeds `source` into the map's `sourcesContent`.     |

### Result

The returned `map` is a Source Map V3 object, ready to `JSON.stringify`:

```ts
interface SourceMap {
  version: 3;
  file: string | null;
  sourceRoot: string | null;
  sources: string[];
  sourcesContent: (string | null)[] | null;
  names: string[];
  mappings: string;
}
```

Columns are 0-indexed UTF-16 code units, matching the convention used by Chrome DevTools and every consumer-side library (`@jridgewell/trace-mapping`, `source-map`, etc.).

## TypeScript stripping

`strip` rewrites the AST as plain JavaScript:

```js
import { parse } from "yuku-parser";
import { strip } from "yuku-codegen";

const { program } = parse(`const x: number = 1;`, { lang: "ts" });
console.log(strip(program).code); // "const x = 1;"
```

Type annotations, type aliases, interfaces, and other type-only constructs are dropped. Constructs that don't have a clean JavaScript equivalent (`enum`, `namespace`, `import = require()`, `export =`) are reported in `errors` and elided. The output is always syntactically valid JavaScript.

## Minification

`minify` applies size-reducing rewrites at print time:

- `true` / `false` → `!0` / `!1`
- `undefined` → `void 0` (in expression position)
- `Infinity` → `1/0`
- numeric literals shortened to their shortest form (`1000000` → `1e6`, `0.5` → `.5`, etc.)
- `obj["foo"]` → `obj.foo` when the key is a valid identifier
- `{ "foo": x }` → `{ foo: x }` when safe

Combine with `format: "compact"` for full minification:

```js
import { minify } from "yuku-codegen";

const { code } = minify(program, { format: "compact" });
```

## License

MIT
