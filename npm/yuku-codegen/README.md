# yuku-codegen

A high-performance JavaScript and TypeScript code generator written in Zig, powered by [Yuku](https://github.com/yuku-toolchain/yuku).

Renders an ESTree / TypeScript-ESTree AST back to source code, with optional Source Map V3 output.

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

All three entry points take the `Program` node off the `ParseResult` returned by [`yuku-parser`](https://www.npmjs.com/package/yuku-parser) and share the same options and result shape.

| Function | Behavior                                                                                              |
| -------- | ----------------------------------------------------------------------------------------------------- |
| `print`  | Renders verbatim, preserving TypeScript syntax.                                                       |
| `strip`  | Drops type-only syntax and emits plain JavaScript. See [TypeScript stripping](#typescript-stripping). |
| `minify` | Applies size-reducing rewrites at print time. See [Minification](#minification).                      |

All return a `CodegenResult`:

```ts
interface CodegenResult {
  code: string;
  errors: Diagnostic[];
  map: SourceMap | null;
}
```

`errors` is empty on a clean run. `map` is non-null only when `sourceMaps` is enabled.

## Options

```js
const out = print(program, {
  format: "pretty",
  indent: 2,
  quotes: "double",
  comments: "some",
  sourceMaps: { lineStarts },
});
```

| Option       | Type                                     | Default    | Description                                                                  |
| ------------ | ---------------------------------------- | ---------- | ---------------------------------------------------------------------------- |
| `format`     | `"pretty" \| "compact"`                  | `"pretty"` | Whitespace mode. `"compact"` emits only the separators the grammar requires. |
| `indent`     | `number`                                 | `2`        | Spaces per indentation level. Applies in pretty mode only.                   |
| `quotes`     | `"double" \| "single"`                   | `"double"` | Quote style for emitted string literals.                                     |
| `comments`   | `boolean \| "some" \| "line" \| "block"` | `"some"`   | Comment passthrough filter. See [Comments](#comments).                       |
| `sourceMaps` | `SourceMapOptions`                       | `undefined` | Pass an object to emit a Source Map V3. Its `lineStarts` is required, the rest of the metadata is optional. See [Source maps](#source-maps). |

## Source maps

Pass a `SourceMapOptions` object to emit a Source Map V3. Its `lineStarts` field is required: feed it the `lineStarts` array from the parser's `ParseResult` (this is what maps generated positions back to the source). The remaining fields (`file`, `sources`, `sourcesContent`, `sourceRoot`) are optional metadata.

```js
import { parse } from "yuku-parser";
import { print } from "yuku-codegen";

const source = `const greet = (name) => "Hello, " + name;`;
const { program, lineStarts } = parse(source);

const { code, map } = print(program, {
  sourceMaps: {
    lineStarts,
    file: "out.js",
    sourceFileName: "in.js",
    sourcesContent: source,
  },
});

await Bun.write("out.js", `${code}\n//# sourceMappingURL=out.js.map`);
await Bun.write("out.js.map", JSON.stringify(map));
```

### `SourceMapOptions`

| Field            | Type       | Description                                                                       |
| ---------------- | ---------- | --------------------------------------------------------------------------------- |
| `lineStarts`     | `number[]` | **Required.** The parser's `ParseResult.lineStarts`, used to map positions to the source. |
| `file`           | `string`   | Output filename, embedded as the map's `file`.                                    |
| `sourceFileName` | `string` | Source filename, embedded as the single entry of `sources`.                         |
| `sourceRoot`     | `string` | Prefix embedded as `sourceRoot`.                                                    |
| `sourcesContent` | `string` | When set, embedded as the single entry of the map's `sourcesContent`. Omit to skip. |

### Output shape

`map` is a Source Map V3 object, ready to serialize with `JSON.stringify`:

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

Columns are 0-indexed UTF-16 code units, matching Chrome DevTools and consumer-side libraries like `@jridgewell/trace-mapping` and `source-map`.

## TypeScript stripping

`strip` rewrites the AST as plain JavaScript.

```js
import { parse } from "yuku-parser";
import { strip } from "yuku-codegen";

const { program } = parse(`const x: number = 1;`, { lang: "ts" });
console.log(strip(program).code); // "const x = 1;"
```

Type annotations, type aliases, interfaces, and other type-only constructs are dropped. Constructs that have no clean JavaScript equivalent (`enum`, `namespace`, `import = require()`, `export =`) are reported in `errors` and elided. The output is always syntactically valid JavaScript.

## Comments

Comments live on the AST nodes they were attached to during parsing. To preserve them, parse with `attachComments: true`:

```js
const { program } = parse(source, { attachComments: true });
print(program).code;
```

The `comments` option then selects which attached comments are emitted. The default is `"some"`, which matches the bundler convention of keeping legal banners, JSDoc, and tree-shaking annotations while dropping plain noise.

| Value     | Behavior                                                        |
| --------- | --------------------------------------------------------------- |
| `"some"`  | Emit legal headers, JSDoc, and `@`/`#` annotations. _(default)_ |
| `true`    | Emit every comment.                                             |
| `false`   | Drop every comment.                                             |
| `"line"`  | Emit `// ...` only.                                             |
| `"block"` | Emit `/* ... */` only.                                          |

```js
const { program } = parse(`// hello\nconst x = 1;`, { attachComments: true });
print(program, { comments: true }).code;
// "// hello\nconst x = 1;"
```

Because comments are attached to nodes, they survive AST transforms: move or replace a node and its comments come with it. See the [yuku-parser comments docs](https://www.npmjs.com/package/yuku-parser#comments) for the comment options.

## Minification

`minify` applies size-reducing rewrites at print time:

- `true` and `false` rewrite to `!0` and `!1`.
- `undefined` rewrites to `void 0` (in expression position).
- `Infinity` rewrites to `1/0`.
- Numeric literals shorten to their shortest form (`1000000` becomes `1e6`, `0.5` becomes `.5`).
- `obj["foo"]` rewrites to `obj.foo` when the key is a valid identifier.
- `{ "foo": x }` rewrites to `{ foo: x }` when safe.

Combine with `format: "compact"` for full minification:

```js
import { minify } from "yuku-codegen";

const { code } = minify(program, { format: "compact" });
```

## License

MIT
