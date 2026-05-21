# yuku-codegen

A high-performance JavaScript and TypeScript code generator written in Zig, powered by [Yuku](https://github.com/yuku-toolchain/yuku).

Renders an ESTree / TypeScript-ESTree AST back to source code, with optional Source Map V3 output. Accepts the `ParseResult` produced by [`yuku-parser`](https://www.npmjs.com/package/yuku-parser) or its `program` node directly.

## Install

```bash
npm install yuku-codegen
```

## Usage

```js
import { parse } from "yuku-parser";
import { print } from "yuku-codegen";

const ast = parse("const x = 1 + 2;");
const { code } = print(ast);

console.log(code); // "const x = 1 + 2;"
```

## API

Three entry points share the same options and result shape. Each accepts the `ParseResult` from `yuku-parser` or a `Program` node.

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
const result = print(ast, {
  format: "pretty",
  indent: 2,
  quotes: "double",
  comments: "some",
  sourceMaps: true,
});
```

| Option       | Type                                     | Default    | Description                                                                  |
| ------------ | ---------------------------------------- | ---------- | ---------------------------------------------------------------------------- |
| `format`     | `"pretty" \| "compact"`                  | `"pretty"` | Whitespace mode. `"compact"` emits only the separators the grammar requires. |
| `indent`     | `number`                                 | `2`        | Spaces per indentation level. Applies in pretty mode only.                   |
| `quotes`     | `"double" \| "single"`                   | `"double"` | Quote style for emitted string literals.                                     |
| `comments`   | `boolean \| "some" \| "line" \| "block"` | `"some"`   | Comment passthrough filter. See [Comments](#comments).                       |
| `sourceMaps` | `boolean \| SourceMapOptions`            | `false`    | `true` emits a Source Map V3 with default metadata; pass an object for fine-grained control. See [Source maps](#source-maps). |

## Source maps

Set `sourceMaps: true` for a Source Map V3 with default metadata, or pass a `SourceMapOptions` object to fill in `file`, `sources`, `sourcesContent`, and `sourceRoot`. `false` (or omitting the field) disables map output.

Source maps require an AST produced by `yuku-parser`. Hand-built or third-party ASTs throw a clear error when `sourceMaps` is enabled.

```js
import { parse } from "yuku-parser";
import { print } from "yuku-codegen";

const ast = parse(`const x = 1;`);
const { map } = print(ast, { sourceMaps: true });
```

For a map with embedded source information:

```js
import { parse } from "yuku-parser";
import { print } from "yuku-codegen";

const source = `const greet = (name) => "Hello, " + name;`;
const ast = parse(source);

const { code, map } = print(ast, {
  sourceMaps: {
    file: "out.js",
    sourceFileName: "in.js",
    sourcesContent: source,
  },
});

await Bun.write("out.js", `${code}\n//# sourceMappingURL=out.js.map`);
await Bun.write("out.js.map", JSON.stringify(map));
```

### `SourceMapOptions`

| Field            | Type     | Description                                                                         |
| ---------------- | -------- | ----------------------------------------------------------------------------------- |
| `file`           | `string` | Output filename, embedded as the map's `file`.                                      |
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

const ast = parse(`const x: number = 1;`, { lang: "ts" });
console.log(strip(ast).code); // "const x = 1;"
```

Type annotations, type aliases, interfaces, and other type-only constructs are dropped. Constructs that have no clean JavaScript equivalent (`enum`, `namespace`, `import = require()`, `export =`) are reported in `errors` and elided. The output is always syntactically valid JavaScript.

## Comments

Comments live on the AST nodes they were attached to during parsing. To preserve them, parse with `attachComments: true`:

```js
const ast = parse(source, { attachComments: true });
print(ast).code;
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
const ast = parse(`// hello\nconst x = 1;`, { attachComments: true });
print(ast, { comments: true }).code;
// "// hello\nconst x = 1;"
```

Because comments are attached to nodes, they survive AST transforms: move or replace a node and its comments come with it. See the [yuku-parser comments docs](https://www.npmjs.com/package/yuku-parser#comments) for the full design rationale.

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

const { code } = minify(ast, { format: "compact" });
```

## License

MIT
