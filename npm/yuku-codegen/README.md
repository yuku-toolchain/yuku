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
import { generate } from "yuku-codegen";

const { program } = parse("const x = 1 + 2;");
const { code } = generate(program);

console.log(code); // "const x = 1 + 2;"
```

## API

`generate` takes the `Program` node off the `ParseResult` returned by [`yuku-parser`](https://www.npmjs.com/package/yuku-parser) and returns a `GenerateResult`:

```ts
interface GenerateResult {
  code: string;
  errors: Diagnostic[];
  map: SourceMap | null;
}
```

`errors` is empty on a clean run. `map` is non-null only when `sourceMap` is enabled.

## Options

Every transformation is an independent flag, so they compose freely:

```js
const { code, map } = generate(program, {
  strip: true,
  minify: true,
  sourceMap: { source },
});
```

| Option      | Type                                                             | Default      | Description                                                                       |
| ----------- | ---------------------------------------------------------------- | ------------ | --------------------------------------------------------------------------------- |
| `strip`     | `boolean`                                                        | `false`      | Drop TypeScript-only syntax and emit plain JavaScript. See [TypeScript stripping](#typescript-stripping). |
| `minify`    | `boolean \| MinifyOptions`                                       | `false`      | `true` for maximum minification, an object for fine-grained control. See [Minification](#minification). |
| `format`    | `"pretty" \| "compact"`                                          | `"pretty"`   | Whitespace mode. `"compact"` emits only the separators the grammar requires.      |
| `indent`    | `number`                                                         | `2`          | Spaces per indentation level. Applies in pretty mode only.                        |
| `quotes`    | `"preserve" \| "double" \| "single" \| "shortest"`               | `"preserve"` | Quote style for string literals. See [Quotes](#quotes).                           |
| `comments`  | `boolean \| "some" \| "none" \| "line" \| "block"`               | `"some"`     | Comment passthrough filter. See [Comments](#comments).                            |
| `sourceMap` | `SourceMapOptions`                                               | `undefined`  | Pass an object to emit a Source Map V3. See [Source maps](#source-maps).          |

## TypeScript stripping

`strip: true` rewrites the AST as plain JavaScript.

```js
import { parse } from "yuku-parser";
import { generate } from "yuku-codegen";

const { program } = parse(`const x: number = 1;`, { lang: "ts" });
console.log(generate(program, { strip: true }).code); // "const x = 1;"
```

Type annotations, type aliases, interfaces, and other type-only constructs are dropped. Constructs that have no clean JavaScript equivalent (`enum`, `namespace`, `import = require()`, `export =`) are reported in `errors` and elided. The output is always syntactically valid JavaScript.

## Minification

`minify: true` enables every switch for maximum minification:

```js
const { code } = generate(program, { minify: true });
```

For fine-grained control, pass an object. Enabled switches override `format` and `quotes`.

| Switch       | Behavior                                                                     |
| ------------ | ---------------------------------------------------------------------------- |
| `whitespace` | Emit compact whitespace.                                                     |
| `syntax`     | Apply size-reducing syntax rewrites (see below).                             |
| `quotes`     | Use whichever quote needs fewer escapes per literal.                         |

```js
// rewrites only, keeping readable output
const { code } = generate(program, { minify: { syntax: true } });
```

The `syntax` rewrites:

- `true` and `false` rewrite to `!0` and `!1`.
- `undefined` rewrites to `void 0` (in expression position).
- `Infinity` rewrites to `1/0`.
- Numeric literals shorten to their shortest form (`1000000` becomes `1e6`, `0.5` becomes `.5`).
- `obj["foo"]` rewrites to `obj.foo` when the key is a valid identifier.
- `{ "foo": x }` rewrites to `{ foo: x }` when safe.

## Quotes

| Value        | Behavior                                                                                     |
| ------------ | -------------------------------------------------------------------------------------------- |
| `"preserve"` | Keep each literal's source quote style, re-escaping the content. _(default)_                 |
| `"double"`   | Force double quotes.                                                                         |
| `"single"`   | Force single quotes.                                                                         |
| `"shortest"` | Pick whichever quote needs fewer escapes per literal; double on a tie.                       |

## Comments

Comments live on the AST nodes they were attached to during parsing. To preserve them, parse with `attachComments: true`:

```js
const { program } = parse(source, { attachComments: true });
generate(program).code;
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
generate(program, { comments: true }).code;
// "// hello\nconst x = 1;"
```

Because comments are attached to nodes, they survive AST transforms: move or replace a node and its comments come with it. See the [yuku-parser comments docs](https://www.npmjs.com/package/yuku-parser#comments) for the comment options.

## Source maps

Pass a `SourceMapOptions` object to emit a Source Map V3. Its `source` field is required: feed it the original source text (this is what maps generated positions back to the source). Without it, no map is produced and `map` is `null`. The remaining fields (`file`, `sources`, `sourcesContent`, `sourceRoot`) are optional metadata.

```js
import { parse } from "yuku-parser";
import { generate } from "yuku-codegen";

const source = `const greet = (name) => "Hello, " + name;`;
const { program } = parse(source);

const { code, map } = generate(program, {
  sourceMap: {
    source,
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
| `source`         | `string`   | **Required.** The original source text, used to map positions to the source. |
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

## License

MIT
