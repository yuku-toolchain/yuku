# yuku-codegen

A fast code generator for any ESTree / TypeScript-ESTree AST, with type stripping, minification, and Source Map V3 output, part of [Yuku](https://yuku.fyi).

It is plain JavaScript with no native binary, and prints any ESTree AST, whichever parser produced it. Its output is byte-identical to Yuku's Zig printer, source maps included, and it runs 2.6x faster than `@babel/generator`, or 3x with source maps on.

- [Install](#install)
- [Usage](#usage)
- [Result](#result)
- [Options](#options)
- [JSX](#jsx)
- [Type stripping](#type-stripping)
- [Minification](#minification)
- [Quotes](#quotes)
- [Comments](#comments)
- [Source maps](#source-maps)

## Install

```bash
npm install yuku-codegen
```

## Usage

```js
import { generate } from "yuku-codegen";
import { parse } from "yuku-parser";

const { code } = generate(parse("const x = 1 + 2;").program);
// "const x = 1 + 2;"
```

## Result

`generate` takes a `Program` node and returns a `GenerateResult`.

```ts
interface GenerateResult {
  code: string;
  diagnostics: Diagnostic[]; // empty on a clean run
  map: SourceMap | null;     // with sourceMap
}
```

`diagnostics` has the same shape as [`yuku-parser`'s](https://www.npmjs.com/package/yuku-parser#diagnostics).

## Options

Every transformation is an independent flag, so they compose freely.

```js
generate(program, { strip: true, minify: true, sourceMap: { source } });
```

| Option      | Type                                                        | Default      | Description                                                                 |
| ----------- | ----------------------------------------------------------- | ------------ | --------------------------------------------------------------------------- |
| `jsx`       | `boolean \| "preserve" \| JSXOptions`                       | `false`      | Preserve JSX or lower it with classic factories. See [JSX](#jsx).           |
| `strip`     | `boolean`                                                   | `false`      | Drop TypeScript-only syntax. See [Type stripping](#type-stripping).         |
| `minify`    | `boolean \| MinifyOptions`                                  | `false`      | Minify the output. See [Minification](#minification).                       |
| `format`    | `"pretty" \| "compact"`                                     | `"pretty"`   | `"compact"` emits only the separators the grammar requires.                 |
| `indent`    | `number`                                                    | `2`          | Spaces per level in pretty format, from 0 to 255.                           |
| `quotes`    | `"preserve" \| "double" \| "single" \| "shortest"`          | `"preserve"` | Quote style for string literals. See [Quotes](#quotes).                     |
| `comments`  | `boolean \| "all" \| "some" \| "none" \| "line" \| "block"` | `"some"`     | Which attached comments to emit. See [Comments](#comments).                 |
| `sourceMap` | `SourceMapOptions`                                          | none         | Emit a Source Map V3. See [Source maps](#source-maps).                      |

## JSX

`jsx: true` lowers JSX while printing with the classic `React.createElement` and `React.Fragment`
factories. Set `pragma` and `pragmaFrag` to use another runtime. Factories must be identifiers or
dotted names already in scope.

```js
generate(program, { jsx: true, strip: true });
generate(program, { jsx: { pragma: "h", pragmaFrag: "Fragment" }, strip: true });
generate(program, { jsx: { pragma: "h", pragmaFrag: "Fragment", pure: true }, strip: true });
```

Calls to the default React factories carry `/* @__PURE__ */` annotations so bundlers can drop
unused elements. The annotation claims the call has no effects, so custom factories are annotated
only when `pure` is set explicitly; `pure: false` removes it for the defaults.

`jsx: "preserve"` keeps JSX for a framework transform such as Vue's JSX plugin while `strip`
removes TypeScript. A custom factory alone does not implement Vue directives or slots.

JSX tag type arguments are erased. Text follows JSX whitespace normalization, and text and quoted
attributes decode XHTML entities. Spreads retain evaluation order. Attached comments follow
`comments`, including those on erased types, closing names, and closing fragments. The runtime
is classic. Imports and comment pragmas are not generated.

## Type stripping

`strip: true` prints a TypeScript AST as plain JavaScript.

```js
generate(parse(`const x: number = 1;`, { lang: "ts" }).program, { strip: true }).code;
// "const x = 1;"
```

Types, interfaces, type aliases, generics, type assertions, `satisfies`, non-null `!`, `declare`, and `abstract` strip cleanly. A few TypeScript features emit runtime values: `enum`, `namespace`, `module`, `export =`, `import = require()`, and parameter properties. Converting them is transpilation, not stripping, so each one is reported in `diagnostics` and left out, a parameter property keeps its plain parameter, and the rest of the file is still emitted. Their ambient forms (`declare enum`, `declare namespace`, `declare module`, `import type X = require(...)`) carry no runtime and strip silently.

## Minification

`minify: true` enables every switch. An object picks them, and enabled switches override `format` and `quotes`.

| Switch       | Behavior                                                    |
| ------------ | ----------------------------------------------------------- |
| `whitespace` | Emit compact whitespace.                                    |
| `syntax`     | Apply the size-reducing syntax rewrites below.              |
| `quotes`     | Use whichever quote needs fewer escapes per literal.        |

```js
generate(program, { minify: { syntax: true } }); // rewrites only, readable output
```

The syntax rewrites:

- `true` and `false` become `!0` and `!1`.
- Numeric literals take their shortest form (`1000000` becomes `1e6`, `0.5` becomes `.5`).
- `obj["foo"]` becomes `obj.foo` when the key is a valid identifier.
- `{ "foo": x }` becomes `{ foo: x }` when safe.
- `</script`, `<!--`, and `-->` are escaped in strings and untagged template literals, so the output is safe to inline in a `<script>` tag. Tagged templates keep their raw text, since the tag reads it.

## Quotes

| Value        | Behavior                                                               |
| ------------ | ---------------------------------------------------------------------- |
| `"preserve"` | Keep each literal's source quote style, re-escaping the content.       |
| `"double"`   | Force double quotes.                                                   |
| `"single"`   | Force single quotes.                                                   |
| `"shortest"` | Pick whichever quote needs fewer escapes per literal, double on a tie. |

## Comments

Comments print from the nodes they are attached to, so parse with [`attachComments: true`](https://www.npmjs.com/package/yuku-parser#comments) to keep them. Because they live on nodes, they move with their node through transforms.

| Value                | Behavior                                                                            |
| -------------------- | ----------------------------------------------------------------------------------- |
| `"some"`             | Legal headers, JSDoc, and `@`/`#` annotations, the bundler convention. The default. |
| `"all"` or `true`    | Every comment.                                                                      |
| `"none"` or `false`  | No comments.                                                                        |
| `"line"`             | `// ...` only.                                                                      |
| `"block"`            | `/* ... */` only.                                                                   |

```js
const { program } = parse(`// hello\nconst x = 1;`, { attachComments: true });
generate(program, { comments: true }).code;
// "// hello\nconst x = 1;"
```

## Source maps

Pass the original source to emit a Source Map V3 alongside the code.

```js
const { code, map } = generate(program, {
  sourceMap: { source, file: "out.js", sourceFileName: "in.js", sourcesContent: source },
});

const output = `${code}\n//# sourceMappingURL=out.js.map`;
const mapJson = JSON.stringify(map);
```

| Field            | Description                                                 |
| ---------------- | ----------------------------------------------------------- |
| `source`         | **Required.** The original source text, positions map to it. |
| `file`           | Output filename, embedded as `file`.                        |
| `sourceFileName` | Source filename, the single entry of `sources`.             |
| `sourceRoot`     | Embedded as `sourceRoot`.                                   |
| `sourcesContent` | The single entry of `sourcesContent`.                       |

`map` is a Source Map V3 object, ready for `JSON.stringify`. Columns are 0-indexed UTF-16 code units, the convention of browser devtools and source map libraries.

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

## License

MIT
