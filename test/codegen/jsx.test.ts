// checks lowered output and executes it against a recording factory to verify JSX semantics

import { expect, test } from "bun:test";
import { parse } from "yuku-parser";
import { generate, type GenerateOptions } from "yuku-codegen";
import { TraceMap, originalPositionFor } from "@jridgewell/trace-mapping";
import { gen } from "./helpers";

function evaluate(source: string, options: GenerateOptions = {}, bindings: object = {}): unknown {
  const result = gen(`const result = ${source};`, { jsx: true, ...options }, "input.tsx", {
    preserveParens: false,
  });
  const React = {
    Fragment: "fragment",
    createElement: (tag: unknown, props: unknown, ...children: unknown[]) => ({
      tag, props, children,
    }),
  };
  return new Function("React", ...Object.keys(bindings), `${result}; return result;`)(
    React, ...Object.values(bindings),
  );
}

test("JSX lowering is opt in and composes with strip and minify", () => {
  const source = `const view: View = <><Box<number> enabled data-id="x">` +
    `{value as number}</Box></>;`;
  expect(gen(source, {}, "input.tsx")).toMatchInlineSnapshot(
    `"const view: View = <><Box<number> enabled data-id="x">{value as number}</Box></>;"`,
  );
  expect(gen(source, { jsx: true, strip: true, minify: true }, "input.tsx"))
    .toBe(
      `const view=/* @__PURE__ */React.createElement(React.Fragment,null,` +
      `/* @__PURE__ */React.createElement(Box,{enabled:!0,"data-id":"x"},value))`,
    );
});

test("factory calls carry a pure annotation for bundler tree shaking", () => {
  for (const comments of [false, true] as const) {
    for (const minify of [false, true] as const) {
      const code = gen(`<div><Box/></div>;`, { jsx: true, comments, minify }, "input.jsx");
      expect(code.split("/* @__PURE__ */").length).toBe(3);
      expect(code.indexOf("/* @__PURE__ */")).toBe(0);
    }
  }
});

test("custom factories are annotated only when pure is set", () => {
  // the annotation claims the factory has no effects, so custom names do not get it for free
  for (const [jsx, annotated] of [
    [true, true],
    [{}, true],
    [{ pragma: "React.createElement", pragmaFrag: "React.Fragment" }, true],
    [{ pragma: "React.createElement", pragmaFrag: "React.Fragment", pure: false }, false],
    [{ pragma: "h", pragmaFrag: "Fragment" }, false],
    [{ pragma: "h", pragmaFrag: "Fragment", pure: true }, true],
    [{ pragmaFrag: "Fragment" }, false],
  ] as const) {
    const code = gen(`<div/>`, { jsx }, "input.jsx");
    expect(code.includes("/* @__PURE__ */"), JSON.stringify(jsx)).toBe(annotated);
  }
});

test("a pure annotation cannot merge a preceding slash into a line comment", () => {
  // compact mode writes `/` and `/* @__PURE__ */` adjacently, which would fuse into `//`
  for (const [source, bindings] of [
    [`a / <div/>`, { a: 6 }],
    [`a / b / <div/>`, { a: 6, b: 2 }],
    [`x = y / <div/>`, { y: 3 }],
  ] as const) {
    for (const minify of [false, true]) {
      expect(evaluate(source, { minify }, bindings), source).toBeNaN();
    }
  }
  expect(gen(`x = y / <div/>`, { jsx: true, format: "compact" }, "input.jsx"))
    .toContain("/ /* @__PURE__ */React.createElement(");
});

test("classic factory options target custom runtimes without changing JSX semantics", () => {
  const h = (tag: unknown, props: unknown, ...children: unknown[]) => ({
    tag, props, children,
  });
  for (const [pragma, pragmaFrag] of [
    ["h", "Fragment"], ["runtime.h", "runtime.Fragment"], ["runtime.default", "Fragment"],
    ["工厂.h", "工厂.Fragment"],
  ]) {
    for (const minify of [false, true]) {
      expect(evaluate(`<><Box enabled />text</>`, {
        jsx: { runtime: "classic", pragma, pragmaFrag }, minify,
      }, { h, Fragment: "fragment", runtime: { h, default: h, Fragment: "fragment" },
        工厂: { h, Fragment: "fragment" }, Box: "box" })).toEqual({
        tag: "fragment", props: null,
        children: [{ tag: "box", props: { enabled: true }, children: [] }, "text"],
      });
    }
  }
});

test("an explicit preserve mode keeps JSX for a framework transform while stripping types", () => {
  const source = `const view: View = <Panel v-model={value as string} />;`;
  expect(gen(source, { jsx: "preserve", strip: true }, "input.tsx"))
    .toMatchInlineSnapshot(`"const view = <Panel v-model={value} />;"`);
  expect(gen(source, { jsx: {} }, "input.tsx"))
    .toBe(gen(source, { jsx: true }, "input.tsx"));
});

test("invalid JSX settings cannot inject expressions into factory names", () => {
  const { program } = parse(`<div/>`, { lang: "jsx" });
  for (const value of ["", "factory()", "foo..bar", "h;evil()", "foo[bar]", "class", "await.h"]) {
    for (const key of ["pragma", "pragmaFrag"] as const) {
      expect(() => generate(program, { jsx: { [key]: value } })).toThrow(TypeError);
    }
  }
  for (const jsx of ["automatic", null, [], { runtime: "automatic" }, { pragma: 42 },
    { pure: "yes" }]) {
    expect(() => generate(program, { jsx } as unknown as GenerateOptions)).toThrow(TypeError);
  }
});

test("tag names distinguish intrinsic, custom, component, member, and namespace names", () => {
  expect(gen(`<div/>;<Widget/>;<Upper-case/>;<ui.Box/>;<this/>;<this.Box/>;<svg:path/>`,
    { jsx: true }, "input.jsx")).toMatchInlineSnapshot(`
"/* @__PURE__ */ React.createElement("div", null);
/* @__PURE__ */ React.createElement(Widget, null);
/* @__PURE__ */ React.createElement("Upper-case", null);
/* @__PURE__ */ React.createElement(ui.Box, null);
/* @__PURE__ */ React.createElement(this, null);
/* @__PURE__ */ React.createElement(this.Box, null);
/* @__PURE__ */ React.createElement("svg:path", null);"
`);
});

test("hyphenated member properties use computed access", () => {
  expect(evaluate(`<ui.foo-bar/>`, {}, { ui: { "foo-bar": "box" } })).toEqual({
    tag: "box", props: null, children: [],
  });
  expect(gen(`<this.foo-bar/>; <foo-bar.Box/>;`, { jsx: true }, "input.jsx"))
    .toMatchInlineSnapshot(`
"/* @__PURE__ */ React.createElement(this["foo-bar"], null);
/* @__PURE__ */ React.createElement("foo-bar".Box, null);"
`);
});

test("text retains significant spaces and joins indented lines", () => {
  for (const [source, children] of [
    ["<div> hello </div>", [" hello "]],
    ["<div> </div>", [" "]],
    ["<div>\n \t \n</div>", []],
    ["<div>hello\n  world\n  again</div>", ["hello world again"]],
    ["<div>  hello\r\n\tworld  </div>", ["  hello world  "]],
    ["<div>\n a &#32; \n</div>", ["a  "]],
    ["<div>&#10;</div>", ["\n"]],
    ["<div>\t hello\t </div>", ["  hello  "]],
  ] as const) {
    expect(evaluate(source), source).toEqual({ tag: "div", props: null, children });
  }
});

test("text and quoted attributes decode entities once while JS strings retain their value", () => {
  const source = `<div
    title='&quot;&apos;&amp;&lt;&gt;&nbsp;&copy;&euro;&thetasym;&#65;&#x1F600;'
    raw="\\n" js={"&amp;"}>
    &amp;lt; &unknown; &AMP; &amp &#xD800; &#x110000; &#X41; &#x; &#;
  </div>`;
  expect(evaluate(source)).toEqual({
    tag: "div",
    props: { title: `"'&<>\u00a0©€ϑA😀`, raw: "\\n", js: "&amp;" },
    children: ["&lt; &unknown; &AMP; &amp &#xD800; &#x110000; &#X41; &#x; &#;"],
  });
  expect(evaluate(`<div>&#${"0".repeat(1_000)}65;&#0;</div>`)).toEqual({
    tag: "div", props: null, children: ["A\0"],
  });
});

test("spread attributes retain evaluation order and spread children stay separate", () => {
  const calls: string[] = [];
  const mark = (name: string, value: unknown) => { calls.push(name); return value; };
  const result = evaluate(`<Box first={mark("first", 1)} {...mark("spread", {first: 2})}
    last={mark("last", 3)}>{mark("child", 4)}{...mark("children", [5, 6])}</Box>`,
    {}, { Box: "box", mark });
  expect(result).toEqual({ tag: "box", props: { first: 2, last: 3 }, children: [4, 5, 6] });
  expect(calls).toEqual(["first", "spread", "last", "child", "children"]);
});

test("sequence expressions and lowered calls keep their required parentheses", () => {
  expect(evaluate(`<div value={(1, 2)}>{(3, 4)}{...(5, [6, 7])}</div>`)).toEqual({
    tag: "div", props: { value: 2 }, children: [4, 6, 7],
  });
  expect(gen(`new (<Box/>).constructor(); new (<></>)();`, { jsx: true }, "input.jsx", {
    preserveParens: false,
  })).toMatchInlineSnapshot(`
"new (/* @__PURE__ */ React.createElement(Box, null)).constructor();
new (/* @__PURE__ */ React.createElement(React.Fragment, null))();"
`);
});

test("a __proto__ attribute produces an own property", () => {
  const value = evaluate(`<div __proto__={payload}/>`, {}, { payload: { polluted: true } }) as {
    props: Record<string, unknown>;
  };
  expect(Object.getPrototypeOf(value.props)).toBe(Object.prototype);
  expect(Object.prototype.hasOwnProperty.call(value.props, "__proto__")).toBe(true);
  expect(value.props["__proto__"]).toEqual({ polluted: true });
});

test("comments inside empty containers survive lowering without adding children", () => {
  const source = `<div>{/* @keep */}x{// @line\n}y</div>`;
  for (const minify of [false, true]) {
    expect(evaluate(source, { comments: true, minify })).toEqual({
      tag: "div", props: null, children: ["x", "y"],
    });
    const code = gen(source, { jsx: true, comments: true, minify }, "input.jsx");
    expect(code).toContain("/* @keep */");
    expect(code).toContain("// @line");
    expect(gen(source, { jsx: true, comments: false }, "input.jsx")).not.toContain("@keep");
  }
});

test("comments inside fragment delimiters survive preservation and lowering", () => {
  // both printers must retain delimiter comments without creating fragment children
  const source = "</* opening */>{/* empty */}text</ /* closing */>";
  for (const jsx of [false, { pure: false }] as const) {
    for (const format of ["pretty", "compact"] as const) {
      const code = gen(source, { jsx, format, comments: "all" }, "input.jsx");
      const reparsed = parse(code, { lang: "jsx" });
      expect(reparsed.diagnostics).toEqual([]);
      expect(reparsed.comments.length).toBe(3);
      expect(gen(source, { jsx, format, comments: false }, "input.jsx"))
        .not.toContain("/*");
    }
  }
  expect(evaluate(source, { comments: "all" })).toEqual({
    tag: "fragment", props: null, children: ["text"],
  });
});

test("lowered values and preserved comments retain source mappings", () => {
  const source = `const view = <Box<number /*type*/> data-id="&amp;">hi{value}</Box /*close*/>;`;
  const { program } = parse(source, { path: "input.tsx", attachComments: true });
  const { code, map } = generate(program, {
    jsx: true, strip: true, comments: "all", format: "compact", sourceMap: { source },
  });
  const trace = new TraceMap(JSON.stringify(map));
  for (const [generated, original] of [
    ["Box", "Box"], ["\"data-id\"", "data-id"], ["\"&\"", "\"&amp;\""],
    ["\"hi\"", "hi"], ["value", "value"], [")", "</Box"],
  ]) {
    expect(originalPositionFor(trace, { line: 1, column: code.indexOf(generated!) }).column,
      generated).toBe(source.indexOf(original!));
  }
});

test("synthetic JSX strings without raw lexemes are already cooked", () => {
  const { program } = parse(`<div title="&amp;">&amp;lt; &#10;</div>`, { path: "input.jsx" });
  const statement = program.body[0]!;
  if (statement.type !== "ExpressionStatement" || statement.expression.type !== "JSXElement") {
    throw new Error("expected a JSX element");
  }
  const attribute = statement.expression.openingElement.attributes[0]!;
  if (attribute.type !== "JSXAttribute" || attribute.value?.type !== "Literal") {
    throw new Error("expected a string attribute");
  }
  attribute.value.value = "&amp;";
  Reflect.deleteProperty(attribute.value, "raw");
  const text = statement.expression.children[0]!;
  if (text.type !== "JSXText") throw new Error("expected JSX text");
  Reflect.deleteProperty(text, "raw");
  expect(generate(program, { jsx: true }).code).toBe(
    `/* @__PURE__ */ React.createElement("div", { title: "&amp;" }, "&lt; \\n");`,
  );
  // cooked values print markup syntax as entities so a reparse keeps them
  expect(generate(program, {}).code).toBe(
    `<div title="&amp;amp;">&amp;lt; &#10;</div>;`,
  );
});

test("lowering preserves comments on closing names and nested erased type arguments", () => {
  const source = `<UI.Box<{x: A<B /*inner*/> /*member*/} /*outer*/> />;
<div>text</div /*closing*/>;
<UI.Box>text</UI /*object*/.Box /*property*/>;
<x:y>text</x /*namespace*/:y /*local*/>;
<x /*tag_namespace*/:y /*tag_local*/
  a /*attr_namespace*/:b /*attr_local*/="value" />;
<><i/></ /*frag_close*/>;`;
  const names = [
    "inner", "member", "outer", "closing", "object", "property", "namespace", "local",
    "tag_namespace", "tag_local", "attr_namespace", "attr_local", "frag_close",
  ];
  for (const strip of [false, true]) {
    for (const minify of [false, true]) {
      const code = gen(source, { jsx: true, comments: "all", strip, minify }, "input.tsx");
      for (const name of names) expect(code.split(`/*${name}*/`).length, name).toBe(2);
      expect(code.indexOf("/*inner*/")).toBeLessThan(code.indexOf("/*member*/"));
      expect(code.indexOf("/*member*/")).toBeLessThan(code.indexOf("/*outer*/"));
      expect(parse(code, { lang: "js" }).diagnostics).toEqual([]);
      expect(code).not.toContain("A<B");
    }
  }
});

test("comment filters apply to erased JSX syntax without changing runtime values", () => {
  const source = `<Box<{
    /*ordinary*/
    // @line
    /* @legal */
  }>>text</Box /*closing*/>`;
  const variants: [GenerateOptions["comments"], string[]][] = [
    ["all", ["ordinary", "@line", "@legal", "closing"]],
    ["block", ["ordinary", "@legal", "closing"]],
    ["line", ["@line"]],
    ["some", ["@legal"]],
    ["none", []],
  ];
  for (const [comments, kept] of variants) {
    for (const minify of [false, true]) {
      const code = gen(source, { jsx: true, comments, minify }, "input.tsx");
      for (const marker of ["ordinary", "@line", "@legal", "closing"]) {
        expect(code.includes(marker), `${comments} ${marker}`).toBe(kept.includes(marker));
      }
      expect(evaluate(source, { comments, minify }, { Box: "box" })).toEqual({
        tag: "box", props: null, children: ["text"],
      });
    }
  }
});

test("line comments on omitted names and types cannot swallow the generated separators", () => {
  for (const source of [
    `<Box<number // type\n> enabled>text</Box // close\n>`,
    `<Box<number /*type*/> {...props}>text</Box /*close*/>`,
  ]) {
    for (const minify of [false, true]) {
      expect(evaluate(
        source,
        { comments: "all", minify },
        { Box: "box", props: { enabled: true } },
      )).toEqual({ tag: "box", props: { enabled: true }, children: ["text"] });
    }
  }
});

test("erased type comments survive wide lists and deeply nested types", () => {
  const wide = `${"number,".repeat(4_095)}number /*wide*/`;
  const deep = `A${"[]".repeat(2_000)} /*deep*/`;
  for (const [types, marker] of [[wide, "wide"], [deep, "deep"]]) {
    const source = `<Box<${types}>>text</Box /*close*/>`;
    for (const minify of [false, true]) {
      const code = gen(source, { jsx: true, comments: "all", minify }, "input.tsx");
      expect(code.split(`/*${marker}*/`).length).toBe(2);
      expect(code.split("/*close*/").length).toBe(2);
      expect(evaluate(source, { comments: "all", minify }, { Box: "box" })).toEqual({
        tag: "box", props: null, children: ["text"],
      });
    }
  }
});
