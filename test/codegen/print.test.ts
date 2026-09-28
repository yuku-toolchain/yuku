import { expect, test } from "bun:test";
import { gen } from "./helpers";

test("array holes keep their elisions", () => {
  expect(
    gen(`const a = [, ,];\nconst b = [1, , 3];\nconst c = [1, 2, ,];\nconst d = [,];`),
  ).toMatchInlineSnapshot(`
      "const a = [, ,];
      const b = [1, , 3];
      const c = [1, 2, ,];
      const d = [,];"
    `);
});

test("directives are printed verbatim, not re-cooked", () => {
  expect(
    gen(
      String.raw`"\x75se strict";
function f() {
  "use asm";
}`,
    ),
  ).toMatchInlineSnapshot(`
  ""\\x75se strict";
  function f() {
    "use asm";
  }"
`);
});

test("normal comments are dropped by default", () => {
  expect(gen(`// hi\nconst x = 1;`)).toMatchInlineSnapshot(`"const x = 1;"`);
});

test("a legal banner comment is kept", () => {
  expect(gen(`/*! ©2025 */\nconst x = 1;`)).toMatchInlineSnapshot(`
    "/*! ©2025 */
    const x = 1;"
  `);
});

test("a pure annotation is kept", () => {
  expect(gen(`const x = /*#__PURE__*/ foo();`)).toMatchInlineSnapshot(
    `"const x = /*#__PURE__*/ foo();"`,
  );
});

test("lone surrogates round-trip in string literals", () => {
  expect(
    gen(
      String.raw`const high = "\uD800";
const low = "\uDC00";
const mixed = "a\uD834b\uDD1Ec";`,
    ),
  ).toMatchInlineSnapshot(`
  "const high = "\\ud800";
  const low = "\\udc00";
  const mixed = "a\\ud834b\\udd1ec";"
`);
});

test("script-close sequences in strings stay literal", () => {
  expect(gen(`const a = "</script>";\nconst b = "<!-- c -->";`)).toMatchInlineSnapshot(`
    "const a = "</script>";
    const b = "<!-- c -->";"
  `);
});

test("template raw text is preserved", () => {
  expect(
    gen(
      "const a = `A\\t\\n`;\nconst b = `head${1}tail`;\nconst c = String.raw`\\d+\\n${2}`;",
    ),
  ).toMatchInlineSnapshot(`
      "const a = \`A\\t\\n\`;
      const b = \`head\${1}tail\`;
      const c = String.raw\`\\d+\\n\${2}\`;"
    `);
});

test("a JSX attribute keeps its raw text", () => {
  expect(
    gen(
      `const a = <a href="&amp;x" title='y' />;\nconst b = <b data-x={"&lt;"} />;`,
      {},
      "input.jsx",
    ),
  ).toMatchInlineSnapshot(`
      "const a = <a href="&amp;x" title='y' />;
      const b = <b data-x={"&lt;"} />;"
    `);
});

test("TS definite assignment assertions", () => {
  expect(gen(`let x!: number;\nclass C {\n  y!: string;\n}`)).toMatchInlineSnapshot(`
    "let x!: number;
    class C {
      y!: string;
    }"
  `);
});

test("TS leading union and intersection operators", () => {
  expect(
    gen(
      `type A = | string;\ntype B = & number;\ntype C = string | number;\ntype D = A & B;`,
    ),
  ).toMatchInlineSnapshot(`
      "type A = | string;
      type B = & number;
      type C = string | number;
      type D = A & B;"
    `);
});

test("an export default value keeps the parens that stop it reading as a declaration", () => {
  const source = [
    "export default (function () {})();",
    "export default (class {}).name;",
    "export default (async function () {})();",
    "export default (function f() {});",
    "export default { a: 1 };",
  ].join("\n");
  expect(gen(source, {}, "input.js", { preserveParens: false })).toMatchInlineSnapshot(`
    "export default (function() {})();
    export default (class {}).name;
    export default (async function() {})();
    export default (function f() {});
    export default { a: 1 };"
  `);
});

test("an `in` inside a for-init arrow body or yield keeps its parens", () => {
  const source = [
    "for (let f = () => (a in b); ; );",
    "for (g = async (x) => (a in b); ; );",
    "function* h() {",
    "  for (let x = yield (a in b); ; );",
    "}",
  ].join("\n");
  expect(gen(source, {}, "input.js", { preserveParens: false })).toMatchInlineSnapshot(`
    "for (let f = () => (a in b);;) ;
    for (g = async (x) => (a in b);;) ;
    function* h() {
      for (let x = yield (a in b);;) ;
    }"
  `);
});

test("compact output keeps the trailing space of JSX text", () => {
  const source = `const a = <p>hello {name}</p>;\nconst b = <p>a <b /> c </p>;`;
  expect(gen(source, { format: "compact" }, "input.jsx")).toMatchInlineSnapshot(
    `"const a=<p>hello {name}</p>;const b=<p>a <b/> c </p>"`,
  );
});

test("an arrow's lone type parameter keeps a trailing comma", () => {
  expect(
    gen(`<T>(x: T) => x;\n<T = U>() => 0;\n<T extends U>() => 0;\nfunction f<T>() {}`),
  ).toMatchInlineSnapshot(`
    "<T,>(x: T) => x;
    <T = U,>() => 0;
    <T extends U>() => 0;
    function f<T>() {}"
  `);
});

test("compact output keeps a type argument closer apart from `>` and `=` operators", () => {
  expect(
    gen(`f<T> == x;\nf<T> >= x;\nf<T> >>> x;\na.b<T> === c;`, { format: "compact" }),
  ).toMatchInlineSnapshot(`"f<T> ==x;f<T> >=x;f<T> >>>x;a.b<T> ===c"`);
});
