import { expect, test } from "bun:test";
import { gen } from "./helpers";

test("array holes keep their elisions", () => {
  expect(
    gen("print", `const a = [, ,];\nconst b = [1, , 3];\nconst c = [1, 2, ,];\nconst d = [,];`),
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
      "print",
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
  expect(gen("print", `// hi\nconst x = 1;`)).toMatchInlineSnapshot(`"const x = 1;"`);
});

test("a legal banner comment is kept", () => {
  expect(gen("print", `/*! ©2025 */\nconst x = 1;`)).toMatchInlineSnapshot(`
    "/*! ©2025 */
    const x = 1;"
  `);
});

test("a pure annotation is kept", () => {
  expect(gen("print", `const x = /*#__PURE__*/ foo();`)).toMatchInlineSnapshot(
    `"const x = /*#__PURE__*/ foo();"`,
  );
});

test("lone surrogates round-trip in string literals", () => {
  expect(
    gen(
      "print",
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
  expect(gen("print", `const a = "</script>";\nconst b = "<!-- c -->";`)).toMatchInlineSnapshot(`
    "const a = "</script>";
    const b = "<!-- c -->";"
  `);
});

test("template raw text is preserved", () => {
  expect(
    gen(
      "print",
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
      "print",
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
  expect(gen("print", `let x!: number;\nclass C {\n  y!: string;\n}`)).toMatchInlineSnapshot(`
    "let x!: number;
    class C {
      y!: string;
    }"
  `);
});

test("TSRX lazy destructuring patterns keep their marker", () => {
  expect(
    gen("print", `let &{ title } = props;\nconst &[first] = values;`, {}, "input.tsrx"),
  ).toMatchInlineSnapshot(`
    "let &{ title } = props;
    const &[first] = values;"
  `);
});

test("TSRX code blocks print inside JSX children", () => {
  expect(
    gen("print", `const view = <section>@{ const label = "ready"; }</section>;`, {}, "input.tsrx"),
  ).toMatchInlineSnapshot(`
    "const view = <section>@{
      const label = "ready";
    }</section>;"
  `);
});

test("TSRX code block render output prints after setup statements", () => {
  expect(
    gen(
      "print",
      `const view = <section>@{ const label = "ready"; <span>{label}</span> }</section>;`,
      {},
      "input.tsrx",
    ),
  ).toMatchInlineSnapshot(`
    "const view = <section>@{
      const label = "ready";
      <span>{label}</span>
    }</section>;"
  `);
});

test("TSRX code blocks print in expression position", () => {
  expect(
    gen(
      "print",
      `const value = @{ const label = "ready"; <span>{label}</span> };`,
      {},
      "input.tsrx",
    ),
  ).toMatchInlineSnapshot(`
    "const value = @{
      const label = "ready";
      <span>{label}</span>
    };"
  `);
});

test("TSRX code blocks print as function bodies", () => {
  expect(
    gen(
      "print",
      `function View(): unknown @{ const label = "ready"; <span>{label}</span> }`,
      {},
      "input.tsrx",
    ),
  ).toMatchInlineSnapshot(`
    "function View(): unknown @{
      const label = "ready";
      <span>{label}</span>
    }"
  `);
});

test("TSRX style elements print raw CSS", () => {
  expect(
    gen(
      "print",
      `const styles = <style>.card { color: red; }</style>;`,
      {},
      "input.tsrx",
    ),
  ).toMatchInlineSnapshot(`"const styles = <style>.card { color: red; }</style>;"`);
});

test("TSRX code block render output can be a style element", () => {
  expect(
    gen(
      "print",
      `const styles = @{ const tone = "red"; <style>.card { color: red; }</style> };`,
      {},
      "input.tsrx",
    ),
  ).toMatchInlineSnapshot(`
    "const styles = @{
      const tone = "red";
      <style>.card { color: red; }</style>
    };"
  `);
});

test("JSX text escapes decoded character references", () => {
  expect(
    gen(
      "print",
      `const view = <p>&quot;&#x41;&#65;&amp;&lt;&gt;&apos;</p>;`,
      {},
      "input.tsrx",
    ),
  ).toMatchInlineSnapshot(`"const view = <p>"AA&amp;&lt;>'</p>;"`);
});

test("TSRX submodule imports print identifier sources", () => {
  expect(
    gen(
      "print",
      `module server { export function load() {} }\nimport { load } from server;`,
      {},
      "input.tsrx",
    ),
  ).toMatchInlineSnapshot(`
    "module server {
      export function load() {}
    }
    import { load } from server;"
  `);
});

test("TSRX if control flow prints template blocks", () => {
  expect(
    gen(
      "print",
      `const view = @if (ready) { const label = "ready"; <span>{label}</span> } @else { <span /> };`,
      {},
      "input.tsrx",
    ),
  ).toMatchInlineSnapshot(`
    "const view = @if (ready) {
      const label = "ready";
      <span>{label}</span>
    } @else {
      <span />
    };"
  `);
});

test("TSRX for control flow prints template blocks", () => {
  expect(
    gen(
      "print",
      `const view = @for (const item of items; index itemIndex; key item.id) { <span>{item}</span> } @empty { <span /> };`,
      {},
      "input.tsrx",
    ),
  ).toMatchInlineSnapshot(`
    "const view = @for (const item of items; index itemIndex; key item.id) {
      <span>{item}</span>
    } @empty {
      <span />
    };"
  `);
});

test("TSRX switch control flow prints template blocks", () => {
  expect(
    gen(
      "print",
      `const view = @switch (status) { @case "ready": { <span /> } @default: { <Fallback /> } };`,
      {},
      "input.tsrx",
    ),
  ).toMatchInlineSnapshot(`
    "const view = @switch (status) {
      @case "ready": {
        <span />
      }
      @default: {
        <Fallback />
      }
    };"
  `);
});

test("TSRX try control flow prints template blocks", () => {
  expect(
    gen(
      "print",
      `const view = @try { <Ready /> } @pending { <Spinner /> } @catch (error: Error, reset) { <Error retry={reset}>{error.message}</Error> };`,
      {},
      "input.tsrx",
    ),
  ).toMatchInlineSnapshot(`
    "const view = @try {
      <Ready />
    } @pending {
      <Spinner />
    } @catch (error: Error, reset) {
      <Error retry={reset}>{error.message}</Error>
    };"
  `);
});

test("TS leading union and intersection operators", () => {
  expect(
    gen(
      "print",
      `type A = | string;\ntype B = & number;\ntype C = string | number;\ntype D = A & B;`,
    ),
  ).toMatchInlineSnapshot(`
      "type A = | string;
      type B = & number;
      type C = string | number;
      type D = A & B;"
    `);
});
