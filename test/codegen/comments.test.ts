import { expect, test } from "bun:test";
import { parse } from "yuku-parser";
import type { GenerateOptions } from "yuku-codegen";
import { commentPlacements, gen } from "./helpers";

const ALL = { comments: true } as const;

test("a blank line between statements is preserved as a group break", () => {
  expect(gen(`foo();\n\n// after blank\nbar();`, ALL)).toMatchInlineSnapshot(`
    "foo();
    // after blank
    bar();"
  `);
});

test("a comment inside an empty class body is kept", () => {
  expect(gen(`class C {\n  // inside empty body\n}`, ALL)).toMatchInlineSnapshot(`
    "class C {
      // inside empty body
    }"
  `);
});

test("a hashbang and a leading comment", () => {
  expect(gen(`#!/usr/bin/env node\n// hi\nconst x = 1;`, ALL)).toMatchInlineSnapshot(`
    "#!/usr/bin/env node
    // hi
    const x = 1;"
  `);
});

test("multiline JSDoc on a function and a method", () => {
  expect(
    gen(
      `/**
 * Adds two numbers.
 * @param {number} a
 * @param {number} b
 */
function add(a, b) {
    return a + b;
}

class Calculator {
    /**
     * Multiplies two numbers.
     * @param {number} a
     * @param {number} b
     */
    multiply(a, b) {
        return a * b;
    }
}`,
      ALL,
    ),
  ).toMatchInlineSnapshot(`
  "/**
   * Adds two numbers.
   * @param {number} a
   * @param {number} b
   */
  function add(a, b) {
    return a + b;
  }
  class Calculator {
    /**
     * Multiplies two numbers.
     * @param {number} a
     * @param {number} b
     */
    multiply(a, b) {
      return a * b;
    }
  }"
`);
});

test("a single-line JSDoc on a function", () => {
  expect(gen(`/** @param x */\nfunction f(x) { return x; }`, ALL)).toMatchInlineSnapshot(`
    "/** @param x */
    function f(x) {
      return x;
    }"
  `);
});

test("leading comments on member keys", () => {
  expect(
    gen(
      `class C {
  /* lead */ method() {}
  /* on get */ get x() {
    return 1;
  }
}`,
      ALL,
    ),
  ).toMatchInlineSnapshot(`
  "class C {
    /* lead */ method() {}
    /* on get */ get x() {
      return 1;
    }
  }"
`);
});

test("a no-side-effects annotation before a declaration", () => {
  expect(gen(`/*#__NO_SIDE_EFFECTS__*/\nfunction make() { return {}; }`, ALL))
    .toMatchInlineSnapshot(`
      "/*#__NO_SIDE_EFFECTS__*/
      function make() {
        return {};
      }"
    `);
});

test("a pure annotation inline before a call", () => {
  expect(gen(`const x = /*#__PURE__*/ foo();`, ALL)).toMatchInlineSnapshot(
    `"const x = /*#__PURE__*/ foo();"`,
  );
});

test("the default keeps spaced annotations and drops plain comments", () => {
  for (const comment of ["/* @__PURE__ */", "/* #__PURE__ */", "/*\t@__NO_SIDE_EFFECTS__ */"]) {
    expect(gen(`const x = ${comment} foo();`), comment).toBe(`const x = ${comment} foo();`);
  }
  for (const comment of ["/* ! not legal */", "/* * not jsdoc */", "/* plain */"]) {
    expect(gen(`const x = ${comment} foo();`), comment).toBe("const x = foo();");
  }
});

test("a trailing same-line comment", () => {
  expect(gen(`foo(); // tail\nbar();`, ALL)).toMatchInlineSnapshot(`
    "foo(); // tail
    bar();"
  `);
});

test("comments around a parameter list stay beside it", () => {
  expect(gen(`function f /* a */ (b /* c */) /* d */ {}`, ALL)).toMatchInlineSnapshot(
    `"function f /* a */(b /* c */) /* d */ {}"`,
  );
});

test("a list item's trailing line comment follows its separator", () => {
  const source = [
    "function f(",
    "  a, // first",
    "  b, // second",
    ") {}",
    "const o = {",
    "  a: 1, // first",
    "  b: 2, // second",
    "};",
    "let x = 1, // first",
    "  y = 2;",
    "enum E {",
    "  A, // first",
    "  B,",
    "}",
  ].join("\n");
  expect(gen(source, ALL)).toMatchInlineSnapshot(`
    "function f(a, // first
      b // second
    ) {}
    const o = { a: 1, // first
      b: 2 // second
    };
    let x = 1, // first
      y = 2;
    enum E {
      A, // first
      B
    }"
  `);
  expect(gen(source, { ...ALL, format: "compact" })).toMatchInlineSnapshot(`
    "function f(a,// first
    b// second
    ){}const o={a:1,// first
    b:2// second
    };let x=1,// first
    y=2;enum E{A,// first
    B}"
  `);
});

test("a leading own-line comment", () => {
  expect(gen(`// hello\nconst x = 1;`, ALL)).toMatchInlineSnapshot(`
    "// hello
    const x = 1;"
  `);
});

test("comments:line keeps only the line comment", () => {
  expect(gen(`// keep me\n/* drop me */\nconst x = 1;`, { comments: "line" }))
    .toMatchInlineSnapshot(`
      "// keep me
      const x = 1;"
    `);
});

test("comments:block keeps only the block comment", () => {
  expect(gen(`// drop me\n/* keep me */\nconst x = 1;`, { comments: "block" }))
    .toMatchInlineSnapshot(`
      "/* keep me */
      const x = 1;"
    `);
});

test("comments:false drops everything", () => {
  expect(gen(`// hidden\nconst x = 1;`, { comments: false })).toMatchInlineSnapshot(
    `"const x = 1;"`,
  );
});

test("a compact block comment after `/` does not open a line comment", () => {
  expect(gen(`x = a / /*c*/ b;`, { ...ALL, format: "compact" }, "input.js")).toMatchInlineSnapshot(
    `"x=a/ /*c*/b"`,
  );
});

test("a comment breaking before a restricted operand keeps it parenthesized", () => {
  const source =
    "function* g() {\n  return (\n    // r\n    a\n  );\n  yield (\n    // y\n    b\n  );\n}";
  const parseOptions = { preserveParens: false };
  expect(gen(source, ALL, "input.js", parseOptions)).toMatchInlineSnapshot(`
    "function* g() {
      return (
      // r
      a);
      yield (
      // y
      b);
    }"
  `);
  expect(gen(source, { ...ALL, format: "compact" }, "input.js", parseOptions))
    .toMatchInlineSnapshot(`
    "function* g(){return (
    // r
    a);yield (
    // y
    b)}"
  `);
});

test("a comment between array holes stays in the array", () => {
  expect(gen("[,/* hole*/,,];\nconst [/* none */] = x;", ALL, "input.js")).toMatchInlineSnapshot(`
    "[/* hole*/, , ,];
    const [/* none */] = x;"
  `);
});

test("a comment in an empty JSX expression container or fragment is kept", () => {
  expect(gen("<div>{/* @ts-expect-error */}</div>;", {}, "input.jsx")).toMatchInlineSnapshot(
    `"<div>{/* @ts-expect-error */}</div>;"`,
  );
  const source = "<div>{/* keep me */}</div>;\n<div>{// note\n}</div>;\n</* a */></ /* b */>;";
  expect(gen(source, ALL, "input.jsx")).toMatchInlineSnapshot(`
    "<div>{/* keep me */}</div>;
    <div>{
      // note
    }</div>;
    </* a */></ /* b */>;"
  `);
});

test("a comment with no node beside it stays inside its parent", () => {
  const source = [
    "const o = { /* none */ };",
    "const {/* none */} = o;",
    "export { /* none */ };",
    "function f(/* none */) {}",
    "function g(a /* maybe */?) {}",
    "const h = async (/* none */) => {};",
    "x = function /* anonymous */ () {};",
    "function i() { return /* nothing */; }",
    "for (;;) { break /* out */; continue /* on */; }",
    "debugger /* stop */;",
    "switch (a) { default /* fallback */: }",
    "type T = [/* none */];",
    "type L = { /* none */ };",
    "interface I { /* none */ }",
    "enum E { /* none */ }",
    "interface J { m(/* none */): void; new (/* none */): J }",
    "declare function d(/* none */): void;",
  ].join("\n");
  expect(gen(source, ALL)).toMatchInlineSnapshot(`
    "const o = {/* none */};
    const {/* none */} = o;
    export {/* none */};
    function f(/* none */) {}
    function g(a /* maybe */?) {}
    const h = async (/* none */) => {};
    x = function(/* anonymous */) {};
    function i() {
      return /* nothing */;
    }
    for (;;) {
      break /* out */;
      continue /* on */;
    }
    debugger /* stop */;
    switch (a) {
    default /* fallback */:
    }
    type T = [/* none */];
    type L = {
      /* none */
    };
    interface I {
      /* none */
    }
    enum E {
      /* none */
    }
    interface J {
      m(/* none */): void;
      new (/* none */): J;
    }
    declare function d(/* none */): void;"
  `);
});

test("a line comment in an empty list breaks it open", () => {
  const source = "const a = [// empty\n];\nconst o = {// empty\n};\nfunction f(// none\n) {}";
  expect(gen(source, ALL, "input.js")).toMatchInlineSnapshot(`
    "const a = [
      // empty
    ];
    const o = {
      // empty
    };
    function f(
      // none
    ) {}"
  `);
});

test("a node the parent writes itself keeps its comments", () => {
  const source = [
    "'use strict' /* c */;",
    "class A { m /* c */ () {} get g(/* none */) { return 1; } }",
    "const o = { m /* c */ () {} };",
    "for (/* c */ const x of y) {}",
    "import { a as /* c */ a } from 'a';",
    'x = <a b= /* c */ "x" />;',
    "type F = () /* c */ => void;",
    "function f(a): a /* c */ is string {}",
  ].join("\n");
  expect(gen(source, ALL, "input.tsx")).toMatchInlineSnapshot(`
    "'use strict' /* c */;
    class A {
      m /* c */ () {}
      get g(/* none */) {
        return 1;
      }
    }
    const o = { m /* c */ () {} };
    for ( /* c */ const x of y) {}
    import { a as /* c */ a } from 'a';
    x = <a b= /* c */ "x" />;
    type F = () => /* c */ void;
    function f(a): a is /* c */ string {}"
  `);
  const keys = 'x = a[/* c */ "b"];\ny = { "c" /* c */\n: 1 };\nclass C { "d" /* c */\n() {} }';
  expect(gen(keys, { ...ALL, minify: true }, "input.js")).toMatchInlineSnapshot(
    `"x=a./* c */b;y={c/* c */:1};class C{d/* c */(){}}"`,
  );
});

test("strip keeps the comments of a cast and drops those of a type-only specifier", () => {
  expect(gen("const a = /*#__PURE__*/ make() as Thing;", { strip: true })).toMatchInlineSnapshot(
    `"const a = /*#__PURE__*/ make();"`,
  );
  const source = 'f(a as T /* c */, b);\nimport { /* c */ type A, B } from "a";';
  expect(gen(source, { ...ALL, strip: true })).toMatchInlineSnapshot(`
    "f(a, /* c */ b);
    import { B } from "a";"
  `);
});

test("a trailing line comment follows a token no line break may precede", () => {
  const source = [
    "x = a as // c\n  T;",
    "x = a satisfies // c\n  T;",
    "function f(a): a is // c\n  string {}",
    "type C<T> = T extends // c\n  string ? 1 : 2;",
    "type I = T[ // c\n  K];",
    "type R = T[ // c\n];",
    "x = (a): b => // c\n  a;",
    "class A { b! // c\n  : T; }",
  ].join("\n");
  expect(gen(source, ALL)).toMatchInlineSnapshot(`
    "x = a as // c
    T;
    x = a satisfies // c
    T;
    function f(a): a is // c
    string {}
    type C<T> = T extends // c
    string ? 1 : 2;
    type I = T[ // c
    K];
    type R = T[ // c
    ];
    x = (a): b => // c
    a;
    class A {
      b! // c
      : T;
    }"
  `);
});

test("a blank line inside a JSDoc comment is kept", () => {
  const source = "/**\n * Summary.\n\n * @param a\n */\nfunction f(a) {}";
  expect(gen(source, ALL, "input.js")).toMatchInlineSnapshot(`
    "/**
     * Summary.

     * @param a
     */
    function f(a) {}"
  `);
});

// one comment in each token gap of a snippet per node kind, printed twice by every plan
test("a comment in any gap survives every plan", () => {
  const plans: GenerateOptions[] = [
    ALL,
    { ...ALL, format: "compact" },
    { ...ALL, minify: true },
    { ...ALL, strip: true },
  ];
  const failures: string[] = [];
  for (const { source, lang } of commentPlacements()) {
    const options = { lang, sourceType: "module" } as const;
    for (const plan of plans) {
      const first = gen(source, plan, undefined, options);
      const second = gen(first, plan, undefined, options);
      const reparsed = parse(first, options);
      const kept = [reparsed, parse(second, options)].map((r) => r.comments.length).join();
      const typed = plan.strip === true && lang !== "js" && lang !== "jsx";
      if (reparsed.diagnostics.length > 0 || (!typed && kept !== "1,1")) {
        failures.push(JSON.stringify([plan, source, first]));
      }
    }
  }
  expect(failures.slice(0, 8)).toEqual([]);
});
