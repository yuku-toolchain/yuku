import { expect, test } from "bun:test";
import { gen } from "./helpers";

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

test("a trailing same-line comment", () => {
  expect(gen(`foo(); // tail\nbar();`, ALL)).toMatchInlineSnapshot(`
    "foo(); // tail
    bar();"
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
