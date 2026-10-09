import { expect, test } from "bun:test";
import { gen } from "./helpers";

test("removes type annotations but keeps comments", () => {
  expect(gen(`// types incoming\nconst x: number = 1;`, { strip: true, comments: true }))
    .toMatchInlineSnapshot(`
      "// types incoming
      const x = 1;"
    `);
});

test("a statement list that strips to nothing leaves no blank line", () => {
  const source = [
    "function f() {",
    "  type T = number;",
    "}",
    "switch (x) {",
    "  case 1: type U = T;",
    "  case 2: g();",
    "  default: interface I {}",
    "}",
  ].join("\n");
  expect(gen(source, { strip: true })).toMatchInlineSnapshot(`
    "function f() {}
    switch (x) {
    case 1:
    case 2:
      g();
    default:
    }"
  `);
});

test("stripping an item or statement leaves no stray separator", () => {
  const source = [
    "function f(this: T, a: number) {}",
    "class C { abstract x: T; declare y: T; z = 1; }",
    'import { type A, B } from "m";',
    'export type { C } from "m";',
    "export interface I {}",
    "export default interface J {}",
    "if (a) interface K {}",
  ].join("\n");
  expect(gen(source, { strip: true })).toMatchInlineSnapshot(`
    "function f(a) {}
    class C {
      z = 1;
    }
    import { B } from "m";
    if (a) ;"
  `);
});

test("a this parameter goes with its comments", () => {
  const source = "class C {\n  m(\n    // why\n    this: C,\n    a: number,\n  ) {}\n}";
  expect(gen(source, { strip: true, comments: true })).toMatchInlineSnapshot(`
    "class C {
      m(a) {}
    }"
  `);
});

test("an uninitialized const is ambient and strips away", () => {
  const source = "export const a: A;\nconst b: B, c = 1;\nexport const d = 1;\nlet e: E;";
  expect(gen(source, { strip: true }, "input.d.ts")).toMatchInlineSnapshot(`
    "export const d = 1;
    let e;"
  `);
});
