import { expect, test } from "bun:test";
import { Analyzer } from "yuku-analyzer";
import { captures } from "./summarize";

test("a read capture and a written capture", () => {
  expect(
    captures(
      `
    let count = 0;
    const step = 2;
    function tick() { count += step; }
  `,
      { path: "input.js" },
    ),
  ).toMatchInlineSnapshot(`"function "tick"  captures: count#0(w), step#1"`);
});

test("a nested closure captures through the inner function", () => {
  expect(
    captures(
      `
    let outer = 1;
    function f() { return () => outer; }
  `,
      { path: "input.js" },
    ),
  ).toMatchInlineSnapshot(`
    "function "f"  captures: outer#0
    arrow  captures: outer#0"
  `);
});

test("a binding shadowed inside the function is not captured", () => {
  expect(
    captures(
      `
    let x = 1;
    function f() { let x = 2; return x; }
  `,
      { path: "input.js" },
    ),
  ).toMatchInlineSnapshot(`"function "f"  captures: (none)"`);
});

test("a local-only function captures nothing", () => {
  expect(
    captures(
      `
    function f(a) { let b = a; return a + b; }
  `,
      { path: "input.js" },
    ),
  ).toMatchInlineSnapshot(`"function "f"  captures: (none)"`);
});

test("this and arguments are not captures", () => {
  expect(
    captures(
      `
    function f() { return function () { return this.x + arguments.length; }; }
  `,
      { path: "input.js" },
    ),
  ).toMatchInlineSnapshot(`
    "function "f"  captures: (none)
    function <anonymous>  captures: (none)"
  `);
});

test("a type-only reference is not a capture", () => {
  expect(
    captures(`
    type T = number;
    const factor = 2;
    function f(x: T) { return x * factor; }
  `),
  ).toMatchInlineSnapshot(`"function "f"  captures: factor#1"`);
});

test("a function inside a namespace captures outer bindings", () => {
  expect(
    captures(`
    const outer = 1;
    namespace N { export function f() { return outer; } }
  `),
  ).toMatchInlineSnapshot(`"function "f"  captures: outer#0"`);
});

test("imported bindings count as captures", () => {
  expect(
    captures(`
    import { helper } from "./h";
    export function run() { return helper(); }
  `),
  ).toMatchInlineSnapshot(`"function "run"  captures: helper#0"`);
});

test("capturesOf throws on a non-function node", () => {
  const module = new Analyzer().addFile("input.js", `let x = 1;`);
  expect(() => module.capturesOf(module.ast)).toThrow();
});
