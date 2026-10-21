import { expect, test } from "bun:test";
import { parse } from "yuku";
import { captures } from "./utils/summarize";

test("a read capture and a written capture", () => {
  const source = `let count = 0; const step = 2; function tick() { count += step; }`;
  expect(captures(source, { path: "input.js" })).toMatchInlineSnapshot(
    `"function "tick"  captures: count#0(w), step#1"`,
  );
});

test("a nested closure captures through the inner function", () => {
  const source = `let outer = 1; function f() { return () => outer; }`;
  expect(captures(source, { path: "input.js" })).toMatchInlineSnapshot(`
    "function "f"  captures: outer#0
    arrow  captures: outer#0"
  `);
});

test("a binding shadowed inside the function is not captured", () => {
  const source = `let x = 1; function f() { let x = 2; return x; }`;
  expect(captures(source, { path: "input.js" })).toMatchInlineSnapshot(
    `"function "f"  captures: (none)"`,
  );
});

test("a local-only function captures nothing", () => {
  const source = `function f(a) { let b = a; return a + b; }`;
  expect(captures(source, { path: "input.js" })).toMatchInlineSnapshot(
    `"function "f"  captures: (none)"`,
  );
});

test("this and arguments are not captures", () => {
  const source = `function f() { return function () { return this.x + arguments.length; }; }`;
  expect(captures(source, { path: "input.js" })).toMatchInlineSnapshot(`
    "function "f"  captures: (none)
    function <anonymous>  captures: (none)"
  `);
});

test("a type-only reference is not a capture", () => {
  const source = `type T = number; const factor = 2; function f(x: T) { return x * factor; }`;
  expect(captures(source)).toMatchInlineSnapshot(`"function "f"  captures: factor#1"`);
});

test("a function inside a namespace captures outer bindings", () => {
  const source = `const outer = 1; namespace N { export function f() { return outer; } }`;
  expect(captures(source)).toMatchInlineSnapshot(`"function "f"  captures: outer#0"`);
});

test("imported bindings count as captures", () => {
  const source = `import { helper } from "./h"; export function run() { return helper(); }`;
  expect(captures(source)).toMatchInlineSnapshot(`"function "run"  captures: helper#0"`);
});

test("capturesOf throws on a non-function node", () => {
  const file = parse(`let x = 1;`, { semantic: true, path: "input.js" });
  expect(() => file.capturesOf(file.program)).toThrow();
});
