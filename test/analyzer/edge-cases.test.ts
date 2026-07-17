import { describe, expect, test } from "bun:test";
import { Analyzer } from "yuku-analyzer";
import { summary } from "./utils/summarize";

describe("write detection through wrappers", () => {
  test("parenthesized and TS-assertion assignment targets are writes", () => {
    expect(summary(`let a = 1; (a) = 2; (a as any) = 3; a! = 4; a;`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          a#0  let
          a → #0 write
          a → #0 write
          a → #0 write
          a → #0"
    `);
  });
});

describe("write detection in loop heads", () => {
  test("array and object destructuring for-of heads are writes", () => {
    expect(summary(`let a, b; for ([a, b] of []) {} for ({ a } of []) {}`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
      "global
        module [strict]
          a#0  let
          b#1  let
          block ForOfStatement
            a → #0 write
            b → #1 write
            block BlockStatement
          block ForOfStatement
            a → #0 write
            block BlockStatement"
    `);
  });

  test("a destructuring default value is a read, its target is a write", () => {
    expect(summary(`let a, b; ({ a = b } = {});`, { path: "input.js" })).toMatchInlineSnapshot(`
      "global
        module [strict]
          a#0  let
          b#1  let
          a → #0 write
          b → #1"
    `);
  });
});

describe("string pool", () => {
  test("a lone surrogate in a module specifier round-trips", () => {
    // an escaped surrogate cannot be sliced from source, so it crosses the
    // wire through the WTF-8 string pool and the decoder must rebuild it.
    // built via fromCharCode so the source file holds no raw surrogate.
    const surrogate = String.fromCharCode(0xd800);
    const module = new Analyzer().addFile(
      "input.js",
      `import x from ${JSON.stringify(surrogate)};`,
    );
    expect(module.imports[0]!.specifier).toBe(surrogate);
    expect(module.imports[0]!.specifier.charCodeAt(0)).toBe(0xd800);
  });
});

describe("import equals", () => {
  test("a qualified-name alias binds a symbol but is not a graph edge", () => {
    expect(summary(`namespace NS { export const B = 1; } import A = NS.B; A;`))
      .toMatchInlineSnapshot(`
      "global
        module [strict]
          NS#0  namespace value-module
          A#2  import
          NS → #0 any
          A → #2
          tsModule
            B#1  const exported"
    `);
  });
});

describe("ambient global augmentation", () => {
  test("declare global opens an ambient block whose vars are ambient", () => {
    expect(summary(`declare global { var g: number; } g;`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          g → free
          tsModule
            g#0  var ambient"
    `);
  });
});
