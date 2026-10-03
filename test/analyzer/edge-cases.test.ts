import { describe, expect, test } from "bun:test";
import { load } from "@yuku-core/wasm";
import { Analyzer, analyze } from "yuku-analyzer";
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
    // an escaped lone surrogate crosses the wire through the WTF-8 string pool
    const surrogate = String.fromCharCode(0xd800);
    const module = new Analyzer().setFile(
      "input.js",
      `import x from ${JSON.stringify(surrogate)};`,
    );
    expect(module.imports[0]!.specifier).toBe(surrogate);
    expect(module.imports[0]!.specifier.charCodeAt(0)).toBe(0xd800);
  });
});

describe("byte source", () => {
  test("a UTF-8 byte source analyzes like its string", () => {
    const source = "\uFEFFconst après = '🎉'; après;";
    const bytes = new TextEncoder().encode(source) as unknown as string;
    const module = new Analyzer().setFile("input.js", bytes);
    expect(module.source).toBe(source);
    expect(module.ast).toEqual(new Analyzer().setFile("input.js", source).ast);
    expect(module.references[0]!.binding?.name).toBe("après");
    expect(() => new Analyzer().setFile("bad.js", new Uint8Array([0xff]) as unknown as string))
      .toThrow(TypeError);
  });
});

describe("import equals", () => {
  test("a qualified-name alias declares a binding but is not a graph edge", () => {
    expect(summary(`namespace NS { export const B = 1; } import A = NS.B; A;`))
      .toMatchInlineSnapshot(`
      "global
        module [strict]
          NS#0  namespace value-module
          A#2  import
          NS → #0 namespace
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
        g#0  var ambient
        module [strict]
          g → #0
          tsModule"
    `);
  });
});

describe("deep trees", () => {
  test("a private name deep in a chain resolves to its class", async () => {
    const source = `class A { #a = 1; m() { return this.#a${".b(x)".repeat(1000)}; } }`;
    for (const core of [undefined, await load()]) {
      expect(analyze(source, { core, path: "input.js" }).diagnostics).toEqual([]);
    }
  });

  test("parentOf climbs from the deepest node to the root", () => {
    const module = analyze("a" + ".b".repeat(100_000), { path: "input.js" });
    let node: any = (module.ast.body[0] as any).expression;
    while (node.type === "MemberExpression") node = node.object;
    let ancestors = 0;
    for (let parent = module.parentOf(node); parent; parent = module.parentOf(parent)) ancestors++;
    expect(ancestors).toBe(100_000 + 2);
  });
});
