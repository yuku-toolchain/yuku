import { describe, expect, test } from "bun:test";
import { parse, Project } from "yuku";
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
    const source = `import x from ${JSON.stringify(surrogate)};`;
    const file = parse(source, { semantic: true, path: "input.js" });
    expect(file.imports[0]!.specifier).toBe(surrogate);
    expect(file.imports[0]!.specifier.charCodeAt(0)).toBe(0xd800);
  });
});

describe("byte source", () => {
  test("a UTF-8 byte source analyzes like its string", () => {
    const source = "\uFEFFconst après = '🎉'; après;";
    const bytes = new TextEncoder().encode(source);
    const file = parse(bytes, { semantic: true, path: "input.js" });
    expect(file.source).toBe(source);
    expect(file.program).toEqual(parse(source, { path: "input.js" }).program);
    expect(file.references[0]!.binding?.name).toBe("après");
  });

  test("bytes that are not UTF-8 are a project diagnostic", () => {
    const project = new Project();
    project.set("bad.js", new Uint8Array([0x22, 0xff, 0x22, 0x3b]));
    expect(project.diagnostics.map(({ path, message }) => `${path}: ${message}`)).toEqual([
      "bad.js: Invalid UTF-8 sequence",
    ]);
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
  test("a private name deep in a chain resolves to its class", () => {
    const source = `class A { #a = 1; m() { return this.#a${".b(x)".repeat(1000)}; } }`;
    expect(parse(source, { path: "input.js" }).diagnostics).toEqual([]);
  });

  test("parentOf climbs from the deepest node to the root", () => {
    const file = parse("a" + ".b".repeat(100_000), { semantic: true, path: "input.js" });
    let node: any = (file.program.body[0] as any).expression;
    while (node.type === "MemberExpression") node = node.object;
    let ancestors = 0;
    for (let parent = file.parentOf(node); parent; parent = file.parentOf(parent)) ancestors++;
    expect(ancestors).toBe(100_000 + 2);
  });
});
