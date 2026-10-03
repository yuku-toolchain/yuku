import { describe, expect, test } from "bun:test";
import { load } from "@yuku-core/wasm";
import { parse, type ParseOptions } from "yuku-parser";

const script: ParseOptions = { sourceType: "script" };

function firstSpan(source: string) {
  const [first] = parse(source, script).diagnostics;
  expect(first, source).toBeDefined();
  return [first!.start, first!.end];
}

describe("path", () => {
  test("names the file in each diagnostic and sets lang and sourceType", () => {
    const { diagnostics } = parse(`let x: number = ;`, { path: "src/a.cts" });
    expect(diagnostics.map((d) => d.path)).toEqual(["src/a.cts"]);
    expect(parse(`let x = ;`).diagnostics[0]!.path).toBeNull();
    expect(parse(`return <div />;`, { path: "a.cjs" }).diagnostics).toHaveLength(1);
    expect(parse(`return 1;`, { path: "a.cjs" }).diagnostics).toEqual([]);
  });

  test("an unknown lang or sourceType throws", () => {
    // @ts-expect-error an invalid lang
    expect(() => parse(`x`, { lang: "typescript" })).toThrow("`lang` must be");
    // @ts-expect-error an invalid sourceType
    expect(() => parse(`x`, { sourceType: "esm" })).toThrow("`sourceType` must be");
  });
});

describe("diagnostics", () => {
  test("a lexical diagnostic points at the cursor", () => {
    expect(firstSpan("let x = 0x_ab")).toEqual([10, 11]);
    expect(firstSpan("let x =   0x_ab")).toEqual([12, 13]);
    expect(firstSpan("0x_ab")).toEqual([2, 3]);
    expect(firstSpan("let x /* c */ = 0x_ab")).toEqual([18, 19]);
    expect(firstSpan("let x = 0b_1")).toEqual([10, 11]);
    expect(firstSpan("let x = 0o_7")).toEqual([10, 11]);
    expect(firstSpan("let x = 1__2")).toEqual([10, 11]);
    expect(firstSpan("let x = 3in")).toEqual([9, 10]);
    expect(firstSpan("let x = 1_;")).toEqual([10, 11]);
    expect(firstSpan("let x = 1._5")).toEqual([9, 10]);
  });

  test("a lexical diagnostic at the end of input is zero-width", () => {
    for (const [source, at] of [
      ["let x = 0b", 10],
      ["let x = 0x", 10],
      ["let x = 1e", 10],
      ["let x = 'abc", 12],
      ["let x = `abc", 12],
      ["let x = #", 9],
      ["let x /* c", 10],
      ["let x = 1; /* c", 15],
    ] as const) expect(firstSpan(source), source).toEqual([at, at]);
  });
});

describe("depth", () => {
  test("a chain of any depth decodes on both cores", async () => {
    const source = "a" + ".b(/* c */ x)".repeat(1000);
    for (const core of [undefined, await load()]) {
      for (const attachComments of [false, true]) {
        let node: any = (parse(source, { core, attachComments }).program.body[0] as any).expression;
        let calls = 0;
        for (; node.type === "CallExpression"; node = node.callee.object) calls++;
        expect(calls).toBe(1000);
        expect(node.name).toBe("a");
      }
    }
  });
});
