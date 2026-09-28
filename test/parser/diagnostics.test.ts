import { describe, expect, test } from "bun:test";
import { parse, type ParseOptions } from "yuku-parser";

const script: ParseOptions = { sourceType: "script" };

function firstSpan(source: string) {
  const [first] = parse(source, script).diagnostics;
  expect(first, source).toBeDefined();
  return [first!.start, first!.end];
}

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
