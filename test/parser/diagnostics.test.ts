import { describe, expect, test } from "bun:test";
import { parse, type ParseOptions } from "yuku";

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

describe("encoding", () => {
  test("bytes that are not UTF-8 read as U+FFFD and report where they start", () => {
    const bytes = new Uint8Array([0x61, 0x3d, 0x22, 0xc3, 0xa9, 0xff, 0x22, 0x3b]);
    const file = parse(bytes, { path: "a.js" });
    expect(file.source).toBe('a="é\uFFFD";');
    expect(file.diagnostics).toEqual([
      {
        severity: "error",
        message: "Invalid UTF-8 sequence",
        path: "a.js",
        start: 4,
        end: 5,
        labels: [],
        help: "Save the file as UTF-8",
      },
    ]);
  });

  test("every malformed sequence starts where Unicode's table 3-7 stops it", () => {
    const offsetOf = (bytes: number[]) => parse(new Uint8Array(bytes)).diagnostics[0]?.start;
    expect(offsetOf([0x61, 0xc0, 0x80])).toBe(1);
    expect(offsetOf([0xe0, 0x80, 0x80])).toBe(0);
    expect(offsetOf([0xed, 0xa0, 0x80])).toBe(0);
    expect(offsetOf([0xf4, 0x90, 0x80, 0x80])).toBe(0);
    expect(offsetOf([0x61, 0xf0, 0x9f, 0x98])).toBe(1);
    expect(offsetOf([0xf0, 0x9f, 0x98, 0x80, 0xf5])).toBe(2);
    expect(offsetOf([0xef, 0xbb, 0xbf, 0x61])).toBeUndefined();
  });

  test("a source that is neither text nor bytes throws", () => {
    expect(() => parse(1 as never)).toThrow("`source` must be a string or UTF-8 bytes");
  });
});

describe("depth", () => {
  test("a chain of any depth decodes", () => {
    const source = "a" + ".b(/* c */ x)".repeat(1000);
    for (const attachComments of [false, true]) {
      let node: any = (parse(source, { attachComments }).program.body[0] as any).expression;
      let calls = 0;
      for (; node.type === "CallExpression"; node = node.callee.object) calls++;
      expect(calls).toBe(1000);
      expect(node.name).toBe("a");
    }
  });
});
