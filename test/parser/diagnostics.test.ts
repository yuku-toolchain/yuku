import { describe, expect, test } from "bun:test";
import { parse, type ParseOptions } from "yuku-parser";

const script: ParseOptions = { sourceType: "script" };

function rejects(source: string, options: ParseOptions = script) {
  expect(parse(source, options).diagnostics.length, source).toBeGreaterThan(0);
}

function accepts(source: string, options: ParseOptions = script) {
  expect(parse(source, options).diagnostics, source).toEqual([]);
}

function firstSpan(source: string) {
  const [first] = parse(source, script).diagnostics;
  expect(first, source).toBeDefined();
  return [first!.start, first!.end];
}

describe("diagnostics", () => {
  test("a lexical error behind a lookahead-driven keyword is reported", () => {
    for (const source of [
      "function f() { let 1x; return 42; }",
      "let 1x;",
      "using 1x;",
      "async 1x;",
      "import 1x;",
      "for (let 1x; ; ) {}",
      "for (using 1x of xs) {}",
    ]) rejects(source);
    rejects("await using 1x;", { sourceType: "module" });
    for (const source of [
      "declare 1x",
      "const enum 1x {}",
      "type 1x = 2",
      "abstract class 1x {}",
      "class C { readonly 1x: number }",
    ]) rejects(source, { lang: "ts" });
  });

  test("lookahead-driven keywords still parse cleanly when the next token is valid", () => {
    for (const source of [
      "let x = 1;",
      "let = 1;",
      "let.foo;",
      "let [a] = xs;",
      "using x = res;",
      "using;",
      "async () => 1;",
      "async;",
      "for (let x of xs) {}",
      "for (let in obj) {}",
    ]) accepts(source);
  });

  test("an initialized for-in or for-of head is rejected outside annex b", () => {
    for (const source of [
      "for (let a = 1 in b);",
      "for (const a = 1 in b);",
      "for (var [a] = 1 in b);",
      "for (var {a} = 1 in b);",
      "for (var a = 1 of b);",
      "for (let a = 1 of b);",
      "for (const a = 1 of b);",
      "async function f() { for await (var a = 1 of b); }",
    ]) rejects(source);
    rejects("for (var a = 1 in b);", { sourceType: "module" });
    rejects("for (var a = 1 in b);", { sourceType: "script", lang: "ts" });
    accepts("for (var a = 1 in b);");
  });

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
