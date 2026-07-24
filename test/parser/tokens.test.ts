// Token stream through the native binding: espree-style classification,
// spans that slice back to the source, and the default-off behavior.
import { describe, expect, test } from "bun:test";
import { parse } from "yuku-parser";

describe("tokens option", () => {
  test("returns every token in source order with matching spans", () => {
    const source = "class C { #x = true; m() { return this.#x ?? null; } }";
    const { tokens, diagnostics } = parse(source, { tokens: true });
    expect(diagnostics).toEqual([]);
    if (!tokens) throw new Error("tokens missing");

    let prevEnd = 0;
    for (const t of tokens) {
      expect(t.start).toBeGreaterThanOrEqual(prevEnd);
      expect(t.end).toBeGreaterThan(t.start);
      expect(source.slice(t.start, t.end)).toBe(t.value);
      prevEnd = t.end;
    }
    expect(tokens.map((t) => [t.type, t.value])).toContainEqual(["PrivateIdentifier", "#x"]);
    expect(tokens.map((t) => [t.type, t.value])).toContainEqual(["Boolean", "true"]);
    expect(tokens.map((t) => [t.type, t.value])).toContainEqual(["Null", "null"]);
  });

  test("distinguishes regex literals from division", () => {
    const { tokens } = parse("a / b; c = /d/g;", { tokens: true });
    if (!tokens) throw new Error("tokens missing");
    expect(tokens[1]).toEqual({ type: "Punctuator", value: "/", start: 2, end: 3 });
    expect(tokens[6]).toEqual({ type: "RegularExpression", value: "/d/g", start: 11, end: 15 });
  });

  test("keeps comments out of the stream", () => {
    const { tokens, comments } = parse("/* c */ let x; // d", { tokens: true });
    if (!tokens) throw new Error("tokens missing");
    expect(comments).toHaveLength(2);
    expect(tokens.map((t) => t.value)).toEqual(["let", "x", ";"]);
  });

  test("is absent unless requested", () => {
    expect(parse("let x;").tokens).toBeUndefined();
    expect(parse("let x;", { tokens: false }).tokens).toBeUndefined();
  });

  test("classifies await by source type", () => {
    const asModule = parse("await 0;", { tokens: true, sourceType: "module" }).tokens;
    expect(asModule?.[0]?.type).toBe("Keyword");
    const asScript = parse("var await;", { tokens: true, sourceType: "script" }).tokens;
    expect(asScript?.[1]?.type).toBe("Identifier");
  });
});
