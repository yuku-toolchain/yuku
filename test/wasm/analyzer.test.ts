import { describe, expect, test } from "bun:test";
import { analyze } from "../../npm/yuku-analyzer-wasm/index.js";
import { jsSource, tsSource } from "./sources";

describe("@yuku-analyzer/wasm", () => {
  test("resolves symbols and references for JS", () => {
    const mod = analyze(jsSource, { path: "input.js" });
    expect(mod.diagnostics).toEqual([]);
    const names = mod.symbols.map((s) => s.name);
    expect(names).toContain("greeting");
    expect(names).toContain("add");
    expect(names).toContain("Foo");
    expect(names).toContain("result");

    const greeting = mod.symbols.find((s) => s.name === "greeting");
    expect(greeting?.references.length).toBeGreaterThan(0);
  });

  test("lists tokens when requested", () => {
    const mod = analyze("const x = 1;", { path: "input.js", tokens: true });
    expect(mod.tokens!.length).toBe(5);
    expect(mod.tokens!.text(1)).toBe("x");
  });

  test("resolves TS-only symbols", () => {
    const mod = analyze(tsSource, { path: "input.ts" });
    const names = mod.symbols.map((s) => s.name);
    expect(names).toContain("Point");
    expect(names).toContain("Mapped");
    expect(names).toContain("Color");
    expect(names).toContain("NS");
    expect(names).toContain("fn");
  });

  test("decodes the AST it analyzed", () => {
    const mod = analyze("const greeting = 1; console.log(greeting);", { path: "input.js" });
    expect(mod.ast.body.map((n) => n.type)).toEqual([
      "VariableDeclaration",
      "ExpressionStatement",
    ]);
  });

  test("handles non-ASCII sources", () => {
    const mod = analyze(`const emoji = "🎉héllo"; const após = emoji;`, { path: "input.js" });
    expect(mod.symbols.map((s) => s.name)).toEqual(["emoji", "após"]);
  });
});
