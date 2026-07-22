import { describe, expect, test } from "bun:test";
import { analyze } from "../../npm/yuku-analyzer-wasm/index.js";

const jsSource = `// leading comment
const greeting = "hello";
function add(a, b = 1, ...rest) {
  return a + b + rest.length;
}
class Foo extends Bar {
  #x = 42;
  static { console.log("static block"); }
  get value() { return this.#x; }
}
export const result = add(1, 2, 3) ?? greeting;
`;

const tsSource = `
interface Point { x: number; y?: string }
type Mapped<T> = { readonly [K in keyof T]-?: T[K] };
enum Color { Red, Green = "g" }
namespace NS { export const v: Point = { x: 1 }; }
const fn = async <T,>(arg: T): Promise<T> => arg;
`;

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
