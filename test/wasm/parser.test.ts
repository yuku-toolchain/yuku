import { describe, expect, test } from "bun:test";
import { parse } from "../../npm/yuku-parser-wasm/index.js";

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

describe("@yuku-parser/wasm", () => {
  test("parses JS with the documented AST shape", () => {
    const { program, diagnostics } = parse(jsSource, { lang: "js" });
    expect(diagnostics).toEqual([]);
    expect(program.type).toBe("Program");

    const fn = program.body.find((n) => n.type === "FunctionDeclaration");
    if (fn?.type !== "FunctionDeclaration") throw new Error("no FunctionDeclaration");
    expect(fn.params).toHaveLength(3);
    expect(fn.params[2]?.type).toBe("RestElement");

    const cls = program.body.find((n) => n.type === "ClassDeclaration");
    if (cls?.type !== "ClassDeclaration") throw new Error("no ClassDeclaration");
    expect(cls.body.body.map((n) => n.type)).toContain("StaticBlock");
  });

  test("node spans slice back to the source text", () => {
    const { program } = parse(jsSource, { lang: "js" });
    const first = program.body[0];
    if (!first) throw new Error("empty body");
    expect(jsSource.slice(first.start, first.end)).toBe(`const greeting = "hello";`);
  });

  test("parses TS declarations", () => {
    const { program, diagnostics } = parse(tsSource, { lang: "ts" });
    expect(diagnostics).toEqual([]);
    const types = program.body.map((n) => n.type);
    expect(types).toContain("TSInterfaceDeclaration");
    expect(types).toContain("TSTypeAliasDeclaration");
    expect(types).toContain("TSEnumDeclaration");
    expect(types).toContain("TSModuleDeclaration");
  });

  test("attaches comments when requested", () => {
    const { program } = parse(jsSource, { lang: "js", attachComments: true });
    const first = program.body[0];
    if (!first) throw new Error("empty body");
    expect(first.comments?.[0]?.value).toContain("leading comment");
  });

  test("accepts pre-encoded UTF-8 bytes like the native parser", () => {
    const bytes = new TextEncoder().encode("const x = 1;\nconst y = x;");
    const { program, diagnostics } = parse(bytes as unknown as string, { lang: "js" });
    expect(diagnostics).toEqual([]);
    expect(program.type).toBe("Program");
    expect(program.body).toHaveLength(2);
  });

  test("maps byte offsets to UTF-16 offsets in non-ASCII sources", () => {
    const source = `const emoji = "🎉héllo"; const após = emoji;`;
    const { program } = parse(source, { lang: "js" });
    const [first, second] = program.body;
    if (first?.type !== "VariableDeclaration" || second?.type !== "VariableDeclaration")
      throw new Error("expected two VariableDeclarations");
    expect(source.slice(first.start, first.end)).toBe(`const emoji = "🎉héllo";`);
    expect(source.slice(second.start, second.end)).toBe(`const após = emoji;`);
    const literal = first.declarations[0]?.init;
    if (literal?.type !== "Literal") throw new Error("expected a Literal init");
    expect(literal.value).toBe("🎉héllo");
  });
});
