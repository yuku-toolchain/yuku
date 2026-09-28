import { describe, expect, test } from "bun:test";
import { generate } from "../../npm/yuku-codegen-wasm/index.js";
import { parse } from "../../npm/yuku-parser-wasm/index.js";
import { jsSource, tsSource } from "./sources";

describe("@yuku-codegen/wasm", () => {
  test("generates code from a parsed AST", () => {
    const { program } = parse(jsSource, { lang: "js" });
    const code = generate(program);
    expect(code).toContain("function add");
    expect(code).toContain("class Foo");
    expect(code).toContain("??");
  });

  test("minifies", () => {
    const { program } = parse(jsSource, { lang: "js" });
    expect(generate(program, { minify: true })).not.toContain("\n  ");
  });

  test("strips TS types", () => {
    const { program } = parse(tsSource, { lang: "ts" });
    const stripped = generate(program, { strip: true });
    expect(stripped).not.toContain("interface");
    expect(stripped).not.toContain(": Promise<");
  });

  test("output reparses to the same program shape", () => {
    const { program } = parse(jsSource, { lang: "js" });
    const reparsed = parse(generate(program), { lang: "js" });
    expect(reparsed.diagnostics).toEqual([]);
    expect(reparsed.program.body.map((n) => n.type)).toEqual(program.body.map((n) => n.type));
  });
});
