import { expect, test } from "bun:test";
import { parse, type Program } from "yuku-parser";
import { generate } from "yuku-codegen";
import { gen } from "./helpers";

test("strip and minify compose in a single call", () => {
  expect(
    gen(`const x: number = 1000000;\nif (true) { obj["foo"] = undefined; }`, {
      strip: true,
      minify: true,
    }),
  ).toMatchInlineSnapshot(`"const x=1e6;if(!0){obj.foo=undefined}"`);
});

test("minify: true forces maximum minification over format and quotes", () => {
  expect(gen(`const s = 'plain';`, { minify: true, format: "pretty", quotes: "preserve" }))
    .toMatchInlineSnapshot(`"const s="plain""`);
});

test("a minify object enables switches individually", () => {
  const source = `const x = 1000000;\nconst s = 'plain';`;
  expect(gen(source, { minify: { syntax: true } })).toMatchInlineSnapshot(`
    "const x = 1e6;
    const s = 'plain';"
  `);
  expect(gen(source, { minify: { whitespace: true, quotes: true } })).toMatchInlineSnapshot(
    `"const x=1000000;const s="plain""`,
  );
});

test("lists longer than a u16 survive the round trip", () => {
  const n = 65_536;
  const lengths = ({ body }: Program) => {
    const last = body.at(-1);
    if (last?.type !== "ExpressionStatement" || last.expression.type !== "ArrayExpression") {
      throw new Error("expected a trailing array");
    }
    return [body.length, last.expression.elements.length];
  };
  const { program } = parse(`${"x;".repeat(n)}[${"0,".repeat(n)}];`);
  expect(lengths(program)).toEqual([n + 1, n]);
  expect(lengths(parse(generate(program).code).program)).toEqual([n + 1, n]);
});
