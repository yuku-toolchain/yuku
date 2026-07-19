import { expect, test } from "bun:test";
import { gen } from "./helpers";

test("strip and minify compose in a single call", () => {
  expect(
    gen(`const x: number = 1000000;\nif (true) { obj["foo"] = undefined; }`, {
      strip: true,
      minify: true,
    }),
  ).toMatchInlineSnapshot(`"const x=1e6;if(!0){obj.foo=void 0}"`);
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
