import { expect, test } from "bun:test";
import { gen } from "./helpers";

test("removes type annotations but keeps comments", () => {
  expect(gen(`// types incoming\nconst x: number = 1;`, { strip: true, comments: true }))
    .toMatchInlineSnapshot(`
      "// types incoming
      const x = 1;"
    `);
});
