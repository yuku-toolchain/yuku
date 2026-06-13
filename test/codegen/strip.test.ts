import { expect, test } from "bun:test";
import { gen } from "./helpers";

test("removes type annotations but keeps comments", () => {
  expect(gen("strip", `// types incoming\nconst x: number = 1;`, { comments: true }))
    .toMatchInlineSnapshot(`
      "// types incoming
      const x = 1;"
    `);
});
