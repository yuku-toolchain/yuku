import { describe, expect, test } from "bun:test";
import { summary } from "./utils/summarize";

describe("imports", () => {
  test("default, named, aliased, and namespace", () => {
    expect(summary(`import d, { x, y as z } from "./m"; import * as ns from "./n";`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            d#0  import
            x#1  import
            z#2  import
            ns#3  import
        imports
          default → #0 from "./m"
          x → #1 from "./m"
          y → #2 from "./m"
          * as #3 from "./n""
      `);
  });

  test("a side-effect import binds nothing", () => {
    expect(summary(`import "./polyfill";`)).toMatchInlineSnapshot(`
      "global
        module [strict]
      imports
        (side-effect) from "./polyfill""
    `);
  });

  test("type-only import, and an inline type specifier", () => {
    expect(summary(`import type T from "./t"; import { value, type Named } from "./m";`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            T#0  type-import
            value#1  import
            Named#2  type-import
        imports
          default → #0 from "./t" type
          value → #1 from "./m"
          Named → #2 from "./m" type"
      `);
  });

  test("a defer phase import", () => {
    expect(summary(`import defer * as ns from "./m";`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            ns#0  import
        imports
          * as #0 from "./m" phase:defer"
      `);
  });
});

describe("exports", () => {
  test("a local named export and a renamed local export", () => {
    expect(summary(`const a = 1; export { a, a as aliased };`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          a#0  const
          a → #0 any
          a → #0 any
      exports
        a → #0
        aliased → #0"
    `);
  });

  test("a direct export and a default export", () => {
    expect(summary(`export const direct = 1; export default function named() {}`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            direct#0  const exported
            named#1  function exported default
            function "named"
        exports
          direct → #0
          default → #1"
      `);
  });

  test("an anonymous default has no local binding", () => {
    expect(summary(`export default 42;`)).toMatchInlineSnapshot(`
      "global
        module [strict]
      exports
        default → (anonymous)"
    `);
  });

  test("star, namespace re-export, and renamed re-export", () => {
    expect(summary(`export * from "./a"; export * as ns from "./b"; export { x as y } from "./c";`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
        exports
          * from "./a"
          * as ns from "./b"
          x as y from "./c""
      `);
  });

  test("a type-only re-export", () => {
    expect(summary(`export type { T } from "./t";`)).toMatchInlineSnapshot(`
      "global
        module [strict]
      exports
        T from "./t" type"
    `);
  });

  test("TS export equals", () => {
    expect(summary(`export = foo;`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          foo → free any
      exports
        export="
    `);
  });

  test("export equals and import equals in a CommonJS .cts script", () => {
    expect(
      summary(`import lib = require("./lib"); const foo = lib; export = foo;`, {
        path: "input.cts",
      }),
    ).toMatchInlineSnapshot(`
        "global
          lib#0  import
          foo#1  const
          lib → #0
          foo → #1 any
        imports
          * as #0 from "./lib"
        exports
          export="
      `);
  });

  test("TS export as namespace", () => {
    expect(summary(`export as namespace MyLib;`, { path: "input.ts" })).toMatchInlineSnapshot(`
      "global
        module [strict]
      exports
        export as namespace MyLib"
    `);
  });
});
