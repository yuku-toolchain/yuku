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

  test("a module's declare module block augments the module it names", () => {
    const augmentation = `declare module "./m" { interface I {} }`;
    expect(summary(`export {}; ${augmentation}`, { lang: "ts" })).toMatchInlineSnapshot(`
      "global
        module [strict]
          tsModule
            I#0  interface ambient exported
            block TSInterfaceDeclaration
      imports
        (augmentation) from "./m" type"
    `);
    expect(summary(`import.meta; ${augmentation}`, { lang: "ts" })).toMatchInlineSnapshot(`
      "global
        module [strict]
          tsModule
            I#0  interface ambient exported
            block TSInterfaceDeclaration
      imports
        (augmentation) from "./m" type"
    `);
    expect(summary(augmentation, { lang: "ts" })).toMatchInlineSnapshot(`
      "global
        module [strict]
          tsModule
            I#0  interface ambient exported
            block TSInterfaceDeclaration"
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
              functionBody BlockStatement
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
        MyLib#0  namespace ambient
        module [strict]
      exports
        export as namespace MyLib"
    `);
  });

  test("export import both imports and exports", () => {
    expect(
      summary(`namespace N {} export import A = N; export import B = require("./b");`, {
        path: "input.ts",
      }),
    ).toMatchInlineSnapshot(`
      "global
        module [strict]
          N#0  namespace
          A#1  import exported
          B#2  import exported
          N → #0 namespace
          tsModule
      imports
        * as #2 from "./b"
      exports
        A → #1
        B → #2"
    `);
  });

  test("a declaration file with no export statement exports every declaration", () => {
    expect(
      summary(
        `import { A } from "./a"; declare const x: A; interface I {} declare namespace N {}
        declare global { var g: 1 } export default function f(): void;`,
        { path: "input.d.ts" },
      ),
    ).toMatchInlineSnapshot(`
      "global
        g#4  var ambient
        module [strict]
          A#0  import
          x#1  const ambient exported
          I#2  interface ambient exported
          N#3  namespace ambient exported
          f#5  function ambient exported default
          A → #0 type
          block TSInterfaceDeclaration
          tsModule
          tsModule
          function "f"
      imports
        A → #0 from "./a"
      exports
        x → #1 type
        I → #2 type
        N → #3 type
        default → #5"
    `);
  });

  test("an export statement or no module syntax ends a declaration file's export context", () => {
    expect(
      summary(`declare const x: 1; declare const y: 2; export { x };`, { path: "input.d.ts" }),
    ).toMatchInlineSnapshot(`
      "global
        module [strict]
          x#0  const ambient
          y#1  const ambient
          x → #0 any
      exports
        x → #0"
    `);
    expect(summary(`declare const x: 1; interface I {}`, { path: "input.d.ts" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            x#0  const ambient
            I#1  interface ambient
            block TSInterfaceDeclaration"
      `);
  });
});
