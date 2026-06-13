import { describe, expect, test } from "bun:test";
import { Analyzer, SymbolFlags } from "yuku-analyzer";
import { summary } from "./summarize";

describe("variable kinds", () => {
  test("var, let, const, using", () => {
    expect(summary(`var a = 1; let b = 2; const c = 3; using d = e();`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            a#0  var
            b#1  let
            c#2  const
            d#3  const
            e → free"
      `);
  });

  test("function and class, declaration and expression", () => {
    expect(
      summary(`function f() {} class C {} const g = function () {}; const D = class {};`, {
        path: "input.js",
      }),
    ).toMatchInlineSnapshot(`
        "global
          module [strict]
            f#0  function
            C#1  class
            g#2  const
            D#3  const
            function "f"
            class "C"
            function <anonymous>
            class <anonymous>"
      `);
  });

  test("parameters: simple, default, rest, destructured", () => {
    expect(
      summary(`function f(a, b = 1, ...rest) {} const g = ({ x }, [y]) => x + y;`, {
        path: "input.js",
      }),
    ).toMatchInlineSnapshot(`
        "global
          module [strict]
            f#0  function
            g#4  const
            function "f"
              a#1  param
              b#2  param
              rest#3  param
            function =>
              x#5  param
              y#6  param
              x → #5
              y → #6"
      `);
  });

  test("a catch binding is a function-scoped variable", () => {
    expect(summary(`try {} catch (err) {}`, { path: "input.js" })).toMatchInlineSnapshot(`
      "global
        module [strict]
          block BlockStatement
          block CatchClause
            err#0  catch"
    `);
  });
});

describe("TypeScript declarations", () => {
  test("interface, type alias, enum, const enum", () => {
    expect(summary(`interface I {} type A = number; enum E {} const enum CE {}`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            I#0  interface
            A#1  type
            E#2  enum
            CE#3  const-enum
            block TSInterfaceDeclaration
            block TSTypeAliasDeclaration"
      `);
  });

  test("a value namespace vs a type-only namespace", () => {
    expect(summary(`namespace V { export const x = 1; } namespace T { type Y = 1; }`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            V#0  namespace value-module
            T#2  namespace
            tsModule
              x#1  const exported
            tsModule
              Y#3  type
              block TSTypeAliasDeclaration"
      `);
  });

  test("namespace members bind in the tsModule scope and resolve", () => {
    expect(summary(`namespace N { const x = 1; x; }`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          N#0  namespace value-module
          tsModule
            x#1  const
            x → #1"
    `);
  });

  test("a type parameter and an infer binding", () => {
    expect(summary(`type F<T> = T extends Array<infer U> ? U : never;`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          F#0  type
          block TSTypeAliasDeclaration
            T#1  type-param
            block TSConditionalType
              U#2  type-param
              T → #1 type
              Array → free type
              U → #2 type"
    `);
  });

  test("every declare form carries the ambient flag", () => {
    expect(summary(`declare const x: number; declare function f(a: number): void;`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            x#0  const ambient
            f#1  function ambient
            function "f"
              a#2  param ambient"
      `);
  });

  test("a declare namespace makes its whole body ambient", () => {
    // ts treats everything inside an ambient context as ambient without its
    // own declare keyword, which also exempts ambient bindings from strict
    // reserved-word checks (`export var static` is legal here)
    expect(summary(`declare namespace Foo { export var static: any; namespace Inner { var y; } }`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            Foo#0  namespace value-module ambient
            tsModule
              static#1  var ambient exported
              Inner#2  namespace value-module ambient
              tsModule
                y#3  var ambient"
      `);
  });
});

describe("declaration merging", () => {
  test("function overloads merge into one symbol with every declarator", () => {
    expect(
      summary(`function f(a: number): void; function f(a: string): void; function f(a: any) {}`),
    ).toMatchInlineSnapshot(`
        "global
          module [strict]
            f#0  function ×3
            function "f"
              a#1  param ambient
            function "f"
              a#2  param ambient
            function "f"
              a#3  param"
      `);
  });

  test("class and interface merge", () => {
    expect(summary(`class C {} interface C {}`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          C#0  class interface ×2
          class "C"
          block TSInterfaceDeclaration"
    `);
  });

  test("enum and namespace merge", () => {
    expect(summary(`enum E { A } namespace E { export const b = 1; }`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          E#0  enum namespace value-module ×2
          tsModule
            b#1  const exported"
    `);
  });

  test("function and namespace merge", () => {
    expect(summary(`function f() {} namespace f { export const g = 1; }`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          f#0  function namespace value-module ×2
          function "f"
          tsModule
            g#1  const exported"
    `);
  });
});

describe("modifiers", () => {
  test("exported and default flags", () => {
    expect(summary(`export const a = 1; export default function named() {}`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            a#0  const exported
            named#1  function exported default
            function "named"
        exports
          a → #0
          default → #1"
      `);
  });

  test("value-space and type-space imports", () => {
    expect(summary(`import v from "./v"; import type T from "./t"; import { type N } from "./n";`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            v#0  import
            T#1  type-import
            N#2  type-import
        imports
          default → #0 from "./v"
          default → #1 from "./t" type
          N → #2 from "./n" type"
      `);
  });
});

describe("value space and type space", () => {
  test("a class lives in both spaces; interface and alias are type-only; const is value-only", () => {
    const module = new Analyzer().addFile(
      "input.ts",
      `class C {} interface I {} type A = 1; const v = 1;`,
    );
    const spaces = (name: string) => {
      const s = module.symbols.find((sym) => sym.name === name)!;
      return [s.has(SymbolFlags.ValueSpace), s.has(SymbolFlags.TypeSpace)];
    };

    expect(spaces("C")).toEqual([true, true]);
    expect(spaces("I")).toEqual([false, true]);
    expect(spaces("A")).toEqual([false, true]);
    expect(spaces("v")).toEqual([true, false]);
  });

  test("an enum lives in both spaces", () => {
    const module = new Analyzer().addFile("input.ts", `enum E { A }`);
    const e = module.symbols.find((s) => s.name === "E")!;
    expect([e.has(SymbolFlags.ValueSpace), e.has(SymbolFlags.TypeSpace)]).toEqual([true, true]);
  });
});
