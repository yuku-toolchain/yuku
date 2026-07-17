import { describe, expect, test } from "bun:test";
import { Analyzer, SymbolFlags } from "yuku-analyzer";
import { summary } from "./utils/summarize";

describe("resolution", () => {
  test("a use resolves to the nearest binding; an inner declaration shadows", () => {
    expect(summary(`let x = 1; function f() { let x = 2; return x; } x;`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            x#0  let
            f#1  function
            x → #0
            function "f"
              x#2  let
              x → #2"
      `);
  });

  test("a free name resolves to nothing", () => {
    expect(summary(`console.log(undefinedGlobal);`, { path: "input.js" })).toMatchInlineSnapshot(`
      "global
        module [strict]
          console → free
          undefinedGlobal → free"
    `);
  });

  test("this and arguments carry no symbol", () => {
    expect(summary(`function f() { return this.x + arguments.length; }`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            f#0  function
            function "f"
              arguments → free"
      `);
  });

  test("a function is visible before its declaration (hoisting)", () => {
    expect(summary(`f(); function f() {}`, { path: "input.js" })).toMatchInlineSnapshot(`
      "global
        module [strict]
          f#0  function
          f → #0
          function "f""
    `);
  });
});

describe("write detection", () => {
  test("plain, compound, and update assignments", () => {
    expect(summary(`let a = 1; a = 2; a += 3; a++; --a; a;`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            a#0  let
            a → #0 write
            a → #0 write
            a → #0 write
            a → #0 write
            a → #0"
      `);
  });

  test("destructuring assignment targets are writes", () => {
    expect(summary(`let a, b; [a] = []; ({ b } = {});`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            a#0  let
            b#1  let
            a → #0 write
            b → #1 write"
      `);
  });

  test("a for-in/of assignment target is a write, a fresh binding is not", () => {
    expect(summary(`let k; for (k in {}) {} for (const v of []) v;`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            k#0  let
            block ForInStatement
              k → #0 write
              block BlockStatement
            block ForOfStatement
              v#1  const
              v → #1"
      `);
  });
});

describe("declaration spaces", () => {
  test("a name used as both a value and a type resolves in each space", () => {
    expect(summary(`class C {} const c: C = new C();`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          C#0  class
          c#1  const
          C → #0 type
          C → #0
          class "C""
    `);
  });

  test("an import used only in a type position is a type reference", () => {
    expect(summary(`import { T } from "./t"; let x: T;`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          T#0  import
          x#1  let
          T → #0 type
      imports
        T → #0 from "./t""
    `);
  });

  test("an inner value binding does not shadow an outer type, and vice versa", () => {
    expect(summary(`type T = string; function f() { const T = 1; let x: T; T; }`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            T#0  type
            f#1  function
            block TSTypeAliasDeclaration
            function "f"
              T#2  const
              x#3  let
              T → #0 type
              T → #2"
      `);
  });

  test("typeof resolves its entity in value space", () => {
    expect(summary(`const v = 1; function f() { type v = string; let a: typeof v; let b: v; }`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            v#0  const
            f#1  function
            function "f"
              v#2  type
              a#3  let
              b#4  let
              v → #0 typeof
              v → #2 type
              block TSTypeAliasDeclaration"
      `);
  });

  test("a dotted type name starts from a namespace, not a shadowing value", () => {
    expect(summary(`namespace N { export type T = number; } function f() { const N = 1; let x: N.T; }`))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            N#0  namespace
            f#2  function
            tsModule
              T#1  type exported
              block TSTypeAliasDeclaration
            function "f"
              N#3  const
              x#4  let
              N → #0 namespace"
      `);
  });

  test("a reference with no binding in its space anywhere is unresolved", () => {
    expect(summary(`function f() { const T = 1; let x: T; T; }`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          f#0  function
          function "f"
            T#1  const
            x#2  let
            T → free type
            T → #1"
    `);
  });

  test("resolve walks the chain per space, and symbols expose the predicates", () => {
    const module = new Analyzer().addFile(
      "input.ts",
      `type T = string; function f() { const T = 1; T; }`,
    );
    const alias = module.symbols.find((s) => s.name === "T" && s.has(SymbolFlags.TypeSpace))!;
    const local = module.symbols.find((s) => s.name === "T" && s.has(SymbolFlags.ValueSpace))!;
    const inner = local.scope;

    expect(module.resolve("T", inner, "type")).toBe(alias);
    expect(module.resolve("T", inner, "value")).toBe(local);
    expect(module.resolve("T", inner, "any")).toBe(local);
    expect(module.resolve("T", inner)).toBe(local);

    expect(alias.visibleIn("type")).toBe(true);
    expect(alias.visibleIn("value")).toBe(false);
    expect(local.visibleIn("typeof")).toBe(true);
    expect(local.has(SymbolFlags.NamespaceSpace)).toBe(false);
  });
});

describe("decorators", () => {
  test("a class decorator resolves outside the class, blind to its type parameters", () => {
    // tc39 decorators evaluate in the scope enclosing the class, so `@dec`
    // must reach the outer declare var even when a type parameter shadows it
    expect(summary(`declare var dec: any; @dec class C<dec> {}`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          dec#0  var ambient
          C#1  class
          dec → #0
          class "C"
            dec#2  type-param"
    `);
  });

  test("a member decorator's inner functions parent outside the class", () => {
    expect(summary(`const v = 1; class C { @((x) => v + x) m() {} }`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          v#0  const
          C#1  class
          class "C"
            function <anonymous>
          function =>
            x#2  param
            v → #0
            x → #2"
    `);
  });
});

describe("JSX", () => {
  test("a component name resolves to its binding, an intrinsic element is free", () => {
    expect(
      summary(`const App = () => <Foo><div /></Foo>; function Foo() { return null; }`, {
        path: "input.jsx",
      }),
    ).toMatchInlineSnapshot(`
        "global
          module [strict]
            App#0  const
            Foo#1  function
            function =>
              Foo → #1
              Foo → #1
            function "Foo""
      `);
  });
});

describe("reference cross-indexes", () => {
  test("unresolvedReferences is exactly the free names", () => {
    const module = new Analyzer().addFile("input.js", `let local = 1; local; free1; free2;`);
    expect(module.unresolvedReferences.map((r) => r.name)).toEqual(["free1", "free2"]);
  });

  test("a symbol's references all point back to it", () => {
    const module = new Analyzer().addFile("input.js", `let x = 1; x; x = 2; x + x;`);
    const x = module.symbols.find((s) => s.name === "x")!;
    expect(x.references.length).toBe(4);
    expect(x.references.every((r) => r.symbol === x)).toBe(true);
  });
});
