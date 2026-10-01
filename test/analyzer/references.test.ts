import { describe, expect, test } from "bun:test";
import { Analyzer, BindingFlags } from "yuku-analyzer";
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
              functionBody BlockStatement
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

  test("this and arguments carry no binding", () => {
    expect(summary(`function f() { return this.x + arguments.length; }`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            f#0  function
            function "f"
              functionBody BlockStatement
                arguments → free"
      `);
  });

  test("a function is visible before its declaration (hoisting)", () => {
    expect(summary(`f(); function f() {}`, { path: "input.js" })).toMatchInlineSnapshot(`
      "global
        module [strict]
          f#0  function
          f → #0
          function "f"
            functionBody BlockStatement"
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
              functionBody BlockStatement
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
              functionBody BlockStatement
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
              functionBody BlockStatement
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
            functionBody BlockStatement
              T#1  const
              x#2  let
              T → free type
              T → #1"
    `);
  });

  test("lookup walks the chain per space, and bindings expose the predicates", () => {
    const module = new Analyzer().setFile(
      "input.ts",
      `type T = string; function f() { const T = 1; T; }`,
    );
    const alias = module.bindings.find((s) => s.name === "T" && s.has(BindingFlags.TypeSpace))!;
    const local = module.bindings.find((s) => s.name === "T" && s.has(BindingFlags.ValueSpace))!;
    const inner = local.scope;

    expect(module.lookup("T", { from: inner, space: "type" })).toBe(alias);
    expect(module.lookup("T", { from: inner, space: "value" })).toBe(local);
    expect(module.lookup("T", { from: inner, space: "any" })).toBe(local);
    expect(module.lookup("T", { from: inner })).toBe(local);

    expect(alias.visibleIn("type")).toBe(true);
    expect(alias.visibleIn("value")).toBe(false);
    expect(local.visibleIn("typeof")).toBe(true);
    expect(local.has(BindingFlags.NamespaceSpace)).toBe(false);
  });
});

describe("decorators", () => {
  test("a class decorator resolves outside the class, blind to its type parameters", () => {
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

  test("a member decorator evaluates in the class scope", () => {
    expect(summary(`const v = 1; class C { @((x) => v + x) m() {} }`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          v#0  const
          C#1  class
          class "C"
            function =>
              x#2  param
              v → #0
              x → #2
            function <anonymous>
              functionBody BlockStatement"
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
            function "Foo"
              functionBody BlockStatement"
      `);
  });

  test("a lowercase member root resolves while a direct lowercase tag stays intrinsic", () => {
    // member roots are expressions while direct lowercase tags are intrinsic
    expect(
      summary(`const motion = {}; const App = () => <><motion.div /><div /></>;`, {
        path: "input.jsx",
      }),
    ).toMatchInlineSnapshot(`
        "global
          module [strict]
            motion#0  const
            App#1  const
            function =>
              motion → #0"
      `);
  });

  test("a tag is a component unless JSX transforms emit it as an intrinsic string", () => {
    const components = ["_Widget", "_widget", "$Widget", "$", "éWidget", "ÉWidget", "Ωmega", "中文"];
    for (const name of components) {
      const module = new Analyzer().setFile("input.jsx", `const ${name} = 1; <${name} />;`);
      expect(module.rootScope.find(name)?.references, name).toHaveLength(1);
    }
    for (const tag of ["widget", "foo-bar", "Foo-Bar", "a:b", "this", "this.Foo"]) {
      const module = new Analyzer().setFile("input.jsx", `<${tag} />;`);
      expect(module.unresolvedReferences, tag).toEqual([]);
    }
  });
});

describe("reference cross-indexes", () => {
  test("unresolvedReferences is exactly the free names", () => {
    const module = new Analyzer().setFile("input.js", `let local = 1; local; free1; free2;`);
    expect(module.unresolvedReferences.map((r) => r.name)).toEqual(["free1", "free2"]);
  });

  test("a binding's references all point back to it", () => {
    const module = new Analyzer().setFile("input.js", `let x = 1; x; x = 2; x + x;`);
    const x = module.bindings.find((s) => s.name === "x")!;
    expect(x.references.length).toBe(4);
    expect(x.references.every((r) => r.binding === x)).toBe(true);
  });
});
