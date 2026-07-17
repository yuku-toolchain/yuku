import { describe, expect, test } from "bun:test";
import { Analyzer } from "yuku-analyzer";
import { summary } from "./utils/summarize";

describe("root scope", () => {
  test("module wraps global, top-level bindings land in module scope", () => {
    expect(summary(`let x = 1;`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          x#0  let"
    `);
  });

  test("a script has only the global scope, non-strict", () => {
    expect(summary(`var x = 1; function f() {}`, { path: "input.cjs" })).toMatchInlineSnapshot(`
      "global
        x#0  var
        f#1  function
        function "f""
    `);
  });
});

describe("lexical scopes", () => {
  test("a block scopes let/const but not var", () => {
    expect(summary(`{ let a = 1; const b = 2; var c = 3; }`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            c#2  var
            block BlockStatement
              a#0  let
              b#1  const"
      `);
  });

  test("a for head and body are distinct block scopes", () => {
    expect(summary(`for (let i = 0; i < 1; i++) { let j = i; }`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            block ForStatement
              i#0  let
              i → #0
              i → #0 write
              block BlockStatement
                j#1  let
                i → #0"
      `);
  });

  test("function and arrow each open a function scope", () => {
    expect(summary(`function f(a) { const g = (b) => a + b; }`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            f#0  function
            function "f"
              a#1  param
              g#2  const
              function =>
                b#3  param
                a → #1
                b → #3"
      `);
  });

  test("a named function expression gets its own name scope", () => {
    expect(summary(`const f = function rec(n) { return rec(n); };`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            f#0  const
            expressionName "rec"
              rec#1  function
              function "rec"
                n#2  param
                rec → #1
                n → #2"
      `);
  });

  test("class declaration and static block", () => {
    expect(summary(`class C { m() {} static { let s = 1; } }`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            C#0  class
            class "C"
              function <anonymous>
              staticBlock
                s#1  let"
      `);
  });

  test("a catch clause scopes its parameter, optional binding adds none", () => {
    expect(summary(`try {} catch (e) { e; } try {} catch { let y; }`, { path: "input.js" }))
      .toMatchInlineSnapshot(`
        "global
          module [strict]
            block BlockStatement
            block CatchClause
              e#0  catch
              e → #0
            block BlockStatement
            block CatchClause
              y#1  let"
      `);
  });

  test("a TS namespace opens a module scope that holds its members", () => {
    expect(summary(`namespace N { export const x = 1; }`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          N#0  namespace value-module
          tsModule
            x#1  const exported"
    `);
  });
});

describe("strict mode", () => {
  test("modules are implicitly strict", () => {
    expect(summary(`let x = 1;`)).toMatchInlineSnapshot(`
      "global
        module [strict]
          x#0  let"
    `);
  });

  test("a use-strict directive turns a sloppy script function strict", () => {
    expect(summary(`function f() { "use strict"; return 1; }`, { path: "input.cjs" }))
      .toMatchInlineSnapshot(`
        "global
          f#0  function
          function "f" [strict]"
      `);
  });
});

describe("hoist targets", () => {
  test("a var in a nested block hoists to the enclosing function scope", () => {
    const module = new Analyzer().addFile("input.js", `function outer() { { var v = 1; } }`);
    const fnScope = module.scopes.find((s) => s.kind === "function")!;
    const blockScope = module.scopes.find((s) => s.kind === "block")!;
    const v = module.symbols.find((s) => s.name === "v")!;

    // the block does not own the var, its hoist target is the function scope,
    // and the symbol is declared directly in that function scope
    expect(blockScope.hoistTarget).toBe(fnScope);
    expect(v.scope).toBe(fnScope);
  });

  test("a function scope is its own hoist target", () => {
    const module = new Analyzer().addFile("input.js", `function f() {}`);
    const fnScope = module.scopes.find((s) => s.kind === "function")!;
    expect(fnScope.hoistTarget).toBe(fnScope);
  });
});
