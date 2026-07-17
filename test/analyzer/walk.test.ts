import { describe, expect, test } from "bun:test";
import { analyze as analyzeFile, type Module } from "yuku-analyzer";
import { b } from "yuku-ast";

function analyze(source: string, path = "input.js"): Module {
  return analyzeFile(source, { path });
}

describe("walk", () => {
  test("visits enter then leave, with catch-alls bracketing typed handlers", () => {
    const module = analyze(`f(x);`);
    const trace: string[] = [];
    module.walk({
      enter: (node) => trace.push(`enter ${node.type}`),
      leave: (node) => trace.push(`leave ${node.type}`),
      CallExpression: {
        enter: () => trace.push("typed-enter CallExpression"),
        leave: () => trace.push("typed-leave CallExpression"),
      },
    });
    expect(trace).toMatchInlineSnapshot(`
      [
        "enter Program",
        "enter ExpressionStatement",
        "enter CallExpression",
        "typed-enter CallExpression",
        "enter Identifier",
        "leave Identifier",
        "enter Identifier",
        "leave Identifier",
        "typed-leave CallExpression",
        "leave CallExpression",
        "leave ExpressionStatement",
        "leave Program",
      ]
    `);
  });

  test("ctx.scope is the lexical scope at each node", () => {
    const module = analyze(`function outer() { const inner = () => bound; } let bound = 1;`);
    const seen: Record<string, string> = {};
    module.walk({
      Identifier(node, ctx) {
        seen[node.name] = ctx.scope.kind;
      },
    });
    // `bound` resolves at use inside the arrow, `inner` is declared in the
    // function scope, `outer` at module scope
    expect(seen).toMatchInlineSnapshot(`
      {
        "bound": "module",
        "inner": "function",
        "outer": "function",
      }
    `);
  });

  test("a class decorator's scope is the enclosing scope, not the class", () => {
    const module = analyze(`let dec = () => {}; @dec class C {}`, "input.ts");
    const seen: string[] = [];
    module.walk({
      Identifier(node, ctx) {
        if (node.name === "dec" && ctx.reference) seen.push(ctx.scope.kind);
      },
    });
    expect(seen).toEqual(["module"]);
  });

  test("ctx.symbol and ctx.reference are the node→model shorthands", () => {
    const module = analyze(`let x = 1; x;`);
    const declSymbols: string[] = [];
    const useReferenceSymbols: (string | null)[] = [];

    module.walk({
      Identifier(_, ctx) {
        if (ctx.symbol) declSymbols.push(ctx.symbol.name);
        if (ctx.reference) useReferenceSymbols.push(ctx.reference.symbol?.name ?? null);
      },
    });
    expect(declSymbols).toContain("x");
    expect(useReferenceSymbols).toContain("x");
  });

  test("a subtree root limits the walk", () => {
    const module = analyze(`a; function f() { b; }`);
    const [fn] = module.findAll("FunctionDeclaration");
    const names: string[] = [];
    module.walk({ Identifier: (node) => names.push(node.name) }, fn);
    expect(names).toEqual(["f", "b"]);
  });
});

describe("mutation", () => {
  test("ctx.remove splices a node out", () => {
    const module = analyze(`a; debugger; b;`);
    module.walk({ DebuggerStatement: (_node, ctx) => ctx.remove() });
    expect(module.ast.body.map((n) => n.type)).toEqual([
      "ExpressionStatement",
      "ExpressionStatement",
    ]);
  });

  test("ctx.stop ends the walk", () => {
    const module = analyze(`a; b; c;`);
    const names: string[] = [];
    module.walk({
      Identifier(node, ctx) {
        names.push(node.name);
        if (node.name === "b") ctx.stop();
      },
    });
    expect(names).toEqual(["a", "b"]);
  });

  test("ctx.skip prevents descent but still leaves", () => {
    const module = analyze(`f(deep);`);
    const entered: string[] = [];
    let leftCall = false;
    module.walk({
      CallExpression: {
        enter: (_node, ctx) => ctx.skip(),
        leave: () => (leftCall = true),
      },
      Identifier: (node) => entered.push(node.name),
    });
    expect(entered).toEqual([]);
    expect(leftCall).toBe(true);
  });
});

describe("walkAsync", () => {
  test("awaited handlers keep the sync walk's exact visit order", async () => {
    const syncTrace: string[] = [];
    analyze(`f(x);`).walk({
      enter: (node) => syncTrace.push(`enter ${node.type}`),
      leave: (node) => syncTrace.push(`leave ${node.type}`),
    });

    const asyncTrace: string[] = [];
    await analyze(`f(x);`).walkAsync({
      async enter(node) {
        await new Promise((resolve) => setTimeout(resolve, 0));
        asyncTrace.push(`enter ${node.type}`);
      },
      async leave(node) {
        await Promise.resolve();
        asyncTrace.push(`leave ${node.type}`);
      },
    });
    expect(asyncTrace).toEqual(syncTrace);
  });

  test("ctx still points at the current node after an await", async () => {
    const module = analyze(`function outer() { const inner = () => bound; } let bound = 1;`);
    const seen: Record<string, string> = {};
    await module.walkAsync({
      async Identifier(node, ctx) {
        await new Promise((resolve) => setTimeout(resolve, 0));
        seen[node.name] = ctx.scope.kind;
      },
    });
    expect(seen).toEqual({ bound: "module", inner: "function", outer: "function" });
  });

  test("mutation works from an async handler", async () => {
    const module = analyze(`a; debugger; b;`);
    await module.walkAsync({
      async DebuggerStatement(_node, ctx) {
        await Promise.resolve();
        ctx.remove();
      },
    });
    expect(module.ast.body.map((n) => n.type)).toEqual([
      "ExpressionStatement",
      "ExpressionStatement",
    ]);
  });

  test("ctx.stop ends the walk, sync handlers mix in", async () => {
    const module = analyze(`a; b; c;`);
    const names: string[] = [];
    await module.walkAsync({
      Identifier(node, ctx) {
        names.push(node.name);
        if (node.name === "b") ctx.stop();
      },
    });
    expect(names).toEqual(["a", "b"]);
  });
});

describe("node queries", () => {
  test("findAll collects matching nodes in source order", () => {
    const module = analyze(`function a() {} function b() {} class C {}`);
    expect(
      module.findAll(["FunctionDeclaration", "ClassDeclaration"]).map((n) => n.id?.name),
    ).toEqual(["a", "b", "C"]);
  });

  test("symbolOf, referenceOf, and scopeOf work on node identity", () => {
    const module = analyze(`function f() { return inner; } let inner = 1;`);
    const [fn] = module.findAll("FunctionDeclaration");
    const fnSymbol = module.symbolOf(fn!.id!);
    expect(fnSymbol?.name).toBe("f");

    const use = fn!.body?.body[0];
    if (use?.type !== "ReturnStatement" || use.argument?.type !== "Identifier") {
      throw new Error("expected a returned identifier");
    }
    const reference = module.referenceOf(use.argument);
    expect(reference?.symbol?.name).toBe("inner");
    expect(module.scopeOf(use.argument)).toBe(reference!.scope);
  });

  test("resolve walks the scope chain from a starting scope", () => {
    const module = analyze(`let outer = 1; function f() { let local = 2; }`);
    const fnScope = module.scopes.find((s) => s.kind === "function")!;
    expect(module.resolve("local", fnScope)?.name).toBe("local");
    expect(module.resolve("outer", fnScope)?.name).toBe("outer");
    expect(module.resolve("missing", fnScope)).toBeNull();
  });

  test("parentOf climbs from a node to the structure around it", () => {
    const module = analyze(
      `const { onPress: handler } = props; const alias = handler;`,
      "input.ts",
    );
    // a destructured binding climbs to the Property that names the prop
    const handler = module
      .findAll("Identifier")
      .find((n) => n.name === "handler" && module.symbolOf(n))!;
    const property = module.parentOf(handler)!;
    expect(
      property.type === "Property" && property.key.type === "Identifier"
        ? property.key.name
        : null,
    ).toBe("onPress");
    expect(module.parentOf(property)?.type).toBe("ObjectPattern");

    // a use climbs to the declarator whose init it is
    const use = module.findAll("Identifier").find((n) => n.name === "handler" && module.referenceOf(n))!;
    const declarator = module.parentOf(use)!;
    expect(declarator.type === "VariableDeclarator" ? declarator.init : null).toBe(use);
  });

  test("parentOf is null at the root and for a foreign node", () => {
    const module = analyze(`let x = 1;`);
    expect(module.parentOf(module.ast)).toBeNull();
    expect(module.parentOf(b.Identifier({ name: "x" }))).toBeNull();
  });

  test("a parameter declaration resolves back through symbolOf", () => {
    // covers params nested in a decorator expression, where the node index is
    // easy to lose
    const module = analyze(`class C { #f; m(@dec((x) => x.#f) p, plain) {} }`, "input.ts");
    void module.ast;
    for (const name of ["x", "p", "plain"]) {
      const symbol = module.symbols.find((s) => s.name === name)!;
      expect(module.symbolOf(symbol.declarations[0]!)).toBe(symbol);
    }
  });
});
