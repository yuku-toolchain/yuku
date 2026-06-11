import { describe, expect, test } from "bun:test";
import { Analyzer, SymbolFlags, type Module } from "yuku-analyzer";

function analyze(source: string, path = "input.js"): Module {
  return new Analyzer().addFile(path, source);
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

  test("ctx.scope is replayed from the native tree", () => {
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

  test("ctx.symbol and ctx.reference are the node→model shorthands", () => {
    const module = analyze(`let x = 1; x;`);
    let declSymbol: string | null = null;
    let useReferenceSymbol: string | null = null;

    module.walk({
      Identifier(_, ctx) {
        if (ctx.symbol) declSymbol = ctx.symbol.name;
        if (ctx.reference) useReferenceSymbol = ctx.reference.symbol?.name ?? null;
      },
    });
    // @ts-expect-error
    expect(declSymbol).toBe("x");
    // @ts-expect-error
    expect(useReferenceSymbol).toBe("x");
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

describe("scan", () => {
  test("scan visits the same identifiers as walk and resolves the same symbols", () => {
    const module = analyze(`let x = 1; function f() { return x; }`);

    const walkHits: string[] = [];
    module.walk({
      Identifier: (node, ctx) =>
        walkHits.push(`${node.name}:${ctx.symbol?.id ?? ctx.reference?.symbol?.id ?? "-"}`),
    });

    const scanHits: string[] = [];
    module.scan({
      Identifier(cursor) {
        const symbol = cursor.symbol ?? cursor.reference?.symbol ?? null;
        scanHits.push(`${cursor.node().name}:${symbol?.id ?? "-"}`);
      },
    });

    expect(scanHits).toEqual(walkHits);
  });

  test("a scan resolves references without materializing nodes", () => {
    const module = analyze(`import { dep } from "./d"; dep(); dep = 1;`, "input.ts");
    const writes: string[] = [];
    module.scan({
      Identifier(cursor) {
        const reference = cursor.reference;
        if (reference?.isWrite && reference.symbol?.has(SymbolFlags.Import))
          writes.push(reference.name);
      },
    });
    expect(writes).toEqual(["dep"]);
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
    const fnSymbol = module.symbolOf(fn.id!);
    expect(fnSymbol?.name).toBe("f");

    const use = fn.body?.body[0];
    const arg = (use as { argument: { type: string; name: string } }).argument;
    const reference = module.referenceOf(arg as never);
    expect(reference?.symbol?.name).toBe("inner");
    expect(module.scopeOf(arg as never)).toBe(reference!.scope);
  });

  test("resolve walks the scope chain from a starting scope", () => {
    const module = analyze(`let outer = 1; function f() { let local = 2; }`);
    const fnScope = module.scopes.find((s) => s.kind === "function")!;
    expect(module.resolve("local", fnScope)?.name).toBe("local");
    expect(module.resolve("outer", fnScope)?.name).toBe("outer");
    expect(module.resolve("missing", fnScope)).toBeNull();
  });

  test("a parameter declaration resolves back through symbolOf", () => {
    // covers params nested in a decorator expression, where the node index is
    // easy to lose
    const module = analyze(`class C { #f; m(@dec((x) => x.#f) p, plain) {} }`, "input.ts");
    void module.ast;
    for (const name of ["x", "p", "plain"]) {
      const symbol = module.symbols.find((s) => s.name === name)!;
      expect(module.symbolOf(symbol.declarations[0])).toBe(symbol);
    }
  });
});
