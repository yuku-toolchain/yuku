import { describe, expect, test } from "bun:test";
import { js, parse, type SourceFile, walk } from "yuku";
import { nodesOf } from "../ast-helpers-for-test";

function fileOf(source: string, path = "input.js"): SourceFile {
  return parse(source, { path, semantic: true });
}

describe("walk", () => {
  test("visits enter then leave, with catch-alls bracketing typed handlers", () => {
    const file = fileOf(`f(x);`);
    const trace: string[] = [];
    walk(file.program, {
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

  test("scopeOf is the lexical scope at each node", () => {
    const file = fileOf(`function outer() { const inner = () => bound; } let bound = 1;`);
    const seen: Record<string, string> = {};
    walk(file.program, {
      Identifier(node) {
        seen[node.name] = file.scopeOf(node).kind;
      },
    });
    expect(seen).toMatchInlineSnapshot(`
      {
        "bound": "module",
        "inner": "functionBody",
        "outer": "function",
      }
    `);
  });

  test("a class decorator's scope is the enclosing scope, not the class", () => {
    const file = fileOf(`let dec = () => {}; @dec class C {}`, "input.ts");
    const seen: string[] = [];
    walk(file.program, {
      Identifier(node) {
        if (node.name === "dec" && file.referenceOf(node)) seen.push(file.scopeOf(node).kind);
      },
    });
    expect(seen).toEqual(["module"]);
  });

  test("a subtree root limits the walk", () => {
    const file = fileOf(`a; function f() { b; }`);
    const [fn] = nodesOf(file.program, "FunctionDeclaration");
    const names: string[] = [];
    walk(fn!, { Identifier: (node) => names.push(node.name) });
    expect(names).toEqual(["f", "b"]);
  });
});

describe("mutation", () => {
  test("ctx.remove splices a node out", () => {
    const file = fileOf(`a; debugger; b;`);
    walk(file.program, { DebuggerStatement: (_node, ctx) => ctx.remove() });
    expect(file.program.body.map((n) => n.type)).toEqual([
      "ExpressionStatement",
      "ExpressionStatement",
    ]);
  });

  test("ctx.stop ends the walk", () => {
    const file = fileOf(`a; b; c;`);
    const names: string[] = [];
    walk(file.program, {
      Identifier(node, ctx) {
        names.push(node.name);
        if (node.name === "b") ctx.stop();
      },
    });
    expect(names).toEqual(["a", "b"]);
  });

  test("ctx.skip prevents descent but still leaves", () => {
    const file = fileOf(`f(deep);`);
    const entered: string[] = [];
    let leftCall = false;
    walk(file.program, {
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

describe("node queries", () => {
  test("bindingOf, referenceOf, and scopeOf work on node identity", () => {
    const file = fileOf(`function f() { return inner; } let inner = 1;`);
    const [fn] = nodesOf(file.program, "FunctionDeclaration");
    const fnBinding = file.bindingOf(fn!.id!);
    expect(fnBinding?.name).toBe("f");

    const use = fn!.body?.body[0];
    if (use?.type !== "ReturnStatement" || use.argument?.type !== "Identifier") {
      throw new Error("expected a returned identifier");
    }
    const reference = file.referenceOf(use.argument);
    expect(reference?.binding?.name).toBe("inner");
    expect(file.scopeOf(use.argument)).toBe(reference!.scope);
  });

  test("node queries find nodes decoded after an earlier query", () => {
    const file = fileOf(`let inner = 1; function f() { return inner; }`);
    const [reference] = file.references;
    expect(file.referenceOf(reference!.node)).toBe(reference!);

    const [fn] = nodesOf(file.program, "FunctionDeclaration");
    expect(file.bindingOf(fn!.id!)?.name).toBe("f");
    expect(file.referenceOf(reference!.node)).toBe(reference!);
  });

  test("lookup walks the scope chain from a scope", () => {
    const file = fileOf(`let outer = 1; function f() { let local = 2; }`);
    const bodyScope = file.scopes.find((s) => s.kind === "functionBody")!;
    expect(bodyScope.lookup("local")?.name).toBe("local");
    expect(bodyScope.lookup("outer")?.name).toBe("outer");
    expect(bodyScope.lookup("missing")).toBeNull();
  });

  test("parentOf climbs from a node to the structure around it", () => {
    const source = `const { onPress: handler } = props; const alias = handler;`;
    const file = fileOf(source, "input.ts");
    const handlers = nodesOf(file.program, "Identifier").filter((node) => node.name === "handler");
    const property = file.parentOf(handlers.find((node) => file.bindingOf(node))!)!;
    expect(
      property.type === "Property" && property.key.type === "Identifier"
        ? property.key.name
        : null,
    ).toBe("onPress");
    expect(file.parentOf(property)?.type).toBe("ObjectPattern");

    const use = handlers.find((node) => file.referenceOf(node))!;
    const declarator = file.parentOf(use)!;
    expect(declarator.type === "VariableDeclarator" ? declarator.init : null).toBe(use);
  });

  test("parentOf is null at the root and for a foreign node", () => {
    const file = fileOf(`let x = 1;`);
    expect(file.parentOf(file.program)).toBeNull();
    expect(file.parentOf(js`x`)).toBeNull();
  });

  test("ancestors yields a node, then each parent up to the root", () => {
    const file = fileOf(`let x = f(1);`);
    const literal = nodesOf(file.program, "Literal")[0]!;
    expect([...file.ancestors(literal)].map((node) => node.type)).toEqual([
      "Literal",
      "CallExpression",
      "VariableDeclarator",
      "VariableDeclaration",
      "Program",
    ]);
  });

  test("nodeAt finds the innermost node at an offset", () => {
    const source = `const total = price + tax;`;
    const file = fileOf(source);
    const price = file.nodeAt(source.indexOf("price"))!;
    expect(price.type === "Identifier" ? price.name : null).toBe("price");
    expect(file.nodeAt(source.indexOf("+"))?.type).toBe("BinaryExpression");
    expect(file.nodeAt(source.length)).toBeNull();
    expect(file.bindingOf(file.nodeAt(source.indexOf("total"))!)?.name).toBe("total");
  });

  test("parentOf reaches the last child of a list longer than a u16", () => {
    const file = fileOf(`[${"0,".repeat(65_536)}];`);
    const first = file.program.body[0];
    if (first?.type !== "ExpressionStatement" || first.expression.type !== "ArrayExpression") {
      throw new Error("expected an array");
    }
    const array = first.expression;
    expect(file.parentOf(array.elements.at(-1)!)).toBe(array);
  });

  test("a parameter declaration resolves back through bindingOf", () => {
    // params nested in a decorator expression
    const file = fileOf(`class C { #f; m(@dec((x) => x.#f) p, plain) {} }`, "input.ts");
    void file.program;
    for (const name of ["x", "p", "plain"]) {
      const binding = file.bindings.find((s) => s.name === name)!;
      expect(file.bindingOf(binding.declarations[0]!)).toBe(binding);
    }
  });
});
