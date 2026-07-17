import { describe, expect, test } from "bun:test";
import { parse } from "yuku-parser";
import { b, nameOf, walk, walkAsync } from "yuku-ast";

function program(source: string) {
  return parse(source, { sourceType: "module", lang: "ts" }).program;
}

describe("walk", () => {
  test("visits enter then leave with catch-alls bracketing typed handlers", () => {
    const trace: string[] = [];
    walk(program(`f(x);`), {
      enter: (node) => trace.push(`enter ${node.type}`),
      leave: (node) => trace.push(`leave ${node.type}`),
      CallExpression: {
        enter: () => trace.push("typed-enter"),
        leave: () => trace.push("typed-leave"),
      },
    });
    expect(trace).toEqual([
      "enter Program",
      "enter ExpressionStatement",
      "enter CallExpression",
      "typed-enter",
      "enter Identifier",
      "leave Identifier",
      "enter Identifier",
      "leave Identifier",
      "typed-leave",
      "leave CallExpression",
      "leave ExpressionStatement",
      "leave Program",
    ]);
  });

  test("state threads to every handler and the root is returned", () => {
    const root = program(`a;`);
    const state: string[] = [];
    const result = walk(root, { Identifier: (_n, ctx) => ctx.state.push("hit") }, state);
    expect(result).toBe(root);
    expect(state).toEqual(["hit"]);
  });

  test("remove splices, replace continues into the replacement", () => {
    const root = program(`a; debugger; b;`);
    walk(root, {
      DebuggerStatement: (_n, ctx) => ctx.remove(),
      Identifier(node, ctx) {
        if (node.name === "a") {
          ctx.replace(b.Identifier({ name: "renamed" }));
        }
      },
    });
    expect(root.body.map((n) => n.type)).toEqual(["ExpressionStatement", "ExpressionStatement"]);
    const first = root.body[0];
    expect(first?.type === "ExpressionStatement" ? nameOf(first.expression) : null).toBe("renamed");
  });

  test("skip suppresses children, stop halts the walk", () => {
    const seen: string[] = [];
    walk(program(`f(deep); after;`), {
      CallExpression: (_n, ctx) => ctx.skip(),
      Identifier(node, ctx) {
        seen.push(node.name);
        if (node.name === "after") ctx.stop();
      },
    });
    expect(seen).toEqual(["after"]);
  });

  test("insertAfter is visited, insertBefore is not", () => {
    const root = program(`a;`);
    const seen: string[] = [];
    walk(root, {
      ExpressionStatement(node, ctx) {
        const name = nameOf(node.expression);
        if (name === null) return;
        seen.push(name);
        if (name === "a") {
          ctx.insertBefore(b.ExpressionStatement({ expression: b.Identifier({ name: "before" }) }));
          ctx.insertAfter(b.ExpressionStatement({ expression: b.Identifier({ name: "after" }) }));
        }
      },
    });
    expect(seen).toEqual(["a", "after"]);
    expect(root.body).toHaveLength(3);
  });

  test("walkAsync awaits handlers and keeps the sync order", async () => {
    const syncTrace: string[] = [];
    walk(program(`f(x);`), {
      enter: (node) => syncTrace.push(node.type),
    });
    const asyncTrace: string[] = [];
    await walkAsync(program(`f(x);`), {
      async enter(node) {
        await new Promise((resolve) => setTimeout(resolve, 0));
        asyncTrace.push(node.type);
      },
    });
    expect(asyncTrace).toEqual(syncTrace);
  });
});

describe("aliases", () => {
  test("alias handlers fire for every member type", () => {
    const seen: string[] = [];
    walk(program(`function f() {} const g = () => 1; class C {}`), {
      Function: (node) => seen.push(node.type),
      Class: (node) => seen.push(node.type),
    });
    expect(seen).toEqual(["FunctionDeclaration", "ArrowFunctionExpression", "ClassDeclaration"]);
  });

  test("aliases enter before the typed handler and leave after it", () => {
    const trace: string[] = [];
    walk(program(`function f() {}`), {
      Function: {
        enter: () => trace.push("alias-enter"),
        leave: () => trace.push("alias-leave"),
      },
      FunctionDeclaration: {
        enter: () => trace.push("typed-enter"),
        leave: () => trace.push("typed-leave"),
      },
    });
    expect(trace).toEqual(["alias-enter", "typed-enter", "typed-leave", "alias-leave"]);
  });

  test("stop inside an alias handler halts everything", () => {
    const seen: string[] = [];
    walk(program(`let a; let b;`), {
      Statement(node, ctx) {
        seen.push(node.type);
        ctx.stop();
      },
      VariableDeclaration: () => seen.push("typed"),
    });
    expect(seen).toEqual(["VariableDeclaration"]);
  });

  test("TSType alias visits type nodes", () => {
    const seen: string[] = [];
    walk(program(`let x: string | number;`), {
      TSType: (node) => seen.push(node.type),
    });
    expect(seen).toEqual(["TSUnionType", "TSStringKeyword", "TSNumberKeyword"]);
  });
});
