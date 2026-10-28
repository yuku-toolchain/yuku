import { describe, expect, test } from "bun:test";
import { b, parse, print, walk, type Expression, type Node, type Visitors } from "yuku";
import { nodesOf } from "../ast-helpers-for-test";

function nameOf(node: Node): string | null {
  return node.type === "Identifier" ? node.name : null;
}

function id(name: string) {
  return b.Identifier({ name });
}

function statement(expression: Expression) {
  return b.ExpressionStatement({ expression });
}

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

  test("remove splices, replace continues into the replacement", () => {
    const root = program(`a; debugger; b;`);
    walk(root, {
      DebuggerStatement: (_n, ctx) => ctx.remove(),
      Identifier(node, ctx) {
        if (node.name === "a") {
          ctx.replace(id("renamed"));
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
          ctx.insertBefore(statement(id("before")));
          ctx.insertAfter(statement(id("after")));
        }
      },
    });
    expect(seen).toEqual(["a", "after"]);
    expect(root.body).toHaveLength(3);
  });

  test("context reports the position and ancestors of the node", () => {
    const positions: string[] = [];
    walk(program(`f(a, b);`), {
      Identifier(node, ctx) {
        const path = ctx.ancestors().map((ancestor) => ancestor.type).join(">");
        positions.push(`${node.name} ${ctx.key}[${ctx.index}] in ${ctx.parent?.type} ${path}`);
      },
    });
    expect(positions).toEqual([
      "f callee[null] in CallExpression Program>ExpressionStatement>CallExpression",
      "a arguments[0] in CallExpression Program>ExpressionStatement>CallExpression",
      "b arguments[1] in CallExpression Program>ExpressionStatement>CallExpression",
    ]);
  });

  test("the root has no place to replace or remove", () => {
    expect(() => walk(program(`a;`), { Program: (_n, ctx) => ctx.remove() })).toThrow(
      "the walk root",
    );
    expect(() => walk(program(`a;`), { Program: (_n, ctx) => ctx.replace(id("b")) })).toThrow(
      "the walk root",
    );
  });

  test("edits that have no place to go throw, and a removed plain field is null", () => {
    const call = (source: string) => program(source).body[0]!;
    const insert: Visitors = { Identifier: (_n, ctx) => ctx.insertBefore(id("b")) };
    expect(() => walk(call(`f(a);`), insert)).toThrow(
      "insertBefore: the node is not in an array field",
    );
    const two = [id("c"), id("d")];
    expect(() => walk(call(`a.b;`), { Identifier: (_n, ctx) => ctx.replace(two) })).toThrow(
      "replace: several nodes need an array field",
    );
    expect(() => walk(call(`f(a);`), { Identifier: (_n, ctx) => ctx.replace([]) })).toThrow(
      "replace: expected a node, or call remove",
    );
    const empty = {} as never;
    expect(() => walk(call(`f(a);`), { Identifier: (_n, ctx) => ctx.replace(empty) })).toThrow(
      "replace: expected an AST node",
    );
    const statement = call(`if (a) b(); else c();`);
    walk(statement, {
      ExpressionStatement(node, ctx) {
        if (print(node).code === "c();") ctx.remove();
      },
    });
    expect(statement.type === "IfStatement" && statement.alternate).toBeNull();
  });

  test("a visitor that is not handlers, or a root that is not a node, throws", () => {
    expect(() => walk(program(`a;`), null as never)).toThrow("visitors must be an object");
    expect(() => walk(program(`a;`), { enter: {} } as never)).toThrow("must be a function");
    expect(() => walk(program(`a;`), { Identifier: 1 } as never)).toThrow(
      "a function or { enter, leave }",
    );
    expect(() => walk({} as never, {})).toThrow("walk: expected an AST node");
  });

  test("a misspelled visitor key throws", () => {
    expect(() => walk(program(`a;`), { Identifer() {} } as Visitors)).toThrow(
      "'Identifer' is not a node type or alias",
    );
  });

  test("walks trees far deeper than the call stack", () => {
    const links = 100_000;
    const source = "x = " + Array.from({ length: links }, (_, i) => `a${i}`).join(" + ") + ";";
    let identifiers = 0;
    walk(program(source), {
      Identifier() {
        identifiers++;
      },
    });
    expect(identifiers).toBe(links + 1);
  });

  test("edits apply the same deeper than the walk recurses", () => {
    const terms = 3_000;
    const chain = Array.from({ length: terms }, (_, i) => `a${i}`).join(" + ");
    const root = program(`x = ${chain};`);
    let removed = 0;
    walk(root, {
      Identifier(node, ctx) {
        if (node.name.startsWith("a")) ctx.replace(id(`b${node.name}`));
      },
      BinaryExpression(node, ctx) {
        if (node.right.type === "Identifier" && node.right.name === "a1500") {
          ctx.replace(node.left);
          removed++;
        }
      },
    });
    const names = nodesOf(root, "Identifier").map((node) => node.name);
    expect(removed).toBe(1);
    expect(names).toHaveLength(terms);
    expect(names.filter((name) => name.startsWith("a"))).toEqual([]);
  });
});

describe("visitors", () => {
  test("a list merges into one pass, entering in order and leaving in reverse", () => {
    const trace: string[] = [];
    const visitor = (name: string): Visitors => ({
      Identifier: {
        enter: () => trace.push(`${name} enter`),
        leave: () => trace.push(`${name} leave`),
      },
    });
    walk(program(`a;`), [visitor("first"), visitor("second")]);
    expect(trace).toEqual(["first enter", "second enter", "second leave", "first leave"]);
  });

  test("every visitor sees a replacement from its start", () => {
    const seen: string[] = [];
    const root = program(`old;`);
    walk(root, [
      {
        Identifier(node, ctx) {
          seen.push(`first ${node.name}`);
          if (node.name === "old") ctx.replace(id("fresh"));
        },
      },
      { Identifier: (node) => seen.push(`second ${node.name}`) },
    ]);
    expect(seen).toEqual(["first old", "first fresh", "second fresh"]);
  });

  test("skip after replace leaves the replacement unvisited", () => {
    const root = program(`a;`);
    let calls = 0;
    walk(root, {
      Identifier(node, ctx) {
        calls++;
        ctx.replace(b.CallExpression({ callee: id("wrap"), arguments: [node] }));
        ctx.skip();
      },
    });
    expect(calls).toBe(1);
    expect(print(root).code).toBe("wrap(a);");
  });

  test("a visitor that keeps replacing its own result stops with an error", () => {
    expect(() =>
      walk(program(`a;`), {
        Identifier(node, ctx) {
          ctx.replace(id(node.name + "a"));
        },
      }),
    ).toThrow("replaced 64 times in one place");
  });

  test("a list replaces a node in an array field, and each new node is visited", () => {
    const root = program(`a; z;`);
    const seen: string[] = [];
    walk(root, {
      ExpressionStatement(node, ctx) {
        const name = nameOf(node.expression);
        seen.push(name ?? "?");
        if (name === "a") ctx.replace([statement(id("b")), statement(id("c"))]);
      },
    });
    expect(seen).toEqual(["a", "b", "c", "z"]);
    expect(print(root).code).toBe("b;\nc;\nz;");
  });

  test("a replaced node's comments move to its replacement", () => {
    const file = parse(`// keep\nold();`, { attachComments: true });
    walk(file.program, {
      ExpressionStatement(node, ctx) {
        if (node.expression.type === "CallExpression" && nameOf(node.expression.callee) === "old") {
          ctx.replace(statement(b.CallExpression({ callee: id("fresh") })));
        }
      },
    });
    expect(print(file).code).toBe("// keep\nfresh();");
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
