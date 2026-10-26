import { describe, expect, test } from "bun:test";
import { b, parse, type ExpressionStatement, type Node } from "yuku";

function expressionOf(source: string): Node {
  return (parse(source).program.body[0] as ExpressionStatement).expression;
}

function shapeOf(node: Node): unknown {
  return JSON.parse(
    JSON.stringify(node, (key, value) => (key === "start" || key === "end" ? undefined : value)),
  );
}

describe("b", () => {
  test("defaults spans and each field that may be false, null, or empty", () => {
    expect(b.CallExpression({ callee: b.Identifier({ name: "f" }) })).toEqual({
      type: "CallExpression",
      start: 0,
      end: 0,
      arguments: [],
      optional: false,
      callee: { type: "Identifier", start: 0, end: 0, name: "f" },
    });
    expect(b.Identifier({ name: "a", start: 1, end: 2 })).toMatchObject({ start: 1, end: 2 });
  });

  test("a given field wins over its default", () => {
    const callee = b.Identifier({ name: "f" });
    expect(b.CallExpression({ callee, optional: true }).optional).toBe(true);
    expect(b.UnaryExpression({ operator: "!", argument: callee }).prefix).toBe(true);
    expect(b.UpdateExpression({ operator: "++", argument: callee }).prefix).toBe(false);
  });

  test("each node gets its own arrays", () => {
    const first = b.ArrayExpression({});
    first.elements.push(b.Identifier({ name: "a" }));
    expect(b.ArrayExpression({}).elements).toEqual([]);
  });

  test("a built node has the shape the parser gives", () => {
    const built = b.CallExpression({
      callee: b.MemberExpression({
        object: b.Identifier({ name: "Math" }),
        property: b.Identifier({ name: "pow" }),
        computed: false,
      }),
      arguments: [b.Identifier({ name: "x" }), b.Literal({ value: 2, raw: "2" })],
    });
    expect(shapeOf(built)).toEqual(shapeOf(expressionOf(`Math.pow(x, 2)`)));
  });

  test("a field that tells the shapes of a type apart has no default", () => {
    const object = b.Identifier({ name: "a" });
    const property = b.Identifier({ name: "b" });
    // @ts-expect-error computed decides between a static and a computed member
    b.MemberExpression({ object, property });
    expect(b.MemberExpression({ object, property, computed: false }).optional).toBe(false);
  });
});
