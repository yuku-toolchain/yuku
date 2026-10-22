import { describe, expect, test } from "bun:test";
import {
  b,
  bindingIdentifiers,
  CHILD_KEYS,
  clone,
  is,
  lexical,
  nameOf,
  parse,
  unwrap,
  type Expression,
} from "yuku";
import { nodesOf } from "../ast-helpers-for-test";

function program(source: string) {
  return parse(source, { sourceType: "module", lang: "ts" }).program;
}

function firstExpression(source: string): Expression {
  const statement = program(source).body[0];
  if (statement?.type !== "ExpressionStatement") {
    throw new Error("expected an expression statement");
  }
  return statement.expression;
}

describe("is", () => {
  test("concrete, alias, and shape guards narrow", () => {
    const call = firstExpression(`f(1)`);
    expect(is.CallExpression(call)).toBe(true);
    expect(is.Expression(call)).toBe(true);
    expect(is.Statement(call)).toBe(false);
    expect(is.oneOf(call, ["CallExpression", "NewExpression"])).toBe(true);
    expect(is.oneOf(call, ["NewExpression"])).toBe(false);
    expect(is.oneOf(null, ["CallExpression"])).toBe(false);
    expect(is.CallExpression(null)).toBe(false);

    expect(is.Identifier(firstExpression(`foo`))).toBe(true);
    expect(is.Identifier(firstExpression(`"foo"`))).toBe(false);
    expect(is.StringLiteral(firstExpression(`"s"`))).toBe(true);
    expect(is.NumericLiteral(firstExpression(`1`))).toBe(true);
    expect(is.NullLiteral(firstExpression(`null`))).toBe(true);
    expect(is.RegExpLiteral(firstExpression(`/x/`))).toBe(true);
    expect(is.BigIntLiteral(firstExpression(`1n`))).toBe(true);

    expect(is.StaticMemberExpression(firstExpression(`a.b`))).toBe(true);
    expect(is.ComputedMemberExpression(firstExpression(`a[b]`))).toBe(true);
    expect(is.Directive(program(`"use strict";`).body[0])).toBe(true);
  });
});

describe("lexical", () => {
  test("names, keywords, and binding validity", () => {
    expect(lexical.isIdentifierName("foo")).toBe(true);
    expect(lexical.isIdentifierName("π")).toBe(true);
    expect(lexical.isIdentifierName("class")).toBe(true);
    expect(lexical.isIdentifierName("1foo")).toBe(false);
    expect(lexical.isIdentifierName("")).toBe(false);
    expect(lexical.isValidIdentifier("foo")).toBe(true);
    expect(lexical.isValidIdentifier("class")).toBe(false);
    expect(lexical.isValidIdentifier("await")).toBe(false);
    expect(lexical.isValidIdentifier("eval")).toBe(true);
    expect(lexical.isEvalOrArguments("arguments")).toBe(true);
    expect(lexical.isEvalOrArguments("argument")).toBe(false);
  });

  // the parser's Unicode tables, on any runtime
  test("code points", () => {
    expect(lexical.isIdentifierStart(0x24)).toBe(true);
    expect(lexical.isIdentifierStart(0x10940)).toBe(true);
    expect(lexical.isIdentifierStart(0x200c)).toBe(false);
    expect(lexical.isIdentifierChar(0x200c)).toBe(true);
    expect(lexical.isIdentifierStart(0x110000)).toBe(false);
    expect(lexical.isIdentifierStart(-1)).toBe(false);
    expect(lexical.isIdentifierName("a\u{10940}")).toBe(true);
    expect(lexical.isIdentifierName("\ud800")).toBe(false);
  });
});

describe("utils", () => {
  test("CHILD_KEYS lists the child fields of each type in source order", () => {
    expect(CHILD_KEYS.CallExpression).toEqual(["callee", "typeArguments", "arguments"]);
    expect(CHILD_KEYS.Literal).toEqual([]);
    expect(CHILD_KEYS.NotANode).toBeUndefined();
  });

  test("nameOf reads identifiers and string literals", () => {
    expect(nameOf(firstExpression(`foo`))).toBe("foo");
    expect(nameOf(firstExpression(`"bar"`))).toBe("bar");
    expect(nameOf(firstExpression(`1`))).toBe(null);
    expect(nameOf(null)).toBe(null);
  });

  test("a literal's value is its runtime value", () => {
    const valueOf = (source: string) => {
      const node = firstExpression(source);
      return node.type === "Literal" ? node.value : undefined;
    };
    expect(valueOf(`"s"`)).toBe("s");
    expect(valueOf(`2`)).toBe(2);
    expect(valueOf(`true`)).toBe(true);
    expect(valueOf(`10n`)).toBe(10n);
    expect(valueOf(`null`)).toBe(null);
    expect(valueOf(`/x/g`)).toEqual(/x/g);
  });

  test("unwrap strips parens and TS assertion wrappers", () => {
    const inner = unwrap(firstExpression(`((x as any))!`));
    expect(nameOf(inner)).toBe("x");
  });

  test("unwrap takes any node and returns what nothing wraps as it is", () => {
    const root = program(`const a = (b as any), c; f(...d);`);
    const [first, second] = nodesOf(root, "VariableDeclarator");
    const [call] = nodesOf(root, "CallExpression");
    expect(nameOf(unwrap(first!.init))).toBe("b");
    expect(unwrap(second!.init)).toBeNull();
    expect(unwrap(call!.arguments[0])?.type).toBe("SpreadElement");
    expect(unwrap(call!.arguments[1])).toBeUndefined();
  });

  test("is.Wrapper matches every form unwrap strips, and narrows to it", () => {
    expect(is.Wrapper(firstExpression(`(x)`))).toBe(true);
    expect(is.Wrapper(firstExpression(`x as any`))).toBe(true);
    expect(is.Wrapper(firstExpression(`x satisfies any`))).toBe(true);
    expect(is.Wrapper(firstExpression(`x!`))).toBe(true);
    expect(is.Wrapper(firstExpression(`<any>x`))).toBe(true);

    expect(is.Wrapper(firstExpression(`x`))).toBe(false);
    expect(is.Wrapper(firstExpression(`f<string>`))).toBe(false);
    expect(is.Wrapper(program(`x;`))).toBe(false);
    expect(is.Wrapper(null)).toBe(false);

    const node = firstExpression(`(x as any)!`);
    expect(is.Wrapper(node) && nameOf(unwrap(node.expression))).toBe("x");
  });

  test("bindingIdentifiers collects destructuring leaves in order", () => {
    const root = program(`const { a, b: [c = 1, ...d], ...e } = obj;`);
    const [declarator] = nodesOf(root, "VariableDeclarator");
    expect(bindingIdentifiers(declarator!.id).map((id) => id.name)).toEqual(["a", "c", "d", "e"]);
  });

  test("builders default spans to 0 and fields to the empty value their type allows", () => {
    const node = b.CallExpression({ callee: b.Identifier({ name: "f" }) });
    expect(node).toEqual({
      type: "CallExpression",
      start: 0,
      end: 0,
      callee: { type: "Identifier", start: 0, end: 0, name: "f" },
      arguments: [],
      optional: false,
    });
    expect(nameOf(b.Identifier({ name: "x", start: 3, end: 4 }))).toBe("x");
    expect(b.UnaryExpression({ operator: "!", argument: node }).prefix).toBe(true);
    expect(is.NullLiteral(b.Literal({ value: null }))).toBe(true);
  });

  test("built nodes never share a list", () => {
    const first = b.ArrayExpression({});
    const second = b.ArrayExpression({});
    first.elements.push(b.Identifier({ name: "a" }));
    expect(second.elements).toEqual([]);
  });

  test("clone copies a tree, sharing no node or list, comments included", () => {
    const root = program(`// lead\nf(a, [b]);`);
    const copy = clone(root);
    expect(copy).toEqual(root);
    expect(copy.body[0]).not.toBe(root.body[0]);
    expect(copy.body[0]!.comments).toEqual(root.body[0]!.comments);
    const [call] = nodesOf(copy, "CallExpression");
    expect(call!.arguments).not.toBe(nodesOf(root, "CallExpression")[0]!.arguments);
    expect(() => clone(null as never)).toThrow("expected an AST node");
  });
});
