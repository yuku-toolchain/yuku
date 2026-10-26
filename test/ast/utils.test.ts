import { describe, expect, test } from "bun:test";
import { CHILD_KEYS, parse, type Expression, type Node } from "yuku";
import {
  is,
  isEvalOrArguments,
  isIdentifierChar,
  isIdentifierName,
  isIdentifierStart,
  isValidIdentifier,
  unwrap,
} from "yuku/utils";
import { nodesOf } from "../ast-helpers-for-test";

function nameOf(node: Node | null | undefined): string | null {
  return node?.type === "Identifier" ? node.name : null;
}

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

describe("names", () => {
  test("names, keywords, and binding validity", () => {
    expect(isIdentifierName("foo")).toBe(true);
    expect(isIdentifierName("π")).toBe(true);
    expect(isIdentifierName("class")).toBe(true);
    expect(isIdentifierName("1foo")).toBe(false);
    expect(isIdentifierName("")).toBe(false);
    expect(isValidIdentifier("foo")).toBe(true);
    expect(isValidIdentifier("class")).toBe(false);
    expect(isValidIdentifier("await")).toBe(false);
    expect(isValidIdentifier("eval")).toBe(true);
    expect(isEvalOrArguments("arguments")).toBe(true);
    expect(isEvalOrArguments("argument")).toBe(false);
  });

  // the parser's Unicode tables, on any runtime
  test("code points", () => {
    expect(isIdentifierStart(0x24)).toBe(true);
    expect(isIdentifierStart(0x10940)).toBe(true);
    expect(isIdentifierStart(0x200c)).toBe(false);
    expect(isIdentifierChar(0x200c)).toBe(true);
    expect(isIdentifierStart(0x110000)).toBe(false);
    expect(isIdentifierStart(-1)).toBe(false);
    expect(isIdentifierName("a\u{10940}")).toBe(true);
    expect(isIdentifierName("\ud800")).toBe(false);
  });
});

describe("utils", () => {
  test("CHILD_KEYS lists the child fields of each type in source order", () => {
    expect(CHILD_KEYS.CallExpression).toEqual(["callee", "typeArguments", "arguments"]);
    expect(CHILD_KEYS.Literal).toEqual([]);
    expect(CHILD_KEYS.NotANode).toBeUndefined();
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
});
