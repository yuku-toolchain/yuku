import { describe, expect, test } from "bun:test";
import { parse, type Expression } from "yuku-parser";
import {
  b,
  bindingIdentifiers,
  collectExportDeclaration,
  collectExports,
  collectImportDeclaration,
  collectImports,
  findAll,
  is,
  isCallOf,
  isIdentifierName,
  isValidIdentifier,
  literalValue,
  nameOf,
  unwrap,
} from "yuku-ast";

function program(source: string) {
  return parse(source, { sourceType: "module", lang: "ts" }).program;
}

function firstExpression(source: string): Expression {
  const statement = program(source).body[0];
  if (statement?.type !== "ExpressionStatement") throw new Error("expected an expression statement");
  return statement.expression;
}

describe("is", () => {
  test("concrete, alias, and shape guards narrow", () => {
    const call = firstExpression(`f(1)`);
    expect(is.CallExpression(call)).toBe(true);
    expect(is.Expression(call)).toBe(true);
    expect(is.Statement(call)).toBe(false);
    expect(is.oneOf(call, ["CallExpression", "NewExpression"])).toBe(true);
    expect(is.CallExpression(null)).toBe(false);

    expect(is.Identifier(firstExpression(`foo`), "foo")).toBe(true);
    expect(is.Identifier(firstExpression(`foo`), "bar")).toBe(false);
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

describe("identifier", () => {
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
    expect(isValidIdentifier("class", false)).toBe(true);
  });
});

describe("utils", () => {
  test("nameOf reads identifiers and string literals", () => {
    expect(nameOf(firstExpression(`foo`))).toBe("foo");
    expect(nameOf(firstExpression(`"bar"`))).toBe("bar");
    expect(nameOf(firstExpression(`1`))).toBe(null);
    expect(nameOf(null)).toBe(null);
  });

  test("literalValue extracts every literal kind", () => {
    expect(literalValue(firstExpression(`"s"`))).toBe("s");
    expect(literalValue(firstExpression(`2`))).toBe(2);
    expect(literalValue(firstExpression(`true`))).toBe(true);
    expect(literalValue(firstExpression(`10n`))).toBe(10n);
    expect(literalValue(firstExpression(`null`))).toBe(null);
    expect(literalValue(firstExpression(`foo`))).toBe(undefined);
  });

  test("unwrap strips parens and TS assertion wrappers", () => {
    const inner = unwrap(firstExpression(`((x as any))!`));
    expect(is.Identifier(inner, "x")).toBe(true);
  });

  test("isCallOf matches the unwrapped callee", () => {
    expect(isCallOf(firstExpression(`require("m")`), "require")).toBe(true);
    expect(isCallOf(firstExpression(`(require as any)("m")`), "require")).toBe(true);
    expect(isCallOf(firstExpression(`require("m")`), ["define", "require"])).toBe(true);
    expect(isCallOf(firstExpression(`other("m")`), "require")).toBe(false);
    expect(isCallOf(null, "require")).toBe(false);
  });

  test("bindingIdentifiers collects destructuring leaves in order", () => {
    const [declarator] = findAll(program(`const { a, b: [c = 1, ...d], ...e } = obj;`), "VariableDeclarator");
    expect(bindingIdentifiers(declarator!.id).map((id) => id.name)).toEqual(["a", "c", "d", "e"]);
  });

  test("builders derive fields from the node type and default spans", () => {
    const node = b.CallExpression({
      callee: b.Identifier({ name: "f" }),
      arguments: [b.Literal({ value: 1, raw: "1" })],
      optional: false,
    });
    expect(node.start).toBe(0);
    expect(isCallOf(node, "f")).toBe(true);
    expect(nameOf(b.Identifier({ name: "x", start: 3, end: 4 }))).toBe("x");
  });

  test("findAll collects in source order", () => {
    const types = findAll(program(`let a: string; let b: number;`), "TSTypeReference");
    expect(types).toEqual([]);
    const names = findAll(program(`a; b;`), "Identifier").map((n) => n.name);
    expect(names).toEqual(["a", "b"]);
  });
});

describe("modules", () => {
  test("collectImports covers default, named, namespace, type, and phase", () => {
    const records = collectImports(
      program(
        `import def from "a"; import * as ns from "b"; import { x, type Y, z as w } from "c"; import type T from "d"; import "side";`,
      ),
    );
    expect(
      records.map((r) => `${r.local}<-${r.imported}@${r.source}${r.typeOnly ? ":type" : ""}`),
    ).toEqual([
      "def<-default@a",
      "ns<-*@b",
      "x<-x@c",
      "Y<-Y@c:type",
      "w<-z@c",
      "T<-default@d:type",
    ]);
  });

  test("per-declaration helpers compose with a walk", () => {
    const root = program(`import { a } from "m"; export const b = 1;`);
    const imported = root.body.flatMap((node) =>
      node.type === "ImportDeclaration" ? collectImportDeclaration(node) : [],
    );
    const exported = root.body.flatMap((node) =>
      node.type === "ExportNamedDeclaration" ? collectExportDeclaration(node) : [],
    );
    expect(imported.map((r) => r.local)).toEqual(["a"]);
    expect(exported.map((r) => r.exported)).toEqual(["b"]);
  });

  test("collectExports covers declarations, specifiers, re-exports, star, and default", () => {
    const records = collectExports(
      program(
        `export const a = 1, { b } = obj; export function f() {} export { c, d as e } from "m"; export * as ns from "n"; export * from "o"; export default class G {}`,
      ),
    );
    expect(records.map((r) => `${r.exported ?? "*"}${r.source ? "@" + r.source : ""}`)).toEqual([
      "a",
      "b",
      "f",
      "c@m",
      "e@m",
      "ns@n",
      "*@o",
      "default",
    ]);
    expect(records.at(-1)?.local).toBe("G");
  });
});
