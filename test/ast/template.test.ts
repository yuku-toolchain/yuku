import { describe, expect, test } from "bun:test";
import {
  b,
  js,
  print,
  ts,
  type ClassExpression,
  type JSXElement,
  type Node,
  type SwitchStatement,
  type TSEnumDeclaration,
  type TSTypeAliasDeclaration,
  type VariableDeclaration,
} from "yuku";

function code(node: Node): string {
  return print(node).code;
}

describe("templates", () => {
  test("an expression without `;`, a statement with it", () => {
    expect(js`a + b`.type).toBe("BinaryExpression");
    expect(js`a + b;`.type).toBe("ExpressionStatement");
    expect(js`return x;`.type).toBe("ReturnStatement");
    expect(js`<a />`.type).toBe("JSXElement");
  });

  test("built nodes are detached and fresh on every call", () => {
    const make = () => js`f(1)`;
    const first = make();
    expect(first).toMatchObject({ start: 0, end: 0 });
    expect(make()).not.toBe(first);
    expect(make()).toEqual(first);
  });

  test("holes take nodes, names, literals, and lists", () => {
    const callee = b.Identifier({ name: "log" });
    expect(code(js`${callee}(${"value"}, ${1}, ${true}, ${null}, ${10n})`)).toBe(
      "log(value, 1, true, null, 10n)",
    );
    expect(code(js`f(first, ${[js`a`, 2]}, last)`)).toBe("f(first, a, 2, last)");
    expect(code(js`let ${"x"} = ${undefined};`)).toBe("let x = undefined;");
  });

  test("a whole statement, a shorthand property, or a type takes a node of its kind", () => {
    expect(code(js`function f() { ${js`return 1;`} }`)).toBe("function f() {\n  return 1;\n}");
    expect(code(js`function f() { ${[js`a();`, js`b()`]} }`)).toBe(
      "function f() {\n  a();\n  b();\n}",
    );
    expect(code(js`({ ${"a"}, ${b.SpreadElement({ argument: js`rest` })} })`)).toBe(
      "{ a, ...rest }",
    );
    expect(code(ts`let x: ${b.TSStringKeyword({})};`)).toBe("let x: string;");
    expect(code(ts`let x: ${"T"}<U>;`)).toBe("let x: T<U>;");
    expect(code(js`<${"div"} />`)).toBe("<div />");
  });

  test("a hole takes the whole node it stands for, or a name inside it", () => {
    const declarators = js<VariableDeclaration>`let a = 1, b;`.declarations;
    expect(code(js`var ${declarators};`)).toBe("var a = 1, b;");
    expect(code(js`let ${"x"};`)).toBe("let x;");
    const classMembers = js<ClassExpression>`(class { m() {} })`.body.body;
    expect(code(js`class A { ${classMembers} }`)).toBe("class A {\n  m() {}\n}");
    const attributes = js<JSXElement>`<b c="d" {...e} />`.openingElement.attributes;
    expect(code(js`<a ${attributes} />`)).toBe(
      '<a c="d" {...e} />',
    );
    expect(code(js`<a ${"disabled"} />`)).toBe("<a disabled />");
    expect(code(js`import { ${"a"} } from "m";`)).toBe('import { a } from "m";');
    const enumMembers = ts<TSEnumDeclaration>`enum F { B = 1 }`.body.members;
    expect(code(ts`enum E { ${"A"}, ${enumMembers} }`)).toBe(
      "enum E {\n  A,\n  B = 1\n}",
    );
    expect(code(ts`class A implements ${"I"} {}`)).toBe("class A implements I {}");
    const union = ts<TSTypeAliasDeclaration>`type Y = A | B;`.typeAnnotation;
    expect(code(ts`let x: ${union};`)).toBe("let x: A | B;");
  });

  test("a node that does not fit the hole throws", () => {
    expect(() => js`let ${js`f()`};`).toThrow("a CallExpression does not fit a VariableDeclarator");
    expect(() => js`({ ${js`f()`} })`).toThrow("a CallExpression does not fit a Property");
    const [kase] = js<SwitchStatement>`switch (x) { case 1: }`.cases;
    expect(() => js`${kase};`).toThrow("a SwitchCase does not fit a statement");
    expect(() => js`<a ${js`f()`} />`).toThrow("a CallExpression does not fit a JSXAttribute");
  });

  test("a string literal goes in as a node", () => {
    expect(code(js`f(${b.Literal({ value: "a b" })})`)).toBe('f("a b")');
    expect(() => js`f(${"a b"})`).toThrow('takes a name as a string, not "a b"');
  });

  test("a nested template literal keeps its backticks and interpolations", () => {
    const name = b.Identifier({ name: "name" });
    expect(code(js`\`hi \${${name}}!\``)).toBe("`hi ${name}!`");
    expect(code(js`f(\`a\`, "\n")`)).toBe('f(`a`, "\\n")');
    expect(code(js`\`a\\\`b\``)).toBe("`a\\`b`");
    expect(() => js`\`hi ${name}\``).toThrow("hole 0 of a template is in text");
  });

  test("malformed templates throw", () => {
    expect(() => js`"${"x"}"`).toThrow("hole 0 of a template is in text");
    expect(() => js`a; b;`).toThrow("one expression or statement, not 2");
    expect(() => js`let = ;`).toThrow(SyntaxError);
    expect(() => js`f(...${"a"})`).not.toThrow();
    expect(() => js`a + ${[js`b`]}`).toThrow("a list fills a template hole in a list");
    expect(() => ts`let x: ${js`f()`};`).toThrow("a CallExpression does not fit a type");
  });
});
