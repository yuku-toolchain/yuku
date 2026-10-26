import { describe, expect, test } from "bun:test";
import {
  js,
  print,
  ts,
  type AssignmentExpression,
  type ClassExpression,
  type Identifier,
  type JSXElement,
  type ObjectExpression,
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
    const callee = js`log`;
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
    const spread = js<ObjectExpression>`({ ...rest })`.properties[0]!;
    expect(code(js`({ ${"a"}, ${spread} })`)).toBe(
      "{ a, ...rest }",
    );
    const string = ts<TSTypeAliasDeclaration>`type S = string;`.typeAnnotation;
    expect(code(ts`let x: ${string};`)).toBe("let x: string;");
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

  test("a hole in a name that holds more renames it, keeping the rest", () => {
    expect(code(ts`function f(${"a"}: ${"T"}, ${js`b`}?: number) {}`)).toBe(
      "function f(a: T, b?: number) {}",
    );
    expect(code(ts`let ${"x"}: number;`)).toBe("let x: number;");
    expect(() => ts`function f(${js<AssignmentExpression>`[a] = 0`.left}: T) {}`).toThrow(
      "a ArrayPattern does not fit a typed name",
    );
  });

  test("a JSX tag closes with its own hole, and a decorated name keeps its decorators", () => {
    const panel = js`<${"Panel"} open>{${js`body`}}</${"Panel"}>`;
    expect(code(panel)).toBe("<Panel open>{body}</Panel>");
    const method = ts`class A { m(@inject ${"service"}: Service) {} }`;
    expect(code(method)).toContain("@inject");
    expect(code(method)).toContain("service: Service");
  });

  test("a binding pattern fills a declarator, and a JSX name may hold dashes", () => {
    const pattern = js<AssignmentExpression>`({ a, b } = 0)`.left;
    expect(code(js`for (const ${pattern} of items) {}`)).toBe("for (const { a, b } of items) {}");
    expect(code(js`<${"my-element"} ${"aria-hidden"} />`)).toBe("<my-element aria-hidden />");
    expect(() => js`f(${"a-b"})`).toThrow('takes a name as a string, not "a-b"');
  });

  test("a node goes in once, and a copy of it everywhere else", () => {
    const name = js<Identifier>`x`;
    const sum = js`${name} + ${name}`;
    expect(sum.type === "BinaryExpression" && sum.left).toBe(name);
    expect(sum.type === "BinaryExpression" && sum.right).not.toBe(name);
    expect(sum.type === "BinaryExpression" && sum.right).toEqual(name);
    const shorthand = js`({ ${name} })`;
    expect(code(shorthand)).toBe("{ x }");
  });

  test("the comments around a hole move to the nodes that fill it", () => {
    const body = js`function f() {
      // first
      ${[js`a();`, js`b();`]} // last
    }`;
    expect(code(body)).toBe("function f() {\n  // first\n  a();\n  b(); // last\n}");
  });

  test("a hole inside quotes fills the text of a string", () => {
    const spec = `./a b's "c"`;
    expect(code(js`import x from "${spec}";`)).toBe(`import x from "./a b's \\"c\\"";`);
    expect(code(js`fetch("/api/${"users"}/${42}")`)).toBe('fetch("/api/users/42")');
    expect(code(js`<a href="${"/x?a=1&b=2"}" />`)).toBe('<a href="/x?a=1&b=2" />');
    expect(code(js`\`hi ${"`${x}` \\"}!\``)).toBe("`hi \\`\\${x}\\` \\\\!`");
    expect(() => js`f("${js`x`}")`).toThrow("a hole inside quotes takes a string or a number");
    expect(() => js`"use ${"strict"}";`).toThrow("hole 0 of a template is in text");
  });

  test("code that needs a generator builds as it would in one", () => {
    expect(js`yield ${"x"}`.type).toBe("YieldExpression");
    expect(js`yield`.type).toBe("YieldExpression");
    expect(js`await ${"x"}`.type).toBe("AwaitExpression");
    expect(js`import x from "m";`.type).toBe("ImportDeclaration");
  });

  test("a node that only exists inside another comes from a template of its container", () => {
    const property = js<ObjectExpression>`({ ${"key"}: ${1} })`.properties[0]!;
    expect(code(js`({ ${[property, property]} })`)).toBe("{ key: 1, key: 1 }");
    const [kase] = js<SwitchStatement>`switch (x) { case ${1}: a(); }`.cases;
    expect(kase!.type).toBe("SwitchCase");
    expect(ts<TSTypeAliasDeclaration>`type _ = Array<${"T"}>;`.typeAnnotation.type).toBe(
      "TSTypeReference",
    );
  });

  test("the comments before an expression stay with it", () => {
    expect(code(js`/* @__PURE__ */ ${js`make()`}`)).toBe("/* @__PURE__ */ make()");
  });

  test("a node that does not fit the hole throws", () => {
    expect(() => js`let ${js`f()`};`).toThrow("a CallExpression does not fit a VariableDeclarator");
    expect(() => js`({ ${js`f()`} })`).toThrow("a CallExpression does not fit a Property");
    const [kase] = js<SwitchStatement>`switch (x) { case 1: }`.cases;
    expect(() => js`${kase};`).toThrow("a SwitchCase does not fit a statement");
    expect(() => js`<a ${js`f()`} />`).toThrow("a CallExpression does not fit a JSXAttribute");
  });

  test("a string literal goes in as a node", () => {
    expect(code(js`f(${js`"a b"`})`)).toBe('f("a b")');
    expect(() => js`f(${"a b"})`).toThrow('takes a name as a string, not "a b"');
  });

  test("a nested template literal keeps its backticks and interpolations", () => {
    const name = js`name`;
    expect(code(js`\`hi \${${name}}!\``)).toBe("`hi ${name}!`");
    expect(code(js`f(\`a\`, "\n")`)).toBe('f(`a`, "\\n")');
    expect(code(js`\`a\\\`b\``)).toBe("`a\\`b`");
    expect(() => js`\`hi ${name}\``).toThrow("a hole inside quotes takes a string or a number");
  });

  test("malformed templates throw", () => {
    expect(() => js`/${"x"}/`).toThrow("hole 0 of a template is in text");
    expect(() => js`a; b;`).toThrow("one expression or statement, not 2");
    expect(() => js`let = ;`).toThrow(SyntaxError);
    expect(() => js`f(...${"a"})`).not.toThrow();
    expect(() => js`a + ${[js`b`]}`).toThrow("a list fills a template hole in a list");
    expect(() => js`f(${{} as never})`).toThrow("template: expected an AST node");
    const deep = "[".repeat(600) + "]".repeat(600);
    const strings = Object.assign([deep], { raw: [deep] }) as unknown as TemplateStringsArray;
    expect(() => js(strings)).toThrow("a template nests deeper than 1000 levels");
    expect(() => ts`let x: ${js`f()`};`).toThrow("a CallExpression does not fit a type");
  });
});
