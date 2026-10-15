import { expect, test } from "bun:test";
import { parse, type Node, type Program } from "yuku";
import { generate, type Printer, type Printers } from "yuku/codegen";
import { findAll, walk } from "yuku/ast";
import { TraceMap, decodedMappings } from "@jridgewell/trace-mapping";
import { gen } from "./helpers";

test("strip and minify compose in a single call", () => {
  expect(
    gen(`const x: number = 1000000;\nif (true) { obj["foo"] = undefined; }`, {
      strip: true,
      minify: true,
    }),
  ).toMatchInlineSnapshot(`"const x=1e6;if(!0){obj.foo=undefined}"`);
});

test("minify: true forces maximum minification over format and quotes", () => {
  expect(gen(`const s = 'plain';`, { minify: true, format: "pretty", quotes: "preserve" }))
    .toMatchInlineSnapshot(`"const s="plain""`);
});

test("a minify object enables switches individually", () => {
  const source = `const x = 1000000;\nconst s = 'plain';`;
  expect(gen(source, { minify: { syntax: true } })).toMatchInlineSnapshot(`
    "const x = 1e6;
    const s = 'plain';"
  `);
  expect(gen(source, { minify: { whitespace: true, quotes: true } })).toMatchInlineSnapshot(
    `"const x=1000000;const s="plain""`,
  );
});

test("lists longer than a u16 survive the round trip", () => {
  const n = 65_536;
  const lengths = ({ body }: Program) => {
    const last = body.at(-1);
    if (last?.type !== "ExpressionStatement" || last.expression.type !== "ArrayExpression") {
      throw new Error("expected a trailing array");
    }
    return [body.length, last.expression.elements.length];
  };
  const { program } = parse(`${"x;".repeat(n)}[${"0,".repeat(n)}];`);
  expect(lengths(program)).toEqual([n + 1, n]);
  expect(lengths(parse(generate(program).code).program)).toEqual([n + 1, n]);
});

test("a compact source map starts each mapping on its token", () => {
  for (const [source, token] of [
    ["function f() { return [1]; }", "["],
    ["x = a - -b;", "-b"],
  ] as const) {
    const options = { format: "compact", sourceMap: { source } } as const;
    const { code, map } = generate(parse(source).program, options);
    const segments = decodedMappings(new TraceMap(JSON.stringify(map)))[0] ?? [];
    const segment = segments.find(([column]) => column === code.indexOf(token));
    expect(segment?.[3], source).toBe(source.indexOf(token));
  }
});

test("a node printed as other text maps to its own column", () => {
  for (const [source, printed, original, minify] of [
    ['<a b="c" />;', '"c"', '"c"', false],
    ['x = a["bc"];', "bc", '"bc"', true],
    ['x = { ["bc"]: 1 };', "bc", '"bc"', true],
  ] as const) {
    const { program } = parse(source, { lang: "jsx" });
    const { code, map } = generate(program, { minify, sourceMap: { source } });
    const segments = decodedMappings(new TraceMap(JSON.stringify(map)))[0] ?? [];
    const segment = segments.find(([column]) => column === code.indexOf(printed));
    expect(segment?.[3], source).toBe(source.indexOf(original));
  }
});

test("the line after a hashbang maps to its own line", () => {
  const source = "#!/usr/bin/env node\nx;\n";
  const { code, map } = generate(parse(source).program, { sourceMap: { source } });
  expect(code).toBe("#!/usr/bin/env node\nx;");
  expect(map?.mappings).toBe("AAAA;AACA");
});

test("any node prints on its own", () => {
  const source = "#!/usr/bin/env node\nconst o = { a: `x` };\nclass C { #p: string; }";
  const printed: string[] = [];
  walk(parse(source, { lang: "ts" }).program, {
    enter(node) {
      if (node.type === "Program" || node.type === "Identifier") return;
      printed.push(`${node.type}: ${generate(node, { format: "compact" }).code}`);
    },
  });
  expect(printed).toMatchInlineSnapshot(`
    [
      "Hashbang: #!/usr/bin/env node",
      "VariableDeclaration: const o={a:\`x\`}",
      "VariableDeclarator: o={a:\`x\`}",
      "ObjectExpression: {a:\`x\`}",
      "Property: a:\`x\`",
      "TemplateLiteral: \`x\`",
      "TemplateElement: x",
      "ClassDeclaration: class C{#p:string}",
      "ClassBody: {#p:string}",
      "PropertyDefinition: #p:string",
      "PrivateIdentifier: #p",
      "TSTypeAnnotation: :string",
      "TSStringKeyword: string",
    ]
  `);
});

test("a lone type strips to nothing", () => {
  const { program } = parse("let x: this | string;", { lang: "ts" });
  const types = findAll(program, ["TSUnionType", "TSThisType", "TSStringKeyword"]);
  expect(types.map((node) => generate(node, { strip: true }).code)).toEqual(["", "", ""]);
});

test("a value that is not a node throws", () => {
  for (const value of [null, undefined, {}, { type: 1 }]) {
    expect(() => generate(value as never)).toThrow("Expected an AST node");
  }
});

// a plain-object dialect node prints through `print`, and a type with no printer throws
test("print prints the node types the printer does not know", () => {
  const { program } = parse("items; render(item);");
  type Each = { type: "EachStatement"; start: number; end: number; list: Node; body: Node[] };
  const [items, render] = program.body;
  if (items?.type !== "ExpressionStatement" || !render) throw new Error("two statements");
  const each: Each = {
    type: "EachStatement",
    start: 0,
    end: 0,
    list: items.expression,
    body: [render],
  };
  const print = {
    EachStatement(node: Each, w: Printer) {
      w.write("each");
      w.space();
      w.write("(");
      w.node(node.list);
      w.write(")");
      w.space();
      w.block(node.body);
    },
  } as unknown as Printers;

  expect(generate(each as unknown as Node, { print }).code).toMatchInlineSnapshot(`
    "each (items) {
      render(item);
    }"
  `);
  expect(generate(each as unknown as Node, { print, format: "compact" }).code)
    .toMatchInlineSnapshot(`"each(items){render(item)}"`);
  expect(() => generate(each as unknown as Node)).toThrow("unsupported node type: EachStatement");
});
