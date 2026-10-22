import { expect, test } from "bun:test";
import {
  parse,
  print,
  walk,
  type Node,
  type Printer,
  type Printers,
  type Program,
} from "yuku";
import { TraceMap, decodedMappings } from "@jridgewell/trace-mapping";
import { nodesOf } from "../ast-helpers-for-test";
import { gen } from "./helpers";

test("strip and minify compose in a single call", () => {
  expect(
    gen(`const x: number = 1000000;\nif (true) { obj["foo"] = undefined; }`, {
      strip: true,
      minify: true,
    }),
  ).toMatchInlineSnapshot(`"const x=1e6;if(!0){obj.foo=undefined}"`);
});

test("minify defaults the format, quotes, and comments, and the options say otherwise", () => {
  const source = `// note\n/*! legal */\nconst x = 1000000;\nconst s = 'plain';`;
  expect(gen(source, { minify: true })).toMatchInlineSnapshot(`
    "/*! legal */
    const x=1e6;const s="plain""
  `);
  const options = { minify: true, format: "pretty", quotes: "preserve", comments: "all" } as const;
  expect(gen(source, options)).toMatchInlineSnapshot(`
    "// note
    /*! legal */
    const x = 1e6;
    const s = 'plain';"
  `);
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
  expect(lengths(parse(print(program).code).program)).toEqual([n + 1, n]);
});

test("a compact source map starts each mapping on its token", () => {
  for (const [source, token] of [
    ["function f() { return [1]; }", "["],
    ["x = a - -b;", "-b"],
  ] as const) {
    const { code, map } = print(parse(source), { format: "compact", sourceMap: true });
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
    const file = parse(source, { lang: "jsx" });
    const { code, map } = print(file, { minify, sourceMap: true });
    const segments = decodedMappings(new TraceMap(JSON.stringify(map)))[0] ?? [];
    const segment = segments.find(([column]) => column === code.indexOf(printed));
    expect(segment?.[3], source).toBe(source.indexOf(original));
  }
});

test("the line after a hashbang maps to its own line", () => {
  const source = "#!/usr/bin/env node\nx;\n";
  const { code, map } = print(parse(source), { sourceMap: true });
  expect(code).toBe("#!/usr/bin/env node\nx;");
  expect(map?.mappings).toBe("AAAA;AACA");
});

test("any node prints on its own", () => {
  const source = "#!/usr/bin/env node\nconst o = { a: `x` };\nclass C { #p: string; }";
  const printed: string[] = [];
  walk(parse(source, { lang: "ts" }).program, {
    enter(node) {
      if (node.type === "Program" || node.type === "Identifier") return;
      printed.push(`${node.type}: ${print(node, { format: "compact" }).code}`);
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
  const types = nodesOf(program, "TSUnionType", "TSThisType", "TSStringKeyword");
  expect(types.map((node) => print(node, { strip: true }).code)).toEqual(["", "", ""]);
});

test("an option outside its values throws", () => {
  const { program } = parse(`a;`);
  expect(() => print(program, { format: "tight" as never })).toThrow('`format` must be "pretty"');
  expect(() => print(program, { quotes: "back" as never })).toThrow("`quotes` must be");
  expect(() => print(program, { comments: true as never })).toThrow("`comments` must be");
  expect(() => print(program, { indent: 256 })).toThrow("`indent` must be an integer");
  expect(() => print(program, { indent: 1.5 })).toThrow("`indent` must be an integer");
  expect(() => print(program, { printers: 1 as never })).toThrow("`printers` must be an object");
  expect(() => print(program, { minify: {} as never })).toThrow("`minify` must be true or false");
  expect(() => print(program, { print: {} } as never)).toThrow("`print` is not a print option");
  const source = { source: "a;" } as never;
  expect(() => print(program, { sourceMap: source })).toThrow("`source` is not a sourceMap option");
  const content = { sourcesContent: "a;" } as never;
  expect(() => print(program, { sourceMap: content })).toThrow("`sourcesContent` must be true");
  expect(() => print(program, { sourceMap: 1 as never })).toThrow("`sourceMap` must be true");
});

test("a value that is not a file or a node throws", () => {
  for (const value of [null, undefined, {}, { type: 1 }]) {
    expect(() => print(value as never)).toThrow("print: expected an AST node");
  }
});

test("a source map needs the file", () => {
  const { program } = parse("x;");
  expect(() => print(program, { sourceMap: true })).toThrow("a source map needs the source");
});

test("a source map names the file's path and embeds its source on request", () => {
  const source = "let a = 1;\n";
  const { map } = print(parse(source, { path: "src/a.ts" }), {
    sourceMap: { file: "a.js", sourceRoot: "/", sourcesContent: true },
  });
  expect(map).toMatchObject({
    version: 3,
    file: "a.js",
    sourceRoot: "/",
    sources: ["src/a.ts"],
    sourcesContent: [source],
  });
});

test("attached comments print by default, and some keeps only the significant ones", () => {
  const file = parse("// note\n/*! legal */\nlet a = 1;", { attachComments: true });
  expect(print(file).code).toBe("// note\n/*! legal */\nlet a = 1;");
  expect(print(file, { format: "compact", comments: "some" }).code).toBe("/*! legal */\nlet a=1");
});

test("printers print the node types the printer does not know", () => {
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
  const printers = {
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

  expect(print(each as unknown as Node, { printers }).code).toMatchInlineSnapshot(`
    "each (items) {
      render(item);
    }"
  `);
  expect(print(each as unknown as Node, { printers, format: "compact" }).code)
    .toMatchInlineSnapshot(`"each(items){render(item)}"`);
  expect(() => print(each as unknown as Node)).toThrow("unsupported node type: EachStatement");
});
