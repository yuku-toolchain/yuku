import { describe, expect, test } from "bun:test";
import { b, parse, print, walk, type Identifier, type Node } from "yuku";
import { nodesOf } from "../ast-helpers-for-test";
import { definition, projectOf, summaryOf } from "./utils/summarize";

function unposition(root: Node): void {
  walk(root, {
    enter(node) {
      node.start = 0;
      node.end = 0;
    },
  });
}

describe("reanalyze", () => {
  test("a pass that replaces nodes resolves the new ones and drops the old", () => {
    const file = parse("let a = 1; f({ a });", { analyze: true });
    const [old] = file.rootScope.find("a")!.references;
    walk(file.program, {
      Property(node, ctx) {
        if (!node.shorthand) return;
        const name = (node.key as Identifier).name;
        ctx.replace(
          b.Property({ kind: "init", key: b.Identifier({ name }), value: b.Identifier({ name }) }),
        );
      },
    });
    file.reanalyze();

    const [property] = nodesOf(file.program, "Property");
    const value = property!.value as Identifier;
    expect(file.referenceOf(value)?.binding).toBe(file.rootScope.find("a"));
    const references = file.rootScope.find("a")!.references;
    expect(references.map((reference) => reference.node)).toEqual([value]);
    expect(file.referenceOf(old!.node)).toBeNull();
    expect(print(file).code).toBe("let a = 1;\nf({ a: a });");
  });

  test("a tree without positions analyzes as its source does", () => {
    const source = [
      "let x = 1, v = 2;",
      "function f<T extends typeof x>(x: number, y: typeof x, z: typeof v): T { var v; return z; }",
      "type A<T> = T extends [infer U, infer V] ? U | V : T;",
      "class C<T> { static s: T; m(): T { return this.m(); } }",
    ].join("\n");
    const file = parse(source, { analyze: true, path: "input.ts" });
    const expected = summaryOf(file);
    unposition(file.program);
    file.reanalyze();
    expect(summaryOf(file)).toBe(expected);
  });

  test("renaming the model's nodes renames the printed code", () => {
    const file = parse("let a = 1; { let a = 2; a++; } a;", { analyze: true });
    const inner = file.scopes[1]!.find("a")!;
    file.program.body.push(b.ExpressionStatement({ expression: b.Identifier({ name: "a" }) }));
    file.reanalyze();
    const outer = file.rootScope.find("a")!;
    for (const node of [...outer.declarations, ...outer.references.map((r) => r.node)]) {
      (node as Identifier).name = "b";
    }
    expect(inner.references).toHaveLength(1);
    expect(print(file).code).toBe("let b = 1;\n{\n  let a = 2;\n  a++;\n}\nb;\nb;");
  });

  test("a tree outside the AST shapes throws and keeps the model", () => {
    const file = parse("let a = 1;", { analyze: true });
    file.program.body.push({ type: "Nope", start: 0, end: 0 } as never);
    expect(() => file.reanalyze()).toThrow("Nope is not a node");
    expect(file.bindings.map((binding) => binding.name)).toEqual(["a"]);
  });

  test("a file of a project relinks the project", () => {
    const project = projectOf({
      "a.ts": `export const value = 1;`,
      "b.ts": `import { value } from "./a.ts"; value;`,
    });
    expect(definition(project, "b.ts", "value")).toBe("a.ts:value");
    const file = project.files.get("a.ts")!;
    (file.bindings[0]!.declarations[0] as Identifier).name = "renamed";
    file.reanalyze();
    expect(definition(project, "b.ts", "value")).toBe("(none)");
    expect(project.diagnostics.map((diagnostic) => diagnostic.message)).toEqual([
      "Module './a.ts' has no exported member 'value'",
    ]);
  });
});
