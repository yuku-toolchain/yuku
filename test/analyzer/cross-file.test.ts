import { describe, expect, test } from "bun:test";
import { Analyzer } from "yuku-analyzer";
import { definition, links, project, references } from "./summarize";

describe("definitionOf", () => {
  test("follows import then renamed re-export to the original binding", () => {
    const analyzer = project({
      "a.ts": `export const value = 1;`,
      "b.ts": `export { value as renamed } from "./a.ts";`,
      "c.ts": `import { renamed } from "./b.ts"; renamed;`,
    });
    expect(definition(analyzer, "c.ts", "renamed")).toBe("a.ts:value");
  });

  test("follows an export * chain", () => {
    const analyzer = project({
      "a.ts": `export const deep = 1;`,
      "b.ts": `export * from "./a.ts";`,
      "c.ts": `import { deep } from "./b.ts"; deep;`,
    });
    expect(definition(analyzer, "c.ts", "deep")).toBe("a.ts:deep");
  });

  test("a namespace import has a module definition with no symbol", () => {
    const analyzer = project({
      "a.ts": `export const x = 1;`,
      "b.ts": `import * as ns from "./a.ts"; ns;`,
    });
    expect(definition(analyzer, "b.ts", "ns")).toBe("a.ts:(namespace)");
  });

  test("a chain that leaves the added file set is null", () => {
    const analyzer = project({ "a.ts": `import { ext } from "external-pkg"; ext;` });
    expect(definition(analyzer, "a.ts", "ext")).toBe("(none)");
  });

  test("a circular re-export terminates", () => {
    const analyzer = project({
      "a.ts": `export { x } from "./b.ts";`,
      "b.ts": `export { x } from "./a.ts";`,
      "c.ts": `import { x } from "./a.ts"; x;`,
    });
    expect(definition(analyzer, "c.ts", "x")).toBe("(none)");
  });
});

describe("referencesOf", () => {
  test("collects local and cross-module uses of a definition", () => {
    const analyzer = project({
      "a.ts": `export function value() {} value();`,
      "b.ts": `import { value } from "./a.ts"; value(); value();`,
    });
    expect(references(analyzer, "a.ts", "value")).toBe("a.ts:value, b.ts:value, b.ts:value");
  });
});

describe("exportedNames", () => {
  test("export * pulls in source names but never default", () => {
    const analyzer = project({
      "a.ts": `export const one = 1;`,
      "lib.ts": `export const two = 2; export default 0; export * from "./a.ts";`,
    });
    expect(analyzer.module("lib.ts")!.exportedNames().sort()).toEqual(["default", "one", "two"]);
    expect(analyzer.module("a.ts")!.exportedNames()).toEqual(["one"]);
  });
});

describe("link diagnostics", () => {
  test("a missing named import is reported", () => {
    expect(
      links({
        "a.ts": `export const present = 1;`,
        "b.ts": `import { absent } from "./a.ts";`,
      }),
    ).toMatchInlineSnapshot(`
      "diagnostics
        b.ts: Module './a.ts' has no export 'absent'
      graph
        a.ts → (none)
        b.ts → a.ts
      exportedNames
        a.ts: present
        b.ts: (none)"
    `);
  });

  test("a name supplied ambiguously by two stars is reported at the import", () => {
    expect(
      links({
        "a.ts": `export const x = 1;`,
        "b.ts": `export const x = 2;`,
        "lib.ts": `export * from "./a.ts"; export * from "./b.ts";`,
        "c.ts": `import { x } from "./lib.ts";`,
      }),
    ).toMatchInlineSnapshot(`
      "diagnostics
        c.ts: Import 'x' of module './lib.ts' is ambiguous: multiple 'export *' declarations supply it
      graph
        a.ts → (none)
        b.ts → (none)
        lib.ts → a.ts, b.ts
        c.ts → lib.ts
      exportedNames
        a.ts: x
        b.ts: x
        lib.ts: x
        c.ts: (none)"
    `);
  });

  test("a clean graph has no diagnostics and wires dependencies", () => {
    expect(
      links({
        "util.ts": `export const helper = 1;`,
        "app.ts": `import { helper } from "./util.ts"; helper;`,
      }),
    ).toMatchInlineSnapshot(`
      "diagnostics
        (none)
      graph
        util.ts → (none)
        app.ts → util.ts
      exportedNames
        util.ts: helper
        app.ts: (none)"
    `);
  });
});

describe("resolution", () => {
  test("the default resolver probes extensions and index files", () => {
    const analyzer = project({
      "src/index.ts": `export const a = 1;`,
      "src/util.ts": `export const b = 2;`,
      "src/main.ts": `import { a } from "./index"; import { b } from "./util"; a; b;`,
    });
    const main = analyzer.module("src/main.ts")!;
    expect(main.dependencies.map((d) => d.path).sort()).toEqual(["src/index.ts", "src/util.ts"]);
  });

  test("a custom resolver maps bare specifiers", () => {
    const analyzer = new Analyzer({
      resolve: (specifier) => (specifier === "@app/lib" ? "lib.ts" : null),
    });
    analyzer.addFile("lib.ts", `export const x = 1;`);
    analyzer.addFile("main.ts", `import { x } from "@app/lib"; x;`);
    expect(analyzer.module("main.ts")!.dependencies.map((d) => d.path)).toEqual(["lib.ts"]);
    expect(definition(analyzer, "main.ts", "x")).toBe("lib.ts:x");
  });
});
