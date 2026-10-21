import { describe, expect, test } from "bun:test";
import { Project } from "yuku";
import { definition, links, projectOf, references } from "./utils/summarize";

describe("definition", () => {
  test("follows import then renamed re-export to the original binding", () => {
    const project = projectOf({
      "a.ts": `export const value = 1;`,
      "b.ts": `export { value as renamed } from "./a.ts";`,
      "c.ts": `import { renamed } from "./b.ts"; renamed;`,
    });
    expect(definition(project, "c.ts", "renamed")).toBe("a.ts:value");
  });

  test("follows an export * chain", () => {
    const project = projectOf({
      "a.ts": `export const deep = 1;`,
      "b.ts": `export * from "./a.ts";`,
      "c.ts": `import { deep } from "./b.ts"; deep;`,
    });
    expect(definition(project, "c.ts", "deep")).toBe("a.ts:deep");
  });

  test("a namespace import has a module definition with no binding", () => {
    const project = projectOf({
      "a.ts": `export const x = 1;`,
      "b.ts": `import * as ns from "./a.ts"; ns;`,
    });
    expect(definition(project, "b.ts", "ns")).toBe("a.ts:(namespace)");
  });

  test("a chain that leaves the added file set is null", () => {
    const project = projectOf({ "a.ts": `import { ext } from "external-pkg"; ext;` });
    expect(definition(project, "a.ts", "ext")).toBe("(none)");
  });

  test("a circular re-export terminates", () => {
    const project = projectOf({
      "a.ts": `export { x } from "./b.ts";`,
      "b.ts": `export { x } from "./a.ts";`,
      "c.ts": `import { x } from "./a.ts"; x;`,
    });
    expect(definition(project, "c.ts", "x")).toBe("(none)");
  });
});

describe("findReferences", () => {
  test("collects local and cross-module uses of a definition", () => {
    const project = projectOf({
      "a.ts": `export function value() {} value();`,
      "b.ts": `import { value } from "./a.ts"; value(); value();`,
    });
    expect(references(project, "a.ts", "value")).toBe("a.ts:value, b.ts:value, b.ts:value");
  });
});

describe("resolveExport", () => {
  test("follows re-exports and stars to the defining binding", () => {
    const project = projectOf({
      "a.ts": `export const one = 1; export default 2;`,
      "lib.ts": `export * from "./a.ts"; export { one as uno } from "./a.ts";
        export * as ns from "./a.ts";`,
    });
    const lib = project.files.get("lib.ts")!;
    expect(lib.resolveExport("one")?.binding?.name).toBe("one");
    expect(lib.resolveExport("uno")?.binding?.name).toBe("one");
    expect(lib.resolveExport("ns")).toEqual({
      file: project.files.get("a.ts")!,
      binding: null,
      augmentations: [],
    });
    expect(lib.resolveExport("default")).toBeNull();
    expect(lib.resolveExport("missing")).toBeNull();
  });
});

describe("exportedNames", () => {
  test("export * pulls in source names but never default", () => {
    const project = projectOf({
      "a.ts": `export const one = 1;`,
      "lib.ts": `export const two = 2; export default 0; export * from "./a.ts";`,
    });
    expect(project.files.get("lib.ts")!.exportedNames().sort()).toEqual(["default", "one", "two"]);
    expect(project.files.get("a.ts")!.exportedNames()).toEqual(["one"]);
  });
});

describe("module augmentation", () => {
  test("merges into the binding the augmented export resolves to", () => {
    const project = projectOf({
      "a.ts": `export interface I { a: 1 }`,
      "b.ts": `export * from "./a.ts";`,
      "c.ts": `export {}; declare module "./b.ts" { interface I { b: I } }`,
      "d.ts": `import type { I } from "./a.ts"; let x: I;`,
    });
    expect(definition(project, "d.ts", "I")).toBe("a.ts:I + c.ts:I");
    expect(references(project, "d.ts", "I")).toBe("c.ts:I, d.ts:I");
    const c = project.files.get("c.ts")!;
    const augmentation = c.bindings.find((binding) => binding.name === "I");
    expect(augmentation?.definition()?.binding?.file.path).toBe("a.ts");
  });

  test("adds a name the module lacks", () => {
    const files = {
      "a.ts": `export const a = 1;`,
      "b.ts": `export {}; declare module "./a.ts" { interface J {} }`,
      "c.ts": `import type { J } from "./a.ts"; let x: J;`,
    };
    expect(definition(projectOf(files), "c.ts", "J")).toBe("b.ts:J");
    expect(links(files)).toMatchInlineSnapshot(`
      "diagnostics
        (none)
      graph
        a.ts → (none)
        b.ts → a.ts
        c.ts → a.ts
      exportedNames
        a.ts: a, J
        b.ts: (none)
        c.ts: (none)"
    `);
  });

  test("a script declares a module instead of augmenting one", () => {
    const project = projectOf({
      "a.ts": `export interface I {}`,
      "b.ts": `declare module "./a.ts" { interface I { b: 1 } }`,
      "c.ts": `import type { I } from "./a.ts"; let x: I;`,
    });
    expect(definition(project, "c.ts", "I")).toBe("a.ts:I");
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
    const project = projectOf({
      "src/index.ts": `export const a = 1;`,
      "src/util.ts": `export const b = 2;`,
      "src/main.ts": `import { a } from "./index"; import { b } from "./util"; a; b;`,
    });
    const main = project.files.get("src/main.ts")!;
    expect(main.dependencies.map((d) => d.path).sort()).toEqual(["src/index.ts", "src/util.ts"]);
  });

  test("the default resolver finds a TypeScript source by the extension it compiles to", () => {
    const project = projectOf({
      "src/a.ts": `export const a = 1;`,
      "src/b.tsx": `export const b = 2;`,
      "src/c.d.ts": `export declare const c: 3;`,
      "src/d.mts": `export const d = 4;`,
      "src/e.js": `export const e = 5;`,
      "src/e.ts": `export const e = 6;`,
      "src/main.ts": `import { a } from "./a.js"; import { b } from "./b.jsx";
        import { c } from "./c.js"; import { d } from "./d.mjs"; import { e } from "./e.js";`,
    });
    const main = project.files.get("src/main.ts")!;
    expect(main.imports.map((record) => record.resolvedFile?.path ?? null)).toEqual([
      "src/a.ts",
      "src/b.tsx",
      "src/c.d.ts",
      "src/d.mts",
      // the named file wins
      "src/e.js",
    ]);
  });

  test("a resolver returns false for an external module and null for an unresolved one", () => {
    const project = new Project({
      resolve: (specifier) => (specifier === "react" ? false : null),
    });
    project.set("main.ts", `import React from "react";\nimport { x } from "./utlis";`);
    expect(project.diagnostics).toEqual([
      {
        severity: "warning",
        message: "Cannot resolve './utlis'",
        path: "main.ts",
        start: 45,
        end: 54,
        labels: [],
        help: null,
      },
    ]);
  });

  test("the default resolver reports a missing file, not a package or an asset", () => {
    const project = projectOf({
      "main.ts": `import "react"; import "./app.css"; import "./missing"; import "./gone.ts";`,
    });
    expect(project.diagnostics.map((d) => d.message)).toEqual([
      "Cannot resolve './missing'",
      "Cannot resolve './gone.ts'",
    ]);
  });

  test("project diagnostics include each module's own", () => {
    const files = { "a.ts": `let x; let x;`, "b.ts": `import { y } from "./a";` };
    const project = projectOf(files, { semanticErrors: true });
    expect(project.diagnostics.map((d) => `${d.path}: ${d.message}`)).toEqual([
      "a.ts: Identifier 'x' has already been declared",
      "b.ts: Module './a' has no export 'y'",
    ]);
  });

  test("a module reports its early errors only with semanticErrors", () => {
    const project = projectOf({ "a.ts": `let x; let x;`, "b.ts": `import { y } from "./a";` });
    expect(project.diagnostics.map((d) => `${d.path}: ${d.message}`)).toEqual([
      "b.ts: Module './a' has no export 'y'",
    ]);
    expect(project.files.get("a.ts")!.rootScope.find("x")!.declarations).toHaveLength(2);
  });

  test("a custom resolver maps bare specifiers", () => {
    const project = new Project({
      resolve: (specifier) => (specifier === "@app/lib" ? "lib.ts" : null),
    });
    project.set("lib.ts", `export const x = 1;`);
    project.set("main.ts", `import { x } from "@app/lib"; x;`);
    expect(project.files.get("main.ts")!.dependencies.map((d) => d.path)).toEqual(["lib.ts"]);
    expect(definition(project, "main.ts", "x")).toBe("lib.ts:x");
  });
});
