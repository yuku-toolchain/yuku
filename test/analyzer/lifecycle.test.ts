import { describe, expect, test } from "bun:test";
import { Analyzer } from "yuku-analyzer";
import { definition, project } from "./utils/summarize";

function messages(analyzer: Analyzer): string[] {
  return analyzer.diagnostics.map((d) => `${d.module}: ${d.message}`);
}

describe("re-adding a path", () => {
  test("replacing a module relinks dependents to the new exports", () => {
    const analyzer = project({
      "a.ts": `export const value = 1;`,
      "b.ts": `import { value } from "./a.ts"; value;`,
    });
    expect(definition(analyzer, "b.ts", "value")).toBe("a.ts:value");
    expect(messages(analyzer)).toEqual([]);

    // a.ts changes: the export is renamed away
    analyzer.addFile("a.ts", `export const renamed = 1;`);
    expect(definition(analyzer, "b.ts", "value")).toBe("(none)");
    expect(messages(analyzer)).toEqual(["b.ts: Module './a.ts' has no export 'value'"]);
  });

  test("replacing a module clears a diagnostic it previously caused", () => {
    const analyzer = project({
      "a.ts": `export const present = 1;`,
      "b.ts": `import { missing } from "./a.ts"; missing;`,
    });
    expect(messages(analyzer)).toEqual(["b.ts: Module './a.ts' has no export 'missing'"]);
    expect(definition(analyzer, "b.ts", "missing")).toBe("(none)");

    // a.ts changes: the missing export now exists
    analyzer.addFile("a.ts", `export const present = 1; export const missing = 2;`);
    expect(messages(analyzer)).toEqual([]);
    expect(definition(analyzer, "b.ts", "missing")).toBe("a.ts:missing");
  });

  test("a replaced path yields a fresh module; identity is the invalidation signal", () => {
    const analyzer = new Analyzer();
    const first = analyzer.addFile("a.ts", `export const value = 1;`);
    const second = analyzer.addFile("a.ts", `export const value = 2;`);
    expect(second).not.toBe(first);
    expect(analyzer.module("a.ts")).toBe(second);
    expect(analyzer.modules.size).toBe(1);
  });

  test("re-adding re-resolves the changed module's own imports", () => {
    const analyzer = project({
      "one.ts": `export const a = 1;`,
      "two.ts": `export const b = 2;`,
      "main.ts": `import { a } from "./one.ts"; a;`,
    });
    expect(analyzer.module("main.ts")!.dependencies.map((d) => d.path)).toEqual(["one.ts"]);

    analyzer.addFile("main.ts", `import { b } from "./two.ts"; b;`);
    expect(analyzer.module("main.ts")!.dependencies.map((d) => d.path)).toEqual(["two.ts"]);
    expect(definition(analyzer, "main.ts", "b")).toBe("two.ts:b");
  });
});

describe("removeFile", () => {
  test("reports whether the path existed", () => {
    const analyzer = project({ "a.ts": `export const x = 1;` });
    expect(analyzer.removeFile("a.ts")).toBe(true);
    expect(analyzer.removeFile("a.ts")).toBe(false);
  });

  test("removing a dependency drops it from the graph", () => {
    const analyzer = project({
      "a.ts": `export const value = 1;`,
      "b.ts": `import { value } from "./a.ts"; value;`,
    });
    expect(analyzer.module("b.ts")!.dependencies.map((d) => d.path)).toEqual(["a.ts"]);

    analyzer.removeFile("a.ts");
    expect(analyzer.module("b.ts")!.dependencies).toEqual([]);
    // the import now resolves to nothing added, i.e. an external module, by design
    expect(definition(analyzer, "b.ts", "value")).toBe("(none)");
  });

  test("re-adding after removal restores resolution", () => {
    const analyzer = project({
      "a.ts": `export const value = 1;`,
      "b.ts": `import { value } from "./a.ts"; value;`,
    });
    analyzer.removeFile("a.ts");
    expect(definition(analyzer, "b.ts", "value")).toBe("(none)");

    analyzer.addFile("a.ts", `export const value = 1;`);
    expect(definition(analyzer, "b.ts", "value")).toBe("a.ts:value");
    expect(messages(analyzer)).toEqual([]);
  });
});

describe("on-demand relinking", () => {
  test("queries reflect the latest sources without an explicit link()", () => {
    const analyzer = project({
      "a.ts": `export const value = 1;`,
      "b.ts": `import { value } from "./a.ts"; value;`,
    });
    // the first cross-file read links lazily
    expect(definition(analyzer, "b.ts", "value")).toBe("a.ts:value");

    // a change followed by a read relinks with no manual link() call
    analyzer.addFile("a.ts", `export const renamed = 1;`);
    expect(definition(analyzer, "b.ts", "value")).toBe("(none)");
  });

  test("an explicit link() agrees with on-demand linking", () => {
    const analyzer = project({
      "a.ts": `export const value = 1;`,
      "b.ts": `import { value } from "./a.ts"; value;`,
    });
    analyzer.link();
    expect(definition(analyzer, "b.ts", "value")).toBe("a.ts:value");
    expect(messages(analyzer)).toEqual([]);
  });
});
