import { describe, expect, test } from "bun:test";
import { parse, Project } from "yuku";
import { definition, projectOf } from "./utils/summarize";

const VALUE_PROJECT = {
  "a.ts": `export const value = 1;`,
  "b.ts": `import { value } from "./a.ts"; value;`,
};

function messages(project: Project): string[] {
  return project.diagnostics.map((diagnostic) => `${diagnostic.path}: ${diagnostic.message}`);
}

function dependencies(project: Project, path: string): (string | null)[] {
  return project.files.get(path)!.dependencies.map((dependency) => dependency.path);
}

describe("re-adding a path", () => {
  test("replacing a module relinks dependents to the new exports", () => {
    const project = projectOf(VALUE_PROJECT);
    expect(definition(project, "b.ts", "value")).toBe("a.ts:value");
    expect(messages(project)).toEqual([]);

    project.set("a.ts", `export const renamed = 1;`);
    expect(definition(project, "b.ts", "value")).toBe("(none)");
    expect(messages(project)).toEqual(["b.ts: Module './a.ts' has no export 'value'"]);
  });

  test("replacing a module clears a diagnostic it previously caused", () => {
    const project = projectOf({
      "a.ts": `export const present = 1;`,
      "b.ts": `import { missing } from "./a.ts"; missing;`,
    });
    expect(messages(project)).toEqual(["b.ts: Module './a.ts' has no export 'missing'"]);
    expect(definition(project, "b.ts", "missing")).toBe("(none)");

    project.set("a.ts", `export const present = 1; export const missing = 2;`);
    expect(messages(project)).toEqual([]);
    expect(definition(project, "b.ts", "missing")).toBe("a.ts:missing");
  });

  test("a file holds only what it was parsed with", () => {
    const file = parse(`let a = 1;`);
    expect(() => file.rootScope).toThrow("parse with `semantic: true`");
    expect(() => file.bindingOf(file.program)).toThrow("parse with `semantic: true`");
    expect(() => file.tokens).toThrow("parse with `tokens: true`");
    const full = parse(`let a = 1;`, { semantic: true, tokens: true });
    expect(full.rootScope.find("a")?.name).toBe("a");
    expect(full.tokens.length).toBe(5);
  });

  test("a replaced module is no longer current, and its cross-file queries throw", () => {
    const project = new Project();
    const first = project.set("a.ts", `export const value = 1;`);
    const second = project.set("a.ts", `export const value = 2;`);
    expect(second).not.toBe(first);
    expect(project.files.get("a.ts")).toBe(second);
    expect(project.files.size).toBe(1);
    expect([first.isCurrent, second.isCurrent]).toEqual([false, true]);
    expect(() => first.dependencies).toThrow("'a.ts' was replaced or deleted");
    expect(() => first.rootScope.find("value")!.definition()).toThrow();
  });

  test("re-adding re-resolves the changed module's own imports", () => {
    const project = projectOf({
      "one.ts": `export const a = 1;`,
      "two.ts": `export const b = 2;`,
      "main.ts": `import { a } from "./one.ts"; a;`,
    });
    expect(dependencies(project, "main.ts")).toEqual(["one.ts"]);

    project.set("main.ts", `import { b } from "./two.ts"; b;`);
    expect(dependencies(project, "main.ts")).toEqual(["two.ts"]);
    expect(definition(project, "main.ts", "b")).toBe("two.ts:b");
  });
});

describe("deleteFile", () => {
  test("reports whether the path existed", () => {
    const project = projectOf({ "a.ts": `export const x = 1;` });
    expect(project.delete("a.ts")).toBe(true);
    expect(project.delete("a.ts")).toBe(false);
  });

  test("removing a dependency drops it from the graph", () => {
    const project = projectOf(VALUE_PROJECT);
    expect(dependencies(project, "b.ts")).toEqual(["a.ts"]);

    project.delete("a.ts");
    expect(project.files.get("b.ts")!.dependencies).toEqual([]);
    // a missing file is an external module
    expect(definition(project, "b.ts", "value")).toBe("(none)");
  });

  test("re-adding after removal restores resolution", () => {
    const project = projectOf(VALUE_PROJECT);
    project.delete("a.ts");
    expect(definition(project, "b.ts", "value")).toBe("(none)");

    project.set("a.ts", `export const value = 1;`);
    expect(definition(project, "b.ts", "value")).toBe("a.ts:value");
    expect(messages(project)).toEqual([]);
  });
});

describe("on-demand relinking", () => {
  test("queries reflect the latest sources", () => {
    const project = projectOf(VALUE_PROJECT);
    expect(definition(project, "b.ts", "value")).toBe("a.ts:value");

    project.set("a.ts", `export const renamed = 1;`);
    expect(definition(project, "b.ts", "value")).toBe("(none)");
  });
});
