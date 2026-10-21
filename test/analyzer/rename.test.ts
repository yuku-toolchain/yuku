import { beforeAll, describe, expect, test } from "bun:test";
import { BindingFlags, parse, print, type ParseOptions, type SourceFile } from "yuku";
import {
  corpusAndProjectFiles,
  corpusPresent,
  forEachCorpusFile,
  type CorpusFile,
} from "../corpus";

function renamed(source: string, from: string, to: string, path = "input.ts"): string {
  const file = parse(source, { semantic: true, path });
  expect(bindingNamed(file, from).rename(to)).toEqual([]);
  return print(file).code;
}

function refused(source: string, from: string, to: string, path = "input.ts"): string[] {
  const file = parse(source, { semantic: true, path });
  const before = print(file).code;
  const diagnostics = bindingNamed(file, from).rename(to);
  expect(print(file).code).toBe(before);
  expect(bindingNamed(file, from).name).toBe(from);
  return diagnostics.map(({ message, start, labels }) =>
    [`${message} @${start}`, ...labels.map((label) => `  ${label.message} @${label.start}`)].join(
      "\n",
    ),
  );
}

function bindingNamed(file: SourceFile, name: string) {
  const binding = file.bindings.find((candidate) => candidate.name === name);
  if (binding === undefined) throw new Error(`no binding '${name}'`);
  return binding;
}

describe("rename", () => {
  test("renames the declaration and every reference", () => {
    expect(renamed(`let a = 1; a++; f(a); let o = { a };`, "a", "count")).toBe(
      "let count = 1;\ncount++;\nf(count);\nlet o = { a: count };",
    );
    expect(renamed(`function f({ a = 1 }) { return a; }`, "a", "b")).toBe(
      "function f({ a: b = 1 }) {\n  return b;\n}",
    );
    expect(renamed(`import { a } from "m"; a();`, "a", "b")).toBe(
      'import { a as b } from "m";\nb();',
    );
    expect(renamed(`const A = 1; <A />;`, "A", "B", "input.tsx")).toBe("const B = 1;\n<B />;");
  });

  test("the file's exports keep their names", () => {
    expect(renamed(`export const a = 1, b = a;`, "a", "x")).toBe(
      "const x = 1, b = x;\nexport { x as a, b };",
    );
    expect(renamed(`const a = 1; export { a };`, "a", "x")).toBe(
      "const x = 1;\nexport { x as a };",
    );
    expect(renamed(`export const C = 1;\nexport type C = typeof C;`, "C", "X")).toBe(
      "const X = 1;\nexport { X as C };\ntype X = typeof X;\nexport type { X as C };",
    );
  });

  test("a declaration file that exports implicitly keeps its exports by refusing", () => {
    expect(refused(`interface Base {}\nexport interface A extends Base {}`, "A", "B", "a.d.ts"))
      .toMatchInlineSnapshot(`
      [
        "Cannot rename 'A', which also names its export @35",
      ]
    `);
  });

  test("the binding and the file read the new name", () => {
    const file = parse(`let a = 1; a;`, { semantic: true });
    const binding = bindingNamed(file, "a");
    expect(binding.rename("b")).toEqual([]);
    expect(binding.name).toBe("b");
    expect(binding.references.map((reference) => reference.name)).toEqual(["b"]);
    expect(file.rootScope.find("b")).toBe(binding);
    expect(file.rootScope.find("a")).toBeNull();
  });

  test("a rename that changes what code means reports why and changes nothing", () => {
    expect(refused(`let a, b;`, "a", "b")).toEqual([
      "Renaming 'a' to 'b' would redeclare 'b' @4\n  'b' is declared here @7",
    ]);
    expect(refused(`let a; function f() { let b; a; } b;`, "a", "b")).toEqual([
      "Renaming 'a' to 'b' would let another 'b' shadow this use @29\n  'b' is declared here @26",
      "Renaming 'a' to 'b' would capture this use of 'b' @34\n  'a' is declared here @4",
    ]);
    expect(refused(`"use strict"; let a;`, "a", "eval")).toEqual([
      "Renaming 'a' to 'eval' would declare 'eval' in strict mode code @18",
    ]);
  });

  test("a binding whose name code reads elsewhere cannot be renamed", () => {
    expect(refused(`enum E { A }`, "A", "B")).toEqual([
      "Cannot rename 'A', which is an enum member read by name @9",
    ]);
    expect(refused(`class C { constructor(public a) {} }`, "a", "b")).toEqual([
      "Cannot rename 'a', which is also a class member @29",
    ]);
    expect(refused(`namespace N { export const a = 1; }`, "a", "b")).toEqual([
      "Cannot rename 'a', which also names its export @27",
    ]);
  });

  test("a diagnostic names the file and can suggest a fix", () => {
    const file = parse(`const A = 1; <A />;`, { semantic: true, path: "input.tsx" });
    const [diagnostic] = bindingNamed(file, "A").rename("div");
    expect(diagnostic).toMatchObject({
      severity: "error",
      message: "Renaming 'A' to 'div' would read this tag as an intrinsic element",
      path: "input.tsx",
      start: 14,
      end: 15,
      help: "Start the name in uppercase",
    });
  });

  test("a name that is not an identifier throws", () => {
    const binding = bindingNamed(parse(`let a;`, { semantic: true }), "a");
    expect(() => binding.rename("1x")).toThrow(TypeError);
    expect(() => binding.rename("class")).toThrow("'class' is not a valid identifier");
  });
});

describe.skipIf(!corpusPresent())("renames across the corpus", () => {
  const SAMPLE_MAX = 8;
  const RENAMES_MAX = 2;
  const failures: string[] = [];
  let renamed = 0;
  let refused = 0;

  function note(detail: string): void {
    if (failures.length < SAMPLE_MAX) failures.push(detail);
  }

  function uses(file: SourceFile, binding: SourceFile["bindings"][number]): number {
    const outside = (reference: (typeof binding.references)[number]) =>
      file.parentOf(reference.node)?.type !== "ExportSpecifier";
    return binding.references.filter(outside).length;
  }

  function shape(file: SourceFile, skip: number): string[] {
    const free = file.references.filter((reference) => reference.binding === null);
    const bindings = file.bindings.filter((binding) => binding.id !== skip);
    return [
      ...free.map((reference) => `free ${reference.name}`),
      ...bindings.map((binding) => `${binding.name} ${uses(file, binding)}`),
      ...file.exportedNames().map((name) => `export ${name}`),
    ].sort();
  }

  function checkFile(file: CorpusFile, source: string): void {
    const options: ParseOptions = { lang: file.lang, sourceType: file.sourceType, semantic: true };
    const first = parse(source, options);
    if (first.diagnostics.length > 0) return;
    const candidates = first.bindings.filter(
      (binding) =>
        binding.references.length > 0 &&
        !binding.has(BindingFlags.EnumMember) &&
        binding.declarations[0]?.type === "Identifier",
    );
    const stride = Math.max(1, Math.floor(candidates.length / RENAMES_MAX));
    for (const candidate of candidates.filter((_, i) => i % stride === 0).slice(0, RENAMES_MAX)) {
      const fresh = parse(source, options);
      const binding = fresh.bindings[candidate.id]!;
      const name = `${binding.name}_renamed`;
      const count = uses(fresh, binding);
      const before = print(fresh).code;
      const expected = shape(fresh, binding.id);
      const where = `${file.path}: ${binding.name}`;
      if (binding.rename(name).length > 0) {
        refused++;
        if (print(fresh).code !== before) note(`${where} refused but changed the code`);
        continue;
      }
      renamed++;
      const after = parse(print(fresh).code, options);
      if (after.diagnostics.length > 0) {
        note(`${where} no longer parses: ${after.diagnostics[0]!.message}`);
        continue;
      }
      const target = after.bindings.find((candidate) => candidate.name === name);
      if (target === undefined || uses(after, target) !== count) {
        note(`${where} loses uses`);
        continue;
      }
      const actual = shape(after, target.id);
      if (actual.join("\n") !== expected.join("\n")) {
        const changed = actual.find((entry, i) => entry !== expected[i]);
        note(`${where} changes ${changed ?? "a binding"}`);
      }
    }
  }

  beforeAll(async () => {
    await forEachCorpusFile(corpusAndProjectFiles(), checkFile);
  }, 1_800_000);

  test("renames keep meaning", () => {
    expect(renamed).toBeGreaterThan(0);
    expect(refused).toBeGreaterThan(0);
    expect(failures).toEqual([]);
  });
});
