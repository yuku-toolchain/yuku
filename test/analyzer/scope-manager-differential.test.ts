// Compares resolutions, write flags, and declared symbols against
// @typescript-eslint/scope-manager on positions both models record.

import { describe, expect, test } from "bun:test";
import { analyze } from "@typescript-eslint/scope-manager";
import { parse as tsParse } from "@typescript-eslint/typescript-estree";
import { Analyzer } from "yuku-analyzer";
import type { SourceLang, SourceType } from "yuku-parser";
import { corpusFiles } from "../corpus";

const UNRESOLVED = -1;

// erased TS wrappers, (a as any) = 1 still assigns to a
const ASSERTION_WRAPPERS = new Set([
  "TSAsExpression",
  "TSSatisfiesExpression",
  "TSNonNullExpression",
  "TSTypeAssertion",
]);

// parameter names in these are labels, not bindings
const SIGNATURE_TYPES = new Set([
  "TSMethodSignature",
  "TSFunctionType",
  "TSConstructorType",
  "TSCallSignatureDeclaration",
  "TSConstructSignatureDeclaration",
  "TSIndexSignature",
]);

function compare(
  label: string,
  source: string,
  sourceType: SourceType,
  lang: SourceLang = "js",
): { mismatches: string[]; compared: number; declarations: number } {
  const module = new Analyzer().addFile("input.js", source, { sourceType, lang });
  const yuku = new Map<number, { def: number; write: boolean }>();
  const referenceAt = new Map<number, (typeof module.references)[number]>();
  for (const reference of module.references) {
    // tsc-differential covers type space
    if (reference.inTypePosition) continue;
    referenceAt.set(reference.node.start, reference);
    const symbol = reference.symbol;
    yuku.set(reference.node.start, {
      def: symbol === null ? UNRESOLVED : Math.min(...symbol.declarations.map((d) => d.start)),
      write: reference.isWrite,
    });
  }
  const yukuDecls = new Set(module.symbols.flatMap((s) => s.declarations.map((d) => d.start)));

  const jsx = lang === "jsx" || lang === "tsx";
  const tree = tsParse(source, { range: true, sourceType, jsx, allowInvalidAST: false });
  const manager = analyze(tree, { sourceType });

  const scopeManager = new Map<number, { def: number; write: boolean }>();
  for (const scope of manager.scopes) {
    for (const reference of scope.references) {
      const starts = (reference.resolved?.defs ?? [])
        .map((def) => def.name?.range?.[0])
        .filter((start): start is number => start !== undefined);
      // no named def means an implicit binding such as arguments
      scopeManager.set(reference.identifier.range[0], {
        def: starts.length === 0 ? UNRESOLVED : Math.min(...starts),
        write: reference.isWrite(),
      });
    }
  }
  const theirDecls = new Map<number, string>();
  for (const scope of manager.scopes) {
    for (const variable of scope.variables) {
      for (const def of variable.defs) {
        const name = def.name as { range?: [number, number]; name?: string; type?: string };
        const position = name?.range?.[0];
        if (position === undefined) continue;
        // only identifier and string enum member names declare
        if (def.type === "TSEnumMemberName" && name.type !== "Identifier" && name.type !== "Literal") {
          continue;
        }
        if (def.type === "Parameter" && SIGNATURE_TYPES.has(def.node.type)) continue;
        if (def.type === "Parameter" && name.name === "this") continue;
        theirDecls.set(position, variable.name);
      }
    }
  }

  const enums = lang === "js" || lang === "jsx" ? [] : module.findAll("TSEnumDeclaration");
  const mismatches: string[] = [];
  let compared = 0;
  for (const [position, theirs] of scopeManager) {
    const ours = yuku.get(position);
    if (ours === undefined) continue;
    if (ours.def !== theirs.def) {
      // scope-manager binds purely lexically, without space rules
      const reference = referenceAt.get(position)!;
      if (ours.def === UNRESOLVED && module.resolve(reference.name, reference.scope, "any") !== null) {
        continue;
      }
      // merged enum declarations resolve members across blocks
      if (ours.def === UNRESOLVED && enums.some((e) => position >= e.start && position < e.end)) {
        continue;
      }
      mismatches.push(`${label} ref@${position}: yuku def@${ours.def}, scope-manager def@${theirs.def}`);
    } else if (ours.write !== theirs.write) {
      const parent = module.parentOf(referenceAt.get(position)!.node);
      const throughAssertion =
        ours.write &&
        parent !== null &&
        ASSERTION_WRAPPERS.has((parent as { type: string }).type);
      if (!throughAssertion) {
        mismatches.push(
          `${label} ref@${position}: yuku write=${ours.write}, scope-manager write=${theirs.write}`,
        );
      }
    }
    compared++;
  }

  let declarations = 0;
  for (const [position, name] of theirDecls) {
    if (yukuDecls.has(position)) declarations++;
    else mismatches.push(`${label} decl@${position} (${name}): scope-manager only`);
  }
  for (const position of yukuDecls) {
    if (!theirDecls.has(position)) {
      mismatches.push(`${label} decl@${position}: yuku only`);
    }
  }

  return { mismatches, compared, declarations };
}

// Triaged divergences. 96e92c0f is a yuku gap (import Y = A prefers
// the namespace in tsc). The rest are scope-manager gaps: f2131ad8
// merges a parameter with a same-named body var, the others leave
// import equals aliases and type-only export specifiers unresolved.
const KNOWN_DIVERGENCES = new Set(
  [
    "46575356e26d7a4a.ts",
    "63bd218519f23e2b.module.ts",
    "96e92c0fb053eed9.ts",
    "9c854a266a3d8cc0.module.ts",
    "f2131ad89bc9a8ba.ts",
  ].map((name) => `test/parser/suite/ts/pass/${name}`),
);

describe("resolution agrees with @typescript-eslint/scope-manager", () => {
  const SNIPPETS = [
    ["var hoisting across blocks", `var x; { var x = 1; } x;`],
    ["let shadowing", `let x = 1; { let x = 2; x; } x;`],
    ["function expression name", `const f = function self() { return self; };`],
    ["catch parameter", `let e = 1; try {} catch (e) { e; } e;`],
    ["class name inner scope", `class C { m() { return C; } } new C();`],
    ["default parameter reads parameter scope", `let y = 1; function f(a = y, y = 2) { return a + y; }`],
    ["closure capture", `function outer() { let v = 1; return () => v; }`],
    ["undeclared global", `undeclared;`],
    ["arguments is implicit", `function f() { return arguments; }`],
    ["named function hoisting", `f(); function f() {}`],
    ["parameter shadows outer", `let p = 1; function f(p) { return p; }`],
    ["for-of iteration variable", `for (const item of []) item;`],
    ["destructuring defaults", `const fallback = 0; const { a = fallback, b: c } = obj; c;`],
    ["import bindings", `import { a, b as c } from "m"; a; c;`, "module"],
    ["export specifiers", `const x = 1; export { x };`, "module"],
    ["getter setter pair", `const o = { get v() { return hidden; }, set v(next) { hidden = next; } }; let hidden;`],
    ["type annotations are erased", `let n: number = 1; n;`, "script", "ts"],
    ["jsx component reference", `const Button = () => null; export const app = <Button />;`, "module", "tsx"],
    ["enum members are lexically visible in the body", `const a = 9; enum E { a, b = a }`, "script", "ts"],
    ["arguments does not see past a function", `var arguments = 1; function f() { return arguments; } arguments;`],
  ] as [name: string, source: string, sourceType?: SourceType, lang?: SourceLang][];

  for (const [name, source, sourceType, lang] of SNIPPETS) {
    test(name, () => {
      const { mismatches, compared } = compare(name, source, sourceType ?? "script", lang ?? "js");
      expect(mismatches).toEqual([]);
      expect(compared).toBeGreaterThan(0);
    });
  }

  test("parameter default closures resolve past body vars", () => {
    const { mismatches } = compare(
      "parameter environment",
      `var x = 1; function f(_ = () => x) { var x = 2; }`,
      "script",
    );
    expect(mismatches).toEqual([]);
  });

  test("switch discriminants resolve in the outer scope", () => {
    const { mismatches } = compare(
      "switch discriminant",
      `let x = 1; switch (x) { case 1: let x = 2; }`,
      "script",
    );
    expect(mismatches).toEqual([]);
  });

  const corpus = corpusFiles();
  const SAMPLE_TARGET = Number(process.env.DIFFERENTIAL_SAMPLE ?? 600);
  const MISMATCH_SAMPLE_MAX = 12;
  const step = Math.max(1, Math.floor(corpus.length / SAMPLE_TARGET));

  test.skipIf(corpus.length === 0)("sampled corpus resolves identically", async () => {
    const mismatchSamples: string[] = [];
    let compared = 0;
    let declarations = 0;
    let filesCompared = 0;
    let unparsable = 0;
    for (let i = 0; i < corpus.length; i += step) {
      const file = corpus[i]!;
      // corpus paths use backslashes on windows
      if (KNOWN_DIVERGENCES.has(file.path.replaceAll("\\", "/"))) continue;
      const source = await Bun.file(file.path).text();
      // non-ascii offsets differ between bytes and UTF-16
      if (Buffer.byteLength(source) !== source.length) continue;
      let result;
      try {
        result = compare(file.path, source, file.sourceType, file.lang);
      } catch {
        unparsable++;
        continue;
      }
      filesCompared++;
      compared += result.compared;
      declarations += result.declarations;
      for (const mismatch of result.mismatches) {
        if (mismatchSamples.length < MISMATCH_SAMPLE_MAX) mismatchSamples.push(mismatch);
      }
    }
    console.log(
      `scope-manager differential: ${compared} references and ${declarations} declarations ` +
        `agreed across ${filesCompared} files (${unparsable} not parsable by typescript-estree)`,
    );
    expect(mismatchSamples).toEqual([]);
    expect(compared).toBeGreaterThan(1000);
    expect(declarations).toBeGreaterThan(1000);
  }, 240_000);
});
