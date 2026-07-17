// Compares type-space resolution against the TypeScript checker,
// which is the authority on TypeScript meaning rules.

import { describe, expect, test } from "bun:test";
import ts from "typescript";
import { Analyzer } from "yuku-analyzer";
import type { SourceLang } from "yuku-parser";
import { corpusFilesUnder } from "../corpus";

const UNRESOLVED = -1;

function tscResolutions(source: string, fileName: string): Map<number, number[]> {
  const sourceFile = ts.createSourceFile(
    fileName,
    source,
    ts.ScriptTarget.ESNext,
    true,
    fileName.endsWith(".tsx") ? ts.ScriptKind.TSX : ts.ScriptKind.TS,
  );
  const host: ts.CompilerHost = {
    getSourceFile: (name) => (name === fileName ? sourceFile : undefined),
    getDefaultLibFileName: () => "lib.d.ts",
    writeFile: () => {},
    getCurrentDirectory: () => "",
    getCanonicalFileName: (name) => name,
    useCaseSensitiveFileNames: () => true,
    getNewLine: () => "\n",
    fileExists: (name) => name === fileName,
    readFile: () => undefined,
  };
  const program = ts.createProgram([fileName], { noLib: true, noResolve: true }, host);
  const checker = program.getTypeChecker();

  const map = new Map<number, number[]>();
  (function visit(node: ts.Node): void {
    if (ts.isIdentifier(node)) {
      const starts: number[] = [];
      for (const decl of checker.getSymbolAtLocation(node)?.declarations ?? []) {
        const name = (decl as { name?: ts.Node }).name;
        if (name !== undefined && name.getSourceFile() === sourceFile) {
          starts.push(name.getStart(sourceFile));
        }
      }
      map.set(node.getStart(sourceFile), starts);
    }
    ts.forEachChild(node, visit);
  })(sourceFile);
  return map;
}

function compare(
  label: string,
  source: string,
  lang: SourceLang = "ts",
): { mismatches: string[]; compared: number } {
  const fileName = lang === "tsx" ? "input.tsx" : "input.ts";
  const module = new Analyzer().addFile(fileName, source, { sourceType: "module", lang });
  const tsc = tscResolutions(source, fileName);
  const enums = module.findAll("TSEnumDeclaration");
  const mismatches: string[] = [];
  let compared = 0;
  for (const reference of module.references) {
    if (!reference.inTypePosition) continue;
    const position = reference.node.start;
    const theirDefs = tsc.get(position);
    if (theirDefs === undefined) continue;
    const symbol = reference.symbol;
    const ourDef =
      symbol === null ? UNRESOLVED : Math.min(...symbol.declarations.map((d) => d.start));
    // tsc merges declarations, agreement is membership in its set
    const agree =
      ourDef === UNRESOLVED ? theirDefs.length === 0 : theirDefs.includes(ourDef);
    if (!agree) {
      // getSymbolAtLocation returns symbols the checker rejects with
      // wrong-space errors, unresolved with a name in some space is that
      if (ourDef === UNRESOLVED && module.resolve(reference.name, reference.scope, "any") !== null) {
        continue;
      }
      // merged enum declarations resolve members across blocks
      if (ourDef === UNRESOLVED && enums.some((e) => position >= e.start && position < e.end)) {
        continue;
      }
      mismatches.push(
        `${label} ref@${position}: yuku def@${ourDef}, tsc def@${theirDefs.join(",") || UNRESOLVED}`,
      );
    }
    compared++;
  }
  return { mismatches, compared };
}

describe("type-space resolution agrees with tsc", () => {
  const SNIPPETS: [name: string, source: string][] = [
    ["type alias reference", `type T = string; let x: T;`],
    [
      "a value binding does not shadow a type",
      `type T = string; function f() { const T = 1; let x: T; return T; }`,
    ],
    ["interface heritage", `interface A {} interface B extends A {}`],
    ["typeof resolves the value side", `const point = { x: 1 }; type P = typeof point;`],
    ["namespace qualifier", `namespace N { export type T = string; } let x: N.T;`],
    ["enum as a type and a qualifier", `enum E { a } let x: E; let y: E.a;`],
    ["type parameters and defaults", `type Box<T, U = T> = { v: T; u: U };`],
    ["infer binds in the conditional", `type El<T> = T extends (infer U)[] ? U : never;`],
    ["mapped type key", `type Keys<T> = { [K in keyof T]: K };`],
    ["class as a type", `class C {} let c: C;`],
    ["import binding in a type", `import type { A } from "m"; let x: A;`],
    ["type predicate parameter", `function isS(v: unknown): v is string { return true; }`],
    ["generic function annotations", `function id<T>(v: T): T { return v; }`],
    ["shadowed type parameter", `type T = number; class C<T> { v: T; }`],
    [
      "class type parameters are out of scope in static members",
      `class C<T> { m(v: T): T { return v } static s(): T { return null as T } }`,
    ],
    [
      "class type parameters are out of scope in computed keys",
      `declare function k<X>(): string; class C<T> { [k<T>()]() {} m(v: T) {} }`,
    ],
    ["infer is visible in the true branch only", `type R<T> = T extends (infer U)[] ? U : U;`],
    ["unresolved stays unresolved", `let x: Missing;`],
  ];

  for (const [name, source] of SNIPPETS) {
    test(name, () => {
      const { mismatches, compared } = compare(name, source);
      expect(mismatches).toEqual([]);
      expect(compared).toBeGreaterThan(0);
    });
  }

  const corpus = corpusFilesUnder("test/parser/suite/ts/pass");
  const SAMPLE_TARGET = Number(process.env.TSC_DIFFERENTIAL_SAMPLE ?? 150);
  const MISMATCH_SAMPLE_MAX = 12;
  const step = Math.max(1, Math.floor(corpus.length / SAMPLE_TARGET));

  test.skipIf(corpus.length === 0)("sampled ts corpus resolves identically", async () => {
    const mismatchSamples: string[] = [];
    let compared = 0;
    let filesCompared = 0;
    for (let i = 0; i < corpus.length; i += step) {
      const file = corpus[i]!;
      if (KNOWN_DIVERGENCES.has(file.path)) continue;
      const source = await Bun.file(file.path).text();
      // non-ascii offsets differ between bytes and UTF-16
      if (Buffer.byteLength(source) !== source.length) continue;
      const result = compare(file.path, source, file.lang);
      filesCompared++;
      compared += result.compared;
      for (const mismatch of result.mismatches) {
        if (mismatchSamples.length < MISMATCH_SAMPLE_MAX) mismatchSamples.push(mismatch);
      }
    }
    console.log(`tsc differential: ${compared} type references agreed across ${filesCompared} files`);
    expect(mismatchSamples).toEqual([]);
    expect(compared).toBeGreaterThan(100);
  }, 600_000);
});

// Triaged divergences. Mostly namespace bodies and aliases merging
// across declare module blocks, which tsc resolves between blocks.
// Two yuku gaps: typeof of a signature-type parameter label, and infer
// shadowing an outer type parameter across nested conditionals. One
// tsc limit: getSymbolAtLocation returns nothing in JSX type arguments.
const KNOWN_DIVERGENCES = new Set(
  [
    "07165d29762103ba.ts",
    "0a4086de64f759e6.ts",
    "19ccfc7b636f7919.ts",
    "1af3dcf81f26daee.ts",
    "1b3a909a7815cda3.ts",
    "233e2d9059f64ac6.ts",
    "236bce9a7054558e.ts",
    "254e4b2327e2d9d4.ts",
    "343c11f6a5f686f7.ts",
    "3a66bcb0ff2adb2c.module.ts",
    "3e4f59684f533541.ts",
    "589ae58b40931bd4.ts",
    "5aa19aef1fb64e43.ts",
    "5b34f29e3a099c02.ts",
    "5cc943c9605bb9a0.ts",
    "5fab12c3da9e3a70.ts",
    "6036f7fdefa93a69.ts",
    "60e70e729005101f.ts",
    "644fb9551c9a9679.ts",
    "67783a94665c9385.ts",
    "6b163bc52d21bf7e.ts",
    "7a3c907379ccee04.ts",
    "7abadbdb73780802.ts",
    "853d3a648c303cca.ts",
    "872d1ece04ec2bd2.ts",
    "8ce86193a732801b.ts",
    "8d1f753780e6f4b6.module.ts",
    "93b3aa21755294a6.ts",
    "990149b3c230f0ac.ts",
    "ac375a0163d54380.ts",
    "ace124dedd033d76.ts",
    "af44b354f11da7ea.ts",
    "b0511888917d9e08.module.ts",
    "c536dc0c8427a171.ts",
    "c70923f25132380e.ts",
    "cdb3932652e8bea5.ts",
    "d3601d7911a8e96e.ts",
    "d6e4a578a83afa49.ts",
    "d761fdeea8ab3049.ts",
    "e16bd3fcce61a2e8.ts",
    "e4ccf9bf028d7e4c.ts",
    "e79056dee69a8570.ts",
    "e9ad3debe1501dc6.module.ts",
    "f21309e4f1ed06c8.ts",
    "f51bc2d6f2f77a88.ts",
    "f948e2f4fdbf827c.ts",
    "f9a6d4f7f53c8135.ts",
    "fba9a71160c95c6a.ts",
    "fe6dd15e338803c5.tsx",
  ].map((name) => `test/parser/suite/ts/pass/${name}`),
);
