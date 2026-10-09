// Compares every reference's binding with the one the TypeScript checker resolves.

import { describe, expect, test } from "bun:test";
import ts from "typescript";
import { analyze } from "yuku-analyzer";
import type { SourceLang, SourceType } from "yuku-parser";
import { corpusFiles, projectFiles, type CorpusFile } from "../corpus";
import { differential, type Comparison, type Known } from "./utils/differential";

const FILE_NAMES: Record<SourceLang, string> = {
  js: "input.js",
  jsx: "input.jsx",
  ts: "input.ts",
  tsx: "input.tsx",
  dts: "input.d.ts",
};

const SCRIPT_KINDS: Record<SourceLang, ts.ScriptKind> = {
  js: ts.ScriptKind.JS,
  jsx: ts.ScriptKind.JSX,
  ts: ts.ScriptKind.TS,
  tsx: ts.ScriptKind.TSX,
  dts: ts.ScriptKind.TS,
};

interface Checker {
  /** Declaration starts, or null to skip. */
  resolve(position: number): number[] | null;
  /** Whether tsc reports an error naming `name` at a position. */
  rejects(name: string, positions: number[]): boolean;
  resolvedUses(): [position: number, name: string][];
}

const { getSetExternalModuleIndicator } = ts as unknown as {
  getSetExternalModuleIndicator(options: ts.CompilerOptions): (file: ts.SourceFile) => void;
};

function checker(source: string, lang: SourceLang, sourceType: SourceType): Checker {
  const fileName = FILE_NAMES[lang];
  const options: ts.CompilerOptions = {
    noLib: true,
    noResolve: true,
    allowJs: true,
    moduleDetection:
      sourceType === "module" ? ts.ModuleDetectionKind.Force : ts.ModuleDetectionKind.Legacy,
  };
  const file = ts.createSourceFile(
    fileName,
    source,
    {
      languageVersion: ts.ScriptTarget.ESNext,
      setExternalModuleIndicator: getSetExternalModuleIndicator(options),
    },
    true,
    SCRIPT_KINDS[lang],
  );
  const host: ts.CompilerHost = {
    getSourceFile: (name) => (name === fileName ? file : undefined),
    getDefaultLibFileName: () => "lib.d.ts",
    writeFile: () => {},
    getCurrentDirectory: () => "",
    getCanonicalFileName: (name) => name,
    useCaseSensitiveFileNames: () => true,
    getNewLine: () => "\n",
    fileExists: (name) => name === fileName,
    readFile: () => undefined,
  };
  const program = ts.createProgram([fileName], options, host);
  const typeChecker = program.getTypeChecker();

  const identifiers = new Map<number, ts.Identifier>();
  (function visit(node: ts.Node): void {
    if (ts.isIdentifier(node)) identifiers.set(node.getStart(file), node);
    ts.forEachChild(node, visit);
  })(file);

  let errors: ts.Diagnostic[] | null = null;

  return {
    resolve(position) {
      const node = identifiers.get(position);
      if (node === undefined || insideWith(node)) return null;
      const starts: number[] = [];
      for (const declaration of symbolOf(typeChecker, node)?.declarations ?? []) {
        if (isAssignmentDeclaration(declaration)) continue;
        const name = ts.getNameOfDeclaration(declaration);
        if (name?.getSourceFile() === file) starts.push(name.getStart(file));
      }
      return starts;
    },
    rejects(name, positions) {
      errors ??= [
        ...program.getSyntacticDiagnostics(file),
        ...program.getSemanticDiagnostics(file),
      ];
      const quoted = `'${name}'`;
      return errors.some(({ start, length, messageText }) => {
        if (start === undefined || length === undefined) return false;
        if (!ts.flattenDiagnosticMessageText(messageText, "\n").includes(quoted)) return false;
        return positions.some((position) => position >= start && position < start + length);
      });
    },
    resolvedUses() {
      const uses: [position: number, name: string][] = [];
      for (const [position, node] of identifiers) {
        if (!isScopeLookup(node)) continue;
        const local = (symbolOf(typeChecker, node)?.declarations ?? []).filter(
          (declaration) =>
            !isAssignmentDeclaration(declaration) &&
            ts.getNameOfDeclaration(declaration)?.getSourceFile() === file,
        );
        if (local.some((declaration) => ts.getNameOfDeclaration(declaration) === node)) continue;
        if (local.length > 0) uses.push([position, node.text]);
      }
      return uses;
    },
  };
}

function symbolOf(typeChecker: ts.TypeChecker, node: ts.Identifier): ts.Symbol | undefined {
  const parent = node.parent;
  if (ts.isShorthandPropertyAssignment(parent) && parent.name === node) {
    return typeChecker.getShorthandAssignmentValueSymbol(parent);
  }
  if (ts.isExportSpecifier(parent) && !parent.parent.parent.moduleSpecifier) {
    return typeChecker.getExportSpecifierLocalTargetSymbol(parent);
  }
  if (inClassExtends(node)) {
    return typeChecker
      .getSymbolsInScope(node, ts.SymbolFlags.Value | ts.SymbolFlags.Alias)
      .find((symbol) => symbol.name === node.text);
  }
  return typeChecker.getSymbolAtLocation(node);
}

// `X.y = 1`, a declaration to tsc's JavaScript binder
function isAssignmentDeclaration(declaration: ts.Declaration): boolean {
  return (
    ts.isIdentifier(declaration) ||
    ts.isBinaryExpression(declaration) ||
    ts.isPropertyAccessExpression(declaration) ||
    ts.isElementAccessExpression(declaration) ||
    ts.isCallExpression(declaration)
  );
}

function isScopeLookup(node: ts.Identifier): boolean {
  if (node.text === "this" || insideWith(node)) return false;
  let entity: ts.Node = node;
  while (ts.isQualifiedName(entity.parent) && entity.parent.left === entity) entity = entity.parent;
  if (ts.isImportTypeNode(entity.parent) && entity.parent.qualifier === entity) return false;
  const parent = node.parent;
  if (ts.isPropertyAccessExpression(parent)) return parent.name !== node;
  if (ts.isQualifiedName(parent)) return parent.right !== node;
  if (ts.isBindingElement(parent) || ts.isImportSpecifier(parent)) {
    return parent.propertyName !== node;
  }
  if (ts.isExportSpecifier(parent)) {
    if (parent.parent.parent.moduleSpecifier !== undefined) return false;
    return parent.propertyName === undefined || parent.propertyName === node;
  }
  if (ts.isJsxOpeningLikeElement(parent) || ts.isJsxClosingElement(parent)) {
    return parent.tagName !== node || !/^[a-z]|-/.test(node.text);
  }
  if (
    ts.isPropertyAssignment(parent) ||
    ts.isPropertyDeclaration(parent) ||
    ts.isPropertySignature(parent) ||
    ts.isMethodDeclaration(parent) ||
    ts.isMethodSignature(parent) ||
    ts.isAccessor(parent) ||
    ts.isEnumMember(parent)
  ) {
    return parent.name !== node;
  }
  return !(
    ts.isLabeledStatement(parent) ||
    ts.isBreakOrContinueStatement(parent) ||
    ts.isMetaProperty(parent) ||
    ts.isNamespaceExportDeclaration(parent) ||
    ts.isJsxNamespacedName(parent) ||
    ts.isJsxAttribute(parent) ||
    ts.isImportAttribute(parent)
  );
}

function insideWith(node: ts.Node): boolean {
  for (let current = node; current.parent !== undefined; current = current.parent) {
    if (ts.isWithStatement(current.parent) && current.parent.statement === current) return true;
  }
  return false;
}

// `a` in `class C extends a.b.c`
function inClassExtends(node: ts.Identifier): boolean {
  let current: ts.Node = node;
  while (ts.isPropertyAccessExpression(current.parent) && current.parent.expression === current) {
    current = current.parent;
  }
  const clause = current.parent?.parent;
  return (
    current !== node &&
    ts.isExpressionWithTypeArguments(current.parent) &&
    clause !== undefined &&
    ts.isHeritageClause(clause) &&
    clause.token === ts.SyntaxKind.ExtendsKeyword &&
    ts.isClassLike(clause.parent)
  );
}

function compare(
  source: string,
  lang: SourceLang,
  sourceType: SourceType = "module",
  path = FILE_NAMES[lang],
): Comparison {
  const module = analyze(source, { path, lang, sourceType });
  const tsc = checker(source, lang, sourceType);
  const mismatches: string[] = [];
  let compared = 0;
  for (const reference of module.references) {
    const position = reference.node.start;
    const theirs = tsc.resolve(position);
    if (theirs === null) continue;
    compared++;
    const ours = reference.binding?.declarations.map((declaration) => declaration.start) ?? [];
    const agree = ours.length === 0 ? theirs.length === 0 : ours.some((d) => theirs.includes(d));
    if (agree || tsc.rejects(reference.name, [position, ...ours, ...theirs])) continue;
    const yuku = ours.join(",") || "unresolved";
    const expected = theirs.join(",") || "unresolved";
    mismatches.push(`${reference.name}@${position}: yuku ${yuku}, tsc ${expected}`);
  }
  const recorded = new Set([
    ...module.references.map((reference) => reference.node.start),
    ...module.bindings.flatMap((binding) => binding.declarations.map((node) => node.start)),
  ]);
  for (const [position, name] of tsc.resolvedUses()) {
    compared++;
    if (!recorded.has(position)) mismatches.push(`${name}@${position}: no yuku reference`);
  }
  return { compared, mismatches };
}

function compareFile(file: CorpusFile, source: string): Comparison {
  return compare(source, file.lang, file.sourceType, file.path);
}

const SUITE = "test/parser/suite/ts/pass";
const PROJECTS = "test/projects";

const KNOWN: Known = {
  "tsc parses `A extends (x: B extends C ? D : E) => 0 ? F : G` differently": {
    [`${SUITE}/7abadbdb73780802.ts`]: ["D@42: yuku unresolved, tsc 42"],
  },
  "tsc resolves a `require` binding to its first block, not its own": {
    [`${PROJECTS}/webpack/lib/config/WebpackOptionsApply.js`]: [
      "ExternalsPlugin@5127: yuku 5059, tsc 4299",
      "ExternalsPlugin@7512: yuku 5915, tsc 4299",
      "ExternalsPlugin@9524: yuku 9457, tsc 4299",
      "ElectronTargetPlugin@11488: yuku 11411, tsc 11139",
      "ElectronTargetPlugin@11764: yuku 11687, tsc 11139",
      "ElectronTargetPlugin@12146: yuku 12069, tsc 11139",
      "ExternalsPlugin@12401: yuku 12334, tsc 4299",
      "OccurrenceChunkIdsPlugin@30080: yuku 29999, tsc 29704",
      "MemoryCachePlugin@42338: yuku 42268, tsc 41401",
      "MemoryWithGcCachePlugin@42607: yuku 42525, tsc 41071",
    ],
    [`${PROJECTS}/webpack/lib/javascript/EnableChunkLoadingPlugin.js`]: [
      "CommonJsChunkLoadingPlugin@3208: yuku 3122, tsc 2819",
    ],
    [`${PROJECTS}/webpack/lib/library/EnableLibraryPlugin.js`]: [
      "AssignLibraryPlugin@3439: yuku 3373, tsc 3046",
      "AssignLibraryPlugin@3776: yuku 3710, tsc 3046",
      "AssignLibraryPlugin@4090: yuku 4024, tsc 3046",
      "AssignLibraryPlugin@4411: yuku 4345, tsc 3046",
      "AssignLibraryPlugin@4732: yuku 4666, tsc 3046",
      "AssignLibraryPlugin@5053: yuku 4987, tsc 3046",
      "AssignLibraryPlugin@5376: yuku 5310, tsc 3046",
      "AssignLibraryPlugin@5709: yuku 5643, tsc 3046",
      "AssignLibraryPlugin@6066: yuku 6000, tsc 3046",
    ],
  },
  "tsc resolves a computed method key in the method's own scope": {
    [`${PROJECTS}/node/lib/internal/encoding.js`]: [
      "inspect@16860: yuku 1068, tsc 17343",
    ],
  },
  "tsc binds `module.exports =` as an export even where `module` is a parameter": {
    [`${PROJECTS}/node/lib/internal/modules/esm/translators.js`]: [
      "module@15256: yuku 14251, tsc unresolved",
    ],
  },
};

const SNIPPETS: [name: string, source: string, lang?: SourceLang][] = [
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
  ["unresolved stays unresolved", `let x: Missing;`],
  [
    "namespace blocks share their exports, not their locals",
    `namespace N { export var x = 1; var y = 2; } namespace N { x; y; }`,
  ],
  [
    "a dotted namespace declares each part",
    `namespace A.B.C { export var v = 1; A; B; C; } namespace A.B { C.v; }`,
  ],
  ["enum blocks share their members", `enum E { a } enum E { b = a }`],
  [
    "the blocks of one ambient module share their exports",
    `declare module "m" { export var x: number; } declare module "m" { let y: typeof x; }`,
  ],
  [
    "a merged enum and namespace share nothing",
    `enum F { b } namespace F { export var x = b }
    namespace G { export let y = 1 } enum G { z = y }`,
  ],
  [
    "an ambient namespace exports every declaration",
    `declare namespace N { var x: number; class C {} }
    declare namespace N { let y: typeof x; let c: C; }`,
  ],
  [
    "a namespace of ambient values or const enums is a value",
    `namespace M { export declare var n: any; }
    namespace E { export const enum K { X } } ~M; E.K.X;`,
  ],
  [
    "signature parameters bind in the signature",
    `type F = (x: number) => typeof x; interface I { m(a: string): typeof a; }
    type M = { [s: string]: typeof s };`,
  ],
  [
    "infer belongs to the conditional whose extends clause holds it",
    `type X<U, T> = T extends (infer U extends number ? U : T) ? U : T;`,
  ],
  [
    "an import alias target resolves as a namespace",
    `namespace A { export namespace X {} } namespace M { var A = 2; import Z = A.X; }`,
  ],
  [
    "a computed key in a type is a value",
    `var obj = { c: "k" }; const sym = Symbol(); interface I { [obj.c]: number }
    type F = ({ [sym]: s }: object) => void;`,
  ],
  [
    "an interface hides its type parameters from computed keys",
    `declare function foo<T>(): string; interface I<T> { [foo<T>()](): void; }`,
  ],
  [
    "a heritage qualifier is a namespace",
    `namespace N { export interface I {} } class C implements N.I {}`,
  ],
  [
    "a class extends expression does not see its type parameters",
    `declare function base<T>(): any;
    class Gen<T> extends base<T>() {} class Ok<T> extends Array<T> {}`,
  ],
  [
    "a signature sees neither its parameters from its type parameters nor its body vars",
    `function f<T extends typeof a>(a: T) {}
    function g(p: typeof b): typeof b { var b = 1; return b; }`,
  ],
  [
    "a type parameter merged with a parameter stays visible",
    `declare function f<Foo extends Bar, Bar>(Bar: any): void`,
  ],
  [
    "declare global declares globally",
    `export {}; declare global { interface Box<T> {} } function f<T>(p: Box<T>) {}`,
  ],
  [
    "a member decorator sees the class type parameters and name",
    `declare function y(a: any): any; type T = number;
    class C<T> { m(@y(null as T) y: number) {} } const D = class E { @y(E) m() {} };`,
  ],
  [
    "a UMD global names the module in its own declarations",
    `export as namespace lib; export interface P {} export interface C { p: lib.P }`,
    "dts",
  ],
  [
    "a bodiless function merges with a class",
    `declare function B(p: string): B; declare class B {} function F(): F; class F {}`,
  ],
];

describe("resolution agrees with tsc", () => {
  for (const [name, source, lang = "ts"] of SNIPPETS) {
    test(name, () => {
      const { mismatches, compared } = compare(source, lang);
      expect(mismatches).toEqual([]);
      expect(compared).toBeGreaterThan(0);
    });
  }

  differential("corpus and projects", [...corpusFiles(), ...projectFiles()], compareFile, KNOWN);
});
