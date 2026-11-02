// import and export records and binding flags against tsc's syntax tree and symbols

import { describe, expect, test } from "bun:test";
import ts from "typescript";
import { type Binding, BindingFlags, parse, type SourceLang, type SourceType } from "yuku";
import { corpusAndProjectFiles, type CorpusFile } from "../corpus";
import {
  differential,
  FILE_NAMES,
  SCRIPT_KINDS,
  singleFileProgram,
  type Comparison,
  type Known,
} from "./utils/differential";

const F = ts.SymbolFlags;

const { getRootDeclaration } = ts as unknown as {
  getRootDeclaration(node: ts.Node): ts.Node;
};
const AMBIENT = (ts.NodeFlags as unknown as { Ambient: number }).Ambient;

const row = (...parts: unknown[]) => JSON.stringify(parts);

function has(node: ts.Node, kind: ts.ModifierSyntaxKind): boolean {
  return ts.canHaveModifiers(node) && (ts.getModifiers(node) ?? []).some((m) => m.kind === kind);
}

function literalOf(node: ts.Expression | undefined): string | null {
  while (node !== undefined && ts.isParenthesizedExpression(node)) node = node.expression;
  return node !== undefined && ts.isStringLiteralLike(node) ? node.text : null;
}

function isModuleSyntax(statement: ts.Statement): boolean {
  if (ts.isImportEqualsDeclaration(statement)) {
    return ts.isExternalModuleReference(statement.moduleReference);
  }
  return (
    ts.isImportDeclaration(statement) ||
    ts.isExportDeclaration(statement) ||
    ts.isExportAssignment(statement) ||
    has(statement, ts.SyntaxKind.ExportKeyword)
  );
}

function namesOf(name: ts.BindingName): string[] {
  if (ts.isIdentifier(name)) return [name.text];
  return name.elements.flatMap((element) =>
    ts.isBindingElement(element) ? namesOf(element.name) : [],
  );
}

function declaredNames(statement: ts.Statement): string[] {
  if (ts.isVariableStatement(statement)) {
    return statement.declarationList.declarations.flatMap((d) => namesOf(d.name));
  }
  if (ts.isModuleDeclaration(statement)) {
    const global = (statement.flags & ts.NodeFlags.GlobalAugmentation) !== 0;
    return ts.isIdentifier(statement.name) && !global ? [statement.name.text] : [];
  }
  if (
    ts.isFunctionDeclaration(statement) ||
    ts.isClassDeclaration(statement) ||
    ts.isInterfaceDeclaration(statement) ||
    ts.isTypeAliasDeclaration(statement) ||
    ts.isEnumDeclaration(statement) ||
    ts.isImportEqualsDeclaration(statement)
  ) {
    return statement.name === undefined ? [""] : [statement.name.text];
  }
  return [];
}

function exportRows(tree: ts.SourceFile, implicit: boolean): string[] {
  const rows: string[] = [];
  for (const statement of tree.statements) {
    if (ts.isExportDeclaration(statement)) {
      const specifier = literalOf(statement.moduleSpecifier);
      const clause = statement.exportClause;
      const typeOnly = statement.isTypeOnly;
      if (clause === undefined) rows.push(row("star", null, null, specifier, typeOnly));
      else if (ts.isNamespaceExport(clause)) {
        rows.push(row("namespace", clause.name.text, null, specifier, typeOnly));
      } else {
        for (const element of clause.elements) {
          const from = specifier === null ? null : (element.propertyName ?? element.name).text;
          const kind = specifier === null ? "named" : "reExport";
          rows.push(row(kind, element.name.text, from, specifier, typeOnly || element.isTypeOnly));
        }
      }
    } else if (ts.isExportAssignment(statement)) {
      const equals = statement.isExportEquals === true;
      rows.push(row(equals ? "equals" : "named", equals ? null : "default", null, null, false));
    } else if (ts.isNamespaceExportDeclaration(statement)) {
      rows.push(row("global", statement.name.text, null, null, false));
    } else {
      const exported = has(statement, ts.SyntaxKind.ExportKeyword);
      if (!exported && !implicit) continue;
      const isDefault = has(statement, ts.SyntaxKind.DefaultKeyword);
      const typeOnly =
        ts.isInterfaceDeclaration(statement) ||
        ts.isTypeAliasDeclaration(statement) ||
        has(statement, ts.SyntaxKind.DeclareKeyword) ||
        !exported ||
        (ts.isImportEqualsDeclaration(statement) && statement.isTypeOnly);
      for (const name of declaredNames(statement)) {
        rows.push(row("named", isDefault ? "default" : name, null, null, typeOnly));
      }
    }
  }
  return rows;
}

function importRows(tree: ts.SourceFile, checker: ts.TypeChecker, isModule: boolean): string[] {
  const rows: string[] = [];
  for (const statement of tree.statements) {
    if (ts.isImportDeclaration(statement)) {
      const specifier = literalOf(statement.moduleSpecifier);
      const clause = statement.importClause;
      const typeOnly = clause?.isTypeOnly ?? false;
      const phase = clause?.phaseModifier === ts.SyntaxKind.DeferKeyword ? "defer" : null;
      const named = clause?.namedBindings;
      if (clause?.name !== undefined) {
        rows.push(row("named", "default", clause.name.text, specifier, typeOnly, phase));
      }
      if (named !== undefined && ts.isNamespaceImport(named)) {
        rows.push(row("namespace", null, named.name.text, specifier, typeOnly, phase));
      }
      if (named !== undefined && ts.isNamedImports(named)) {
        for (const element of named.elements) {
          const imported = (element.propertyName ?? element.name).text;
          const only = typeOnly || element.isTypeOnly;
          rows.push(row("named", imported, element.name.text, specifier, only, phase));
        }
      }
      const binds = clause?.name !== undefined || (named !== undefined && !isEmpty(named));
      if (!binds) rows.push(row("sideEffect", null, null, specifier, typeOnly, phase));
    } else if (ts.isImportEqualsDeclaration(statement)) {
      const reference = statement.moduleReference;
      if (!ts.isExternalModuleReference(reference)) continue;
      const specifier = literalOf(reference.expression);
      rows.push(
        row("importEquals", null, statement.name.text, specifier, statement.isTypeOnly, null),
      );
    } else if (
      ts.isExportDeclaration(statement) &&
      statement.moduleSpecifier !== undefined &&
      statement.exportClause !== undefined &&
      isEmpty(statement.exportClause)
    ) {
      const specifier = literalOf(statement.moduleSpecifier);
      rows.push(row("sideEffect", null, null, specifier, statement.isTypeOnly, null));
    } else if (
      isModule &&
      ts.isModuleDeclaration(statement) &&
      ts.isStringLiteralLike(statement.name) &&
      statement.body !== undefined
    ) {
      rows.push(row("augmentation", null, null, statement.name.text, true, null));
    }
  }
  (function visit(node: ts.Node): void {
    if (ts.isCallExpression(node)) {
      const specifier = node.arguments.length > 0 ? literalOf(node.arguments[0]) : null;
      const callee = node.expression;
      if (specifier !== null && callee.kind === ts.SyntaxKind.ImportKeyword) {
        rows.push(row("dynamic", null, null, specifier, false, null));
      } else if (specifier !== null && ts.isMetaProperty(callee)) {
        rows.push(row("dynamic", null, null, specifier, false, callee.name.text));
      } else if (
        specifier !== null &&
        node.arguments.length === 1 &&
        ts.isIdentifier(callee) &&
        callee.text === "require"
      ) {
        const declarations = checker.getSymbolAtLocation(callee)?.declarations ?? [];
        if (!declarations.some((declaration) => declaration.getSourceFile() === tree)) {
          rows.push(row("require", null, null, specifier, false, null));
        }
      }
    }
    ts.forEachChild(node, visit);
  })(tree);
  return rows;
}

function isEmpty(bindings: ts.NamedImportBindings | ts.NamedExportBindings): boolean {
  return (ts.isNamedImports(bindings) || ts.isNamedExports(bindings)) && !bindings.elements.length;
}

function usesImportMeta(tree: ts.SourceFile): boolean {
  let found = false;
  (function visit(node: ts.Node): void {
    if (ts.isMetaProperty(node) && node.keywordToken === ts.SyntaxKind.ImportKeyword) {
      found ||= node.name.text === "meta";
    }
    if (!found) ts.forEachChild(node, visit);
  })(tree);
  return found;
}

function isBodiless(node: ts.Node): boolean {
  if (
    ts.isFunctionDeclaration(node) ||
    ts.isMethodDeclaration(node) ||
    ts.isConstructorDeclaration(node) ||
    (ts.isAccessor(node) && ts.isClassLike(node.parent))
  ) {
    return node.body === undefined;
  }
  return false;
}

function isAmbient(declaration: ts.Declaration): boolean {
  if ((declaration.flags & AMBIENT) !== 0) return true;
  for (let node: ts.Node = declaration; !ts.isSourceFile(node); node = node.parent) {
    if (isBodiless(node)) return true;
    if (ts.isBlock(node) || ts.isClassLike(node)) return false;
  }
  return false;
}

function inGlobal(declaration: ts.Declaration): boolean {
  for (let node: ts.Node = declaration; !ts.isSourceFile(node); node = node.parent) {
    if (ts.isModuleDeclaration(node) && (node.flags & ts.NodeFlags.GlobalAugmentation) !== 0) {
      return true;
    }
  }
  return false;
}

function isCatchVariable(declaration: ts.Declaration): boolean {
  const root = getRootDeclaration(declaration);
  return ts.isVariableDeclaration(root) && ts.isCatchClause(root.parent);
}

function isConstVariable(declaration: ts.Declaration): boolean {
  if (!ts.isVariableDeclaration(declaration) && !ts.isBindingElement(declaration)) return false;
  const flags = ts.getCombinedNodeFlags(declaration);
  return (flags & ts.NodeFlags.BlockScoped) !== 0 && (flags & ts.NodeFlags.Let) === 0;
}

// the declarations of a name both local and exported sit on its local symbol, their flags apart
function exportSymbolOf(symbol: ts.Symbol): ts.Symbol | undefined {
  return (symbol as unknown as { exportSymbol?: ts.Symbol }).exportSymbol;
}

function isExportedSymbol(symbol: ts.Symbol): boolean {
  if (exportSymbolOf(symbol) !== undefined) return true;
  const parent = (symbol as unknown as { parent?: ts.Symbol }).parent;
  if (parent === undefined || (parent.flags & (F.ValueModule | F.NamespaceModule)) === 0) {
    return false;
  }
  return (symbol.flags & (F.EnumMember | F.TypeParameter)) === 0;
}

// a catch variable is function scoped by Yuku's definition and block scoped by tsc's
function expectedFlags(symbol: ts.Symbol): Record<string, boolean> {
  const declarations = symbol.declarations ?? [];
  const inCatch = declarations.some(isCatchVariable);
  const isModule = declarations.some(ts.isModuleDeclaration);
  const flags = symbol.flags | (exportSymbolOf(symbol)?.flags ?? 0);
  const flag = (mask: number) => (flags & mask) !== 0;
  return {
    FunctionScopedVariable: flag(F.FunctionScopedVariable) || inCatch,
    BlockScopedVariable: flag(F.BlockScopedVariable) && !inCatch,
    Function: flag(F.Function),
    Class: flag(F.Class),
    RegularEnum: flag(F.RegularEnum),
    ConstEnum: flag(F.ConstEnum),
    ValueModule: flag(F.ValueModule) && isModule,
    Interface: flag(F.Interface),
    TypeAlias: flag(F.TypeAlias),
    TypeParameter: flag(F.TypeParameter),
    NamespaceModule: isModule,
    EnumMember: flag(F.EnumMember),
    Import: flag(F.Alias),
    Parameter: declarations.some((d) => ts.isParameter(getRootDeclaration(d))),
    CatchVariable: inCatch,
    Const: declarations.some(isConstVariable),
    Ambient: declarations.length > 0 && declarations.every(isAmbient),
    Default: declarations.some((d) => has(d, ts.SyntaxKind.DefaultKeyword)),
  };
}

function nameStarts(tree: ts.SourceFile, declarations: readonly ts.Declaration[]): number[] {
  const starts: number[] = [];
  for (const declaration of declarations) {
    const name = ts.getNameOfDeclaration(declaration);
    if (name !== undefined && name.getSourceFile() === tree) starts.push(name.getStart(tree));
  }
  return starts;
}

function compareRows(
  subject: string,
  ours: string[],
  theirs: string[],
  mismatches: string[],
): number {
  const counts = new Map<string, number>();
  for (const entry of ours) counts.set(entry, (counts.get(entry) ?? 0) + 1);
  for (const entry of theirs) counts.set(entry, (counts.get(entry) ?? 0) - 1);
  for (const [entry, count] of counts) {
    if (count > 0) mismatches.push(`${subject} yuku only: ${entry}`);
    if (count < 0) mismatches.push(`${subject} tsc only: ${entry}`);
  }
  return Math.max(ours.length, theirs.length);
}

function compare(
  source: string,
  lang: SourceLang,
  sourceType: SourceType = "module",
  path = FILE_NAMES[lang],
): Comparison | null {
  const { program, file: tree } = singleFileProgram(
    { name: FILE_NAMES[lang], source, kind: SCRIPT_KINDS[lang], sourceType },
    { allowJs: true },
  );
  // tsc reads a different program from the file
  if (program.getSyntacticDiagnostics(tree).length > 0) return null;
  const checker = program.getTypeChecker();
  const file = parse(source, { analyze: true, path, lang, sourceType });
  const mismatches: string[] = [];
  const hasSyntax = tree.statements.some(isModuleSyntax);
  const implicit =
    lang === "dts" &&
    sourceType === "module" &&
    hasSyntax &&
    !tree.statements.some((s) => ts.isExportDeclaration(s) || ts.isExportAssignment(s));

  let compared = compareRows(
    "export",
    file.exports.map((e) => row(e.kind, e.name, e.fromName, e.specifier, e.typeOnly)),
    exportRows(tree, implicit),
    mismatches,
  );
  compared += compareRows(
    "import",
    // tsc does not model the source phase of a static import
    file.imports.map((i) =>
      row(
        i.kind,
        i.name,
        i.local?.name ?? null,
        i.specifier,
        i.typeOnly,
        i.kind !== "dynamic" && i.phase === "source" ? null : i.phase,
      ),
    ),
    importRows(tree, checker, hasSyntax || file.moduleFlags.usesImportMeta),
    mismatches,
  );

  compared++;
  if (file.moduleFlags.usesImportMeta !== usesImportMeta(tree)) {
    mismatches.push(`usesImportMeta: yuku ${file.moduleFlags.usesImportMeta}`);
  }

  const moduleSymbol = checker.getSymbolAtLocation(tree);
  const flags = file.moduleFlags;
  const byAssignment =
    sourceType === "commonjs" ||
    flags.usesModule ||
    flags.usesExports ||
    file.exports.some((record) => record.kind === "equals");
  if (moduleSymbol !== undefined && !byAssignment) {
    const exports = checker
      .getExportsOfModule(moduleSymbol)
      .filter((symbol) =>
        (symbol.declarations ?? []).some((d) => (d.flags & ts.NodeFlags.JSDoc) === 0),
      );
    const records = file.exports.filter(
      (record) => record.kind !== "global" && record.kind !== "star",
    );
    compared += compareRows(
      "exported name",
      [...new Set(records.flatMap((record) => record.name ?? []))],
      exports.map((symbol) => symbol.name).filter((name) => name !== "export="),
      mismatches,
    );
    for (const record of records) {
      if (record.kind !== "named" || record.name === null) continue;
      const exported = exports.find((symbol) => symbol.name === record.name);
      if (exported === undefined) continue;
      const specifier = exported.declarations?.find(ts.isExportSpecifier);
      const assignment = exported.declarations?.find(ts.isExportAssignment);
      const target =
        specifier !== undefined
          ? checker.getExportSpecifierLocalTargetSymbol(specifier)
          : assignment !== undefined
            ? ts.isIdentifier(assignment.expression)
              ? checker.getSymbolAtLocation(assignment.expression)
              : undefined
            : exported;
      const theirs = nameStarts(tree, target?.declarations ?? []);
      const ours = record.local?.declarations.map((node) => node.start) ?? [];
      const statement = record.node as { declaration?: { id?: unknown } };
      if (record.local === null && statement.declaration?.id === null) continue;
      compared++;
      if (ours.length === 0 ? theirs.length > 0 : !ours.some((s) => theirs.includes(s))) {
        mismatches.push(`export ${record.name} local: yuku ${ours}, tsc ${theirs}`);
      }
    }
  }

  const identifiers = new Map<number, ts.Identifier>();
  (function visit(node: ts.Node): void {
    if (ts.isIdentifier(node)) identifiers.set(node.getStart(tree), node);
    ts.forEachChild(node, visit);
  })(tree);

  for (const binding of file.bindings) {
    const symbol = symbolOf(binding, identifiers, checker, tree, lang);
    if (symbol === undefined) continue;
    const expected = expectedFlags(symbol);
    const where = `${binding.name}@${binding.declarations[0]!.start}`;
    for (const [name, value] of Object.entries(expected)) {
      compared++;
      const mask = BindingFlags[name as keyof typeof BindingFlags];
      if (binding.has(mask) !== value) mismatches.push(`${where} ${name}: yuku ${!value}`);
    }
    const declarations = symbol.declarations ?? [];
    if (declarations.some(inGlobal)) continue;
    compared++;
    if (binding.has(BindingFlags.Exported) !== isExportedSymbol(symbol)) {
      mismatches.push(`${where} Exported: yuku ${!isExportedSymbol(symbol)}`);
    }
  }
  return { compared, mismatches };
}

// the symbol of the same declarations, for a name tsc and Yuku both bind as one
function symbolOf(
  binding: Binding,
  identifiers: Map<number, ts.Identifier>,
  checker: ts.TypeChecker,
  tree: ts.SourceFile,
  lang: SourceLang,
): ts.Symbol | undefined {
  const first = binding.declarations[0];
  if (first?.type !== "Identifier") return undefined;
  const node = identifiers.get(first.start);
  const symbol = node === undefined ? undefined : checker.getSymbolAtLocation(node);
  const declarations = symbol?.declarations ?? [];
  if (symbol === undefined || declarations.length === 0) return undefined;
  const assigned = (d: ts.Declaration) =>
    ts.isIdentifier(d) ||
    ts.isBinaryExpression(d) ||
    ts.isPropertyAccessExpression(d) ||
    ts.isElementAccessExpression(d) ||
    ts.isCallExpression(d) ||
    (d.flags & ts.NodeFlags.JSDoc) !== 0;
  if (declarations.some(assigned)) return undefined;
  // a parameter property reads as its property
  if (
    declarations.some((d) => ts.isParameter(d) && ts.isParameterPropertyDeclaration(d, d.parent))
  ) {
    return undefined;
  }
  // `export as namespace N` declares a global alias to tsc and a namespace in the file to Yuku
  if (declarations.some(ts.isNamespaceExportDeclaration)) return undefined;
  // tsc's JavaScript binder aliases a `require` variable and classes a function
  const javascript = lang === "js" || lang === "jsx";
  if (javascript && (symbol.flags & (F.Alias | F.Class)) !== 0) {
    if (!binding.has(BindingFlags.Import) && !binding.has(BindingFlags.Class)) return undefined;
  }
  const ours = binding.declarations.map((d) => d.start).sort((a, b) => a - b);
  const theirs = nameStarts(tree, declarations).sort((a, b) => a - b);
  // an unnamed default merges into tsc's `default`, and the rest bind apart
  if (theirs.length !== declarations.length) return undefined;
  return ours.join() === theirs.join() ? symbol : undefined;
}

function compareFile(file: CorpusFile, source: string): Comparison | null {
  return compare(source, file.lang, file.sourceType, file.path);
}

const SUITE = "test/parser/suite/ts/pass";

const KNOWN: Known = {
  "tsc reads `<!--` in a script as operators, not an HTML-like comment": {
    "test/parser/suite/js/pass/4186deb1eb914895.js": [
      'import yuku only: ["dynamic",null,null,"a",false,null]',
    ],
  },
  "tsc rejects `declare import`": {
    [`${SUITE}/7158dfb3b9420822.ts`]: ["a@34 Ambient: yuku false"],
  },
  "tsc rejects `global` without `declare`, which stays ambient": {
    [`${SUITE}/8ea0d1d485714be4.ts`]: ["Array@63 Ambient: yuku true", "T@69 Ambient: yuku true"],
    [`${SUITE}/95e3afef0093e6a4.module.ts`]: [
      "Array@111 Ambient: yuku true",
      "T@117 Ambient: yuku true",
    ],
    [`${SUITE}/ec55cf0b0ee51f09.ts`]: ["Array@86 Ambient: yuku true", "T@92 Ambient: yuku true"],
  },
  "tsc rejects `export` on an ambient module": {
    [`${SUITE}/93fb16865e3bdeb3.module.ts`]: [
      'import tsc only: ["augmentation",null,null,"M",true,null]',
    ],
  },
  "tsc instantiates a namespace for its `export type { x }`, which emits nothing": {
    "test/projects/svelte/packages/svelte/src/compiler/types/template.d.ts": [
      "AST@696 ValueModule: yuku false",
    ],
  },
};

type Snippet = [name: string, source: string, lang?: SourceLang, sourceType?: SourceType];

const SNIPPETS: Snippet[] = [
  ["a dotted namespace exports its first name", `export namespace A.B.C { export const v = 1 }`],
  ["a dotted `module` exports its first name", `export module A.B { export const v = 1 }`],
  ["a declared dotted namespace", `export declare namespace A.B { const v: number }`],
  ["a type-only dotted namespace", `export namespace A.B { export type T = 1 } export {};`],
  [
    "a declaration module exports dotted namespaces implicitly",
    `declare namespace A.B { const v: number } declare const c: number; import "m";`,
    "dts",
  ],
  ["a default interface", `export default interface A { a: string }`],
  ["a default class and function", `export default class C {} `],
  ["an anonymous default", `export default function () {}`],
  ["a default expression", `const a = 1; export default a;`],
  ["declared exports", `export declare const a: number; export declare function f(): void;`],
  [
    "every declaration kind",
    `export const a = 1, { b, c: [d] } = o; export function f() {}
    export class C {} export enum E { m } export const enum K { m } export interface I {}
    export type T = 1; export namespace N {} export import X = N;`,
  ],
  [
    "specifiers",
    `const a = 1; export { a, a as b, a as default, type a as c }; export type { a as d };`,
  ],
  [
    "re-exports",
    `export { a, b as c, type d } from "m"; export * from "m"; export * as n from "m";
    export type { e } from "m"; export type * from "m"; export type * as t from "m";
    export {} from "m";`,
  ],
  ["string names", `const a = 1; export { a as "x y" }; export { "p q" as b } from "m";`],
  ["export equals", `const a = 1; export = a;`],
  ["a UMD global", `export as namespace N; export const a = 1;`, "dts"],
  [
    "imports",
    `import d, { a, b as c, type e } from "m"; import * as n from "m"; import "m";
    import {} from "m"; import type t from "m"; import type { u } from "m";
    import type * as v from "m";
    import defer * as w from "m";`,
  ],
  ["import equals", `import a = require("m"); import type b = require("m"); import c = a.d;`],
  [
    "calls",
    `const a = import("m"); const b = require("m"); function f(require) { require("n"); }`,
    "js",
  ],
  ["an augmentation", `export {}; declare module "m" { export const a: number; }`],
  ["a script's ambient module", `declare module "m" { export const a: number; }`],
  ["import.meta", `const url = import.meta.url;`],
  [
    "index signatures declare nothing exported",
    `export interface I { [k: string]: any }
    export type T = { [k: string]: any }; export class C { [k: string]: any }`,
  ],
  [
    "type parameters of a declaration are ambient",
    `declare function f<T>(a: T): void;
    declare class C<T> { m<U>(p: U): void; [k: string]: any }
    declare namespace N { function g<V>(q: V): void; interface I<W> { [k: string]: W } }`,
  ],
  [
    "a signature with no body is ambient",
    `function f<T>(a: T): void; function f(a: any) {}
    abstract class C { abstract m<T>(p: T): void; }`,
  ],
  [
    "a namespace of types is not instantiated",
    `namespace N { export type T = 1 } namespace M { interface I {} }
    namespace O { namespace P { export type T = 1 } } namespace Q { export const a = 1 }`,
  ],
  [
    "bindings of every kind",
    `var a; let b; const c = 1; function f(p, { q }) { try {} catch (e) {} }
    class C {} enum E { m } const enum K { m } interface I<T> {} type A<U> = { [V in keyof U]: V };
    type B<W> = W extends (infer X)[] ? X : never;`,
  ],
];

describe("records and flags agree with tsc", () => {
  for (const [name, source, lang = "ts", sourceType = "module"] of SNIPPETS) {
    test(name, () => {
      const result = compare(source, lang, sourceType)!;
      expect(result.mismatches).toEqual([]);
      expect(result.compared).toBeGreaterThan(0);
    });
  }

  test("spaces name the flags of tsc's meanings", () => {
    const kinds: [keyof typeof BindingFlags, number][] = [
      ["FunctionScopedVariable", F.FunctionScopedVariable],
      ["BlockScopedVariable", F.BlockScopedVariable],
      ["Function", F.Function],
      ["Class", F.Class],
      ["RegularEnum", F.RegularEnum],
      ["ConstEnum", F.ConstEnum],
      ["ValueModule", F.ValueModule],
      ["Interface", F.Interface],
      ["TypeAlias", F.TypeAlias],
      ["TypeParameter", F.TypeParameter],
      ["NamespaceModule", F.NamespaceModule],
      ["EnumMember", F.EnumMember],
    ];
    for (const [name, flag] of kinds) {
      const mask = BindingFlags[name];
      expect([name, (BindingFlags.ValueSpace & mask) !== 0]).toEqual([
        name,
        (F.Value & flag) !== 0,
      ]);
      expect([name, (BindingFlags.TypeSpace & mask) !== 0]).toEqual([name, (F.Type & flag) !== 0]);
      expect([name, (BindingFlags.NamespaceSpace & mask) !== 0]).toEqual([
        name,
        (F.Namespace & flag) !== 0,
      ]);
    }
  });

  differential("corpus and projects", corpusAndProjectFiles(), compareFile, KNOWN);
});
