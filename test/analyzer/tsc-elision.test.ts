// Compares the imports Yuku's type positions keep with the ones tsc keeps per file.

import { describe, expect, test } from "bun:test";
import ts from "typescript";
import { analyze, BindingFlags, type Binding, type Module } from "yuku-analyzer";
import type { Node, SourceLang, SourceType, TSImportEqualsDeclaration } from "yuku-parser";
import { corpusFiles, projectFiles, type CorpusFile } from "../corpus";
import { differential, type Comparison, type Known } from "./utils/differential";

interface Emitted {
  kept: Set<string>;
  rejects(name: string, positions: number[]): boolean;
}

const { getSetExternalModuleIndicator } = ts as unknown as {
  getSetExternalModuleIndicator(options: ts.CompilerOptions): (file: ts.SourceFile) => void;
};

function emit(source: string, lang: SourceLang, sourceType: SourceType): Emitted {
  const fileName = lang === "tsx" ? "input.tsx" : "input.ts";
  const options: ts.CompilerOptions = {
    noLib: true,
    noResolve: true,
    module: ts.ModuleKind.Preserve,
    target: ts.ScriptTarget.ESNext,
    jsx: ts.JsxEmit.Preserve,
    // parameter decorators exist only under the legacy flag
    experimentalDecorators: true,
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
    lang === "tsx" ? ts.ScriptKind.TSX : ts.ScriptKind.TS,
  );
  let output = "";
  const host: ts.CompilerHost = {
    getSourceFile: (name) => (name === fileName ? file : undefined),
    getDefaultLibFileName: () => "lib.d.ts",
    writeFile: (_, text) => (output = text),
    getCurrentDirectory: () => "",
    getCanonicalFileName: (name) => name,
    useCaseSensitiveFileNames: () => true,
    getNewLine: () => "\n",
    fileExists: (name) => name === fileName,
    readFile: () => undefined,
  };
  const program = ts.createProgram([fileName], options, host);
  program.emit(file);

  const js = ts.createSourceFile("output.jsx", output, ts.ScriptTarget.ESNext, false);
  const kept = new Set<string>();
  for (const statement of js.statements) {
    if (ts.isImportDeclaration(statement)) {
      const clause = statement.importClause;
      if (clause?.name !== undefined) kept.add(clause.name.text);
      const bindings = clause?.namedBindings;
      if (bindings !== undefined && ts.isNamespaceImport(bindings)) kept.add(bindings.name.text);
      if (bindings !== undefined && ts.isNamedImports(bindings)) {
        for (const element of bindings.elements) kept.add(element.name.text);
      }
    } else if (ts.isVariableStatement(statement)) {
      for (const declaration of statement.declarationList.declarations) {
        if (ts.isIdentifier(declaration.name) && isRequire(declaration.initializer)) {
          kept.add(declaration.name.text);
        }
      }
    }
  }

  let errors: readonly ts.Diagnostic[] | null = null;
  return {
    kept,
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
  };
}

function isRequire(node: ts.Expression | undefined): boolean {
  return (
    node !== undefined &&
    ts.isCallExpression(node) &&
    ts.isIdentifier(node.expression) &&
    node.expression.text === "require"
  );
}

interface Candidate {
  name: string;
  binding: Binding;
  exported: boolean;
}

function candidates(module: Module): Candidate[] {
  const found: Candidate[] = [];
  for (const statement of module.ast.body) {
    const exported = statement.type === "ExportNamedDeclaration";
    const node = exported ? statement.declaration : statement;
    if (node?.type === "ImportDeclaration" && node.importKind !== "type") {
      for (const specifier of node.specifiers) {
        if (specifier.type === "ImportSpecifier" && specifier.importKind === "type") continue;
        const binding = module.bindingOf(specifier.local);
        if (binding !== null) found.push({ name: specifier.local.name, binding, exported });
      }
    } else if (
      node?.type === "TSImportEqualsDeclaration" &&
      node.importKind !== "type" &&
      node.moduleReference.type === "TSExternalModuleReference"
    ) {
      const binding = module.bindingOf(node.id);
      if (binding !== null) found.push({ name: node.id.name, binding, exported });
    }
  }
  return found;
}

function keeps(module: Module, binding: Binding, seen: Set<Binding>): boolean {
  if (binding.has(BindingFlags.ValueSpace) || seen.has(binding)) return false;
  seen.add(binding);
  for (const reference of binding.references) {
    if (reference.inTypePosition) continue;
    const alias = aliasOf(module, reference.node);
    if (alias === null) return true;
    if (module.parentOf(alias)?.type === "ExportNamedDeclaration") return true;
    const aliased = module.bindingOf(alias.id);
    if (aliased !== null && keeps(module, aliased, seen)) return true;
  }
  return false;
}

function aliasOf(module: Module, node: Node): TSImportEqualsDeclaration | null {
  let child = node;
  let parent = module.parentOf(child);
  while (parent?.type === "TSQualifiedName" && parent.left === child) {
    child = parent;
    parent = module.parentOf(child);
  }
  if (parent?.type !== "TSImportEqualsDeclaration" || parent.moduleReference !== child) return null;
  return parent;
}

function jsxFactories(module: Module): Set<string> {
  if (module.findAll(["JSXElement", "JSXFragment"]).length === 0) return new Set();
  const names = new Set(["React"]);
  for (const match of module.source.matchAll(/@jsx(?:Frag)?\s+([A-Za-z_$][\w$]*)/g)) {
    names.add(match[1]!);
  }
  return names;
}

function compare(
  source: string,
  lang: SourceLang,
  sourceType: SourceType = "module",
  path?: string,
): Comparison | null {
  const module = analyze(source, { path, lang, sourceType });
  if (module.diagnostics.some((d) => d.severity === "error")) return null;
  const list = candidates(module);
  if (list.length === 0) return { compared: 0, mismatches: [] };
  const tsc = emit(source, lang, sourceType);
  const factories = jsxFactories(module);
  const mismatches: string[] = [];
  let compared = 0;
  for (const { name, binding, exported } of list) {
    if (factories.has(name)) continue;
    compared++;
    const ours = exported || keeps(module, binding, new Set());
    const theirs = tsc.kept.has(name);
    if (ours === theirs) continue;
    const positions = [
      ...binding.declarations.map((declaration) => declaration.start),
      ...binding.references.map((reference) => reference.node.start),
    ];
    if (tsc.rejects(name, positions)) continue;
    mismatches.push(`${name}: yuku ${ours ? "keeps" : "drops"}, tsc ${theirs ? "keeps" : "drops"}`);
  }
  return { compared, mismatches };
}

function compareFile(file: CorpusFile, source: string): Comparison | null {
  return compare(source, file.lang, file.sourceType, file.path);
}

const SUITE = "test/parser/suite/ts/pass";

const KNOWN: Known = {
  "tsc keeps an import merged with an exported function, a conflict it does not report": {
    [`${SUITE}/1af1672d6a4bfcc3.module.ts`]: ["Foo: yuku drops, tsc keeps"],
  },
};

const SNIPPETS: [name: string, source: string, lang?: SourceLang][] = [
  ["a runtime use keeps an import", `import { A } from "a"; A;`],
  ["an unused import is dropped", `import { A } from "a";`],
  ["a type use drops an import", `import { A } from "a"; let x: A;`],
  ["a typeof query drops an import", `import { A } from "a"; let x: typeof A;`],
  ["a type argument drops an import", `import { A } from "a"; f<A>();`],
  [
    "implements drops, extends keeps",
    `import { A, I } from "a"; class C extends A implements I {}`,
  ],
  ["a decorator keeps an import", `import { A } from "a"; @A class C {}`],
  ["an assertion keeps its operand", `import { A, T } from "a"; let x = (A as T)!;`],
  ["an enum initializer keeps an import", `import { A } from "a"; enum E { x = A }`],
  ["an export keeps an import", `import { A } from "a"; export { A };`],
  ["a default export keeps an import", `import A from "a"; export default A;`],
  [
    "a type-only export drops an import",
    `import { A, B } from "a"; export type { A }; export { type B };`,
  ],
  ["a namespace import keeps on a member use", `import * as ns from "a"; ns.f();`],
  ["a namespace import drops on a qualified type", `import * as ns from "a"; let x: ns.T;`],
  ["a require import keeps on a runtime use", `import x = require("m"); x();`],
  ["a require import drops on a type use", `import x = require("m"); let t: x.T;`],
  [
    "an alias chain keeps its target when read",
    `import * as lib from "lib"; import H = lib.H; import G = H.G; G;`,
  ],
  [
    "an alias chain drops its target when only typed",
    `import * as lib from "lib"; import H = lib.H; let h: H;`,
  ],
  ["an exported alias keeps its target", `import * as lib from "lib"; export import H = lib.H;`],
  [
    "an import merged with a local value is type-only",
    `import { A } from "a"; function A() {} A();`,
  ],
  ["an unused JSX import is dropped", `import { h } from "p"; const a = <div />;`, "tsx"],
];

describe("type positions agree with the imports tsc elides", () => {
  for (const [name, source, lang = "ts"] of SNIPPETS) {
    test(name, () => {
      const result = compare(source, lang)!;
      expect(result.mismatches).toEqual([]);
      expect(result.compared).toBeGreaterThan(0);
    });
  }

  test("tsc keeps an import a computed key in a type reads, though the key never runs", () => {
    const result = compare(`import { A } from "a"; type T = { [A]: 1 };`, "ts")!;
    expect(result.mismatches).toEqual(["A: yuku drops, tsc keeps"]);
  });

  const files = [...corpusFiles(), ...projectFiles()].filter(
    (file) => file.lang === "ts" || file.lang === "tsx",
  );
  differential("corpus and projects", files, compareFile, KNOWN);
});
