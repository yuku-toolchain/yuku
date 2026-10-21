// the imports Yuku's type positions keep against the ones tsc keeps per file

import { describe, expect, test } from "bun:test";
import ts from "typescript";
import { nodesOf } from "../ast-helpers-for-test";
import {
  type Binding,
  BindingFlags,
  type Node,
  parse,
  type SourceFile,
  type SourceLang,
  type SourceType,
  type TSImportEqualsDeclaration,
} from "yuku";
import { corpusAndProjectFiles, type CorpusFile } from "../corpus";
import { differential, singleFileProgram, type Comparison, type Known } from "./utils/differential";

interface Emitted {
  kept: Set<string>;
  rejects(name: string, positions: number[]): boolean;
}

function emit(source: string, lang: SourceLang, sourceType: SourceType): Emitted {
  const tsx = lang === "tsx";
  let output = "";
  const { program, file, rejects } = singleFileProgram(
    {
      name: tsx ? "input.tsx" : "input.ts",
      source,
      kind: tsx ? ts.ScriptKind.TSX : ts.ScriptKind.TS,
      sourceType,
    },
    {
      module: ts.ModuleKind.Preserve,
      target: ts.ScriptTarget.ESNext,
      jsx: ts.JsxEmit.Preserve,
      // parameter decorators exist only under the legacy flag
      experimentalDecorators: true,
    },
    (_, text) => (output = text),
  );
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

  return { kept, rejects };
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

function candidates(file: SourceFile): Candidate[] {
  const found: Candidate[] = [];
  for (const statement of file.program.body) {
    const exported = statement.type === "ExportNamedDeclaration";
    const node = exported ? statement.declaration : statement;
    if (node?.type === "ImportDeclaration" && node.importKind !== "type") {
      for (const specifier of node.specifiers) {
        if (specifier.type === "ImportSpecifier" && specifier.importKind === "type") continue;
        const binding = file.bindingOf(specifier.local);
        if (binding !== null) found.push({ name: specifier.local.name, binding, exported });
      }
    } else if (
      node?.type === "TSImportEqualsDeclaration" &&
      node.importKind !== "type" &&
      node.moduleReference.type === "TSExternalModuleReference"
    ) {
      const binding = file.bindingOf(node.id);
      if (binding !== null) found.push({ name: node.id.name, binding, exported });
    }
  }
  return found;
}

function keeps(file: SourceFile, binding: Binding, seen: Set<Binding>): boolean {
  if (binding.has(BindingFlags.ValueSpace) || seen.has(binding)) return false;
  seen.add(binding);
  for (const reference of binding.references) {
    if (reference.inTypePosition) continue;
    const alias = aliasOf(file, reference.node);
    if (alias === null) return true;
    if (file.parentOf(alias)?.type === "ExportNamedDeclaration") return true;
    const aliased = file.bindingOf(alias.id);
    if (aliased !== null && keeps(file, aliased, seen)) return true;
  }
  return false;
}

function aliasOf(file: SourceFile, node: Node): TSImportEqualsDeclaration | null {
  let child = node;
  let parent = file.parentOf(child);
  while (parent?.type === "TSQualifiedName" && parent.left === child) {
    child = parent;
    parent = file.parentOf(child);
  }
  if (parent?.type !== "TSImportEqualsDeclaration" || parent.moduleReference !== child) return null;
  return parent;
}

function jsxFactories(file: SourceFile): Set<string> {
  if (nodesOf(file.program, "JSXElement", "JSXFragment").length === 0) return new Set();
  const names = new Set(["React"]);
  for (const match of file.source.matchAll(/@jsx(?:Frag)?\s+([A-Za-z_$][\w$]*)/g)) {
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
  const file = parse(source, { semantic: true, path, lang, sourceType, semanticErrors: true });
  if (file.diagnostics.some((d) => d.severity === "error")) return null;
  const list = candidates(file);
  if (list.length === 0) return { compared: 0, mismatches: [] };
  const tsc = emit(source, lang, sourceType);
  const factories = jsxFactories(file);
  const mismatches: string[] = [];
  let compared = 0;
  for (const { name, binding, exported } of list) {
    if (factories.has(name)) continue;
    compared++;
    const ours = exported || keeps(file, binding, new Set());
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

  const files = corpusAndProjectFiles().filter((file) => file.lang === "ts" || file.lang === "tsx");
  differential("corpus and projects", files, compareFile, KNOWN);
});
