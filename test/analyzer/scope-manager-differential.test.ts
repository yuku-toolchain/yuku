// Compares references, writes, declarations, and scopes with @typescript-eslint/scope-manager.

import { describe, expect, test } from "bun:test";
import { analyze, type Scope as TheirScope } from "@typescript-eslint/scope-manager";
import { parse as tsParse } from "@typescript-eslint/typescript-estree";
import { analyze as analyzeFile, type Binding, type Module, type Scope } from "yuku-analyzer";
import type { Node, SourceLang, SourceType } from "yuku-parser";
import { corpusFiles, projectFiles, type CorpusFile } from "../corpus";
import { differential, type Comparison, type Known } from "./utils/differential";

const UNRESOLVED = -1;

// `(a as any) = 1` writes `a`
const ERASED_WRAPPERS = new Set([
  "TSAsExpression",
  "TSSatisfiesExpression",
  "TSNonNullExpression",
  "TSTypeAssertion",
]);

interface Resolution {
  name: string;
  def: number;
  write: boolean;
}

function scopeManager(source: string, sourceType: SourceType, lang: SourceLang) {
  const jsx = lang === "jsx" || lang === "tsx";
  const tree = tsParse(source, { range: true, sourceType, jsx, allowInvalidAST: false });
  const manager = analyze(tree, { sourceType, lib: [] });
  const references = new Map<number, Resolution>();
  const declarations = new Map<number, string[]>();
  for (const scope of manager.scopes) {
    for (const reference of scope.references) {
      const starts = (reference.resolved?.defs ?? [])
        .map((def) => def.name?.range?.[0])
        .filter((start): start is number => start !== undefined);
      // no named def means an implicit binding such as arguments
      references.set(reference.identifier.range[0], {
        name: reference.identifier.name,
        def: starts.length === 0 ? UNRESOLVED : Math.min(...starts),
        write: reference.isWrite(),
      });
    }
    for (const variable of scope.variables) {
      for (const def of variable.defs) {
        const name = def.name;
        const position = name?.range?.[0];
        if (position === undefined) continue;
        const named = name.type === "Identifier" || name.type === "Literal";
        if (def.type === "TSEnumMemberName" && !named) continue;
        const isThis = name.type === "Identifier" && name.name === "this";
        if (def.type === "Parameter" && isThis) continue;
        const owners = declarations.get(position) ?? [];
        owners.push(theirOwner(scope));
        declarations.set(position, owners);
      }
    }
  }
  return { references, declarations };
}

function theirOwner(scope: TheirScope): string {
  if (scope.block.type === "Program") return scope.type;
  if (scope.block.type === "TSModuleDeclaration" && scope.block.kind === "global") return "global";
  const upper = scope.upper;
  if (scope.type === "block" && upper?.type === "catch" && upper.block.body === scope.block) {
    return String(upper.block.range[0]);
  }
  return String(scope.block.range[0]);
}

const BODIES = new Set(["TSModuleBlock", "TSEnumBody"]);

function ownerOf(module: Module, scope: Scope): string {
  const node = scope.node;
  if (node.type === "Program") return scope.kind;
  const parent = module.parentOf(node)!;
  const isBody =
    scope.kind === "functionBody" || BODIES.has(node.type) || parent.type === "CatchClause";
  return String((isBody ? parent : node).start);
}

function declarationOwner(module: Module, binding: Binding, node: Node): string | null {
  if (module.parentOf(module.parentOf(node)!)?.type === "TSInferType") return null;
  if (binding.scope.kind !== "tsModule") return ownerOf(module, binding.scope);
  for (const ancestor of module.ancestors(node)) {
    if (ancestor.type === "TSModuleBlock") return String(module.parentOf(ancestor)!.start);
  }
  return ownerOf(module, binding.scope);
}

// `B` in `namespace A.B {}`, `s` in `[s: string]: T`, `N` in `export as namespace N`
const UNMODELED_PARENTS = new Set([
  "TSQualifiedName",
  "TSIndexSignature",
  "TSNamespaceExportDeclaration",
]);

function isUnmodeled(module: Module, node: Node): boolean {
  const parent = module.parentOf(node);
  if (parent !== null && UNMODELED_PARENTS.has(parent.type)) return true;
  return parent?.type === "TSModuleDeclaration" && parent.id.type === "TSQualifiedName";
}

function isIntrinsicTag(module: Module, position: number): boolean {
  const node = module.nodeAt(position);
  if (node?.type !== "JSXIdentifier") return false;
  const parent = module.parentOf(node);
  if (parent?.type === "JSXNamespacedName") return true;
  return parent?.type !== "JSXMemberExpression" && /^[a-z]|-/.test(node.name);
}

function unmodeled(module: Module, binding: Binding): boolean {
  return binding.declarations.some((node) => isUnmodeled(module, node));
}

function compare(source: string, sourceType: SourceType, lang: SourceLang): Comparison | null {
  let theirs;
  try {
    theirs = scopeManager(source, sourceType, lang);
  } catch {
    // typescript-estree rejects the file
    return null;
  }
  const module = analyzeFile(source, { sourceType, lang });
  const mismatches: string[] = [];
  let compared = 0;

  for (const reference of module.references) {
    const position = reference.node.start;
    const their = theirs.references.get(position);
    if (their === undefined || reference.inTypePosition) continue;
    const binding = reference.binding;
    if (binding !== null && !binding.scope.contains(reference.scope)) continue;
    if (binding !== null && unmodeled(module, binding)) continue;
    compared++;
    const starts = binding?.declarations.map((declaration) => declaration.start) ?? [];
    const def = binding === null ? UNRESOLVED : Math.min(...starts);
    if (def !== their.def) {
      const node = their.def === UNRESOLVED ? null : module.nodeAt(their.def);
      const target = node === null ? null : module.bindingOf(node);
      const consistent = binding === null || binding.visibleIn(reference.space);
      if (consistent && target !== null && !target.visibleIn(reference.space)) continue;
      mismatches.push(`${reference.name}@${position}: yuku ${def}, scope-manager ${their.def}`);
    } else if (reference.isWrite !== their.write) {
      const parent = module.parentOf(reference.node);
      if (reference.isWrite && parent !== null && ERASED_WRAPPERS.has(parent.type)) continue;
      mismatches.push(`${reference.name}@${position}: yuku write ${reference.isWrite}`);
    }
  }

  const recorded = new Set([
    ...module.references.map((reference) => reference.node.start),
    ...module.bindings.flatMap((binding) => binding.declarations.map((node) => node.start)),
  ]);
  for (const [position, their] of theirs.references) {
    if (isIntrinsicTag(module, position)) continue;
    compared++;
    if (!recorded.has(position)) mismatches.push(`${their.name}@${position}: no yuku reference`);
  }

  const ours = new Map<number, string | null>();
  for (const binding of module.bindings) {
    for (const node of binding.declarations) {
      if (!isUnmodeled(module, node)) ours.set(node.start, declarationOwner(module, binding, node));
    }
  }
  for (const [position, owners] of theirs.declarations) {
    compared++;
    const owner = ours.get(position);
    if (owner === undefined) {
      mismatches.push(`declaration@${position}: scope-manager only`);
    } else if (owner !== null && !owners.includes(owner)) {
      mismatches.push(`declaration@${position}: yuku scope ${owner}, scope-manager ${owners}`);
    }
  }
  for (const position of ours.keys()) {
    if (!theirs.declarations.has(position)) mismatches.push(`declaration@${position}: yuku only`);
  }
  return { compared, mismatches };
}

function compareFile(file: CorpusFile, source: string): Comparison | null {
  return compare(source, file.sourceType, file.lang);
}

const SUITE = "test/parser/suite/ts/pass";

const KNOWN: Known = {
  "scope-manager resolves an import equals alias to nothing": {
    [`${SUITE}/3a66bcb0ff2adb2c.module.ts`]: ["x@268: yuku 175, scope-manager -1"],
  },
  "scope-manager merges a parameter with a same-named body var": {
    [`${SUITE}/f2131ad89bc9a8ba.ts`]: [
      "s@4207: yuku 4193, scope-manager 4172",
      "s@4476: yuku 4462, scope-manager 4441",
      "s@4762: yuku 4734, scope-manager 4710",
      "t@1282: yuku 1219, scope-manager 1194",
      "t@3315: yuku 3238, scope-manager 3213",
    ],
  },
  "scope-manager scopes a declaration in statement position apart from tsc": {
    [`${SUITE}/4d85c34e00391b61.ts`]: [
      "declaration@196: yuku scope 170, scope-manager global",
      "declaration@365: yuku scope global, scope-manager 335",
    ],
  },
  "typescript-estree reads `<!--` in a script as operators, not an HTML-like comment": {
    "test/parser/suite/js/pass/158dc2b44b1958390.js": ["bar@8: no yuku reference"],
    "test/parser/suite/js/pass/367c3d5dca7f95a5.js": ["b@5: no yuku reference"],
    "test/parser/suite/js/pass/9361ed8ad34bb5b9.js": ["bar@8: no yuku reference"],
  },
};

type Snippet = [name: string, source: string, sourceType?: SourceType, lang?: SourceLang];

const SNIPPETS: Snippet[] = [
  ["var hoisting across blocks", `var x; { var x = 1; } x;`],
  ["let shadowing", `let x = 1; { let x = 2; x; } x;`],
  ["function expression name", `const f = function self() { return self; };`],
  ["catch parameter", `let e = 1; try {} catch (e) { e; } e;`],
  ["class name inner scope", `class C { m() { return C; } } new C();`],
  [
    "default parameter reads parameter scope",
    `let y = 1; function f(a = y, y = 2) { return a + y; }`,
  ],
  ["closure capture", `function outer() { let v = 1; return () => v; }`],
  ["undeclared global", `undeclared;`],
  ["arguments is implicit", `function f() { return arguments; }`],
  ["named function hoisting", `f(); function f() {}`],
  ["parameter shadows outer", `let p = 1; function f(p) { return p; }`],
  ["for-of iteration variable", `for (const item of []) item;`],
  ["destructuring defaults", `const fallback = 0; const { a = fallback, b: c } = obj; c;`],
  ["import bindings", `import { a, b as c } from "m"; a; c;`, "module"],
  ["export specifiers", `const x = 1; export { x };`, "module"],
  [
    "getter setter pair",
    `const o = { get v() { return hidden; }, set v(next) { hidden = next; } }; let hidden;`,
  ],
  ["type annotations are erased", `let n: number = 1; n;`, "script", "ts"],
  [
    "jsx component reference",
    `const Button = () => null; export const app = <Button />;`,
    "module",
    "tsx",
  ],
  ["enum members are visible in the body", `const a = 9; enum E { a, b = a }`, "script", "ts"],
  [
    "arguments does not see past a function",
    `var arguments = 1; function f() { return arguments; } arguments;`,
  ],
  [
    "parameter default closures resolve past body vars",
    `var x = 1; function f(_ = () => x) { var x = 2; }`,
  ],
  ["switch discriminants resolve outside", `let x = 1; switch (x) { case 1: let x = 2; }`],
  ["signature parameters declare", `type F = (a: number, b: string) => void;`, "script", "ts"],
  [
    "declarations land in the same scopes",
    `function f(a, d = 1) { var x; { let y; var z; } try {} catch (e) { let c; } }
    namespace N { export const n = 1; } enum E { A } const g = function named() {};`,
    "script",
    "ts",
  ],
];

describe("resolution agrees with @typescript-eslint/scope-manager", () => {
  for (const [name, source, sourceType = "script", lang = "js"] of SNIPPETS) {
    test(name, () => {
      const result = compare(source, sourceType, lang)!;
      expect(result.mismatches).toEqual([]);
      expect(result.compared).toBeGreaterThan(0);
    });
  }

  differential("corpus and projects", [...corpusFiles(), ...projectFiles()], compareFile, KNOWN);
});
