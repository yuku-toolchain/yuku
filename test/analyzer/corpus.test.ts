// the semantic model of every corpus and project file holds each invariant

import { beforeAll, describe, expect, test } from "bun:test";
import {
  BindingFlags,
  type Node,
  parse,
  type Scope,
  type SourceFile,
  type SourceType,
  walk,
} from "yuku";
import { astDiffPath, nodesOf } from "../ast-helpers-for-test";
import { corpusAndProjectFiles, corpusPresent, forEachCorpusFile } from "../corpus";

const SAMPLE_MAX = 8;

const INVARIANTS = {
  crashed: "analysis never crashes",
  crossIndex: "scope and binding cross-indexes are symmetric",
  resolution: "every reference resolves to what lookup finds from its scope",
  nodeIdentity: "node-to-model lookups round-trip",
  scopeMatch: "the walked scope matches the binder's scope at every reference",
  parentMatch: "parentOf matches the walk's parent at every node",
  captures: "capturesOf matches an independent recomputation",
  records: "module records are well formed",
  determinism: "analysis is deterministic",
  earlyNodes: "nodes read before the program are the program's own",
  ordering: "bindings and references are recorded in walk (source) order",
};

type Invariant = keyof typeof INVARIANTS;

const violations = {} as Record<Invariant, string[]>;
for (const invariant of Object.keys(INVARIANTS) as Invariant[]) violations[invariant] = [];
let analyzed = 0;

// resolution can differ from lookup for these
const POSITION_DEPENDENT =
  BindingFlags.TypeParameter | BindingFlags.Parameter | BindingFlags.FunctionScopedVariable;

const FUNCTION_TYPES = [
  "FunctionDeclaration",
  "FunctionExpression",
  "ArrowFunctionExpression",
] as const;

function note(invariant: Invariant, detail: string): void {
  if (violations[invariant].length < SAMPLE_MAX) violations[invariant].push(detail);
}

function expectedCaptures(file: SourceFile, fn: Node): Set<number> {
  const fnScope = file.scopes.find((scope) => scope.node === fn && scope.kind === "function");
  if (fnScope === undefined) return new Set();
  const within = (scope: Scope | null): boolean => {
    for (let current = scope; current; current = current.parent) {
      if (current === fnScope) return true;
    }
    return false;
  };
  const captured = new Set<number>();
  for (const reference of file.references) {
    if (reference.inTypePosition || reference.binding === null) continue;
    if (reference.node.start < fn.start || reference.node.end > fn.end) continue;
    if (within(reference.binding.scope)) continue;
    captured.add(reference.binding.id);
  }
  return captured;
}

function fingerprint(file: SourceFile): string {
  return JSON.stringify([
    file.scopes.map((scope) => scope.kind),
    file.bindings.map((binding) => binding.flags),
    file.references.map((reference) => reference.binding?.id ?? -1),
  ]);
}

function outOfWalkOrder<T>(
  items: readonly T[],
  walkOrder: Map<Node, number>,
  nodeOf: (item: T) => Node | undefined,
): T | null {
  let previous = -1;
  for (const item of items) {
    const node = nodeOf(item);
    if (node === undefined) continue;
    const at = walkOrder.get(node);
    if (at === undefined) continue;
    if (at < previous) return item;
    previous = at;
  }
  return null;
}

function check(path: string, source: string, sourceType: SourceType): void {
  let file: SourceFile;
  try {
    file = parse(source, { semantic: true, path, sourceType });
    // a decode fault throws here, not later
    void file.program;
    void file.scopes;
    void file.bindings;
    void file.references;
    void file.imports;
    void file.exports;
  } catch (error) {
    note("crashed", `${path}: ${(error as Error).message}`);
    return;
  }
  analyzed++;

  // each reference is unresolved or owned by one binding
  let ownedReferences = 0;
  for (const binding of file.bindings) {
    for (const reference of binding.references) {
      if (reference.binding !== binding) note("crossIndex", `${path}: ${binding.name} back-ref`);
    }
    ownedReferences += binding.references.length;
  }
  for (const scope of file.scopes) {
    for (const binding of scope.bindings) {
      if (binding.scope !== scope) note("crossIndex", `${path}: ${binding.name} scope back-ref`);
    }
  }
  const resolved = file.references.filter((reference) => reference.binding !== null).length;
  if (ownedReferences !== resolved) {
    note("crossIndex", `${path}: owned ${ownedReferences} != resolved ${resolved}`);
  }

  for (const reference of file.references) {
    const { name, scope, space } = reference;
    const expected = scope.lookup(name, space);
    if (expected === reference.binding || name === "arguments") continue;
    if (expected !== null && expected.has(POSITION_DEPENDENT)) continue;
    note("resolution", `${path}: ${name} resolves apart from lookup`);
  }

  for (const binding of file.bindings) {
    const declaration = binding.declarations[0];
    if (declaration === undefined) continue;
    const owner = file.bindingOf(declaration);
    if (owner !== binding) {
      const found = owner === null ? "null" : `#${owner.id}`;
      note("nodeIdentity", `${path}: bindingOf(decl ${binding.name}) is ${found}`);
    }
  }
  for (const reference of file.references) {
    if (file.referenceOf(reference.node) !== reference) {
      note("nodeIdentity", `${path}: referenceOf(${reference.name})`);
    }
    if (file.bindingOf(reference.node) !== reference.binding) {
      note("nodeIdentity", `${path}: bindingOf(ref ${reference.name})`);
    }
  }

  const walkOrder = new Map<Node, number>();
  walk(file.program, {
    enter(node, ctx) {
      walkOrder.set(node, walkOrder.size);
      const reference = file.referenceOf(node);
      const scope = reference === null ? null : file.scopeOf(node);
      if (reference !== null && scope !== reference.scope) {
        const detail = `${reference.name} at ${scope!.id} vs ref ${reference.scope.id}`;
        note("scopeMatch", `${path}: ${detail}`);
      }
      if (file.parentOf(node) !== (ctx.parent ?? null)) {
        note("parentMatch", `${path}: ${node.type} parentOf disagrees with ctx.parent`);
      }
    },
  });

  const reference = outOfWalkOrder(file.references, walkOrder, (r) => r.node);
  if (reference !== null) {
    note("ordering", `${path}: reference ${reference.name} out of walk order`);
  }
  const binding = outOfWalkOrder(file.bindings, walkOrder, (b) => b.declarations[0]);
  if (binding !== null) note("ordering", `${path}: binding ${binding.name} out of walk order`);

  for (const fn of nodesOf(file.program, ...FUNCTION_TYPES)) {
    let native: Set<number>;
    try {
      native = new Set(file.capturesOf(fn).map((c) => c.binding.id));
    } catch (error) {
      note("crashed", `${path}: capturesOf: ${(error as Error).message}`);
      continue;
    }
    const expected = expectedCaptures(file, fn);
    if (native.size !== expected.size || [...native].some((id) => !expected.has(id))) {
      note("captures", `${path}: ${fn.type} native=[${[...native]}] expected=[${[...expected]}]`);
    }
  }

  for (const record of file.imports) {
    if (record.local && (record.local.flags & BindingFlags.Import) === 0) {
      note("records", `${path}: import local '${record.local.name}' not flagged import`);
    }
  }
  for (const record of file.exports) {
    if (record.local && !file.bindings.includes(record.local)) {
      note("records", `${path}: export local not in bindings`);
    }
  }

  const again = parse(source, { semantic: true, path, sourceType });
  if (fingerprint(file) !== fingerprint(again)) note("determinism", `${path}: non-deterministic`);
  checkEarlyNodes(path, again, file);
}

function checkEarlyNodes(path: string, lazy: SourceFile, eager: SourceFile): void {
  const references = [...lazy.references].reverse();
  const early: Node[] = [];
  for (const reference of references) {
    early.push(reference.node);
    if (lazy.referenceOf(reference.node) !== reference) {
      note("earlyNodes", `${path}: referenceOf(${reference.name}) before the program`);
    }
  }
  for (const binding of lazy.bindings) early.push(...binding.declarations);
  const difference = astDiffPath(lazy.program, eager.program);
  if (difference !== null) note("earlyNodes", `${path}: ${difference}`);
  const nodes = new Set<Node>();
  walk(lazy.program, {
    enter(node) {
      nodes.add(node);
    },
  });
  const stray = early.find((node) => !nodes.has(node));
  if (stray !== undefined) {
    note("earlyNodes", `${path}: an early ${stray.type} is not in the program`);
  }
}

describe.skipIf(!corpusPresent())("analyzer corpus invariants", () => {
  beforeAll(async () => {
    await forEachCorpusFile(corpusAndProjectFiles(), (file, source) =>
      check(file.path, source, file.sourceType),
    );
  }, 300_000);

  test("the corpus is non-empty", () => {
    expect(analyzed).toBeGreaterThan(1000);
  });

  for (const [invariant, name] of Object.entries(INVARIANTS) as [Invariant, string][]) {
    test(name, () => {
      expect(violations[invariant]).toEqual([]);
    });
  }
});
