import { beforeAll, describe, expect, test } from "bun:test";
import { Analyzer, SymbolFlags, type Module } from "yuku-analyzer";
import type { Node } from "yuku-parser";
import { corpusPresent, forEachCorpusFile } from "../corpus";

const SAMPLE_MAX = 8;

// one list per invariant, a check pushes a `path: detail` line on violation
const violations = {
  crashed: [] as string[],
  crossIndex: [] as string[],
  resolutionScope: [] as string[],
  nodeIdentity: [] as string[],
  walkScanParity: [] as string[],
  scopeMatch: [] as string[],
  captures: [] as string[],
  determinism: [] as string[],
  records: [] as string[],
};
let analyzed = 0;

function note(list: string[], detail: string): void {
  if (list.length < SAMPLE_MAX) list.push(detail);
}

function captureOracle(module: Module, fn: Node): Set<number> {
  const fnScope = module.scopes.find((s) => s.node === fn && s.kind === "function");
  if (fnScope === undefined) return new Set();
  const within = (scope: typeof fnScope | null): boolean => {
    for (let s: typeof fnScope | null = scope; s; s = s.parent) if (s === fnScope) return true;
    return false;
  };
  const captured = new Set<number>();
  for (const reference of module.references) {
    if (reference.kind !== "value" || reference.symbol === null) continue;
    if (reference.node.start < fn.start || reference.node.end > fn.end) continue;
    if (within(reference.symbol.scope)) continue;
    captured.add(reference.symbol.id);
  }
  return captured;
}

function fingerprint(module: Module): string {
  return JSON.stringify([
    module.scopes.map((s) => s.kind),
    module.symbols.map((s) => s.flags),
    module.references.map((r) => r.symbol?.id ?? -1),
  ]);
}

function check(path: string, source: string): void {
  let module: Module;
  try {
    module = new Analyzer().addFile(path, source);
    // touch every section so a decode fault throws here, not later
    void module.ast;
    void module.scopes;
    void module.symbols;
    void module.references;
    void module.imports;
    void module.exports;
  } catch (error) {
    note(violations.crashed, `${path}: ${(error as Error).message}`);
    return;
  }
  analyzed++;

  // cross-index symmetry. back-references agree and the reference set
  // partitions cleanly into resolved (owned by one symbol) and unresolved
  let ownedReferences = 0;
  for (const symbol of module.symbols) {
    for (const reference of symbol.references) {
      if (reference.symbol !== symbol)
        note(violations.crossIndex, `${path}: ${symbol.name} back-ref`);
    }
    ownedReferences += symbol.references.length;
  }
  for (const scope of module.scopes) {
    for (const binding of scope.bindings) {
      if (binding.scope !== scope)
        note(violations.crossIndex, `${path}: ${binding.name} scope back-ref`);
    }
  }
  const resolved = module.references.filter((r) => r.symbol !== null).length;
  if (ownedReferences !== resolved) {
    note(violations.crossIndex, `${path}: owned ${ownedReferences} != resolved ${resolved}`);
  }
  if (module.unresolvedReferences.length + resolved !== module.references.length) {
    note(violations.crossIndex, `${path}: partition mismatch`);
  }

  // resolution soundness. a resolved binding is visible from the use site,
  // its scope an ancestor-or-self of the reference scope
  for (const reference of module.references) {
    if (reference.symbol && !reference.symbol.scope.contains(reference.scope)) {
      note(violations.resolutionScope, `${path}: ${reference.name} resolves out of scope`);
    }
  }

  // node identity round-trips, exact in both directions. materialize the
  // whole AST first so the registration holds no matter which path built a
  // node first
  void module.ast;
  for (const symbol of module.symbols) {
    const decl = symbol.declarations[0];
    if (decl === undefined) continue;
    const owner = module.symbolOf(decl);
    if (owner !== symbol) {
      note(
        violations.nodeIdentity,
        `${path}: symbolOf(decl ${symbol.name}) is ${owner === null ? "null" : `#${owner.id}`}`,
      );
    }
  }
  for (const reference of module.references) {
    if (module.referenceOf(reference.node) !== reference) {
      note(violations.nodeIdentity, `${path}: referenceOf(${reference.name})`);
    }
    if (module.symbolOf(reference.node) !== reference.symbol) {
      note(violations.nodeIdentity, `${path}: symbolOf(ref ${reference.name})`);
    }
  }

  // walk and scan visit the same nodes in the same order. and the per-node
  // scope (ctx.scope) agrees with the per-reference scope at every reference.
  // both come from the binder, so a disagreement means a decode or index bug.
  const walked: string[] = [];
  module.walk({
    enter(node, ctx) {
      walked.push(node.type);
      const reference = ctx.reference;
      if (reference !== null && ctx.scope !== reference.scope) {
        note(violations.scopeMatch, `${path}: ${reference.name} ctx ${ctx.scope.id} vs ref ${reference.scope.id}`);
      }
    },
  });
  const scanned: string[] = [];
  module.scan({ enter: (cursor) => scanned.push(cursor.type) });
  if (walked.length !== scanned.length || walked.some((t, i) => t !== scanned[i])) {
    note(violations.walkScanParity, `${path}: walk ${walked.length} vs scan ${scanned.length}`);
  }

  // captures match an independent oracle for every function
  for (const fn of module.findAll([
    "FunctionDeclaration",
    "FunctionExpression",
    "ArrowFunctionExpression",
  ])) {
    let native: Set<number>;
    try {
      native = new Set(module.capturesOf(fn).map((c) => c.symbol.id));
    } catch {
      continue;
    }
    const oracle = captureOracle(module, fn);
    if (native.size !== oracle.size || [...native].some((id) => !oracle.has(id))) {
      note(
        violations.captures,
        `${path}: ${fn.type} native=[${[...native]}] oracle=[${[...oracle]}]`,
      );
    }
  }

  // module records are well formed
  for (const record of module.imports) {
    if (record.local && (record.local.flags & SymbolFlags.Import) === 0) {
      note(violations.records, `${path}: import local '${record.local.name}' not flagged import`);
    }
  }
  for (const record of module.exports) {
    if (record.local && !module.symbols.includes(record.local)) {
      note(violations.records, `${path}: export local not in symbols`);
    }
  }

  // determinism. a second independent analysis yields an identical model
  const again = new Analyzer().addFile(path, source);
  if (fingerprint(module) !== fingerprint(again)) {
    note(violations.determinism, `${path}: non-deterministic`);
  }
}

describe.skipIf(!corpusPresent())("analyzer corpus invariants", () => {
  beforeAll(async () => {
    await forEachCorpusFile((file, source) => check(file.path, source));
  }, 300_000);

  test("the corpus is non-empty", () => {
    expect(analyzed).toBeGreaterThan(1000);
  });

  test("analysis never crashes", () => {
    expect(violations.crashed).toEqual([]);
  });

  test("scope and symbol cross-indexes are symmetric", () => {
    expect(violations.crossIndex).toEqual([]);
  });

  test("every resolved reference is in scope of its binding", () => {
    expect(violations.resolutionScope).toEqual([]);
  });

  test("node-to-model lookups round-trip", () => {
    expect(violations.nodeIdentity).toEqual([]);
  });

  test("walk and scan agree node for node", () => {
    expect(violations.walkScanParity).toEqual([]);
  });

  test("the walked scope matches the binder's scope at every reference", () => {
    expect(violations.scopeMatch).toEqual([]);
  });

  test("capturesOf matches an independent oracle", () => {
    expect(violations.captures).toEqual([]);
  });

  test("module records are well formed", () => {
    expect(violations.records).toEqual([]);
  });

  test("analysis is deterministic", () => {
    expect(violations.determinism).toEqual([]);
  });
});
