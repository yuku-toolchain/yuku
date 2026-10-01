import { beforeAll, describe, expect, test } from "bun:test";
import { Analyzer, BindingFlags, type Module } from "yuku-analyzer";
import type { Node } from "yuku-parser";
import { corpusPresent, forEachCorpusFile } from "../corpus";

const SAMPLE_MAX = 8;

// one list per invariant, a check pushes a `path: detail` line on violation
const violations = {
  crashed: [] as string[],
  crossIndex: [] as string[],
  resolution: [] as string[],
  nodeIdentity: [] as string[],
  scopeMatch: [] as string[],
  parentMatch: [] as string[],
  captures: [] as string[],
  determinism: [] as string[],
  records: [] as string[],
  ordering: [] as string[],
};
let analyzed = 0;

// resolution can differ from lookup for these
const POSITION_DEPENDENT =
  BindingFlags.TypeParameter | BindingFlags.Parameter | BindingFlags.FunctionScopedVariable;

function note(list: string[], detail: string): void {
  if (list.length < SAMPLE_MAX) list.push(detail);
}

function expectedCaptures(module: Module, fn: Node): Set<number> {
  const fnScope = module.scopes.find((s) => s.node === fn && s.kind === "function");
  if (fnScope === undefined) return new Set();
  const within = (scope: typeof fnScope | null): boolean => {
    for (let s: typeof fnScope | null = scope; s; s = s.parent) if (s === fnScope) return true;
    return false;
  };
  const captured = new Set<number>();
  for (const reference of module.references) {
    if (reference.inTypePosition || reference.binding === null) continue;
    if (reference.node.start < fn.start || reference.node.end > fn.end) continue;
    if (within(reference.binding.scope)) continue;
    captured.add(reference.binding.id);
  }
  return captured;
}

function fingerprint(module: Module): string {
  return JSON.stringify([
    module.scopes.map((s) => s.kind),
    module.bindings.map((s) => s.flags),
    module.references.map((r) => r.binding?.id ?? -1),
  ]);
}

function check(path: string, source: string): void {
  let module: Module;
  try {
    module = new Analyzer().setFile(path, source);
    // a decode fault throws here, not later
    void module.ast;
    void module.scopes;
    void module.bindings;
    void module.references;
    void module.imports;
    void module.exports;
  } catch (error) {
    note(violations.crashed, `${path}: ${(error as Error).message}`);
    return;
  }
  analyzed++;

  // back-references agree, and each reference is unresolved or owned by one binding
  let ownedReferences = 0;
  for (const binding of module.bindings) {
    for (const reference of binding.references) {
      if (reference.binding !== binding) {
        note(violations.crossIndex, `${path}: ${binding.name} back-ref`);
      }
    }
    ownedReferences += binding.references.length;
  }
  for (const scope of module.scopes) {
    for (const binding of scope.bindings) {
      if (binding.scope !== scope) {
        note(violations.crossIndex, `${path}: ${binding.name} scope back-ref`);
      }
    }
  }
  const resolved = module.references.filter((r) => r.binding !== null).length;
  if (ownedReferences !== resolved) {
    note(violations.crossIndex, `${path}: owned ${ownedReferences} != resolved ${resolved}`);
  }
  if (module.unresolvedReferences.length + resolved !== module.references.length) {
    note(violations.crossIndex, `${path}: partition mismatch`);
  }

  // resolution agrees with lookup
  for (const reference of module.references) {
    const { name, scope, space } = reference;
    const expected = module.lookup(name, { from: scope, space });
    if (expected === reference.binding || name === "arguments") continue;
    if (expected !== null && expected.has(POSITION_DEPENDENT)) continue;
    note(violations.resolution, `${path}: ${name} resolves apart from lookup`);
  }

  // node identity round-trips both ways
  void module.ast;
  for (const binding of module.bindings) {
    const decl = binding.declarations[0];
    if (decl === undefined) continue;
    const owner = module.bindingOf(decl);
    if (owner !== binding) {
      note(
        violations.nodeIdentity,
        `${path}: bindingOf(decl ${binding.name}) is ${owner === null ? "null" : `#${owner.id}`}`,
      );
    }
  }
  for (const reference of module.references) {
    if (module.referenceOf(reference.node) !== reference) {
      note(violations.nodeIdentity, `${path}: referenceOf(${reference.name})`);
    }
    if (module.bindingOf(reference.node) !== reference.binding) {
      note(violations.nodeIdentity, `${path}: bindingOf(ref ${reference.name})`);
    }
  }

  const walkOrder = new Map<Node, number>();
  module.walk({
    enter(node, ctx) {
      walkOrder.set(node, walkOrder.size);
      const reference = ctx.reference;
      if (reference !== null && ctx.scope !== reference.scope) {
        note(
          violations.scopeMatch,
          `${path}: ${reference.name} ctx ${ctx.scope.id} vs ref ${reference.scope.id}`,
        );
      }
      if (module.parentOf(node) !== (ctx.parent ?? null)) {
        note(violations.parentMatch, `${path}: ${node.type} parentOf disagrees with ctx.parent`);
      }
    },
  });

  // ids ascend with source
  let prevRef = -1;
  for (const reference of module.references) {
    const at = walkOrder.get(reference.node);
    if (at === undefined) continue;
    if (at < prevRef) {
      note(violations.ordering, `${path}: reference ${reference.name} out of walk order`);
      break;
    }
    prevRef = at;
  }
  let prevSym = -1;
  for (const binding of module.bindings) {
    const decl = binding.declarations[0];
    if (decl === undefined) continue;
    const at = walkOrder.get(decl);
    if (at === undefined) continue;
    if (at < prevSym) {
      note(violations.ordering, `${path}: binding ${binding.name} out of walk order`);
      break;
    }
    prevSym = at;
  }

  // captures recomputed independently from the reference table
  for (const fn of module.findAll([
    "FunctionDeclaration",
    "FunctionExpression",
    "ArrowFunctionExpression",
  ])) {
    let native: Set<number>;
    try {
      native = new Set(module.capturesOf(fn).map((c) => c.binding.id));
    } catch {
      continue;
    }
    const expected = expectedCaptures(module, fn);
    if (native.size !== expected.size || [...native].some((id) => !expected.has(id))) {
      note(
        violations.captures,
        `${path}: ${fn.type} native=[${[...native]}] expected=[${[...expected]}]`,
      );
    }
  }

  // module records are well formed
  for (const record of module.imports) {
    if (record.local && (record.local.flags & BindingFlags.Import) === 0) {
      note(violations.records, `${path}: import local '${record.local.name}' not flagged import`);
    }
  }
  for (const record of module.exports) {
    if (record.local && !module.bindings.includes(record.local)) {
      note(violations.records, `${path}: export local not in bindings`);
    }
  }

  // a second analysis yields an identical model
  const again = new Analyzer().setFile(path, source);
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

  test("scope and binding cross-indexes are symmetric", () => {
    expect(violations.crossIndex).toEqual([]);
  });

  test("every reference resolves to what lookup finds from its scope", () => {
    expect(violations.resolution).toEqual([]);
  });

  test("node-to-model lookups round-trip", () => {
    expect(violations.nodeIdentity).toEqual([]);
  });

  test("the walked scope matches the binder's scope at every reference", () => {
    expect(violations.scopeMatch).toEqual([]);
  });

  test("parentOf matches the walk's parent at every node", () => {
    expect(violations.parentMatch).toEqual([]);
  });

  test("capturesOf matches an independent recomputation", () => {
    expect(violations.captures).toEqual([]);
  });

  test("module records are well formed", () => {
    expect(violations.records).toEqual([]);
  });

  test("analysis is deterministic", () => {
    expect(violations.determinism).toEqual([]);
  });

  test("bindings and references are recorded in walk (source) order", () => {
    expect(violations.ordering).toEqual([]);
  });
});
