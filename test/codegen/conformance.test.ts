// The JS printer against the Zig printer over the whole corpus, the deep chains, and the
// instantiation expressions, one test per plan of `conformance.ts`. Code, source map mappings, and
// diagnostics must match byte for byte.
//
// The Zig side is `zig build codegen-reference`, which `bun test:codegen` builds first.

import { describe, expect, test } from "bun:test";
import { corpusPresent } from "../corpus";
import { conformanceInputs, describeMismatch, PLANS, runPlan } from "./conformance";

const SAMPLE_MAX = 3;
const TIMEOUT_MS = 120_000;

describe.skipIf(!corpusPresent())("conformance with the Zig printer", () => {
  const files = conformanceInputs();

  for (const plan of PLANS) {
    test(
      plan.name,
      () => {
        const result = runPlan(plan, files);
        expect(result.compared).toBeGreaterThan(0);
        const sample = result.mismatches.slice(0, SAMPLE_MAX);
        expect(sample.map((mismatch) => describeMismatch(mismatch))).toEqual([]);
      },
      TIMEOUT_MS,
    );
  }
});
