import { describe, expect, test } from "bun:test";
import type { Node } from "yuku-parser";
import { parse } from "yuku-parser";
import { walk } from "yuku-ast";
import { corpusFiles, corpusPresent } from "../corpus";

// quasis and expressions interleave in source, but walk order is by
// key, matching every ESTree walker
const INTERLEAVED_TYPES = new Set(["TemplateLiteral", "TSTemplateLiteralType"]);

/** The first child entered out of source order, if any. */
function walkOrderViolation(program: Node): string | null {
  let violation: string | null = null;
  const lastStart = new Map<Node, number>();
  walk(program, {
    enter(node, ctx) {
      const parent = ctx.parent;
      if (parent === null || INTERLEAVED_TYPES.has(parent.type)) return;
      const prev = lastStart.get(parent);
      if (prev !== undefined && node.start < prev) {
        violation = `${node.type} at ${node.start} entered after a sibling at ${prev} inside ${parent.type}`;
        ctx.stop();
        return;
      }
      lastStart.set(parent, node.start);
    },
  });
  return violation;
}

describe("walk order", () => {
  test("siblings are entered in source order", () => {
    const { program } = parse(`const a = f(1, x + y, [b, ...c]); class C { m() {} n() {} }`, {
      sourceType: "module",
      lang: "ts",
    });
    expect(walkOrderViolation(program)).toBeNull();
  });

  test.skipIf(!corpusPresent())("holds across the parser corpus", async () => {
    const violations: string[] = [];
    let checked = 0;
    for (const file of corpusFiles()) {
      const source = await Bun.file(file.path).text();
      const { program, diagnostics } = parse(source, {
        sourceType: file.sourceType,
        lang: file.lang,
      });
      if (diagnostics.length > 0) continue;
      checked++;
      const violation = walkOrderViolation(program);
      if (violation !== null && violations.length < 8) {
        violations.push(`${file.path}: ${violation}`);
      }
    }
    console.log(`walk order: ${checked} files in source order`);
    expect(violations).toEqual([]);
    expect(checked).toBeGreaterThan(1000);
  }, 240_000);
});
