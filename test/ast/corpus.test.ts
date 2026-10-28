import { beforeAll, describe, expect, test } from "bun:test";
import { parse, walk, type Node, type SourceFile } from "yuku";
import { astDiffPath } from "../ast-helpers-for-test";
import {
  corpusAndProjectFiles,
  corpusPresent,
  forEachCorpusFile,
  type CorpusFile,
} from "../corpus";

type Invariant = "walk" | "options" | "tokens";

const INVARIANTS: Invariant[] = ["walk", "options", "tokens"];
const SAMPLE_MAX = 8;

const failures = Object.fromEntries(INVARIANTS.map((name) => [name, [] as string[]])) as Record<
  Invariant,
  string[]
>;
const checks = Object.fromEntries(INVARIANTS.map((name) => [name, 0])) as Record<Invariant, number>;

function note(invariant: Invariant, detail: string): void {
  if (failures[invariant].length < SAMPLE_MAX) failures[invariant].push(detail);
}

function checkWalk(file: CorpusFile, program: Node, copy: Node): void {
  checks.walk++;
  const entered = new Set<Node>();
  let leaves = 0;
  walk(program, {
    enter(node) {
      if (entered.has(node)) note("walk", `${file.path}: enters a ${node.type} twice`);
      entered.add(node);
    },
    leave() {
      leaves++;
    },
  });
  if (entered.size !== leaves) note("walk", `${file.path}: enters and leaves differ`);

  const edited = structuredClone(copy);
  walk(edited, {
    Identifier(node, ctx) {
      ctx.replace(structuredClone(node));
      ctx.skip();
    },
  });
  if (!Bun.deepEquals(edited, copy, true)) note("walk", `${file.path}: replacing with clones`);
}

function checkOptions(file: CorpusFile, source: string, plain: SourceFile): void {
  checks.options++;
  const options = { lang: file.lang, sourceType: file.sourceType, preserveParens: false };
  const full = parse(source, { ...options, tokens: true, semantic: true });
  const difference = astDiffPath(full.program, plain.program);
  if (difference !== null) note("options", `${file.path}: ${difference}`);
  if (full.diagnostics.length !== plain.diagnostics.length) {
    note("options", `${file.path}: diagnostics differ`);
  }

  checks.tokens++;
  const { tokens } = full;
  walk(full.program, {
    enter(node, ctx) {
      const [from, to] = tokens.range(node);
      if (from === to) return;
      const inside = tokens.start(from) >= node.start && tokens.end(to - 1) <= node.end;
      const before = from === 0 || tokens.end(from - 1) <= node.start;
      const after = to === tokens.length || tokens.start(to) >= node.end;
      if (!inside || !before || !after) {
        note("tokens", `${file.path}: ${node.type} at ${node.start}`);
        ctx.stop();
      }
    },
  });
}

function checkFile(file: CorpusFile, source: string): void {
  const options = { lang: file.lang, sourceType: file.sourceType, preserveParens: false };
  const plain = parse(source, options);
  if (plain.diagnostics.length > 0) return;
  checkWalk(file, plain.program, structuredClone(plain.program));
  checkOptions(file, source, plain);
}

describe.skipIf(!corpusPresent())("AST tools across the corpus", () => {
  beforeAll(async () => {
    await forEachCorpusFile(corpusAndProjectFiles(), checkFile);
  }, 1_800_000);

  for (const invariant of INVARIANTS) {
    test(invariant, () => {
      expect(checks[invariant]).toBeGreaterThan(0);
      expect(failures[invariant]).toEqual([]);
    });
  }
});
