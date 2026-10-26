import { beforeAll, describe, expect, test } from "bun:test";
import { js, parse, ts, walk, type Node, type SourceFile } from "yuku";
import { is } from "yuku/utils";
import { astDiffPath } from "../ast-helpers-for-test";
import {
  corpusAndProjectFiles,
  corpusPresent,
  forEachCorpusFile,
  type CorpusFile,
} from "../corpus";

type Invariant = "walk" | "options" | "tokens" | "template" | "text";

const INVARIANTS: Invariant[] = ["walk", "options", "tokens", "template", "text"];
const CONTEXTS = [
  ["async function* _() {\n", "\n}", "module"],
  ["", "", "module"],
  ["", "", "commonjs"],
] as const;
const SAMPLE_MAX = 8;
const CANDIDATES_MAX = 3;
const CHILDREN_MAX = 3;
const TEXT_MAX = 400;
const IGNORED = new Set(["start", "end", "raw", "comments"]);
const NOT_HOLES = new Set(["TemplateElement", "JSXText", "SwitchCase"]);

const failures = Object.fromEntries(INVARIANTS.map((name) => [name, [] as string[]])) as Record<
  Invariant,
  string[]
>;
const checks = Object.fromEntries(INVARIANTS.map((name) => [name, 0])) as Record<Invariant, number>;

function note(invariant: Invariant, detail: string): void {
  if (failures[invariant].length < SAMPLE_MAX) failures[invariant].push(detail);
}

function isEmpty(value: unknown): boolean {
  return value == null || value === false || (Array.isArray(value) && value.length === 0);
}

function sameTree(a: unknown, b: unknown): boolean {
  if (a === b) return true;
  if (isEmpty(a) && isEmpty(b)) return true;
  if (a instanceof RegExp || b instanceof RegExp) return String(a) === String(b);
  if (typeof a !== "object" || typeof b !== "object" || a === null || b === null) return false;
  if (Array.isArray(a) !== Array.isArray(b)) return false;
  const left = a as Record<string, unknown>;
  const right = b as Record<string, unknown>;
  for (const key of new Set([...Object.keys(left), ...Object.keys(right)])) {
    if (!IGNORED.has(key) && !sameTree(left[key], right[key])) return false;
  }
  return true;
}

function childrenOf(node: Node): Node[] {
  const children: Node[] = [];
  for (const [key, value] of Object.entries(node)) {
    if (key === "comments") continue;
    for (const item of Array.isArray(value) ? value : [value]) {
      if (item !== null && typeof item === "object" && typeof item.type === "string") {
        children.push(item as Node);
      }
    }
  }
  return children;
}

function fragment(text: string, lang: "jsx" | "tsx", statement: boolean): Node | null {
  for (const [before, after, sourceType] of CONTEXTS) {
    const code = `${before}${text}${after}`;
    const { program, diagnostics } = parse(code, { lang, sourceType, preserveParens: false });
    if (diagnostics.length > 0) continue;
    const first = program.body.length === 1 ? program.body[0] : undefined;
    const wrapped = first?.type === "FunctionDeclaration" ? first.body!.body : [];
    const body = before === "" ? program.body : wrapped;
    if (body.length !== 1) continue;
    const root = body[0]!;
    if (statement) return root;
    return root.type === "ExpressionStatement" && !text.endsWith(";") ? root.expression : null;
  }
  return null;
}

function contains(root: Node, target: Node): boolean {
  let found = false;
  walk(root, {
    enter(node, ctx) {
      if (node === target) {
        found = true;
        ctx.stop();
      }
    },
  });
  return found;
}

function candidatesOf(program: Node, source: string): Node[] {
  const all: Node[] = [];
  walk(program, {
    enter(node) {
      if (node === program || node.end - node.start > TEXT_MAX) return;
      if ((!is.Expression(node) && !is.Statement(node)) || is.Directive(node)) return;
      if (source.slice(node.start, node.end).includes("__yuku_hole_")) return;
      if (childrenOf(node).length > 0) all.push(node);
    },
  });
  const stride = Math.max(1, Math.floor(all.length / CANDIDATES_MAX));
  return all.filter((_, i) => i % stride === 0).slice(0, CANDIDATES_MAX);
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

function checkTemplates(file: CorpusFile, source: string, program: Node): void {
  const typed = file.lang === "ts" || file.lang === "tsx" || file.lang === "dts";
  for (const node of candidatesOf(program, source)) {
    const slice = source.slice(node.start, node.end);
    const terminator = node.type === "ExpressionStatement" && !slice.endsWith(";") ? ";" : "";
    const text = `${slice}${terminator}`;
    if (!sameTree(fragment(text, typed ? "tsx" : "jsx", is.Statement(node)), node)) continue;
    const where = `${file.path}: [${node.type}] \`${text.slice(0, 80)}\``;
    for (const child of childrenOf(node).slice(0, CHILDREN_MAX)) {
      if (!isHole(source, node, child)) continue;
      const before = source.slice(node.start, child.start);
      const after = `${source.slice(child.end, node.end)}${terminator}`;
      const ends = is.Statement(child) || is.ModuleDeclaration(child) ? [";", ""] : [""];
      const parts = ends.map((end) => [before, `${end}${after}`]);
      checkTemplate(`${where} [${node.type} > ${child.type}]`, node, child, typed, parts);
      if (child.type === "Literal" && typeof child.value === "string") {
        checkText(`${where} [${node.type} > string]`, node, child.value, typed, [
          `${before}"`,
          `"${after}`,
        ]);
      }
    }
  }
}

function isHole(source: string, node: Node, child: Node): boolean {
  if (NOT_HOLES.has(child.type) || is.Directive(child) || child.end <= child.start) return false;
  if (child.start === node.start && child.end === node.end) return false;
  const jsx = node.type === "JSXElement" || node.type === "JSXFragment";
  if (jsx && (node.children as Node[]).includes(child)) return false;
  return !/[\w$]/.test(source[child.start - 1] ?? "") && !/[\w$]/.test(source[child.end] ?? "");
}

function checkTemplate(
  where: string,
  node: Node,
  child: Node,
  typed: boolean,
  variants: string[][],
): void {
  for (const parts of variants) {
    const raw = parts.map(escaped);
    const strings = Object.assign([...raw], { raw }) as TemplateStringsArray;
    let built: Node;
    try {
      built = (typed ? ts : js)(strings, child);
    } catch (error) {
      if (error instanceof SyntaxError) continue;
      const message = (error as Error).message;
      if (!/in text, not code|one expression or statement/.test(message)) {
        note("template", `${where} throws ${message}`);
      }
      return;
    }
    if (built.type !== node.type) return;
    checks.template++;
    if (!sameTree(built, node)) note("template", `${where} rebuilds differently`);
    else if (!contains(built, child)) note("template", `${where} copies its value`);
    return;
  }
}

function checkText(
  where: string,
  node: Node,
  value: string,
  typed: boolean,
  parts: string[],
): void {
  const raw = parts.map(escaped);
  const strings = Object.assign([...raw], { raw }) as TemplateStringsArray;
  let built: Node;
  try {
    built = (typed ? ts : js)(strings, value);
  } catch (error) {
    if (error instanceof SyntaxError) return;
    note("text", `${where} throws ${(error as Error).message}`);
    return;
  }
  checks.text++;
  if (!sameTree(built, node)) note("text", `${where} rebuilds differently`);
}

function escaped(code: string): string {
  return code.replace(/(\\*)(`|\$\{)/g, (_, slashes: string, token: string) => {
    return `${slashes}${slashes}\\${token}`;
  });
}

function checkFile(file: CorpusFile, source: string): void {
  const options = { lang: file.lang, sourceType: file.sourceType, preserveParens: false };
  const plain = parse(source, options);
  if (plain.diagnostics.length > 0) return;
  checkWalk(file, plain.program, structuredClone(plain.program));
  checkOptions(file, source, plain);
  checkTemplates(file, source, plain.program);
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
