// every plan reparses, keeps each comment, and is idempotent, and print and compact keep the AST

import { beforeAll, describe, expect, test } from "bun:test";
import { parse, type ParseOptions, print, type PrintOptions, type SourceLang } from "yuku";
import { astDiffPath } from "../ast-helpers-for-test";
import {
  corpusAndProjectFiles,
  corpusPresent,
  forEachCorpusFile,
  type CorpusFile,
} from "../corpus";

type Plan = "print" | "compact" | "strip" | "minify";

const PLANS: Record<Plan, PrintOptions> = {
  print: { comments: "all" },
  compact: { format: "compact", comments: "all" },
  strip: { strip: true, comments: "all" },
  minify: { minify: true, comments: "all" },
};

const SAMPLE_MAX = 8;
const failures: Record<Plan, string[]> = { print: [], compact: [], strip: [], minify: [] };
let checked = 0;

function note(plan: Plan, detail: string): void {
  if (failures[plan].length < SAMPLE_MAX) failures[plan].push(detail);
}

function checkFile(file: CorpusFile, source: string): void {
  const parseOptions: ParseOptions = {
    lang: file.lang,
    sourceType: file.sourceType,
    attachComments: true,
    // the printer must rebuild exactly the parens precedence requires
    preserveParens: false,
  };
  const ast = parse(source, parseOptions);
  if (ast.diagnostics.length > 0) return;
  checked++;

  const typed = file.lang !== "js" && file.lang !== "jsx";
  for (const [plan, options] of Object.entries(PLANS) as [Plan, PrintOptions][]) {
    let code: string;
    try {
      code = print(ast.program, options).code;
    } catch (error) {
      note(plan, `${file.path}: threw ${(error as Error).message}`);
      continue;
    }

    const lang = plan === "strip" ? stripLang(file.lang) : file.lang;
    const reparseOptions = { ...parseOptions, lang };
    const reparsed = parse(code, reparseOptions);
    if (reparsed.diagnostics.length > 0) {
      note(plan, `${file.path}: reparse failed`);
      continue;
    }

    if (!(plan === "strip" && typed) && commentKey(ast) !== commentKey(reparsed)) {
      note(plan, `${file.path}: comment lost or duplicated`);
      continue;
    }

    if (plan === "print" || plan === "compact") {
      const diff = astDiffPath(ast.program, reparsed.program);
      if (diff) {
        note(plan, `${file.path}: ast roundtrip ${diff}`);
        continue;
      }
    }

    let second: string;
    try {
      second = print(reparsed.program, options).code;
    } catch (error) {
      note(plan, `${file.path}: second pass threw ${(error as Error).message}`);
      continue;
    }
    if (second === code) continue;

    const reparsedTwice = parse(second, reparseOptions);
    const bare = { ...options, comments: "none" as const };
    if (print(reparsed.program, bare).code !== print(reparsedTwice.program, bare).code) {
      note(plan, `${file.path}: not idempotent (code)`);
    } else if (commentKey(reparsed) !== commentKey(reparsedTwice)) {
      note(plan, `${file.path}: not idempotent (comment lost or duplicated)`);
    }
  }
}

// a moved or re-indented comment keeps its key
function commentKey(result: { comments: readonly { type: string; value: string }[] }): string {
  const normalize = (value: string) =>
    value
      .replace(/\r\n?/g, "\n")
      .split("\n")
      .map((line) => line.trim())
      .join("\n");
  return result.comments
    .map((comment) => `${comment.type}:${normalize(comment.value)}`)
    .sort()
    .join("\0");
}

function stripLang(lang: SourceLang): SourceLang {
  if (lang === "tsx") return "jsx";
  if (lang === "ts" || lang === "dts") return "js";
  return lang;
}

describe.skipIf(!corpusPresent())("codegen corpus invariants", () => {
  beforeAll(async () => {
    await forEachCorpusFile(corpusAndProjectFiles(), checkFile);
  }, 300_000);

  test("the corpus is non-empty", () => {
    expect(checked).toBeGreaterThan(1000);
  });

  test("print round-trips and is idempotent", () => {
    expect(failures.print).toEqual([]);
  });

  test("compact round-trips and is idempotent", () => {
    expect(failures.compact).toEqual([]);
  });

  test("strip reparses and is idempotent", () => {
    expect(failures.strip).toEqual([]);
  });

  test("minify reparses and is idempotent", () => {
    expect(failures.minify).toEqual([]);
  });
});
