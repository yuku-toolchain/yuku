import { existsSync } from "node:fs";
import { mkdir } from "node:fs/promises";
import { basename, dirname, join } from "node:path";
import { Glob } from "bun";
import { diff } from "jest-diff";
import {
  parse,
  langFromPath,
  sourceTypeFromPath,
  type ParseOptions,
  type SourceLang,
} from "yuku-parser";
import {
  print,
  strip,
  minify,
  type CodegenOptions,
  type CodegenResult,
} from "yuku-codegen";

type Op = "print" | "strip" | "minify";

const OPS: Record<Op, (ast: ReturnType<typeof parse>, o: CodegenOptions) => CodegenResult> = {
  print,
  strip,
  minify,
};

interface Suite {
  path: string;
  op: Op;
  options?: CodegenOptions;
  parseOptions?: Partial<ParseOptions>;
}

const SUITES: Suite[] = [
  { path: "test/codegen/cases/print", op: "print" },
  { path: "test/codegen/cases/print-all", op: "print", options: { comments: true } },
  { path: "test/codegen/cases/print-line", op: "print", options: { comments: "line" } },
  { path: "test/codegen/cases/print-block", op: "print", options: { comments: "block" } },
  { path: "test/codegen/cases/print-none", op: "print", options: { comments: false } },
  { path: "test/codegen/cases/strip", op: "strip", options: { comments: true } },
  { path: "test/codegen/cases/minify", op: "minify", options: { format: "compact" } },
];

const CORPUS_DIRS = [
  "test/parser/suite/js/pass",
  "test/parser/suite/jsx/pass",
  "test/parser/suite/ts/pass",
];

const CORPUS_OPS: { op: Op; options: CodegenOptions; outLang: (l: SourceLang) => SourceLang }[] = [
  { op: "print", options: { format: "pretty", comments: true }, outLang: (l) => l },
  { op: "strip", options: { format: "pretty", comments: true }, outLang: stripLang },
  { op: "minify", options: { format: "compact", comments: true }, outLang: (l) => l },
];

const updateSnapshots = process.argv.includes("--update-snapshots");
let failed = 0;

await runCases();
await runCorpus();
process.exit(failed > 0 ? 1 : 0);

async function runCases() {
  console.log("cases");
  for (const suite of SUITES) {
    const files = [...new Glob(`${suite.path}/*.{js,jsx,ts,tsx}`).scanSync(".")].sort();
    let ok = 0;
    for (const file of files) {
      if (await runCase(suite, file)) ok++;
      else failed++;
    }
    console.log(`  ${suite.op.padEnd(7)} ${basename(suite.path).padEnd(14)} ${ok}/${files.length}`);
  }
}

async function runCase(suite: Suite, file: string): Promise<boolean> {
  const source = await Bun.file(file).text();
  const ast = parse(source, {
    lang: langFromPath(file),
    sourceType: sourceTypeFromPath(file),
    attachComments: true,
    ...suite.parseOptions,
  });

  let got: string;
  try {
    got = OPS[suite.op](ast, suite.options ?? {}).code;
  } catch (e) {
    console.log(`    ✗ ${file}: ${suite.op} threw: ${e}`);
    return false;
  }

  const snapPath = join(dirname(file), "snapshots", `${basename(file)}.snapshot`);
  if (!existsSync(snapPath) || updateSnapshots) {
    await mkdir(dirname(snapPath), { recursive: true });
    await Bun.write(snapPath, got);
    return true;
  }
  const want = await Bun.file(snapPath).text();
  if (got === want) return true;
  console.log(`    ✗ ${file}: snapshot mismatch`);
  console.log("      " + (diff(want, got, { contextLines: 2 }) ?? "").replace(/\n/g, "\n      "));
  return false;
}

async function runCorpus() {
  console.log("\ncorpus");
  const totals = newCounts();
  for (const dir of CORPUS_DIRS) {
    if (!existsSync(dir)) {
      console.log(`  ${dir}: not found (run \`bun load-files\`)`);
      continue;
    }
    const files = [...new Glob("**/*.{js,jsx,ts,tsx,mjs,cjs,mts,cts}").scanSync({ cwd: dir })];
    const t = performance.now();
    const errs: string[] = [];
    const counts = newCounts();

    for (let i = 0; i < files.length; i += 256) {
      const batch = files.slice(i, i + 256);
      const results = await Promise.all(batch.map((f) => corpusFile(join(dir, f))));
      for (const r of results) for (const op of CORPUS_OPS) {
        const c = counts[op.op];
        const s = r[op.op];
        if (s.status === "pass") c.ok++;
        else if (s.status === "skip") c.skip++;
        else {
          c.bad++;
          errs.push(`${s.file}: ${op.op} ${s.err}`);
        }
      }
    }

    const ms = Math.round(performance.now() - t);
    console.log(`  ${dir}: ${ms}ms, ${summary(counts)}`);
    for (const e of errs.slice(0, 10)) console.log(`    ✗ ${e}`);
    if (errs.length > 10) console.log(`    ... ${errs.length - 10} more`);

    for (const op of CORPUS_OPS) {
      totals[op.op].ok += counts[op.op].ok;
      totals[op.op].bad += counts[op.op].bad;
      totals[op.op].skip += counts[op.op].skip;
      failed += counts[op.op].bad;
    }
  }
  console.log(`  total: ${summary(totals)}`);
}

type Counts = Record<Op, { ok: number; bad: number; skip: number }>;

function newCounts(): Counts {
  return {
    print: { ok: 0, bad: 0, skip: 0 },
    strip: { ok: 0, bad: 0, skip: 0 },
    minify: { ok: 0, bad: 0, skip: 0 },
  };
}

function summary(c: Counts): string {
  return CORPUS_OPS.map((o) => {
    const x = c[o.op];
    return `${o.op} ${x.ok}/${x.ok + x.bad}${x.skip ? ` (+${x.skip} skipped)` : ""}`;
  }).join(", ");
}

type OpStatus =
  | { status: "pass" }
  | { status: "skip" }
  | { status: "fail"; file: string; err: string };

async function corpusFile(file: string): Promise<Record<Op, OpStatus>> {
  const lang = langFromPath(file);
  const sourceType = sourceTypeFromPath(file);
  const source = await Bun.file(file).text();
  const ast = parse(source, { lang, sourceType, attachComments: true });
  const out: Record<Op, OpStatus> = {
    print: { status: "skip" },
    strip: { status: "skip" },
    minify: { status: "skip" },
  };
  // skip parser-corpus oddities (file doesn't parse under its canonical lang)
  if (ast.diagnostics.length > 0) return out;

  for (const op of CORPUS_OPS) {
    let code: string;
    try {
      code = OPS[op.op](ast, op.options).code;
    } catch (e) {
      out[op.op] = { status: "fail", file, err: `threw: ${e}` };
      continue;
    }
    const reparse = parse(code, { lang: op.outLang(lang), sourceType });
    out[op.op] = reparse.diagnostics.length === 0
      ? { status: "pass" }
      : { status: "fail", file, err: "reparse failed" };
  }
  return out;
}

function stripLang(lang: SourceLang): SourceLang {
  if (lang === "tsx") return "jsx";
  if (lang === "ts" || lang === "dts") return "js";
  return lang;
}
