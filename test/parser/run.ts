import { mkdir } from "node:fs/promises";
import { basename, dirname, join } from "node:path";
import { Glob } from "bun";
import equal from "fast-deep-equal";
import { diff } from "jest-diff";
import {
  parse,
  type ParseOptions,
  type ParseResult,
  type Diagnostic,
  type SourceLang,
} from "yuku-parser";
import { deserializeAstJson, formatDiagnostics, serializeAstJson } from "../ast-helpers-for-test";

type Expect = "pass" | "fail" | "snapshot";

interface TestSuite {
  path: string;
  expect: Expect;
  lang: SourceLang[];
  options?: Partial<ParseOptions>;
  recursive?: boolean;
  allowErrors?: boolean;
  skipOnCI?: boolean;
}

interface FileResult {
  file: string;
  passed: boolean;
  snapshotCompared: boolean;
  reason?: string;
  source?: string;
  diagnostics?: Diagnostic[];
}

interface SuiteResult {
  suite: TestSuite;
  files: FileResult[];
}

const suites: TestSuite[] = [
  { path: "suite/js/pass", expect: "snapshot", lang: ["js"], options: { semanticErrors: true } },
  { path: "suite/js/fail", expect: "fail", lang: ["js"] },
  {
    path: "suite/js/semantic",
    expect: "fail",
    lang: ["js"],
    options: { semanticErrors: true },
  },
  { path: "suite/jsx/pass", expect: "snapshot", lang: ["jsx"], options: { semanticErrors: true } },
  { path: "suite/jsx/fail", expect: "fail", lang: ["jsx"] },
  { path: "suite/ts/pass", expect: "snapshot", lang: ["ts", "tsx"], skipOnCI: true, options: { semanticErrors: true } },
  // { path: "suite/ts/fail", expect: "fail", lang: ["ts", "tsx"] },
  // { path: "suite/ts/semantic", expect: "fail", lang: ["ts", "tsx"], options: { semanticErrors: true } },
  { path: "misc/jsx", expect: "snapshot", lang: ["jsx"], recursive: false, allowErrors: true },
  { path: "misc/js", expect: "snapshot", lang: ["js"], recursive: false, allowErrors: true },
  {
    path: "misc/js/preserve-parens-disabled",
    expect: "snapshot",
    lang: ["js"],
    allowErrors: true,
    options: { preserveParens: false },
  },
  {
    path: "misc/js/allow-return-outside-function",
    expect: "snapshot",
    lang: ["js"],
    allowErrors: true,
    options: { allowReturnOutsideFunction: true },
  },
];

type SnapshotResult =
  | { status: "no_snapshot" }
  | { status: "match" }
  | { status: "mismatch"; snapshot: unknown };

const PARSER_DIR = "test/parser";
const RESULTS_DIR = "test/results/parser";
const isCI = !!process.env.CI;
const updateSnapshots = process.argv.includes("--update-snapshots");

function langFromPath(path: string): SourceLang {
  if (path.endsWith(".tsx")) return "tsx";
  if (path.endsWith(".jsx")) return "jsx";
  if (path.endsWith(".d.ts")) return "dts";
  if (path.endsWith(".ts")) return "ts";
  return "js";
}

function baseName(file: string): string {
  const name = basename(file);
  const dot = name.indexOf(".");
  return dot >= 0 ? name.substring(0, dot) : name;
}

function isSourceFile(path: string, langs: SourceLang[]): boolean {
  if (path.includes("/snapshots/") || path.endsWith(".snapshot.json")) return false;
  return langs.includes(langFromPath(path));
}

async function collectFiles(suite: TestSuite): Promise<string[]> {
  const pattern = suite.recursive === false ? "*" : "**/*";
  const glob = new Glob(`${PARSER_DIR}/${suite.path}/${pattern}`);
  const files: string[] = [];
  for await (const file of glob.scan(".")) {
    if (isSourceFile(file, suite.lang)) files.push(file);
  }
  return files;
}

function parseFile(content: string, file: string, suite: TestSuite) {
  return parse(content, {
    sourceType: file.includes(".module.") ? "module" : "script",
    lang: langFromPath(file),
    preserveParens: true,
    ...suite.options,
  });
}

function runTest(file: string, content: string, parsed: ParseResult, suite: TestSuite): boolean {
  const hasErrors = parsed.diagnostics.length > 0;

  switch (suite.expect) {
    case "pass":
      return !hasErrors;

    case "fail":
      return hasErrors;

    case "snapshot":
      if (hasErrors && !suite.allowErrors) return false;
      return true;
  }
}

async function checkSnapshot(file: string, parsed: ParseResult): Promise<SnapshotResult> {
  const snapshotFile = join(dirname(file), "snapshots", `${baseName(file)}.snapshot.json`);

  if (!(await Bun.file(snapshotFile).exists())) {
    return { status: "no_snapshot" };
  }

  const snapshot = deserializeAstJson(await Bun.file(snapshotFile).text());

  if (equal(parsed, snapshot)) {
    return { status: "match" };
  }

  if (updateSnapshots) {
    await Bun.write(snapshotFile, serializeAstJson(parsed, 2));
    return { status: "match" };
  }

  return { status: "mismatch", snapshot };
}

let progressCurrent = 0;
let progressTotal = 0;

function showProgress(file: string, passed: boolean) {
  if (isCI) return;
  progressCurrent++;
  const icon = passed ? "\x1b[32m✓\x1b[0m" : "\x1b[31m✗\x1b[0m";
  const label = file.length > 60 ? `...${file.slice(-57)}` : file;
  process.stdout.write(`\r\x1b[K  ${icon} ${progressCurrent}/${progressTotal}  ${label}`);
}

function clearProgress() {
  if (!isCI) process.stdout.write("\r\x1b[K");
}

async function runSuite(suite: TestSuite, files: string[]): Promise<SuiteResult> {
  const result: SuiteResult = { suite, files: [] };

  for (const file of files) {
    const content = await Bun.file(file).text();
    const parsed = parseFile(content, file, suite);
    let passed = runTest(file, content, parsed, suite);

    const entry: FileResult = { file, passed, snapshotCompared: false };

    if (parsed.diagnostics.length > 0) {
      entry.source = content;
      entry.diagnostics = parsed.diagnostics;
    }

    if (passed && suite.expect === "snapshot") {
      const snap = await checkSnapshot(file, parsed);
      if (snap.status !== "no_snapshot") {
        entry.snapshotCompared = true;
      }
      if (snap.status === "mismatch") {
        passed = false;
        entry.passed = false;
        entry.reason = "snapshot mismatch";
        clearProgress();
        console.log(`\nx ${file} (${entry.reason})\n${diff(snap.snapshot, parsed, { contextLines: 2 })}\n`);
      }
    }

    if (!passed) {
      clearProgress();
      if (
        suite.expect === "pass" ||
        (suite.expect === "snapshot" && !suite.allowErrors && parsed.diagnostics.length > 0)
      ) {
        entry.reason ??= "parse errors";
        console.log(`\nx ${file} (${entry.reason})`);
        console.log(formatDiagnostics(content, parsed.diagnostics, file));
      } else if (suite.expect === "fail" && parsed.diagnostics.length === 0) {
        entry.reason ??= "expected error, but parsed successfully";
        console.log(`\nx ${file} (${entry.reason})`);
      }
    }

    result.files.push(entry);
    showProgress(file, passed);
  }

  return result;
}

function writeResultFile(result: SuiteResult): string {
  const passed = result.files.filter((f) => f.passed).length;
  const failed = result.files.filter((f) => !f.passed).length;
  const total = result.files.length;
  const rate = ((passed / total) * 100).toFixed(2);

  const lines: string[] = [
    result.suite.path,
    "=".repeat(result.suite.path.length),
    `Passed:       ${passed}/${total} (${rate}%)`,
    `Failed:       ${failed}`,
  ];

  if (result.suite.expect === "snapshot") {
    const comparisons = result.files.filter((f) => f.snapshotCompared).length;
    const mismatches = result.files.filter((f) => f.snapshotCompared && !f.passed).length;
    if (comparisons > 0) {
      lines.push(`AST mismatch: ${mismatches}/${comparisons}`);
    }
  }

  lines.push("");

  const sorted = [...result.files].sort((a, b) => a.file.localeCompare(b.file));
  for (const { file, passed, reason, source, diagnostics } of sorted) {
    const suffix = !passed && reason ? ` (${reason})` : "";
    lines.push(`${passed ? "✓" : "✗"} ${file}${suffix}`);
    if (diagnostics && diagnostics.length > 0 && source) {
      lines.push(formatDiagnostics(source, [diagnostics[0]], file, { showFilename: false }));
      lines.push("");
    }
  }

  lines.push("");
  return lines.join("\n");
}

console.clear();
console.log("");

const suiteFiles = new Map<TestSuite, string[]>();
for (const suite of suites) {
  if (suite.skipOnCI && isCI) continue;
  const files = await collectFiles(suite);
  suiteFiles.set(suite, files);
  progressTotal += files.length;
}

const results: SuiteResult[] = [];
for (const [suite, files] of suiteFiles) {
  results.push(await runSuite(suite, files));
}

clearProgress();

await mkdir(RESULTS_DIR, { recursive: true });

let totalFailed = 0;
for (const result of results) {
  if (result.files.length === 0) continue;

  const failed = result.files.filter((f) => !f.passed).length;
  totalFailed += failed;

  const name = result.suite.path.replace(/^suite\//, "").replace(/\//g, "_");
  await Bun.write(`${RESULTS_DIR}/${name}.txt`, writeResultFile(result));
}

console.log(`Results saved to ${RESULTS_DIR}/\n`);

if (isCI && totalFailed > 0) {
  process.exit(1);
}
