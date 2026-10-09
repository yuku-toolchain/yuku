import { basename, dirname, join } from "node:path";
import { Glob } from "bun";
import equal from "fast-deep-equal";
import { diff } from "jest-diff";
import {
  parse,
  langFromPath,
  type ParseOptions,
  type ParseResult,
  type Diagnostic,
  type SourceLang,
} from "yuku-parser";
import { deserializeAstJson, formatDiagnostics, serializeAstJson } from "../ast-helpers-for-test";

interface TestSuite {
  path: string;
  // `snapshot` files may report errors, and a missing snapshot is written on first run
  expect: "pass" | "fail" | "snapshot";
  lang: SourceLang[];
  recursive?: boolean;
  options?: Partial<ParseOptions>;
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

const SUITE_DIR = "test/parser/suite";
const MISC_DIR = "test/parser/misc";
const RESULTS_DIR = "test/parser/results";

const suites: TestSuite[] = [
  {
    path: `${SUITE_DIR}/js/pass`,
    expect: "pass",
    lang: ["js"],
    options: { semanticErrors: true },
  },
  {
    path: `${SUITE_DIR}/js/fail`,
    expect: "fail",
    lang: ["js"],
  },
  {
    path: `${SUITE_DIR}/js/semantic`,
    expect: "fail",
    lang: ["js"],
    options: { semanticErrors: true },
  },
  {
    path: `${SUITE_DIR}/jsx/pass`,
    expect: "pass",
    lang: ["jsx"],
    options: { semanticErrors: true },
  },
  {
    path: `${SUITE_DIR}/jsx/fail`,
    expect: "fail",
    lang: ["jsx"],
  },
  {
    path: `${SUITE_DIR}/jsx/semantic`,
    expect: "fail",
    lang: ["jsx"],
    options: { semanticErrors: true },
  },
  {
    path: `${SUITE_DIR}/ts/pass`,
    expect: "pass",
    lang: ["ts", "tsx", "dts"],
    options: { semanticErrors: true },
  },
  {
    path: `${SUITE_DIR}/ts/semantic`,
    expect: "fail",
    lang: ["ts", "tsx", "dts"],
    options: { semanticErrors: true },
  },
  {
    path: `${MISC_DIR}/jsx`,
    expect: "snapshot",
    lang: ["jsx"],
    recursive: false,
  },
  {
    path: `${MISC_DIR}/js`,
    expect: "snapshot",
    lang: ["js"],
    recursive: false,
  },
  {
    path: `${MISC_DIR}/ts`,
    expect: "snapshot",
    lang: ["ts", "tsx", "dts"],
    recursive: false,
    options: { semanticErrors: true },
  },
  {
    path: `${MISC_DIR}/js/preserve-parens-disabled`,
    expect: "snapshot",
    lang: ["js"],
    options: { preserveParens: false },
  },
  {
    path: `${MISC_DIR}/ts/preserve-parens-disabled`,
    expect: "snapshot",
    lang: ["ts"],
    options: { preserveParens: false },
  },
  {
    path: `${MISC_DIR}/js/semantic`,
    expect: "snapshot",
    lang: ["js"],
    options: { semanticErrors: true },
  },
  {
    path: `${MISC_DIR}/js/commonjs`,
    expect: "snapshot",
    lang: ["js"],
    options: { sourceType: "commonjs", semanticErrors: true },
  },
  {
    path: `${MISC_DIR}/comments`,
    expect: "snapshot",
    lang: ["js", "ts", "tsx"],
    recursive: false,
    options: { attachComments: true },
  },
];

type SnapshotResult =
  | { status: "no_snapshot" }
  | { status: "match" }
  | { status: "mismatch"; snapshot: unknown };

const isCI = !!process.env.CI;
const updateSnapshots = process.argv.includes("--update-snapshots");

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
  const glob = new Glob(`${suite.path}/${pattern}`);
  const files: string[] = [];
  for await (const file of glob.scan(".")) {
    if (isSourceFile(file, suite.lang)) files.push(file);
  }
  return files;
}

function parseFile(source: string, file: string, suite: TestSuite): ParseResult {
  return parse(source, {
    sourceType: file.includes(".module.") ? "module" : "script",
    lang: langFromPath(file),
    preserveParens: true,
    ...suite.options,
  });
}

async function checkSnapshot(
  file: string,
  parsed: ParseResult,
  suite: TestSuite,
): Promise<SnapshotResult> {
  const snapshotFile = join(dirname(file), "snapshots", `${baseName(file)}.snapshot.json`);

  const comparable = {
    program: parsed.program,
    comments: parsed.comments,
    diagnostics: parsed.diagnostics,
  };

  if (!(await Bun.file(snapshotFile).exists())) {
    if (suite.expect !== "snapshot") return { status: "no_snapshot" };
    await Bun.write(snapshotFile, serializeAstJson(comparable, 2));
    return { status: "match" };
  }

  const snapshot = deserializeAstJson(await Bun.file(snapshotFile).text());
  if (equal(comparable, snapshot)) return { status: "match" };

  if (updateSnapshots) {
    await Bun.write(snapshotFile, serializeAstJson(comparable, 2));
    return { status: "match" };
  }

  return { status: "mismatch", snapshot };
}

let progressCurrent = 0;
let progressTotal = 0;

function progressLabel(file: string): string {
  return file.length > 60 ? `...${file.slice(-57)}` : file;
}

function progressStart(file: string) {
  if (isCI) return;
  process.stdout.write(
    `\r\x1b[K  \x1b[33m·\x1b[0m ${progressCurrent + 1}/${progressTotal}  ${progressLabel(file)}`,
  );
}

function progressEnd(file: string, passed: boolean) {
  if (isCI) return;
  progressCurrent++;
  const icon = passed ? "\x1b[32m✓\x1b[0m" : "\x1b[31m✗\x1b[0m";
  process.stdout.write(
    `\r\x1b[K  ${icon} ${progressCurrent}/${progressTotal}  ${progressLabel(file)}`,
  );
}

function clearProgress() {
  if (!isCI) process.stdout.write("\r\x1b[K");
}

async function runSuite(suite: TestSuite, files: string[]): Promise<SuiteResult> {
  const result: SuiteResult = { suite, files: [] };
  for (const file of files) {
    progressStart(file);
    const entry = await runFile(suite, file);
    result.files.push(entry);
    progressEnd(file, entry.passed);
  }
  return result;
}

async function runFile(suite: TestSuite, file: string): Promise<FileResult> {
  const source = await Bun.file(file).text();
  const parsed = parseFile(source, file, suite);
  const { diagnostics } = parsed;
  const entry: FileResult = { file, passed: true, snapshotCompared: false };
  if (diagnostics.length > 0) {
    entry.source = source;
    entry.diagnostics = diagnostics;
  }

  if (suite.expect === "fail") {
    if (diagnostics.length === 0) markFailed(entry, "expected error, but parsed successfully");
    return entry;
  }

  if (suite.expect === "pass" && diagnostics.length > 0) {
    markFailed(entry, "parse errors");
    console.log(formatDiagnostics(source, diagnostics, file));
    return entry;
  }

  const snapshot = await checkSnapshot(file, parsed, suite);
  entry.snapshotCompared = snapshot.status !== "no_snapshot";
  if (snapshot.status === "mismatch") {
    markFailed(entry, "snapshot mismatch");
    console.log(`${diff(snapshot.snapshot, parsed, { contextLines: 2 })}\n`);
  }
  return entry;
}

function markFailed(entry: FileResult, reason: string) {
  entry.passed = false;
  entry.reason = reason;
  clearProgress();
  console.log(`\nx ${entry.file} (${reason})`);
}

function formatResultFile(result: SuiteResult): string {
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

  if (result.suite.expect !== "fail") {
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
      lines.push(formatDiagnostics(source, [diagnostics[0]!], file, { showFilename: false }));
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
  const files = await collectFiles(suite);
  suiteFiles.set(suite, files);
  progressTotal += files.length;
}

const results: SuiteResult[] = [];
for (const [suite, files] of suiteFiles) {
  results.push(await runSuite(suite, files));
}

clearProgress();

let totalFailed = 0;
for (const result of results) {
  if (result.files.length === 0) continue;

  totalFailed += result.files.filter((f) => !f.passed).length;

  const name = result.suite.path.replace(`${SUITE_DIR}/`, "").replaceAll("/", "_");
  await Bun.write(`${RESULTS_DIR}/${name}.txt`, formatResultFile(result));
}

console.log(`Results saved to ${RESULTS_DIR}/\n`);

if (totalFailed > 0) {
  process.exit(1);
}
