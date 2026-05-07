import { mkdir } from "node:fs/promises";
import { Glob } from "bun";
import { parse, type SourceLang } from "yuku-parser";
import { minify, langFromPath, type MinifyOptions } from "yuku-minify";
import { formatDiagnostics } from "../ast-helpers-for-test";

interface TestSuite {
  path: string;
  lang: SourceLang[];
  recursive?: boolean;
  options?: Partial<MinifyOptions>;
  skipOnCI?: boolean;
}

type Status = "pass" | "fail" | "skip";

interface FileResult {
  file: string;
  status: Status;
  reason?: string;
  source?: string;
  output?: string;
  diagnostics?: import("yuku-parser").Diagnostic[];
}

interface SuiteResult {
  suite: TestSuite;
  files: FileResult[];
}

const SUITE_DIR = "test/suite";
const MISC_DIR = "test/parser/misc";

// Round-trip re-parse layer: for each corpus file that parses cleanly,
// minify it and assert the output re-parses without errors.
const suites: TestSuite[] = [
  { path: `${SUITE_DIR}/js/pass`, lang: ["js"] },
  { path: `${SUITE_DIR}/ts/pass`, lang: ["ts", "tsx"] },
  { path: `${MISC_DIR}/js`, lang: ["js"], recursive: false },
  { path: `${MISC_DIR}/ts`, lang: ["ts", "tsx"], recursive: false },
];

const RESULTS_DIR = "test/minifier/results";
const isCI = !!process.env.CI;

function isSourceFile(path: string, langs: SourceLang[]): boolean {
  if (path.includes("/snapshots/") || path.endsWith(".snapshot.json")) return false;
  if (path.endsWith(".d.ts")) return false;
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

function sourceTypeOf(file: string): "module" | "script" {
  return file.includes(".module.") ? "module" : "script";
}

function runFile(file: string, content: string, suite: TestSuite): FileResult {
  const lang = langFromPath(file);
  const sourceType = sourceTypeOf(file);

  // 1. parse the original. skip files that don't parse cleanly:
  //    we can't expect the minifier to produce correct output for input
  //    the parser already disagrees with.
  const original = parse(content, { sourceType, lang });
  if (original.diagnostics.length > 0) {
    return { file, status: "skip", reason: "original has parse errors" };
  }

  // 2. minify.
  let minified;
  try {
    minified = minify(content, { sourceType, lang, ...suite.options });
  } catch (err) {
    return { file, status: "fail", reason: `minify threw: ${(err as Error).message}`, source: content };
  }

  if (minified.errors.length > 0) {
    return {
      file,
      status: "fail",
      reason: "codegen errors during minify",
      source: content,
    };
  }

  // 3. re-parse the output.
  const reparsed = parse(minified.code, { sourceType: "module", lang: lang === "tsx" ? "tsx" : lang === "ts" ? "ts" : "js" });
  if (reparsed.diagnostics.length > 0) {
    return {
      file,
      status: "fail",
      reason: "minified output failed to re-parse",
      source: content,
      output: minified.code,
      diagnostics: reparsed.diagnostics,
    };
  }

  return { file, status: "pass" };
}

let progressCurrent = 0;
let progressTotal = 0;

function showProgress(file: string, status: Status) {
  if (isCI) return;
  progressCurrent++;
  const icon =
    status === "pass" ? "\x1b[32m✓\x1b[0m" : status === "skip" ? "\x1b[33m·\x1b[0m" : "\x1b[31m✗\x1b[0m";
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
    const entry = runFile(file, content, suite);

    if (entry.status === "fail") {
      clearProgress();
      console.log(`\nx ${file} (${entry.reason})`);
      if (entry.diagnostics && entry.output) {
        console.log(formatDiagnostics(entry.output, entry.diagnostics, file));
      }
    }

    result.files.push(entry);
    showProgress(file, entry.status);
  }

  return result;
}

function writeResultFile(result: SuiteResult): string {
  const passed = result.files.filter((f) => f.status === "pass").length;
  const failed = result.files.filter((f) => f.status === "fail").length;
  const skipped = result.files.filter((f) => f.status === "skip").length;
  const total = result.files.length;
  const tested = total - skipped;
  const rate = tested === 0 ? "0.00" : ((passed / tested) * 100).toFixed(2);

  const lines: string[] = [
    `${result.suite.path}  (round-trip)`,
    "=".repeat(result.suite.path.length + 14),
    `Passed:  ${passed}/${tested} (${rate}%)`,
    `Failed:  ${failed}`,
    `Skipped: ${skipped}`,
    "",
  ];

  const sorted = [...result.files].sort((a, b) => a.file.localeCompare(b.file));
  for (const { file, status, reason, output, diagnostics } of sorted) {
    const icon = status === "pass" ? "✓" : status === "skip" ? "·" : "✗";
    const suffix = reason ? ` (${reason})` : "";
    lines.push(`${icon} ${file}${suffix}`);
    if (status === "fail" && diagnostics && diagnostics.length > 0 && output) {
      lines.push(formatDiagnostics(output, [diagnostics[0]], file, { showFilename: false }));
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

  totalFailed += result.files.filter((f) => f.status === "fail").length;

  const name = result.suite.path
    .replace(/^test\/suite\//, "")
    .replace(/^test\/parser\//, "")
    .replace(/\//g, "_");
  await Bun.write(`${RESULTS_DIR}/${name}_round-trip.txt`, writeResultFile(result));
}

console.log(`Results saved to ${RESULTS_DIR}/\n`);

if (isCI && totalFailed > 0) {
  process.exit(1);
}
