/**
 * Minifier test runner.
 *
 * Each `cases/<name>.<ext>` is minified and checked three ways:
 *
 *   1. reparse  output parses with zero semantic diagnostics.
 *   2. snapshot bytes match `cases/snapshots/<name>.<ext>.snap`. Missing
 *               snapshots are created on first local run; delete a `.snap`
 *               to regenerate after intentional changes. In CI a missing
 *               snapshot fails so PRs can't merge without it.
 *   3. run()    if the case file exports a `run` function, the runner
 *               imports both the original and the minified output, calls
 *               each, and asserts deep-equal results. Cases that can't
 *               run standalone (e.g. JSX needing a runtime) simply omit
 *               the export.
 *
 * After the cases we minify the parser corpus end to end as a codegen
 * safety net: every parseable input must produce a parseable output.
 */

import { Glob } from "bun";
import { parse, langFromPath, sourceTypeFromPath } from "yuku-parser";
import { minify } from "yuku-minify";
import { existsSync } from "node:fs";
import { mkdir, rm, writeFile } from "node:fs/promises";
import path from "node:path";
import { formatDiagnostics } from "../ast-helpers-for-test";

const CASES_DIR = "test/minifier/cases";
const SNAPS_DIR = path.join(CASES_DIR, "snapshots");
const TMP_DIR = path.join(CASES_DIR, "__tmp__");
const CORPUS_DIRS = [
  "test/parser/suite/js/pass",
  "test/parser/suite/jsx/pass",
  "test/parser/suite/ts/pass",
];
const CI = !!process.env.CI;

let failed = 0;

await mkdir(SNAPS_DIR, { recursive: true });
await rm(TMP_DIR, { recursive: true, force: true });
await mkdir(TMP_DIR, { recursive: true });

await runCases();
await runCorpus();

await rm(TMP_DIR, { recursive: true, force: true });
process.exit(failed > 0 ? 1 : 0);

async function runCases() {
  const files = [...new Glob("*.{ts,tsx,js,jsx}").scanSync({ cwd: CASES_DIR })].sort();
  console.log(`cases (${files.length})`);
  for (const file of files) await runCase(file);
}

async function runCase(file: string) {
  const srcPath = path.join(CASES_DIR, file);
  const source = await Bun.file(srcPath).text();
  const lang = langFromPath(file);

  let minified: string;
  try {
    minified = minify(source, { lang }).code;
  } catch (e) {
    return fail(file, "minify", String(e));
  }

  const reparsed = parse(minified, { lang, semanticErrors: true });
  if (reparsed.diagnostics.length > 0) {
    return fail(file, "reparse", formatDiagnostics(minified, reparsed.diagnostics, file));
  }

  const minBytes = Buffer.byteLength(minified, "utf8");
  const srcBytes = Buffer.byteLength(source, "utf8");
  const size = `${formatBytes(srcBytes)} → ${formatBytes(minBytes)} (${percent(minBytes, srcBytes)})`;

  const snap = path.join(SNAPS_DIR, file + ".snap");
  if (!existsSync(snap)) {
    if (CI) return fail(file, "snapshot", `missing ${snap}; commit a snapshot from a local run`);
    await writeFile(snap, minified);
    return pass(file, `created snapshot, ${size}`);
  }
  const expected = await Bun.file(snap).text();
  if (expected !== minified) {
    const delta = minBytes - Buffer.byteLength(expected, "utf8");
    const sign = delta > 0 ? "+" : "";
    return fail(
      file,
      "snapshot",
      `size delta: ${sign}${delta} bytes\n--- expected\n${expected}\n--- actual\n${minified}`,
    );
  }

  const orig = await tryImport(path.resolve(srcPath));
  if (orig && typeof orig.run === "function") {
    const ext = path.extname(file);
    const tmp = path.resolve(
      TMP_DIR,
      file.slice(0, -ext.length) + "." + Bun.hash(minified).toString(16) + ext,
    );
    await writeFile(tmp, minified);
    const min = await tryImport(tmp);
    if (!min || typeof min.run !== "function") {
      return fail(file, "behavior", "minified module did not load");
    }
    const a = call(orig.run as () => unknown);
    const b = call(min.run as () => unknown);
    if (a !== b) return fail(file, "behavior", `original: ${a}\nminified: ${b}`);
  }

  pass(file, size);
}

async function runCorpus() {
  console.log("\ncorpus");
  let totalOk = 0,
    totalAll = 0,
    totalSkip = 0,
    totalSrc = 0,
    totalMin = 0;
  for (const dir of CORPUS_DIRS) {
    if (!existsSync(dir)) {
      console.log(`  ${dir}: not found (run \`bun load-files\`)`);
      continue;
    }
    const files = [...new Glob("**/*.{js,jsx,ts,tsx,mjs,cjs,mts,cts}").scanSync({ cwd: dir })];
    const t = performance.now();
    let ok = 0,
      bad = 0,
      skip = 0,
      src = 0,
      min = 0;
    const errs: string[] = [];
    for (let i = 0; i < files.length; i += 256) {
      const batch = files.slice(i, i + 256);
      const out = await Promise.all(batch.map((f) => corpusFile(path.join(dir, f))));
      for (const r of out) {
        if (r.status === "pass") {
          ok++;
          src += r.src;
          min += r.min;
        } else if (r.status === "skip") skip++;
        else {
          bad++;
          errs.push(r.err);
        }
      }
    }
    const ms = Math.round(performance.now() - t);
    const sizes =
      ok > 0 ? `, ${formatBytes(src)} → ${formatBytes(min)} (${percent(min, src)})` : "";
    console.log(`  ${dir}: ${ok}/${ok + bad} (${ms}ms${skip ? `, ${skip} skipped` : ""}${sizes})`);
    for (const e of errs.slice(0, 10)) console.log(`    ✗ ${e}`);
    if (errs.length > 10) console.log(`    ... ${errs.length - 10} more`);
    failed += bad;
    totalOk += ok;
    totalAll += ok + bad;
    totalSkip += skip;
    totalSrc += src;
    totalMin += min;
  }
  const totalSizes =
    totalOk > 0
      ? `, ${formatBytes(totalSrc)} → ${formatBytes(totalMin)} (${percent(totalMin, totalSrc)})`
      : "";
  console.log(
    `  total: ${totalOk}/${totalAll}${totalSkip ? `, ${totalSkip} skipped` : ""}${totalSizes}`,
  );
}

type CorpusResult =
  | { status: "pass"; src: number; min: number }
  | { status: "skip" }
  | { status: "fail"; err: string };

async function corpusFile(file: string): Promise<CorpusResult> {
  const lang = langFromPath(file);
  const sourceType = sourceTypeFromPath(file);
  const source = await Bun.file(file).text();
  let minified: string;
  try {
    minified = minify(source, { lang, sourceType }).code;
  } catch (e) {
    return { status: "fail", err: `${file}: minify threw: ${e}` };
  }
  if (parse(minified, { lang, sourceType }).diagnostics.length === 0) {
    return {
      status: "pass",
      src: Buffer.byteLength(source, "utf8"),
      min: Buffer.byteLength(minified, "utf8"),
    };
  }
  // if the input itself doesn't parse under the canonical sourceType, it's a
  // parser-corpus oddity (e.g. `await` as identifier in `.js`), not a regression.
  if (parse(source, { lang, sourceType }).diagnostics.length > 0) return { status: "skip" };
  return { status: "fail", err: `${file}: reparse failed` };
}

function pass(file: string, note?: string) {
  console.log(`  ✓ ${file}${note ? ` (${note})` : ""}`);
}

function fail(file: string, layer: string, details: string) {
  failed++;
  console.log(`  ✗ ${file} [${layer}]`);
  console.log(
    details
      .split("\n")
      .map((l) => "    " + l)
      .join("\n"),
  );
}

async function tryImport(p: string): Promise<Record<string, unknown> | null> {
  try {
    return await import(p);
  } catch {
    return null;
  }
}

function call(fn: () => unknown): string {
  try {
    return JSON.stringify(fn());
  } catch (e) {
    return `THREW: ${e}`;
  }
}

function formatBytes(b: number): string {
  if (b < 1024) return `${b} B`;
  if (b < 1024 * 1024) return `${(b / 1024).toFixed(1)} kB`;
  return `${(b / 1024 / 1024).toFixed(2)} MB`;
}

function percent(part: number, whole: number): string {
  if (whole === 0) return "0%";
  return `${((part / whole) * 100).toFixed(1)}%`;
}
