#!/usr/bin/env bun
// Codegen benchmark driver: yuku's src/parser/codegen vs oxc_codegen.
//
// Builds both harnesses (ReleaseFast / cargo --release), runs each over the
// shared parser-benchmark files, and prints a comparison table. Both harnesses
// parse once then time codegen-only in a self-calibrating loop, emitting one
// JSON line. See ./README.md for methodology.
//
// Usage: bun bench/codegen/run.ts [seconds] [file...]

import { spawnSync } from "bun";
import { existsSync } from "node:fs";

const ROOT = new URL("../../", import.meta.url).pathname;
const SECONDS = process.argv[2] && !process.argv[2].includes(".") ? process.argv[2] : "3";
const FILES = process.argv.slice(2).filter((a) => a.includes("."));
const DEFAULT_FILES = ["react.js", "calcom.tsx", "typescript.js"];

const YUKU_BIN = `${ROOT}zig-out/bin/yuku-codegen-bench`;
const OXC_BIN = `${ROOT}bench/codegen/oxc/target/release/oxc-codegen-bench`;

type Row = {
  tool: string;
  file: string;
  input_bytes: number;
  output_bytes: number;
  iters: number;
  mean_ns: number;
  best_ns: number;
  mean_mb_s: number;
  best_mb_s: number;
};

function run(cmd: string[]): Row {
  const r = spawnSync({ cmd, cwd: ROOT, stdout: "pipe", stderr: "pipe" });
  const out = r.stdout.toString().trim();
  if (!r.success || !out) {
    throw new Error(`failed: ${cmd.join(" ")}\n${r.stderr.toString()}`);
  }
  return JSON.parse(out.split("\n").pop()!);
}

function build() {
  console.log("Building harnesses...");
  const zig = spawnSync({
    cmd: ["zig", "build", "install"],
    cwd: ROOT,
    stdout: "inherit",
    stderr: "inherit",
  });
  if (!zig.success) throw new Error("zig build failed");
  const cargo = spawnSync({
    cmd: ["cargo", "build", "--release", "--manifest-path", "bench/codegen/oxc/Cargo.toml"],
    cwd: ROOT,
    stdout: "inherit",
    stderr: "inherit",
  });
  if (!cargo.success) throw new Error("cargo build failed");
  if (!existsSync(YUKU_BIN)) throw new Error(`missing ${YUKU_BIN}`);
  if (!existsSync(OXC_BIN)) throw new Error(`missing ${OXC_BIN}`);
}

function fmtNs(ns: number): string {
  if (ns < 1_000) return `${ns.toFixed(0)} ns`;
  if (ns < 1_000_000) return `${(ns / 1_000).toFixed(1)} µs`;
  return `${(ns / 1_000_000).toFixed(2)} ms`;
}

function pad(s: string, n: number): string {
  return s.length >= n ? s : s + " ".repeat(n - s.length);
}
function lpad(s: string, n: number): string {
  return s.length >= n ? s : " ".repeat(n - s.length) + s;
}

build();

const files = FILES.length ? FILES : DEFAULT_FILES;
console.log(`\nRunning codegen benchmark (${SECONDS}s/case, codegen-only, pretty, comments off)\n`);

const cols = ["file", "size", "yuku best", "oxc best", "yuku MB/s", "oxc MB/s", "speedup"];
const widths = [16, 9, 11, 11, 11, 11, 9];
console.log(cols.map((c, i) => pad(c, widths[i])).join("  "));
console.log(widths.map((w) => "-".repeat(w)).join("  "));

for (const f of files) {
  const path = `profiler/files/${f}`;
  if (!existsSync(`${ROOT}${path}`)) {
    console.log(`${pad(f, 16)}  (missing — run \`bun load-files\`)`);
    continue;
  }
  const yuku = run([YUKU_BIN, path, SECONDS]);
  const oxc = run([OXC_BIN, path, SECONDS]);
  // speedup of oxc relative to yuku (best/min time). >1 means oxc faster.
  const speedup = yuku.best_ns / oxc.best_ns;
  const arrow = speedup >= 1 ? `oxc ${speedup.toFixed(2)}x` : `yuku ${(1 / speedup).toFixed(2)}x`;
  const sizeMB = (yuku.input_bytes / 1024 / 1024).toFixed(2);
  const row = [
    pad(f, widths[0]),
    lpad(`${sizeMB}M`, widths[1]),
    lpad(fmtNs(yuku.best_ns), widths[2]),
    lpad(fmtNs(oxc.best_ns), widths[3]),
    lpad(yuku.best_mb_s.toFixed(0), widths[4]),
    lpad(oxc.best_mb_s.toFixed(0), widths[5]),
    lpad(arrow, widths[6]),
  ];
  console.log(row.join("  "));
}

console.log(
  "\nbest = fastest single codegen pass (min of self-calibrated loop). " +
    "speedup = yuku_time / oxc_time.",
);
