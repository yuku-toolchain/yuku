import { readFileSync } from "node:fs";
import binding from "../npm/yuku-parser/binding.js";
import { decode } from "../npm/yuku-parser/decode.js";

const src = readFileSync("test/fixture.ts", "utf8");
const opts = { lang: "ts" } as const;
const bytes = src.length;

function stats(times: number[]) {
  times.sort((a, b) => a - b);
  return { min: times[0], med: times[times.length >> 1] };
}

function bench(label: string, fn: () => number, warmup = 10, iters = 50) {
  let sink = 0;
  for (let i = 0; i < warmup; i++) sink += fn();
  const times: number[] = [];
  for (let i = 0; i < iters; i++) {
    const t0 = performance.now();
    sink += fn();
    times.push(performance.now() - t0);
  }
  const { min, med } = stats(times);
  const mbps = bytes / 1e6 / (med / 1000);
  console.log(
    `${label.padEnd(24)} min ${min.toFixed(2)}ms  med ${med.toFixed(2)}ms  ${mbps.toFixed(0)} MB/s`,
  );
  if (sink === -1) console.log("");
}

// isolate the native parse + serialize (Zig side)
bench("native parse (ts)", () => binding.parse(src, opts).byteLength);

// isolate decode: one buffer, build the full ESTree tree each iteration
const bufTs = binding.parse(src, { lang: "ts" });
const bufJs = binding.parse(src, { lang: "js" });
bench("decode ts (eager tree)", () => decode(bufTs, src).program.body.length);
bench("decode js (eager tree)", () => decode(bufJs, src).program.body.length);

// full pipeline for reference
bench("parse + decode (ts)", () => decode(binding.parse(src, opts), src).program.body.length);
