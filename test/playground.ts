import { parse } from "yuku-parser";

const source = await Bun.file("test/fixture.ts").text();

function bench(label: string, fn: () => void, iters = 30, warmup = 10) {
  for (let i = 0; i < warmup; i++) fn();
  const samples: number[] = [];
  for (let i = 0; i < iters; i++) {
    const t = Bun.nanoseconds();
    fn();
    samples.push((Bun.nanoseconds() - t) / 1e6);
  }
  samples.sort((a, b) => a - b);
  const median = samples[Math.floor(samples.length / 2)];
  const min = samples[0];
  const p90 = samples[Math.floor(samples.length * 0.9)];
  const mean = samples.reduce((a, b) => a + b, 0) / samples.length;
  console.log(
    `${label.padEnd(28)} median=${median.toFixed(2)}ms  min=${min.toFixed(2)}ms  p90=${p90.toFixed(2)}ms  mean=${mean.toFixed(2)}ms`,
  );
}

bench("just parse", () => {
  parse(source, { lang: "ts" });
});

bench("parse + access program", () => {
  const _ = parse(source, { lang: "ts" }).program;
});
