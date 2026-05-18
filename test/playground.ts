import { parse } from "yuku-parser";
import { print } from "yuku-codegen";
import { parse as babelParse } from "@babel/parser";
import babelGen from "@babel/generator";

const generate = (babelGen as any).default ?? babelGen;

const source = await Bun.file("test/fixture.ts").text();
const filename = "fixture.ts";

const yukuAst = parse(source, { lang: "ts" });
const babelAst = babelParse(source, {
  sourceType: "module",
  plugins: ["typescript"],
});

const yukuOpts = {
  format: "pretty",
  sourceMaps: {
    source,
    file: "fixture.js",
    sourceFileName: filename,
    sourcesContent: true,
  },
} as const;

const babelOpts = {
  sourceMaps: true,
  sourceFileName: filename,
  filename: "fixture.js",
};

// warm up
for (let i = 0; i < 3; i++) {
  print(yukuAst.program, yukuOpts);
  generate(babelAst, babelOpts, source);
}

const ITER = 10;

function bench(label: string, fn: () => { code: string; map: any }) {
  const t = performance.now();
  let codeBytes = 0;
  let mapBytes = 0;
  for (let i = 0; i < ITER; i++) {
    const r = fn();
    codeBytes += r.code.length;
    mapBytes += r.map?.mappings.length ?? 0;
  }
  const ms = performance.now() - t;
  console.log(
    `${label.padEnd(16)} avg ${(ms / ITER).toFixed(2).padStart(7)}ms  code=${(codeBytes / ITER / 1024 / 1024).toFixed(2)}MB  mappings=${(mapBytes / ITER / 1024).toFixed(0)}KB`,
  );
}

console.log(`source: ${(source.length / 1024 / 1024).toFixed(2)}MB\n`);

bench("yuku  (no map)", () =>
  print(yukuAst.program, { format: "pretty" }) as any,
);
bench("yuku  (w/ map)", () => print(yukuAst.program, yukuOpts) as any);
bench("babel (no map)", () => generate(babelAst, {}, source) as any);
bench("babel (w/ map)", () => generate(babelAst, babelOpts, source) as any);
