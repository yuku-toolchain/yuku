// Runs print / strip / minify against every parseable file in the
// corpus. Each output is reparsed to verify it is syntactically valid.
import { Glob } from "bun";
import {
  parse,
  langFromPath,
  sourceTypeFromPath,
  type SourceLang,
} from "yuku-parser";
import { print, strip, minify, type CodegenResult } from "yuku-codegen";

const CORPUS_DIRS = [
  "test/parser/suite/js/pass",
  "test/parser/suite/jsx/pass",
  "test/parser/suite/ts/pass",
];

type Op = {
  name: string;
  run: (program: any) => CodegenResult;
  outLang: (input: SourceLang) => SourceLang;
};

const ops: Op[] = [
  {
    name: "print",
    run: (p) => print(p, { format: "pretty" }),
    outLang: (l) => l,
  },
  {
    name: "strip",
    run: (p) => strip(p, { format: "pretty" }),
    outLang: jsLang,
  },
  {
    name: "minify",
    run: (p) => minify(p, { format: "compact" }),
    outLang: (l) => l,
  },
];

type Tally = { ok: number; bad: number; errs: string[] };
const tally: Record<string, Tally> = Object.fromEntries(
  ops.map((o) => [o.name, { ok: 0, bad: 0, errs: [] } satisfies Tally]),
);

let totalFiles = 0;
let totalSkip = 0;

for (const dir of CORPUS_DIRS) {
  const files = [
    ...new Glob("**/*.{ts,tsx,js,jsx}").scanSync({ cwd: dir }),
  ].sort();
  const start = performance.now();
  let dirFiles = 0;
  let dirSkip = 0;

  for (const f of files) {
    const file = `${dir}/${f}`;
    const lang = langFromPath(file);
    const sourceType = sourceTypeFromPath(file);
    const source = await Bun.file(file).text();
    if (parse(source, { lang, sourceType }).diagnostics.length > 0) {
      dirSkip++;
      continue;
    }
    dirFiles++;
    const ast = parse(source, { lang, sourceType });

    for (const op of ops) {
      const t = tally[op.name];
      let out: string;
      try {
        out = op.run(ast.program).code;
      } catch (e) {
        t.bad++;
        t.errs.push(`${file}: ${op.name} threw: ${e}`);
        continue;
      }
      const reparse = parse(out, { lang: op.outLang(lang), sourceType });
      if (reparse.diagnostics.length === 0) {
        t.ok++;
      } else {
        t.bad++;
        t.errs.push(`${file}: ${op.name} reparse failed`);
      }
    }
  }

  const ms = Math.round(performance.now() - start);
  console.log(
    `  ${dir}: ${dirFiles} files (${ms}ms${dirSkip ? `, ${dirSkip} skipped` : ""})`,
  );
  totalFiles += dirFiles;
  totalSkip += dirSkip;
}

let failed = 0;
console.log();
for (const [op, t] of Object.entries(tally)) {
  const total = t.ok + t.bad;
  console.log(`  ${op.padEnd(8)} ${t.ok}/${total}`);
  for (const e of t.errs.slice(0, 5)) console.log(`    ✗ ${e}`);
  if (t.errs.length > 5) console.log(`    ... ${t.errs.length - 5} more`);
  failed += t.bad;
}
console.log(
  `\n  total: ${totalFiles} files${totalSkip ? `, ${totalSkip} skipped` : ""}`,
);
process.exit(failed > 0 ? 1 : 0);

function jsLang(lang: SourceLang): SourceLang {
  if (lang === "tsx") return "jsx";
  if (lang === "ts" || lang === "dts") return "js";
  return lang;
}
