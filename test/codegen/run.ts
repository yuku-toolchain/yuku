import { Glob } from "bun";
import { parse, langFromPath, sourceTypeFromPath } from "yuku-parser";
import { print } from "yuku-codegen";

const CORPUS_DIRS = [
  "test/parser/suite/js/pass",
  "test/parser/suite/jsx/pass",
  "test/parser/suite/ts/pass",
];

let failed = 0;
let totalOk = 0;
let totalAll = 0;
let totalSkip = 0;

for (const dir of CORPUS_DIRS) {
  const files = [...new Glob("**/*.{ts,tsx,js,jsx}").scanSync({ cwd: dir })].sort();
  const t = performance.now();
  let ok = 0, bad = 0, skip = 0;
  const errs: string[] = [];
  for (const f of files) {
    const file = `${dir}/${f}`;
    const lang = langFromPath(file);
    const sourceType = sourceTypeFromPath(file);
    const source = await Bun.file(file).text();
    if (parse(source, { lang, sourceType }).diagnostics.length > 0) {
      skip++;
      continue;
    }
    let out: string;
    try {
      const ast = parse(source, { lang, sourceType });
      out = print(ast.program, { format: "pretty" }).code;
    } catch (e) {
      bad++;
      errs.push(`${file}: generate threw: ${e}`);
      continue;
    }
    if (parse(out, { lang, sourceType }).diagnostics.length === 0) {
      ok++;
    } else {
      bad++;
      errs.push(`${file}: reparse failed`);
    }
  }
  const ms = Math.round(performance.now() - t);
  console.log(`  ${dir}: ${ok}/${ok + bad} (${ms}ms${skip ? `, ${skip} skipped` : ""})`);
  for (const e of errs.slice(0, 10)) console.log(`    ✗ ${e}`);
  if (errs.length > 10) console.log(`    ... ${errs.length - 10} more`);
  failed += bad;
  totalOk += ok;
  totalAll += ok + bad;
  totalSkip += skip;
}
console.log(`  total: ${totalOk}/${totalAll}${totalSkip ? `, ${totalSkip} skipped` : ""}`);

process.exit(failed > 0 ? 1 : 0);
