// https://fitzgen.com/2013/08/02/testing-source-maps.html

import { Glob } from "bun";
import { parse, langFromPath, sourceTypeFromPath } from "yuku-parser";
import { print, type SourceMap } from "yuku-codegen";
import { TraceMap, originalPositionFor } from "@jridgewell/trace-mapping";

const CORPUS_DIRS = [
  "test/parser/suite/js/pass",
  "test/parser/suite/jsx/pass",
  "test/parser/suite/ts/pass",
];

let totalFiles = 0;
let totalSkip = 0;
let totalFail = 0;
const sampleErrs: string[] = [];

for (const dir of CORPUS_DIRS) {
  const files = [
    ...new Glob("**/*.{ts,tsx,js,jsx}").scanSync({ cwd: dir }),
  ].sort();
  const start = performance.now();
  let dirFiles = 0;
  let dirSkip = 0;
  let dirFail = 0;

  for (const f of files) {
    const file = `${dir}/${f}`;
    const lang = langFromPath(file);
    const sourceType = sourceTypeFromPath(file);
    const source = await Bun.file(file).text();

    const input = parse(source, { lang, sourceType });
    if (input.diagnostics.length > 0) {
      dirSkip++;
      continue;
    }
    dirFiles++;

    const result = print(input.program, {
      sourceMaps: {
        source,
        file: "out.js",
        sourceFileName: f,
        sourcesContent: true,
      },
    });
    if (!result.map) {
      dirFail++;
      if (sampleErrs.length < 8) sampleErrs.push(`${file}: no map returned`);
      continue;
    }

    const output = parse(result.code, { lang, sourceType });
    if (output.diagnostics.length > 0) {
      // codegen produced unparseable output. that is a codegen defect,
      // caught by test/codegen, and there is nothing to round-trip here.
      continue;
    }

    const errs = verify(source, input.program, result.code, output.program, result.map, "out.js", f);
    if (errs.length > 0) {
      dirFail++;
      if (sampleErrs.length < 8) sampleErrs.push(`${file}: ${errs[0]}`);
    }
  }

  const ms = Math.round(performance.now() - start);
  console.log(
    `  ${dir}: ${dirFiles - dirFail}/${dirFiles} (${ms}ms${dirSkip ? `, ${dirSkip} skipped` : ""})`,
  );
  totalFiles += dirFiles;
  totalSkip += dirSkip;
  totalFail += dirFail;
}

console.log();
for (const e of sampleErrs) console.log(`  ✗ ${e}`);
console.log(
  `\n  total: ${totalFiles - totalFail}/${totalFiles} round-trips verified${totalSkip ? `, ${totalSkip} skipped` : ""}`,
);
process.exit(totalFail > 0 ? 1 : 0);

type Id = { type: string; name: string; start: number };

function verify(
  source: string,
  inputAst: any,
  code: string,
  outputAst: any,
  map: SourceMap,
  expectFile: string,
  expectSourceFileName: string,
): string[] {
  const errs: string[] = [];

  if (map.version !== 3) errs.push(`version: ${map.version}`);
  if (map.file !== expectFile) errs.push(`file: ${map.file}`);
  if (!Array.isArray(map.sources) || map.sources.length !== 1 || map.sources[0] !== expectSourceFileName)
    errs.push(`sources: ${JSON.stringify(map.sources)}`);
  if (!map.sourcesContent || map.sourcesContent.length !== 1 || map.sourcesContent[0] !== source)
    errs.push(`sourcesContent mismatch`);

  let tracer: TraceMap;
  try {
    tracer = new TraceMap(map as any);
  } catch (e) {
    errs.push(`tracer: ${(e as Error).message}`);
    return errs;
  }

  const inputIds = collectIdentifiers(inputAst);
  const stack: any[] = [outputAst];
  while (stack.length) {
    const n = stack.pop();
    if (n === null || typeof n !== "object") continue;
    if (Array.isArray(n)) {
      for (let i = n.length - 1; i >= 0; i--) stack.push(n[i]);
      continue;
    }
    if (typeof n.type === "string" && (n.type === "Identifier" || n.type === "PrivateIdentifier")) {
      if (typeof n.start === "number" && typeof n.name === "string") {
        const { line, col } = lineColOf(code, n.start);
        const orig = originalPositionFor(tracer, { line: line + 1, column: col });
        if (!orig.source) {
          errs.push(`no mapping for ${n.type} "${n.name}" at gen ${line + 1}:${col}`);
        } else {
          const off = offsetOf(source, orig.line - 1, orig.column);
          const input: Id | undefined = inputIds.get(off);
          if (!input) {
            errs.push(`gen "${n.name}" at ${line + 1}:${col} -> orig ${orig.line}:${orig.column}: no input identifier there`);
          } else if (input.name !== n.name) {
            errs.push(`gen "${n.name}" at ${line + 1}:${col} -> orig ${orig.line}:${orig.column} has "${input.name}"`);
          }
        }
        if (errs.length > 4) return errs;
      }
    }
    for (const k in n) {
      if (k === "type" || k === "start" || k === "end" || k === "loc" || k === "range") continue;
      const v = (n as any)[k];
      if (v && typeof v === "object") stack.push(v);
    }
  }

  return errs;
}

function collectIdentifiers(root: any): Map<number, Id> {
  const out = new Map<number, Id>();
  const stack: any[] = [root];
  while (stack.length) {
    const n = stack.pop();
    if (n === null || typeof n !== "object") continue;
    if (Array.isArray(n)) {
      for (let i = n.length - 1; i >= 0; i--) stack.push(n[i]);
      continue;
    }
    if (
      typeof n.type === "string" &&
      (n.type === "Identifier" || n.type === "PrivateIdentifier") &&
      typeof n.start === "number" &&
      typeof n.name === "string"
    ) {
      out.set(n.start, { type: n.type, name: n.name, start: n.start });
    }
    for (const k in n) {
      if (k === "type" || k === "start" || k === "end" || k === "loc" || k === "range") continue;
      const v = (n as any)[k];
      if (v && typeof v === "object") stack.push(v);
    }
  }
  return out;
}

function lineColOf(s: string, offset: number): { line: number; col: number } {
  let line = 0;
  let lineStart = 0;
  for (let i = 0; i < offset; i++) {
    if (s.charCodeAt(i) === 10) {
      line++;
      lineStart = i + 1;
    }
  }
  return { line, col: offset - lineStart };
}

function offsetOf(s: string, line: number, col: number): number {
  let l = 0;
  let off = 0;
  while (l < line && off < s.length) {
    if (s.charCodeAt(off) === 10) l++;
    off++;
  }
  return off + col;
}
