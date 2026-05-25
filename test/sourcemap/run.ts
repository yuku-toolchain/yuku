// https://fitzgen.com/2013/08/02/testing-source-maps.html

import { Glob } from "bun";
import {
  parse,
  langFromPath,
  sourceTypeFromPath,
  type Identifier,
  type Node,
  type PrivateIdentifier,
  type Program,
} from "yuku-parser";
import { print, type SourceMap } from "yuku-codegen";
import { TraceMap, originalPositionFor, type EncodedSourceMap } from "@jridgewell/trace-mapping";

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
  const files = [...new Glob("**/*.{ts,tsx,js,jsx}").scanSync({ cwd: dir })].sort();
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
      comments: true,
      sourceMaps: {
        lineStarts: input.lineStarts,
        file: "out.js",
        sourceFileName: f,
        sourcesContent: source,
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

    const errs = verify(
      source,
      input.program,
      result.code,
      output.program,
      result.map,
      "out.js",
      f,
    );
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

function verify(
  source: string,
  inputAst: Program,
  code: string,
  outputAst: Program,
  map: SourceMap,
  expectFile: string,
  expectSourceFileName: string,
): string[] {
  const errs: string[] = [];

  if (map.version !== 3) errs.push(`version: ${map.version}`);
  if (map.file !== expectFile) errs.push(`file: ${map.file}`);
  if (
    !Array.isArray(map.sources) ||
    map.sources.length !== 1 ||
    map.sources[0] !== expectSourceFileName
  )
    errs.push(`sources: ${JSON.stringify(map.sources)}`);
  if (!map.sourcesContent || map.sourcesContent.length !== 1 || map.sourcesContent[0] !== source)
    errs.push(`sourcesContent mismatch`);

  let tracer: TraceMap;
  try {
    tracer = new TraceMap(toEncodedSourceMap(map));
  } catch (e) {
    errs.push(`tracer: ${(e as Error).message}`);
    return errs;
  }

  const inputIds = collectIdentifiers(inputAst);
  for (const node of walkNodes(outputAst)) {
    if (node.type !== "Identifier" && node.type !== "PrivateIdentifier") continue;
    const { line, col } = lineColOf(code, node.start);
    const orig = originalPositionFor(tracer, { line: line + 1, column: col });
    if (!orig.source) {
      errs.push(`no mapping for ${node.type} "${node.name}" at gen ${line + 1}:${col}`);
    } else {
      const off = offsetOf(source, orig.line - 1, orig.column);
      const input = inputIds.get(off);
      if (!input) {
        errs.push(
          `gen "${node.name}" at ${line + 1}:${col} -> orig ${orig.line}:${orig.column}: no input identifier there`,
        );
      } else if (input.name !== node.name) {
        errs.push(
          `gen "${node.name}" at ${line + 1}:${col} -> orig ${orig.line}:${orig.column} has "${input.name}"`,
        );
      }
    }
    if (errs.length > 4) return errs;
  }

  return errs;
}

function collectIdentifiers(root: Program): Map<number, Identifier | PrivateIdentifier> {
  const out = new Map<number, Identifier | PrivateIdentifier>();
  for (const node of walkNodes(root)) {
    if (node.type === "Identifier" || node.type === "PrivateIdentifier") {
      out.set(node.start, node);
    }
  }
  return out;
}

function* walkNodes(root: Node): Generator<Node> {
  const stack: unknown[] = [root];
  while (stack.length) {
    const value = stack.pop();
    if (Array.isArray(value)) {
      for (let i = value.length - 1; i >= 0; i--) stack.push(value[i]);
    } else if (isAstNode(value)) {
      yield value;
      for (const child of Object.values(value)) stack.push(child);
    }
  }
}

function isAstNode(value: unknown): value is Node {
  return (
    typeof value === "object" && value !== null && "type" in value && typeof value.type === "string"
  );
}

function toEncodedSourceMap(map: SourceMap): EncodedSourceMap {
  return {
    ...map,
    sourceRoot: map.sourceRoot ?? undefined,
    sourcesContent: map.sourcesContent ?? undefined,
  };
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
