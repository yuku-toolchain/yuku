// https://fitzgen.com/2013/08/02/testing-source-maps.html

import {
  parse,
  type Identifier,
  type Node,
  type PrivateIdentifier,
  type Program,
} from "yuku-parser";
import { generate, type SourceMap } from "yuku-codegen";
import { TraceMap, originalPositionFor, type EncodedSourceMap } from "@jridgewell/trace-mapping";
import { CORPUS_DIRS, corpusFilesUnder, loadedProjects } from "../corpus";

const OUT_FILE = "out.js";

let totalFiles = 0;
let totalSkip = 0;
let totalFail = 0;
const sampleErrs: string[] = [];

const groups = [
  ...CORPUS_DIRS.map((dir) => ({ dir, files: corpusFilesUnder(dir) })),
  ...loadedProjects().map(({ root, files }) => ({ dir: root, files })),
];

for (const { dir, files } of groups) {
  const start = performance.now();
  let dirFiles = 0;
  let dirSkip = 0;
  let dirFail = 0;

  for (const { path: file, relative: f, lang, sourceType } of files) {
    const source = await Bun.file(file).text();

    const input = parse(source, { lang, sourceType });
    if (input.diagnostics.length > 0) {
      dirSkip++;
      continue;
    }
    dirFiles++;

    const result = generate(input.program, {
      comments: true,
      sourceMap: {
        source,
        file: OUT_FILE,
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
    // unparseable output is a codegen defect, caught by test/codegen
    if (output.diagnostics.length > 0) continue;

    const errs = verify(source, input.program, result.code, output.program, result.map, f);
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
const skipped = totalSkip ? `, ${totalSkip} skipped` : "";
console.log(`\n  total: ${totalFiles - totalFail}/${totalFiles} round-trips verified${skipped}`);
process.exit(totalFail > 0 ? 1 : 0);

function verify(
  source: string,
  inputAst: Program,
  code: string,
  outputAst: Program,
  map: SourceMap,
  expectSourceFileName: string,
): string[] {
  const errs: string[] = [];

  if (map.version !== 3) errs.push(`version: ${map.version}`);
  if (map.file !== OUT_FILE) errs.push(`file: ${map.file}`);
  if (
    !Array.isArray(map.sources) ||
    map.sources.length !== 1 ||
    map.sources[0] !== expectSourceFileName
  ) {
    errs.push(`sources: ${JSON.stringify(map.sources)}`);
  }
  if (!map.sourcesContent || map.sourcesContent.length !== 1 || map.sourcesContent[0] !== source) {
    errs.push(`sourcesContent mismatch`);
  }

  let tracer: TraceMap;
  try {
    tracer = new TraceMap(toEncodedSourceMap(map));
  } catch (e) {
    errs.push(`tracer: ${(e as Error).message}`);
    return errs;
  }

  const inputIds = collectIdentifiers(inputAst);
  const codeLines = lineStarts(code);
  const sourceLines = lineStarts(source);
  for (const node of walkNodes(outputAst)) {
    if (node.type !== "Identifier" && node.type !== "PrivateIdentifier") continue;
    const { line, col } = lineColOf(codeLines, node.start);
    const orig = originalPositionFor(tracer, { line: line + 1, column: col });
    if (!orig.source) {
      errs.push(`no mapping for ${node.type} "${node.name}" at gen ${line + 1}:${col}`);
    } else {
      const input = inputIds.get((sourceLines[orig.line - 1] ?? source.length) + orig.column);
      const at = `gen "${node.name}" at ${line + 1}:${col} -> orig ${orig.line}:${orig.column}`;
      if (!input) {
        errs.push(`${at}: no input identifier there`);
      } else if (input.name !== node.name) {
        errs.push(`${at} has "${input.name}"`);
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

// length of the line terminator at `i`, else 0
function lineBreakLen(s: string, i: number): number {
  const c = s.charCodeAt(i);
  if (c === 13) return s.charCodeAt(i + 1) === 10 ? 2 : 1;
  if (c === 10 || c === 0x2028 || c === 0x2029) return 1;
  return 0;
}

function lineStarts(s: string): number[] {
  const starts = [0];
  for (let i = 0; i < s.length; ) {
    const brk = lineBreakLen(s, i);
    if (brk > 0) starts.push((i += brk));
    else i++;
  }
  return starts;
}

function lineColOf(starts: number[], offset: number) {
  let lo = 0;
  let hi = starts.length - 1;
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    if (starts[mid]! <= offset) lo = mid;
    else hi = mid - 1;
  }
  return { line: lo, col: offset - starts[lo]! };
}
