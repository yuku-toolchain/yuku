// every printed identifier maps back to the same identifier in its source
// https://fitzgen.com/2013/08/02/testing-source-maps.html

import {
  parse,
  print,
  type Identifier,
  type Node,
  type PrivateIdentifier,
  type Program,
  type SourceMap,
} from "yuku";
import { TraceMap, originalPositionFor, type EncodedSourceMap } from "@jridgewell/trace-mapping";
import { CORPUS_DIRS, corpusFilesUnder, loadedProjects } from "../corpus";

const OUT_FILE = "out.js";
const SAMPLE_MAX = 8;

let checkedTotal = 0;
let skippedTotal = 0;
let failedTotal = 0;
const samples: string[] = [];

const groups = [
  ...CORPUS_DIRS.map((dir) => ({ dir, files: corpusFilesUnder(dir) })),
  ...loadedProjects().map(({ root, files }) => ({ dir: root, files })),
];

for (const { dir, files } of groups) {
  const start = performance.now();
  let checked = 0;
  let skipped = 0;
  let failed = 0;

  for (const { path, relative, lang, sourceType } of files) {
    const source = await Bun.file(path).text();

    const input = parse(source, { path: relative, lang, sourceType });
    if (input.diagnostics.length > 0) {
      skipped++;
      continue;
    }
    checked++;

    const result = print(input, {
      comments: "all",
      sourceMap: { file: OUT_FILE, sourcesContent: true },
    });
    if (!result.map) {
      failed++;
      if (samples.length < SAMPLE_MAX) samples.push(`${path}: no map returned`);
      continue;
    }

    const output = parse(result.code, { lang, sourceType });
    // unparseable output is a codegen defect, caught by test/codegen
    if (output.diagnostics.length > 0) continue;

    const errors = verify(source, input.program, result.code, output.program, result.map, relative);
    if (errors.length > 0) {
      failed++;
      if (samples.length < SAMPLE_MAX) samples.push(`${path}: ${errors[0]}`);
    }
  }

  const ms = Math.round(performance.now() - start);
  console.log(
    `  ${dir}: ${checked - failed}/${checked} (${ms}ms${skipped ? `, ${skipped} skipped` : ""})`,
  );
  checkedTotal += checked;
  skippedTotal += skipped;
  failedTotal += failed;
}

console.log();
for (const sample of samples) console.log(`  ✗ ${sample}`);
const verified = `${checkedTotal - failedTotal}/${checkedTotal} round-trips verified`;
console.log(`\n  total: ${verified}${skippedTotal ? `, ${skippedTotal} skipped` : ""}`);
process.exit(failedTotal > 0 ? 1 : 0);

function verify(
  source: string,
  inputAst: Program,
  code: string,
  outputAst: Program,
  map: SourceMap,
  sourceFileName: string,
): string[] {
  const errors: string[] = [];

  if (map.version !== 3) errors.push(`version: ${map.version}`);
  if (map.file !== OUT_FILE) errors.push(`file: ${map.file}`);
  const { sources } = map;
  if (!Array.isArray(sources) || sources.length !== 1 || sources[0] !== sourceFileName) {
    errors.push(`sources: ${JSON.stringify(sources)}`);
  }
  if (!map.sourcesContent || map.sourcesContent.length !== 1 || map.sourcesContent[0] !== source) {
    errors.push(`sourcesContent mismatch`);
  }

  let tracer: TraceMap;
  try {
    tracer = new TraceMap(toEncodedSourceMap(map));
  } catch (error) {
    errors.push(`tracer: ${(error as Error).message}`);
    return errors;
  }

  const inputIdentifiers = collectIdentifiers(inputAst);
  const codeLines = lineStarts(code);
  const sourceLines = lineStarts(source);
  for (const node of walkNodes(outputAst)) {
    if (node.type !== "Identifier" && node.type !== "PrivateIdentifier") continue;
    const { line, column } = lineColumnOf(codeLines, node.start);
    const original = originalPositionFor(tracer, { line: line + 1, column });
    if (!original.source) {
      errors.push(`no mapping for ${node.type} "${node.name}" at gen ${line + 1}:${column}`);
    } else {
      const offset = (sourceLines[original.line - 1] ?? source.length) + original.column;
      const input = inputIdentifiers.get(offset);
      const at =
        `gen "${node.name}" at ${line + 1}:${column} -> orig ${original.line}:${original.column}`;
      if (!input) {
        errors.push(`${at}: no input identifier there`);
      } else if (input.name !== node.name) {
        errors.push(`${at} has "${input.name}"`);
      }
    }
    if (errors.length > 4) return errors;
  }

  return errors;
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

// the length of the line terminator at `index`, else 0
function lineBreakLength(text: string, index: number): number {
  const code = text.charCodeAt(index);
  if (code === 13) return text.charCodeAt(index + 1) === 10 ? 2 : 1;
  if (code === 10 || code === 0x2028 || code === 0x2029) return 1;
  return 0;
}

function lineStarts(text: string): number[] {
  const starts = [0];
  for (let index = 0; index < text.length; ) {
    const length = lineBreakLength(text, index);
    if (length > 0) starts.push((index += length));
    else index++;
  }
  return starts;
}

function lineColumnOf(starts: number[], offset: number): { line: number; column: number } {
  let low = 0;
  let high = starts.length - 1;
  while (low < high) {
    const middle = (low + high + 1) >> 1;
    if (starts[middle]! <= offset) low = middle;
    else high = middle - 1;
  }
  return { line: low, column: offset - starts[low]! };
}
