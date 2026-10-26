import { is } from "yuku/utils";
import {
  walk,
  type AliasMap,
  type AliasName,
  type Diagnostic,
  type Node,
  type NodeOfType,
  type NodeType,
} from "yuku";

const BIG_INT_PREFIX = "(BigInt) ";
const REGEXP_PREFIX = "(RegExp) ";
const NUMBER_PREFIX = "(Number) ";
const REGEXP_LITERAL = /^\/(.+)\/([dgimsuyv]*)$/;
const TAB_WIDTH = 4;
const UNCOMPARED_KEYS = new Set(["start", "end", "comments"]);

// JSON has no bigint, regexp, or non-finite number, so snapshots store them as tagged strings
export function serializeAstJson(data: unknown, space?: string | number): string {
  return JSON.stringify(
    data,
    (_, value) => {
      if (typeof value === "bigint") return `${BIG_INT_PREFIX}${value}n`;
      if (value instanceof RegExp) return `${REGEXP_PREFIX}${value.toString()}`;
      if (typeof value === "number" && !Number.isFinite(value)) return `${NUMBER_PREFIX}${value}`;
      return value;
    },
    space,
  );
}

export function deserializeAstJson(json: string): unknown {
  return JSON.parse(json, (_, value) => {
    if (typeof value === "object" && value !== null && typeof value.value === "bigint") {
      value.bigint = value.value.toString();
      return value;
    }
    if (typeof value !== "string") return value;
    if (value.startsWith(BIG_INT_PREFIX)) {
      return BigInt(value.slice(BIG_INT_PREFIX.length).replace(/n$/, "").replaceAll("_", ""));
    }
    if (value.startsWith(NUMBER_PREFIX)) return Number(value.slice(NUMBER_PREFIX.length));
    if (value.startsWith(REGEXP_PREFIX)) {
      const match = value.slice(REGEXP_PREFIX.length).match(REGEXP_LITERAL);
      if (match === null) return null;
      try {
        return new RegExp(match[1]!, match[2]);
      } catch {
        return null;
      }
    }
    return value;
  });
}

// ignores what codegen normalizes, spans, comments, and the raw of strings, bigints, and regexps
export function astDiffPath(a: unknown, b: unknown, path = "program"): string | null {
  if (a === b) return null;
  if (typeof a === "number" && Number.isNaN(a) && typeof b === "number" && Number.isNaN(b)) {
    return null;
  }
  if (a === null || b === null || typeof a !== "object" || typeof b !== "object") {
    return showDifference(path, a, b);
  }

  if (a instanceof RegExp || b instanceof RegExp) {
    const same =
      a instanceof RegExp && b instanceof RegExp && a.source === b.source && a.flags === b.flags;
    return same ? null : showDifference(path, a, b);
  }

  if (Array.isArray(a) || Array.isArray(b)) {
    if (!Array.isArray(a) || !Array.isArray(b)) return showDifference(path, a, b);
    if (a.length !== b.length) return `${path}: array length ${a.length} != ${b.length}`;
    for (let i = 0; i < a.length; i++) {
      const difference = astDiffPath(a[i], b[i], `${path}[${i}]`);
      if (difference) return difference;
    }
    return null;
  }

  const aKeys = comparableKeys(a);
  const bKeys = comparableKeys(b);
  if (aKeys.length !== bKeys.length || aKeys.some((key, i) => key !== bKeys[i])) {
    return `${path}: keys [${aKeys.join(", ")}] != [${bKeys.join(", ")}]`;
  }
  for (const key of aKeys) {
    const difference = astDiffPath(
      (a as Record<string, unknown>)[key],
      (b as Record<string, unknown>)[key],
      `${path}.${key}`,
    );
    if (difference) return difference;
  }
  return null;
}

function showDifference(path: string, a: unknown, b: unknown): string {
  return `${path}: ${showValue(a)} != ${showValue(b)}`;
}

function comparableKeys(node: object): string[] {
  const { type, value } = node as { type?: unknown; value?: unknown };
  const skipRaw =
    type === "Literal" &&
    (typeof value === "string" || typeof value === "bigint" || "regex" in node);
  return Object.keys(node)
    .filter((key) => !UNCOMPARED_KEYS.has(key) && !(skipRaw && key === "raw"))
    .sort();
}

function showValue(value: unknown): string {
  const text = value === undefined ? "undefined" : serializeAstJson(value);
  return text.length > 80 ? `${text.slice(0, 77)}...` : text;
}

interface Position {
  line: number;
  column: number;
}

interface LineMarker {
  line: number;
  columnStart: number;
  columnEnd: number;
  message: string;
  isPrimary: boolean;
}

function positionOf(source: string, offset: number): Position {
  const end = Math.max(0, Math.min(offset, source.length));
  let line = 0;
  let column = 0;
  for (let i = 0; i < end; i++) {
    if (source[i] === "\n") {
      line++;
      column = 0;
    } else {
      column++;
    }
  }
  return { line, column };
}

function expandTabs(text: string): string {
  let out = "";
  for (const char of text) {
    out += char === "\t" ? " ".repeat(TAB_WIDTH - (out.length % TAB_WIDTH)) : char;
  }
  return out;
}

function buildMarkers(source: string, lines: string[], diagnostic: Diagnostic): LineMarker[] {
  const markers: LineMarker[] = [];

  const addSpan = (start: number, end: number, message: string, isPrimary: boolean) => {
    const from = positionOf(source, start);
    const to = positionOf(source, Math.max(end, start + 1));
    if (from.line === to.line) {
      const columnEnd = Math.max(to.column, from.column + 1);
      markers.push({ line: from.line, columnStart: from.column, columnEnd, message, isPrimary });
      return;
    }
    const firstLength = lines[from.line]?.length ?? 0;
    const firstEnd = Math.max(firstLength, from.column + 1);
    markers.push({
      line: from.line,
      columnStart: from.column,
      columnEnd: firstEnd,
      message: "",
      isPrimary,
    });
    for (let line = from.line + 1; line < to.line; line++) {
      const length = lines[line]?.length ?? 0;
      if (length === 0) continue;
      markers.push({ line, columnStart: 0, columnEnd: length, message: "", isPrimary });
    }
    const lastEnd = Math.max(to.column, 1);
    markers.push({ line: to.line, columnStart: 0, columnEnd: lastEnd, message, isPrimary });
  };

  addSpan(diagnostic.start, diagnostic.end, "", true);
  for (const label of diagnostic.labels) addSpan(label.start, label.end, label.message, false);
  return markers;
}

function renderUnderlines(line: string, markers: LineMarker[], gutter: string): string[] {
  const rows: string[] = [];
  const sorted = [...markers].sort((a, b) => {
    if (a.isPrimary !== b.isPrimary) return a.isPrimary ? -1 : 1;
    return a.columnStart - b.columnStart;
  });
  const placed: { start: number; end: number }[][] = [];

  for (const marker of sorted) {
    const start = expandTabs(line.slice(0, marker.columnStart)).length;
    const end = expandTabs(line.slice(0, marker.columnEnd)).length;
    const width = Math.max(end - start, 1);
    const message = marker.message ? ` ${marker.message}` : "";
    const labelEnd = start + width + message.length;

    let row = placed.findIndex((spans) =>
      spans.every((span) => labelEnd <= span.start || start >= span.end),
    );
    if (row === -1) {
      row = placed.length;
      placed.push([]);
    }
    placed[row]!.push({ start, end: labelEnd });

    while (rows.length <= row) rows.push("");
    const mark = marker.isPrimary ? "^" : "~";
    rows[row] = gutter + " ".repeat(start) + mark.repeat(width) + message;
  }

  return rows;
}

export interface FormatDiagnosticsOptions {
  /** Whether to show the `--> filename:line:col` location line. @default true */
  showFilename?: boolean;
}

export function formatDiagnostics(
  source: string,
  diagnostics: readonly Diagnostic[],
  filename: string,
  options?: FormatDiagnosticsOptions,
): string {
  const lines = source.split("\n");
  const output: string[] = [];

  for (let index = 0; index < diagnostics.length; index++) {
    const diagnostic = diagnostics[index]!;
    const position = positionOf(source, diagnostic.start);
    const markers = buildMarkers(source, lines, diagnostic);

    const markersByLine = new Map<number, LineMarker[]>();
    for (const marker of markers) {
      const group = markersByLine.get(marker.line);
      if (group === undefined) markersByLine.set(marker.line, [marker]);
      else group.push(marker);
    }

    const shown = new Set<number>();
    for (const { line } of markers) {
      for (let neighbor = line - 1; neighbor <= line + 1; neighbor++) {
        if (neighbor >= 0 && neighbor < lines.length) shown.add(neighbor);
      }
    }

    const displayed = [...shown].sort((a, b) => a - b);
    const lineNumberMax = displayed.length > 0 ? displayed[displayed.length - 1]! + 1 : 1;
    const gutterWidth = Math.max(String(lineNumberMax).length, 2);
    const blank = " ".repeat(gutterWidth);

    output.push(`${diagnostic.severity}: ${diagnostic.message}`);
    if (options?.showFilename !== false) {
      output.push(`${blank} --> ${filename}:${position.line + 1}:${position.column + 1}`);
    }
    output.push(`${blank} |`);

    let previous = -2;
    for (const line of displayed) {
      if (previous >= 0 && line > previous + 1) output.push(`${".".repeat(gutterWidth)} |`);
      previous = line;

      const text = lines[line] ?? "";
      output.push(`${String(line + 1).padStart(gutterWidth)} | ${expandTabs(text)}`);
      const lineMarkers = markersByLine.get(line);
      if (lineMarkers !== undefined) {
        output.push(...renderUnderlines(text, lineMarkers, `${blank} | `));
      }
    }

    output.push(`${blank} |`);

    if (diagnostic.help) {
      output.push(`${blank} = help: ${diagnostic.help}`);
      output.push("");
    }

    if (index < diagnostics.length - 1) output.push("");
  }

  return output.join("\n");
}

type NodeOfKind<K> = K extends AliasName ? AliasMap[K] : K extends NodeType ? NodeOfType<K> : never;

export function nodesOf<K extends NodeType | AliasName>(
  root: Node,
  ...kinds: K[]
): NodeOfKind<K>[] {
  const byName = is as unknown as Record<string, (node: Node) => boolean>;
  const guards = kinds.map((kind) => byName[kind]!);
  const found: Node[] = [];
  walk(root, {
    enter(node) {
      if (guards.some((guard) => guard(node))) found.push(node);
    },
  });
  return found as NodeOfKind<K>[];
}
