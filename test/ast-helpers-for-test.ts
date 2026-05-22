import type { Diagnostic } from "yuku-parser";

const BIG_INT_PREFIX = "(BigInt) ";
const REGEXP_PREFIX = "(RegExp) ";
const REGEXP_LITERAL = /^\/(.+)\/([dgimsuyv]*)$/;

// JSON can't represent BigInt or RegExp values natively, so we use tagged strings
// in test snapshots to preserve them.

export function serializeAstJson(obj: unknown, space?: string | number): string {
  return JSON.stringify(
    obj,
    (_, value) => {
      if (typeof value === "bigint") {
        return `${BIG_INT_PREFIX}${value}n`;
      }

      if (value instanceof RegExp) {
        return `${REGEXP_PREFIX}${value.toString()}`;
      }

      return value;
    },
    space,
  );
}

export function deserializeAstJson<T = unknown>(jsonString: string): T {
  return JSON.parse(jsonString, (_, value) => {
    if (typeof value === "object" && value !== null && typeof value.value === "bigint") {
      value.bigint = value.value.toString();
      return value;
    }

    if (typeof value !== "string") {
      return value;
    }

    if (value.startsWith(BIG_INT_PREFIX)) {
      return BigInt(value.slice(BIG_INT_PREFIX.length).replace(/n$/, "").replaceAll("_", ""));
    }

    if (value.startsWith(REGEXP_PREFIX)) {
      const match = value.slice(REGEXP_PREFIX.length).match(REGEXP_LITERAL);

      if (match) {
        try {
          return new RegExp(match[1]!, match[2]);
        } catch {
          return null;
        }
      }

      return null;
    }

    return value;
  }) as T;
}

//

interface Pos {
  line: number;
  col: number;
}

interface LineMarker {
  line: number;
  startCol: number;
  endCol: number;
  message: string;
  isPrimary: boolean;
}

function offsetToPos(source: string, offset: number): Pos {
  const o = Math.max(0, Math.min(offset, source.length));
  let line = 0;
  let col = 0;
  for (let i = 0; i < o; i++) {
    if (source[i] === "\n") {
      line++;
      col = 0;
    } else {
      col++;
    }
  }
  return { line, col };
}

function expandTabs(text: string, tw = 4): string {
  let out = "";
  for (const ch of text) {
    if (ch === "\t") {
      out += " ".repeat(tw - (out.length % tw));
    } else {
      out += ch;
    }
  }
  return out;
}

function mapCol(text: string, col: number, tw = 4): number {
  let expanded = 0;
  const end = Math.min(col, text.length);
  for (let i = 0; i < end; i++) {
    if (text[i] === "\t") {
      expanded += tw - (expanded % tw);
    } else {
      expanded++;
    }
  }
  return expanded;
}

function buildMarkers(source: string, sourceLines: string[], diag: Diagnostic): LineMarker[] {
  const markers: LineMarker[] = [];

  const addSpan = (start: number, end: number, message: string, isPrimary: boolean) => {
    const s = offsetToPos(source, start);
    const e = offsetToPos(source, Math.max(end, start + 1));

    if (s.line === e.line) {
      markers.push({
        line: s.line,
        startCol: s.col,
        endCol: Math.max(e.col, s.col + 1),
        message,
        isPrimary,
      });
    } else {
      const firstLineLen = sourceLines[s.line]?.length ?? 0;
      markers.push({
        line: s.line,
        startCol: s.col,
        endCol: Math.max(firstLineLen, s.col + 1),
        message: "",
        isPrimary,
      });
      for (let ln = s.line + 1; ln < e.line; ln++) {
        const len = sourceLines[ln]?.length ?? 0;
        if (len > 0) {
          markers.push({
            line: ln,
            startCol: 0,
            endCol: len,
            message: "",
            isPrimary,
          });
        }
      }
      markers.push({
        line: e.line,
        startCol: 0,
        endCol: Math.max(e.col, 1),
        message,
        isPrimary,
      });
    }
  };

  addSpan(diag.start, diag.end, "", true);
  for (const label of diag.labels) {
    addSpan(label.start, label.end, label.message, false);
  }

  return markers;
}

function renderUnderlines(rawLine: string, markers: LineMarker[], gutter: string): string[] {
  const rows: string[] = [];
  const sorted = [...markers].sort((a, b) => {
    if (a.isPrimary !== b.isPrimary) return a.isPrimary ? -1 : 1;
    return a.startCol - b.startCol;
  });

  const placed: { start: number; end: number }[][] = [];

  for (const marker of sorted) {
    const expStart = mapCol(rawLine, marker.startCol);
    const expEnd = mapCol(rawLine, marker.endCol);
    const width = Math.max(expEnd - expStart, 1);
    const msg = marker.message ? ` ${marker.message}` : "";
    const totalEnd = expStart + width + msg.length;

    let rowIdx = placed.findIndex((row) =>
      row.every((e) => totalEnd <= e.start || expStart >= e.end),
    );
    if (rowIdx === -1) {
      rowIdx = placed.length;
      placed.push([]);
    }
    placed[rowIdx].push({ start: expStart, end: totalEnd });

    while (rows.length <= rowIdx) rows.push("");
    const ch = marker.isPrimary ? "^" : "~";
    rows[rowIdx] = gutter + " ".repeat(expStart) + ch.repeat(width) + msg;
  }

  return rows;
}

export interface FormatDiagnosticsOptions {
  /** Whether to show the `--> filename:line:col` location line. @default true */
  showFilename?: boolean;
}

export function formatDiagnostics(
  source: string,
  diagnostics: Diagnostic[],
  filename: string,
  options?: FormatDiagnosticsOptions,
): string {
  const sourceLines = source.split("\n");
  const output: string[] = [];

  for (let di = 0; di < diagnostics.length; di++) {
    const diag = diagnostics[di];
    const pos = offsetToPos(source, diag.start);
    const markers = buildMarkers(source, sourceLines, diag);

    const markersByLine = new Map<number, LineMarker[]>();
    for (const m of markers) {
      if (!markersByLine.has(m.line)) markersByLine.set(m.line, []);
      markersByLine.get(m.line)?.push(m);
    }

    const affectedLines = new Set(markers.map((m) => m.line));
    const contextLines = new Set<number>();
    for (const ln of affectedLines) {
      for (let d = -1; d <= 1; d++) {
        const n = ln + d;
        if (n >= 0 && n < sourceLines.length) contextLines.add(n);
      }
    }

    const displayLines = [...contextLines].sort((a, b) => a - b);
    const maxNum = displayLines.length > 0 ? displayLines[displayLines.length - 1] + 1 : 1;
    const gw = Math.max(String(maxNum).length, 2);
    const pad = (n: number) => String(n).padStart(gw);
    const lineGutter = (n: number) => `${pad(n + 1)} | `;
    const emptyGutter = `${" ".repeat(gw)} | `;
    const blankGutter = `${" ".repeat(gw)} |`;

    output.push(`${diag.severity}: ${diag.message}`);
    if (options?.showFilename !== false) {
      output.push(`${" ".repeat(gw)} --> ${filename}:${pos.line + 1}:${pos.col + 1}`);
    }
    output.push(blankGutter);

    let prev = -2;
    for (const ln of displayLines) {
      if (prev >= 0 && ln > prev + 1) {
        output.push(`${".".repeat(gw)} |`);
      }
      prev = ln;

      output.push(lineGutter(ln) + expandTabs(sourceLines[ln] ?? ""));

      const lineM = markersByLine.get(ln);
      if (lineM && lineM.length > 0) {
        for (const row of renderUnderlines(sourceLines[ln] ?? "", lineM, emptyGutter)) {
          output.push(row);
        }
      }
    }

    output.push(blankGutter);

    if (diag.help) {
      output.push(`${" ".repeat(gw)} = help: ${diag.help}`);
      output.push("");
    }

    if (di < diagnostics.length - 1) output.push("");
  }

  return output.join("\n");
}

export function printDiagnostics(
  source: string,
  diagnostics: Diagnostic[],
  filename: string,
): void {
  console.log(formatDiagnostics(source, diagnostics, filename));
}
