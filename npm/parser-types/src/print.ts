import type { Diagnostic } from ".";

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

const R = "\x1b[0m";
const B = "\x1b[1m";
const DIM = "\x1b[2m";
const RED = "\x1b[91m";
const YEL = "\x1b[93m";
const BLU = "\x1b[94m";
const CYN = "\x1b[96m";

const SEV_COLOR: Record<string, string> = {
	error: RED,
	warning: YEL,
	info: BLU,
	hint: CYN,
};

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

function buildMarkers(
	source: string,
	sourceLines: string[],
	diag: Diagnostic,
): LineMarker[] {
	const markers: LineMarker[] = [];

	const addSpan = (
		start: number,
		end: number,
		message: string,
		isPrimary: boolean,
	) => {
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

function renderUnderlines(
	rawLine: string,
	markers: LineMarker[],
	sevColor: string,
	gutter: string,
): string[] {
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
		const color = marker.isPrimary ? sevColor : BLU;
		rows[rowIdx] =
			gutter +
			" ".repeat(expStart) +
			B +
			color +
			ch.repeat(width) +
			R +
			color +
			msg +
			R;
	}

	return rows;
}

export function printDiagnostics(
	source: string,
	diagnostics: Diagnostic[],
	filename: string,
): void {
	const sourceLines = source.split("\n");
	const output: string[] = [];

	for (let di = 0; di < diagnostics.length; di++) {
		const diag = diagnostics[di];
		const sevColor = SEV_COLOR[diag.severity] ?? RED;
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
		const maxNum =
			displayLines.length > 0 ? displayLines[displayLines.length - 1] + 1 : 1;
		const gw = Math.max(String(maxNum).length, 2);
		const pad = (n: number) => String(n).padStart(gw);
		const lineGutter = (n: number) => `${DIM}${pad(n + 1)} |${R} `;
		const emptyGutter = `${DIM}${" ".repeat(gw)} |${R} `;
		const blankGutter = `${DIM}${" ".repeat(gw)} |${R}`;

		output.push(`${B}${sevColor}${diag.severity}${R}${B}: ${diag.message}${R}`);
		output.push(
			`${DIM}${" ".repeat(gw)} --> ${filename}:${pos.line + 1}:${pos.col + 1}${R}`,
		);
		output.push(blankGutter);

		let prev = -2;
		for (const ln of displayLines) {
			if (prev >= 0 && ln > prev + 1) {
				output.push(`${DIM}${".".repeat(gw)} |${R}`);
			}
			prev = ln;

			output.push(lineGutter(ln) + expandTabs(sourceLines[ln] ?? ""));

			const lineM = markersByLine.get(ln);
			if (lineM && lineM.length > 0) {
				for (const row of renderUnderlines(
					sourceLines[ln] ?? "",
					lineM,
					sevColor,
					emptyGutter,
				)) {
					output.push(row);
				}
			}
		}

		output.push(blankGutter);

		if (diag.help) {
			output.push(`${DIM}${" ".repeat(gw)} =${R} ${CYN}help${R}: ${diag.help}`);
			output.push("");
		}

		if (di < diagnostics.length - 1) output.push("");
	}

	console.log(output.join("\n"));
}
