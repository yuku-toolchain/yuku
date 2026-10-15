import { CHAR_CR, CHAR_LF, CHAR_LS, CHAR_PS } from "./utils";

const MAPPING_FIELDS = 3;
const SEGMENT_BYTES_MAX = 1 + 4 * 7;

const VLQ_DIGITS = new TextEncoder().encode(
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
);
const VLQ_ZERO = VLQ_DIGITS[0]!;
const CHAR_COMMA = 0x2c;
const CHAR_SEMICOLON = 0x3b;

const LINE_SEPARATOR = String.fromCharCode(CHAR_LS);
const PARAGRAPH_SEPARATOR = String.fromCharCode(CHAR_PS);

const LATIN1 = new TextDecoder("latin1");

export class Mappings {
  data: Int32Array;
  length = 0;

  constructor(capacity: number) {
    this.data = new Int32Array(capacity * MAPPING_FIELDS);
  }

  push(committed: number, colOffset: number, sourceOffset: number): void {
    if (this.length + MAPPING_FIELDS > this.data.length) {
      const data = new Int32Array(this.data.length * 2);
      data.set(this.data);
      this.data = data;
    }
    const at = this.length;
    this.data[at] = committed;
    this.data[at + 1] = colOffset;
    this.data[at + 2] = sourceOffset;
    this.length = at + MAPPING_FIELDS;
  }
}

export function encodeMappings(output: string, source: string, mappings: Mappings): string {
  const { data, length } = mappings;
  const outputStarts = lineStarts(output);
  const sourceStarts = lineStarts(source);
  let bytes = new Uint8Array(Math.max((length / MAPPING_FIELDS) * 6, 1024));
  let at = 0;

  let genLine = 0;
  let sourceLine = 0;
  let lastGenLine = 0;
  let lastGenCol = 0;
  let lastSourceLine = 0;
  let lastSourceCol = 0;

  // a mapping waits until the next one moves on, so the last of several on one column wins
  let hasPending = false;
  let pendingGenLine = 0;
  let pendingGenCol = 0;
  let pendingSourceLine = 0;
  let pendingSourceCol = 0;

  // the pass past the last mapping only writes the one still pending
  for (let i = 0; i <= length; i += MAPPING_FIELDS) {
    const done = i === length;
    let genCol = 0;
    let sourceCol = 0;
    if (!done) {
      const committed = data[i]!;
      while (genLine + 1 < outputStarts.length && outputStarts[genLine + 1]! <= committed) genLine++;
      genCol = committed - outputStarts[genLine]! + data[i + 1]!;
      const sourceOffset = data[i + 2]!;
      sourceLine = lineOf(sourceStarts, sourceOffset, sourceLine);
      sourceCol = sourceOffset - sourceStarts[sourceLine]!;
    }

    if (hasPending && (done || pendingGenLine !== genLine || pendingGenCol !== genCol)) {
      const lines = pendingGenLine - lastGenLine;
      if (at + lines + SEGMENT_BYTES_MAX > bytes.length) bytes = grow(bytes, at, lines);
      at = writeSegment(
        bytes,
        at,
        lines,
        lines > 0 ? pendingGenCol : pendingGenCol - lastGenCol,
        pendingSourceLine - lastSourceLine,
        pendingSourceCol - lastSourceCol,
      );
      lastGenLine = pendingGenLine;
      lastGenCol = pendingGenCol;
      lastSourceLine = pendingSourceLine;
      lastSourceCol = pendingSourceCol;
    }
    if (done) break;

    hasPending = true;
    pendingGenLine = genLine;
    pendingGenCol = genCol;
    pendingSourceLine = sourceLine;
    pendingSourceCol = sourceCol;
  }
  return LATIN1.decode(bytes.subarray(0, at));
}

function lineStarts(text: string): number[] {
  const starts = [0];
  if (
    text.indexOf("\r") === -1 &&
    text.indexOf(LINE_SEPARATOR) === -1 &&
    text.indexOf(PARAGRAPH_SEPARATOR) === -1
  ) {
    for (let at = text.indexOf("\n"); at !== -1; at = text.indexOf("\n", at + 1)) {
      starts.push(at + 1);
    }
    return starts;
  }
  for (let i = 0; i < text.length; i++) {
    const c = text.charCodeAt(i);
    if (c === CHAR_CR) {
      if (text.charCodeAt(i + 1) === CHAR_LF) i++;
      starts.push(i + 1);
    } else if (c === CHAR_LF || c === CHAR_LS || c === CHAR_PS) {
      starts.push(i + 1);
    }
  }
  return starts;
}

function lineOf(starts: number[], offset: number, hint: number): number {
  const last = starts.length - 1;
  if (starts[hint]! <= offset) {
    if (hint === last || offset < starts[hint + 1]!) return hint;
    if (hint + 1 === last || offset < starts[hint + 2]!) return hint + 1;
  }
  let low = 0;
  let high = last;
  while (low < high) {
    const mid = (low + high + 1) >>> 1;
    if (starts[mid]! <= offset) low = mid;
    else high = mid - 1;
  }
  return low;
}

function grow(
  bytes: Uint8Array<ArrayBuffer>,
  length: number,
  lines: number,
): Uint8Array<ArrayBuffer> {
  const grown = new Uint8Array(Math.max(length + lines + SEGMENT_BYTES_MAX, bytes.length * 2));
  grown.set(bytes.subarray(0, length));
  return grown;
}

// a segment of deltas, after a `;` per generated line it moves down or a `,` on the same line
function writeSegment(
  bytes: Uint8Array,
  start: number,
  lines: number,
  genCol: number,
  sourceLine: number,
  sourceCol: number,
): number {
  let at = start;
  if (lines > 0) {
    for (let k = 0; k < lines; k++) bytes[at++] = CHAR_SEMICOLON;
  } else if (at > 0) {
    bytes[at++] = CHAR_COMMA;
  }
  at = writeVlq(bytes, at, genCol);
  // the source index delta, always zero with one source
  bytes[at++] = VLQ_ZERO;
  at = writeVlq(bytes, at, sourceLine);
  return writeVlq(bytes, at, sourceCol);
}

function writeVlq(bytes: Uint8Array, start: number, value: number): number {
  let bits = value < 0 ? (-value << 1) | 1 : value << 1;
  let at = start;
  do {
    const digit = bits & 0x1f;
    bits >>>= 5;
    bytes[at++] = VLQ_DIGITS[bits === 0 ? digit : digit | 0x20]!;
  } while (bits !== 0);
  return at;
}
