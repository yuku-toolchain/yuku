import type { Mappings } from "./sourcemap";
import {
  CHAR_BANG,
  CHAR_DOT,
  CHAR_EQUALS,
  CHAR_LF,
  CHAR_LT,
  CHAR_MINUS,
  CHAR_PLUS,
  CHAR_QUESTION,
  CHAR_SLASH,
  CHAR_SPACE,
  isIdCont,
} from "./utils";

// where a leading `{`, `function`, `class`, or `let[` would misparse as a block or declaration
export const LEAD_NONE = 0;
export const LEAD_STMT = 1;
export const LEAD_ARROW = 2;
export const LEAD_EXPORT_DEFAULT = 3;

// the byte before `.` in `1 .x`, which would lex as a fraction unspaced
const AFTER_BARE_INTEGER = 0xffff;

// the byte each punctuator fuses with, as in `<!` or `!=`
const FUSES_AFTER = new Uint16Array(128);
FUSES_AFTER[CHAR_PLUS] = CHAR_PLUS;
FUSES_AFTER[CHAR_MINUS] = CHAR_MINUS;
FUSES_AFTER[CHAR_SLASH] = CHAR_SLASH;
FUSES_AFTER[CHAR_LT] = CHAR_LT;
FUSES_AFTER[CHAR_BANG] = CHAR_LT;
FUSES_AFTER[CHAR_EQUALS] = CHAR_BANG;
FUSES_AFTER[CHAR_QUESTION] = CHAR_QUESTION;
FUSES_AFTER[CHAR_DOT] = AFTER_BARE_INTEGER;

const CHUNK_LENGTH = 16 * 1024;

const SPACED_ASCII: string[] = [];
for (let c = 0; c < 128; c++) SPACED_ASCII.push(" " + String.fromCharCode(c));

const SPACES: string[] = [""];
const SPACES_CACHED_MAX = 256;

function spaces(n: number): string {
  if (n < SPACES.length) return SPACES[n]!;
  if (n >= SPACES_CACHED_MAX) return " ".repeat(n);
  while (SPACES.length <= n) SPACES.push(SPACES[SPACES.length - 1] + " ");
  return SPACES[n]!;
}

const FLATTEN_SINK = { value: 0 };

// a string search flattens the rope, so joining the chunks later copies flat strings
function flatten(s: string): void {
  FLATTEN_SINK.value ^= s.indexOf(" ");
}

export class Output {
  readonly pretty: boolean;
  readonly mappings: Mappings | null;
  text = "";
  chunks: string[] | null = null;
  spilled = 0;
  lastChar = 0;
  heldSpaces = 0;
  heldLiteral = false;
  bareIntegerEnd = -1;
  mapStart = -1;
  lead: number = LEAD_NONE;

  constructor(pretty: boolean, mappings: Mappings | null) {
    this.pretty = pretty;
    this.mappings = mappings;
  }

  finish(): string {
    if (this.chunks === null) {
      flatten(this.text);
      return this.text;
    }
    this.chunks.push(this.text);
    return this.chunks.join("");
  }

  length(): number {
    return this.spilled + this.text.length;
  }

  lastByte(): number {
    if (this.heldSpaces > 0) return CHAR_SPACE;
    return this.length() === 0 ? 0 : this.lastChar;
  }

  write(s: string): void {
    const first = s.charCodeAt(0);
    if (!this.pretty) this.dropHeldSpace(first);
    const last = s.length === 1 ? first : s.charCodeAt(s.length - 1);
    if (last === CHAR_SPACE) return this.writeHolding(s, first, false);
    this.lead = LEAD_NONE;
    const held = this.heldSpaces;
    if (held === 0) {
      if (first < 128 && FUSES_AFTER[first] !== 0) this.separateToken(first);
      if (this.mapStart >= 0) this.placeMapping(0);
      this.text += s;
    } else {
      if (this.mapStart >= 0) this.placeMapping(held);
      this.appendAfterHeld(s, first, held);
    }
    this.lastChar = last;
  }

  writeName(s: string): void {
    this.lead = LEAD_NONE;
    const held = this.heldSpaces;
    if (held === 0) {
      if (this.mapStart >= 0) this.placeMapping(0);
      this.text += s;
    } else {
      if (this.mapStart >= 0) this.placeMapping(held);
      this.appendAfterHeld(s, s.charCodeAt(0), held);
    }
    this.lastChar = s.charCodeAt(s.length - 1);
  }

  writeKeyword(word: string): void {
    this.write(word);
    this.heldSpaces++;
    this.heldLiteral = false;
  }

  writeSpaced(pretty: string, compact: string): void {
    this.write(this.pretty ? pretty : compact);
  }

  writeLiteral(s: string): void {
    if (s.length === 0) return;
    const last = s.charCodeAt(s.length - 1);
    if (last === CHAR_SPACE) return this.writeHolding(s, s.charCodeAt(0), true);
    this.lead = LEAD_NONE;
    const held = this.heldSpaces;
    if (this.mapStart >= 0) this.placeMapping(held);
    if (held === 0) this.text += s;
    else this.appendAfterHeld(s, s.charCodeAt(0), held);
    this.lastChar = last;
  }

  // a comment leaves the lead and the mapping to the next token
  writeComment(s: string): void {
    if (s.length === 0) return;
    let end = s.length;
    while (end > 0 && s.charCodeAt(end - 1) === CHAR_SPACE) end--;
    if (end > 0 && this.heldSpaces !== 0) this.commitSpaces();
    this.appendRest(s, end, false);
  }

  atLineStart(): boolean {
    return this.lastChar === CHAR_LF;
  }

  space(): void {
    if (!this.pretty) return;
    this.lead = LEAD_NONE;
    if (this.mapStart >= 0) this.placeMapping(this.heldSpaces);
    this.heldSpaces++;
    this.heldLiteral = false;
  }

  endLine(indent: number): void {
    if (this.length() === 0) return;
    if (this.lastChar !== CHAR_LF) {
      this.text += "\n";
      this.lastChar = CHAR_LF;
    }
    this.heldSpaces = this.pretty ? indent : 0;
    this.heldLiteral = false;
  }

  recordMapping(span: { start: number; end: number }): void {
    if (this.mappings === null) return;
    const start = span.start;
    // a node without a span or with a zero span is synthetic
    if (typeof start !== "number") return;
    if (start === 0 && span.end === 0) return;
    this.mapStart = start;
  }

  spillWhenFull(): void {
    if (this.text.length >= CHUNK_LENGTH) this.spill();
  }

  private writeHolding(s: string, first: number, literal: boolean): void {
    let end = s.length;
    while (end > 0 && s.charCodeAt(end - 1) === CHAR_SPACE) end--;
    if (end > 0) {
      if (this.heldSpaces !== 0) this.commitSpaces();
      else if (!literal) this.separateToken(first);
    }
    this.lead = LEAD_NONE;
    // spaces alone map where the held spaces end
    if (this.mapStart >= 0) this.placeMapping(this.heldSpaces);
    this.appendRest(s, end, literal);
  }

  private appendRest(s: string, end: number, literal: boolean): void {
    if (end > 0) {
      this.text += end === s.length ? s : s.slice(0, end);
      this.lastChar = s.charCodeAt(end - 1);
    }
    if (end < s.length) {
      this.heldSpaces += s.length - end;
      this.heldLiteral = literal;
    }
  }

  private appendAfterHeld(s: string, first: number, held: number): void {
    this.heldSpaces = 0;
    this.heldLiteral = false;
    if (held === 1 && s.length === 1 && first < 128) {
      this.text += SPACED_ASCII[first]!;
    } else {
      this.text += spaces(held);
      this.text += s;
    }
  }

  private commitSpaces(): void {
    this.text += spaces(this.heldSpaces);
    this.lastChar = CHAR_SPACE;
    this.heldSpaces = 0;
    this.heldLiteral = false;
  }

  // compact mode drops a keyword's lone trailing space before punctuation, never a literal's
  private dropHeldSpace(next: number): void {
    if (this.heldSpaces !== 1 || this.heldLiteral) return;
    if (this.length() === 0) return;
    if (isIdCont(this.lastChar) && !isIdCont(next)) this.heldSpaces = 0;
  }

  private separateToken(next: number): void {
    if (next >= 128) return;
    const prev = FUSES_AFTER[next]!;
    if (prev === 0) return;
    const fuses =
      prev === AFTER_BARE_INTEGER ? this.bareIntegerEnd === this.length() : this.lastChar === prev;
    if (!fuses) return;
    this.text += " ";
    this.lastChar = CHAR_SPACE;
  }

  private spill(): void {
    const text = this.text;
    flatten(text);
    this.spilled += text.length;
    this.text = "";
    if (this.chunks === null) this.chunks = [text];
    else this.chunks.push(text);
  }

  private placeMapping(colOffset: number): void {
    this.mappings!.push(this.length(), colOffset, this.mapStart);
    this.mapStart = -1;
  }
}
