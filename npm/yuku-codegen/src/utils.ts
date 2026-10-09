export const CHAR_NUL = 0x00;
export const CHAR_BACKSPACE = 0x08;
export const CHAR_TAB = 0x09;
export const CHAR_LF = 0x0a;
export const CHAR_VT = 0x0b;
export const CHAR_FF = 0x0c;
export const CHAR_CR = 0x0d;
export const CHAR_SPACE = 0x20;
export const CHAR_BANG = 0x21;
export const CHAR_DOUBLE_QUOTE = 0x22;
export const CHAR_DOLLAR = 0x24;
export const CHAR_SINGLE_QUOTE = 0x27;
export const CHAR_OPEN_PAREN = 0x28;
export const CHAR_STAR = 0x2a;
export const CHAR_PLUS = 0x2b;
export const CHAR_MINUS = 0x2d;
export const CHAR_DOT = 0x2e;
export const CHAR_SLASH = 0x2f;
export const CHAR_0 = 0x30;
export const CHAR_LT = 0x3c;
export const CHAR_EQUALS = 0x3d;
export const CHAR_GT = 0x3e;
export const CHAR_QUESTION = 0x3f;
export const CHAR_OPEN_BRACKET = 0x5b;
export const CHAR_BACKSLASH = 0x5c;
export const CHAR_BACKTICK = 0x60;
export const CHAR_OPEN_BRACE = 0x7b;
export const CHAR_LS = 0x2028;
export const CHAR_PS = 0x2029;

const CHAR_9 = 0x39;
const CHAR_UNDERSCORE = 0x5f;

const ID_CONTINUE_ASCII = new Uint8Array(128);
for (let c = 0; c < 128; c++) {
  const letter = (c >= 0x41 && c <= 0x5a) || (c >= 0x61 && c <= 0x7a);
  const digit = c >= CHAR_0 && c <= CHAR_9;
  if (letter || digit || c === CHAR_UNDERSCORE || c === CHAR_DOLLAR) ID_CONTINUE_ASCII[c] = 1;
}

export function isIdCont(code: number): boolean {
  return code >= 0x80 || ID_CONTINUE_ASCII[code] === 1;
}

const WORD_OPERATORS = new Set(["in", "instanceof", "typeof", "void", "delete"]);

export function isWordOp(op: string): boolean {
  return WORD_OPERATORS.has(op);
}

const IDENTIFIER_NAME = /^[$_\p{ID_Start}][$\p{ID_Continue}]*$/u;

export function isIdentifierName(s: string): boolean {
  return IDENTIFIER_NAME.test(s);
}

function isAsciiWhitespace(code: number): boolean {
  return code === CHAR_SPACE || (code >= CHAR_TAB && code <= CHAR_CR);
}

export function isAsciiDigit(code: number): boolean {
  return code >= CHAR_0 && code <= CHAR_9;
}

export function scriptEscape(s: string, i: number): string | null {
  const code = s.charCodeAt(i);
  if (code === CHAR_LT) return scriptOpenAt(s, i) ? "<\\" : null;
  if (code === CHAR_GT) {
    const closes =
      i >= 2 && s.charCodeAt(i - 1) === CHAR_MINUS && s.charCodeAt(i - 2) === CHAR_MINUS;
    return closes ? "\\>" : null;
  }
  return null;
}

function scriptOpenAt(s: string, i: number): boolean {
  if (s.startsWith("!--", i + 1)) return true;
  if (s.charCodeAt(i + 1) !== CHAR_SLASH) return false;
  if (s.slice(i + 2, i + 8).toLowerCase() !== "script") return false;
  const after = i + 8;
  if (after === s.length) return true;
  const next = s.charCodeAt(after);
  return isAsciiWhitespace(next) || next === CHAR_SLASH || next === CHAR_GT;
}

export function isMinimalInteger(raw: string): boolean {
  if (raw.charCodeAt(raw.length - 1) === CHAR_0) return false;
  for (let i = 0; i < raw.length; i++) {
    if (!isAsciiDigit(raw.charCodeAt(i))) return false;
  }
  return true;
}

export function isBareInteger(text: string): boolean {
  if (text.length === 0 || !isAsciiDigit(text.charCodeAt(0))) return false;
  for (let i = 1; i < text.length; i++) {
    const code = text.charCodeAt(i);
    if (!isAsciiDigit(code) && code !== CHAR_UNDERSCORE) return false;
  }
  return true;
}

const EXPONENT_MAGNITUDE_MAX = 100_000;
const DIGITS_MAX = 128;

export function shortestDecimal(s: string): string {
  const e = s.search(/[eE]/);
  const expAt = e < 0 ? s.length : e;
  const mantissa = s.slice(0, expAt);
  const dotAt = mantissa.indexOf(".");
  const intPart = dotAt === -1 ? mantissa : mantissa.slice(0, dotAt);
  const fracPart = dotAt === -1 ? "" : mantissa.slice(dotAt + 1);

  let exp = 0;
  if (expAt !== s.length) {
    const parsed = parseExponent(s.slice(expAt + 1));
    if (parsed === null) return s;
    if (parsed > EXPONENT_MAGNITUDE_MAX || parsed < -EXPONENT_MAGNITUDE_MAX) return s;
    exp = parsed;
  }
  exp -= fracPart.length;

  if (intPart.length + fracPart.length > DIGITS_MAX) return s;
  let d = intPart + fracPart;
  let lead = 0;
  while (lead < d.length && d.charCodeAt(lead) === CHAR_0) lead++;
  if (lead === d.length) return "0";
  d = d.slice(lead);
  let trail = d.length;
  while (d.charCodeAt(trail - 1) === CHAR_0) {
    trail--;
    exp++;
  }
  d = d.slice(0, trail);

  if (exp === 0) return d.length <= s.length ? d : s;

  const expLen = d.length + 1 + String(exp).length;
  let out: string | null;
  if (expLen < fixedLen(d.length, exp)) {
    out = expLen <= DIGITS_MAX ? d + "e" + exp : null;
  } else {
    out = writeFixed(d, exp);
  }
  if (out === null) return s;
  return out.length <= s.length ? out : s;
}

function fixedLen(dlen: number, exp: number): number {
  if (exp > 0) return exp <= DIGITS_MAX ? dlen + exp : Infinity;
  const f = -exp;
  if (f < dlen) return dlen + 1;
  return 1 + f <= DIGITS_MAX ? 1 + f : Infinity;
}

function writeFixed(d: string, exp: number): string | null {
  if (exp > 0) {
    if (d.length + exp > DIGITS_MAX) return null;
    return d + "0".repeat(exp);
  }
  const f = -exp;
  if (f < d.length) {
    if (d.length + 1 > DIGITS_MAX) return null;
    const head = d.length - f;
    return d.slice(0, head) + "." + d.slice(head);
  }
  if (1 + f > DIGITS_MAX) return null;
  return "." + "0".repeat(f - d.length) + d;
}

function parseExponent(text: string): number | null {
  let i = 0;
  let negative = false;
  if (text.charCodeAt(0) === CHAR_PLUS || text.charCodeAt(0) === CHAR_MINUS) {
    negative = text.charCodeAt(0) === CHAR_MINUS;
    i = 1;
  }
  if (i === text.length) return null;
  let value = 0;
  for (; i < text.length; i++) {
    const code = text.charCodeAt(i);
    if (!isAsciiDigit(code)) return null;
    value = value * 10 + (code - CHAR_0);
    if (value > Number.MAX_SAFE_INTEGER / 10) return negative ? -Infinity : Infinity;
  }
  return negative ? -value : value;
}

export function stripUnderscores(raw: string): string | null {
  if (!raw.includes("_")) return raw;
  if (raw.length > DIGITS_MAX) return null;
  return raw.replace(/_/g, "");
}

export function isJsdocBody(value: string): boolean {
  const lines = value.split("\n");
  if (lines.length < 2) return false;
  for (let i = 1; i < lines.length; i++) {
    if (!/^[ \t]*(\*|$)/.test(lines[i]!)) return false;
  }
  return true;
}

export function trimStartSpaceTab(line: string): string {
  return line.replace(/^[ \t]+/, "");
}

export function isSignificantBlockComment(value: string): boolean {
  if (value.length === 0) return false;
  const first = value.charCodeAt(0);
  if (first === CHAR_BANG || first === CHAR_STAR) return true;
  if (/^[ \t\n\v\f\r]*[@#]/.test(value)) return true;
  return value.includes("@license") || value.includes("@preserve") || value.includes("@cc_on");
}

export function hasLineTerminator(text: string): boolean {
  return /[\n\r\u2028\u2029]/.test(text);
}

export function isLoneSurrogateAt(s: string, i: number): boolean {
  const code = s.charCodeAt(i);
  if (code < 0xd800 || code > 0xdfff) return false;
  if (code <= 0xdbff) {
    const next = s.charCodeAt(i + 1);
    return !(next >= 0xdc00 && next <= 0xdfff);
  }
  const prev = i > 0 ? s.charCodeAt(i - 1) : 0;
  return !(prev >= 0xd800 && prev <= 0xdbff);
}

export function surrogateEscape(code: number): string {
  return "\\u" + code.toString(16);
}
