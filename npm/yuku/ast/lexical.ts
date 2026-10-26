import { decodeBase64 } from "../core/base64";
import { CHAR, CHUNKS, START } from "./identifier-tables";

const start = decodeBase64(START);
const char = decodeBase64(CHAR);
const chunks = decodeBase64(CHUNKS);

function has(roots: Uint8Array, cp: number): boolean {
  if (cp >>> 0 !== cp) return false;
  const chunk = cp >>> 9;
  if (chunk >= roots.length) return false;
  return ((chunks[(roots[chunk]! << 6) | ((cp >>> 3) & 63)]! >>> (cp & 7)) & 1) === 1;
}

/** True if the code point `cp` can start an identifier. */
export function isIdentifierStart(cp: number): boolean {
  return has(start, cp);
}

/** True if the code point `cp` can continue an identifier. */
export function isIdentifierChar(cp: number): boolean {
  return has(char, cp);
}

/** True if `name` is an `IdentifierName`, reserved words included. */
export function isIdentifierName(name: string): boolean {
  if (name.length === 0) return false;
  let cp = name.codePointAt(0)!;
  if (!has(start, cp)) return false;
  for (let i = cp > 0xffff ? 2 : 1; i < name.length; i += cp > 0xffff ? 2 : 1) {
    cp = name.codePointAt(i)!;
    if (!has(char, cp)) return false;
  }
  return true;
}

const keywords = new Set([
  "break",
  "case",
  "catch",
  "continue",
  "debugger",
  "default",
  "do",
  "else",
  "finally",
  "for",
  "function",
  "if",
  "return",
  "switch",
  "throw",
  "try",
  "var",
  "const",
  "while",
  "with",
  "new",
  "this",
  "super",
  "class",
  "extends",
  "export",
  "import",
  "null",
  "true",
  "false",
  "in",
  "instanceof",
  "typeof",
  "void",
  "delete",
]);

const strictReservedWords = new Set([
  "implements",
  "interface",
  "let",
  "package",
  "private",
  "protected",
  "public",
  "static",
  "yield",
]);

/** True if `word` is a keyword of the core grammar, not `enum`, `await`, or strict-only words. */
export function isKeyword(word: string): boolean {
  return keywords.has(word);
}

/** True for `enum`, and `await` when `inModule`. */
export function isReservedWord(word: string, inModule = false): boolean {
  return (inModule && word === "await") || word === "enum";
}

/** True if `word` is reserved in strict mode code. */
export function isStrictReservedWord(word: string, inModule = false): boolean {
  return isReservedWord(word, inModule) || strictReservedWords.has(word);
}

/** True for `eval` and `arguments`, which strict mode code cannot bind. */
export function isEvalOrArguments(word: string): boolean {
  return word === "eval" || word === "arguments";
}

/** True if `name` is an identifier no code reserves. */
export function isValidIdentifier(name: string): boolean {
  if (isKeyword(name) || isStrictReservedWord(name, true)) return false;
  return isIdentifierName(name);
}
