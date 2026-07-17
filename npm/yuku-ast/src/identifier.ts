const ID_START = /^[$_\p{ID_Start}]$/u;
const ID_CONTINUE = /^[$\u200C\u200D\p{ID_Continue}]$/u;
const IDENTIFIER_NAME = /^[$_\p{ID_Start}][$\u200C\u200D\p{ID_Continue}]*$/u;

const MAX_CODE_POINT = 0x10ffff;

/**
 * True if the Unicode code point `cp` can start an identifier: any
 * character with the `ID_Start` property, plus `$` and `_`. Takes a
 * numeric code point as from `codePointAt`.
 */
export function isIdentifierStart(cp: number): boolean {
  if (!Number.isInteger(cp) || cp < 0 || cp > MAX_CODE_POINT) return false;
  return ID_START.test(String.fromCodePoint(cp));
}

/**
 * True if the Unicode code point `cp` can appear after the first
 * character of an identifier: any character with the `ID_Continue`
 * property, plus `$`, `_`, and the ZWNJ and ZWJ joiners.
 */
export function isIdentifierChar(cp: number): boolean {
  if (!Number.isInteger(cp) || cp < 0 || cp > MAX_CODE_POINT) return false;
  return ID_CONTINUE.test(String.fromCodePoint(cp));
}

/**
 * True if `name` is a valid ECMAScript `IdentifierName`, the grammar an
 * identifier token must satisfy. Validates a raw string, not a node.
 * Reserved words are syntactically identifier names, so
 * `isIdentifierName("class")` is true, see {@link isValidIdentifier}.
 */
export function isIdentifierName(name: string): boolean {
  return IDENTIFIER_NAME.test(name);
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

const strictBindOnlyReservedWords = new Set(["eval", "arguments"]);

/**
 * True if `word` is a reserved keyword of the core grammar. Does not
 * include `enum`, `await`, or the strict-mode-only words, see
 * {@link isReservedWord} and {@link isStrictReservedWord}.
 */
export function isKeyword(word: string): boolean {
  return keywords.has(word);
}

/**
 * True if `word` is unconditionally reserved: `enum` in any context,
 * and `await` when `inModule`.
 */
export function isReservedWord(word: string, inModule = false): boolean {
  return (inModule && word === "await") || word === "enum";
}

/**
 * True if `word` is reserved in strict mode: everything
 * {@link isReservedWord} covers, plus `let`, `static`, `yield`, and
 * friends.
 */
export function isStrictReservedWord(word: string, inModule = false): boolean {
  return isReservedWord(word, inModule) || strictReservedWords.has(word);
}

/** True for `eval` and `arguments`, reserved only as strict-mode binding targets. */
export function isStrictBindOnlyReservedWord(word: string): boolean {
  return strictBindOnlyReservedWords.has(word);
}

/**
 * True if `word` is reserved as a strict-mode binding target:
 * everything {@link isStrictReservedWord} covers, plus `eval` and
 * `arguments`.
 */
export function isStrictBindReservedWord(word: string, inModule = false): boolean {
  return isStrictReservedWord(word, inModule) || isStrictBindOnlyReservedWord(word);
}

/**
 * True if `name` can be used as an identifier binding: a valid
 * {@link isIdentifierName} that, when `reserved` is true (the default),
 * is neither a keyword nor a strict-mode reserved word. The check to
 * reach for when turning an arbitrary string into a local binding name.
 */
export function isValidIdentifier(name: string, reserved = true): boolean {
  if (reserved && (isKeyword(name) || isStrictReservedWord(name, true))) return false;
  return isIdentifierName(name);
}
