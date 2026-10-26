import type {
  BigIntLiteral,
  BooleanLiteral,
  ComputedMemberExpression,
  Directive,
  Expression,
  Node,
  NodeOfType,
  NodeType,
  NullLiteral,
  NumericLiteral,
  ParenthesizedExpression,
  PrivateFieldExpression,
  RegExpLiteral,
  StandardNodeType,
  StaticMemberExpression,
  StringLiteral,
  TSAsExpression,
  TSNonNullExpression,
  TSSatisfiesExpression,
  TSTypeAssertion,
} from "../types";
import { decodeBase64 } from "../core/base64";
import { ALIAS_GROUPS, ALIAS_MEMBERS, type AliasMap, type AliasName } from "./aliases";
import { NODE_TYPES } from "./generated";
import { CHAR, CHUNKS, START } from "./identifier-tables";

export type MaybeNode = Node | null | undefined;

/** Parentheses or an erased TypeScript assertion around an `expression`. */
export type Wrapper =
  | ParenthesizedExpression
  | TSAsExpression
  | TSSatisfiesExpression
  | TSNonNullExpression
  | TSTypeAssertion;

const WRAPPERS: ReadonlySet<string> = new Set<Wrapper["type"]>([
  "ParenthesizedExpression",
  "TSAsExpression",
  "TSSatisfiesExpression",
  "TSNonNullExpression",
  "TSTypeAssertion",
]);

type _missing = Record<Exclude<StandardNodeType, (typeof NODE_TYPES)[number]>, never>;
type _extra = Record<Exclude<(typeof NODE_TYPES)[number], StandardNodeType>, never>;
const _exhaustive: [_missing, _extra] = [{}, {}];
void _exhaustive;

type ConcreteGuards = { [K in StandardNodeType]: (node: MaybeNode) => node is NodeOfType<K> };

type AliasGuards = { [K in AliasName]: (node: MaybeNode) => node is AliasMap[K] };

const concrete = Object.fromEntries(
  NODE_TYPES.map((type) => [type, (node: MaybeNode) => node != null && node.type === type]),
) as ConcreteGuards;

const aliases = Object.fromEntries(
  (Object.keys(ALIAS_GROUPS) as AliasName[]).map((name) => {
    const types = ALIAS_MEMBERS[name]!;
    return [name, (node: MaybeNode) => node != null && types.has(node.type)];
  }),
) as AliasGuards;

const shapes = {
  /** Whether `node.type` is one of `types`. */
  oneOf: <const K extends NodeType>(node: MaybeNode, types: readonly K[]): node is NodeOfType<K> =>
    node != null && (types as readonly string[]).includes(node.type),

  StringLiteral: (node: MaybeNode): node is StringLiteral =>
    node?.type === "Literal" && typeof node.value === "string",
  NumericLiteral: (node: MaybeNode): node is NumericLiteral =>
    node?.type === "Literal" && typeof node.value === "number",
  BooleanLiteral: (node: MaybeNode): node is BooleanLiteral =>
    node?.type === "Literal" && typeof node.value === "boolean",
  NullLiteral: (node: MaybeNode): node is NullLiteral =>
    node?.type === "Literal" && node.value === null && !("regex" in node),
  BigIntLiteral: (node: MaybeNode): node is BigIntLiteral =>
    node?.type === "Literal" && "bigint" in node,
  RegExpLiteral: (node: MaybeNode): node is RegExpLiteral =>
    node?.type === "Literal" && "regex" in node,

  ComputedMemberExpression: (node: MaybeNode): node is ComputedMemberExpression =>
    node?.type === "MemberExpression" && node.computed,
  StaticMemberExpression: (node: MaybeNode): node is StaticMemberExpression =>
    node?.type === "MemberExpression" && !node.computed && node.property.type === "Identifier",
  PrivateFieldExpression: (node: MaybeNode): node is PrivateFieldExpression =>
    node?.type === "MemberExpression" && node.property.type === "PrivateIdentifier",

  Directive: (node: MaybeNode): node is Directive =>
    node?.type === "ExpressionStatement" && node.directive != null,

  Wrapper: (node: MaybeNode): node is Wrapper => node != null && WRAPPERS.has(node.type),
};

/** Type guards by node `type`, alias, and shape, each accepting `null` and `undefined`. */
export const is: Omit<ConcreteGuards, keyof typeof shapes> & AliasGuards & typeof shapes = {
  ...concrete,
  ...aliases,
  ...shapes,
};

/** The expression inside every {@link Wrapper} around `node`, or `node` itself. */
export function unwrap<T extends MaybeNode>(node: T): T extends Wrapper ? Expression : T {
  let current: MaybeNode = node;
  while (is.Wrapper(current)) current = current.expression;
  return current as T extends Wrapper ? Expression : T;
}

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
