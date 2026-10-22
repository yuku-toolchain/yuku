import type {
  BigIntLiteral,
  BooleanLiteral,
  ComputedMemberExpression,
  Directive,
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
import { ALIAS_GROUPS, type AliasMap, type AliasName } from "./aliases";
import { NODE_TYPES } from "./generated";

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
    const types = new Set<string>(ALIAS_GROUPS[name]);
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
