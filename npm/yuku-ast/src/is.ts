import type {
  BigIntLiteral,
  BooleanLiteral,
  ComputedMemberExpression,
  Directive,
  Identifier,
  Node,
  NodeOfType,
  NodeType,
  NullLiteral,
  NumericLiteral,
  PrivateFieldExpression,
  RegExpLiteral,
  StaticMemberExpression,
  StringLiteral,
} from "@yuku-toolchain/types";
import { ALIAS_GROUPS, type AliasMap, type AliasName } from "./aliases.ts";
import { NODE_TYPES } from "./generated.ts";

type MaybeNode = Node | null | undefined;

// the generated list must cover every NodeType, both directions checked
type _missing = Record<Exclude<NodeType, (typeof NODE_TYPES)[number]>, never>;
type _extra = Record<Exclude<(typeof NODE_TYPES)[number], NodeType>, never>;
const _exhaustive: [_missing, _extra] = [{}, {}];
void _exhaustive;

function aliasGuard<K extends AliasName>(name: K): (node: MaybeNode) => node is AliasMap[K] {
  const set = new Set<string>(ALIAS_GROUPS[name]);
  return (node): node is AliasMap[K] => node != null && set.has(node.type);
}

type ConcreteGuards = { [K in NodeType]: (node: MaybeNode) => node is NodeOfType<K> };

const concrete = Object.fromEntries(
  NODE_TYPES.map((type) => [type, (node: MaybeNode) => node != null && node.type === type]),
) as ConcreteGuards;

/**
 * Type guards, one per concrete node `type` plus alias and shape
 * guards. Every guard accepts `null` and `undefined` and narrows.
 *
 * @example
 * is.CallExpression(node)
 * is.Identifier(node, "require")
 * is.Expression(node)
 * is.StringLiteral(node)
 */
export const is = {
  ...concrete,

  /** True when `node.type` is one of `types`, narrowing to that union. */
  oneOf: <const K extends NodeType>(node: MaybeNode, types: readonly K[]): node is NodeOfType<K> =>
    node != null && types.some((type) => type === node.type),

  /** An `Identifier`, optionally with the exact `name`. */
  Identifier: (node: MaybeNode, name?: string): node is Identifier =>
    node?.type === "Identifier" && (name === undefined || node.name === name),

  Expression: aliasGuard("Expression"),
  Statement: aliasGuard("Statement"),
  Declaration: aliasGuard("Declaration"),
  ModuleDeclaration: aliasGuard("ModuleDeclaration"),
  Function: aliasGuard("Function"),
  Class: aliasGuard("Class"),
  Method: aliasGuard("Method"),
  Loop: aliasGuard("Loop"),
  Pattern: aliasGuard("Pattern"),
  JSX: aliasGuard("JSX"),
  TSType: aliasGuard("TSType"),

  StringLiteral: (node: MaybeNode): node is StringLiteral =>
    node?.type === "Literal" && typeof node.value === "string",
  NumericLiteral: (node: MaybeNode): node is NumericLiteral =>
    node?.type === "Literal" && typeof node.value === "number",
  BooleanLiteral: (node: MaybeNode): node is BooleanLiteral =>
    node?.type === "Literal" && typeof node.value === "boolean",
  NullLiteral: (node: MaybeNode): node is NullLiteral =>
    node?.type === "Literal" && node.raw === "null",
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
};
