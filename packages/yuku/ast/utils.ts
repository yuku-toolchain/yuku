import type {
  BindingPattern,
  CallExpression,
  Expression,
  Identifier,
  Node,
  NodeOfType,
  NodeType,
  ParenthesizedExpression,
  StringLiteral,
  TSAsExpression,
  TSNonNullExpression,
  TSSatisfiesExpression,
  TSTypeAssertion,
} from "../types";
import { is, type MaybeNode } from "./is";
import { walk } from "./walk";

/**
 * The static name a node denotes: an `Identifier`'s `name` or a string
 * `Literal`'s `value`, as in a `ModuleExportName` or a static property
 * key. Null for anything else.
 */
export function nameOf(node: Identifier | StringLiteral): string;
export function nameOf(node: MaybeNode): string | null;
export function nameOf(node: MaybeNode): string | null {
  if (node == null) return null;
  if (node.type === "Identifier") return node.name;
  if (node.type === "Literal" && typeof node.value === "string") return node.value;
  return null;
}

/**
 * The runtime value of a `Literal` node: string, number, boolean,
 * bigint, RegExp, or null for the null literal. Undefined for
 * non-literal nodes.
 */
export function literalValue(
  node: MaybeNode,
): string | number | boolean | bigint | RegExp | null | undefined {
  if (node?.type !== "Literal") return undefined;
  if ("bigint" in node) return BigInt(node.bigint);
  return node.value;
}

/**
 * An expression that only wraps another one: parentheses and the
 * TypeScript assertion forms erased on the way to JavaScript.
 */
export type Wrapper =
  | ParenthesizedExpression
  | TSAsExpression
  | TSSatisfiesExpression
  | TSNonNullExpression
  | TSTypeAssertion;

const WRAPPERS = new Set<NodeType>([
  "ParenthesizedExpression",
  "TSAsExpression",
  "TSSatisfiesExpression",
  "TSNonNullExpression",
  "TSTypeAssertion",
]);

/**
 * True when `node` is a {@link Wrapper}, whose `expression` is the
 * wrapped node.
 *
 * @example
 * isWrapper(node) ? node.expression : node
 */
export function isWrapper(node: MaybeNode): node is Wrapper {
  return node != null && WRAPPERS.has(node.type);
}

/**
 * The expression inside parentheses and erased TypeScript assertion
 * wrappers: `((x as any))!` unwraps to `x`. Returns any other node,
 * `null`, or `undefined` as it is.
 */
export function unwrap<T extends MaybeNode>(node: T): T extends Wrapper ? Expression : T {
  let current: MaybeNode = node;
  while (isWrapper(current)) current = current.expression;
  return current as T extends Wrapper ? Expression : T;
}

/**
 * True when `node` calls an `Identifier` named `name`, or any of
 * `name` when given an array. The callee is read through {@link unwrap}.
 *
 * @example
 * isCallOf(node, "require")
 * isCallOf(node, ["defineConfig", "defineProject"])
 */
export function isCallOf(
  node: MaybeNode,
  name: string | readonly string[],
): node is CallExpression {
  if (node?.type !== "CallExpression") return false;
  const callee = unwrap(node.callee);
  if (callee.type !== "Identifier") return false;
  return typeof name === "string" ? callee.name === name : name.includes(callee.name);
}

/**
 * Every binding `Identifier` a pattern introduces, in source order:
 * the pattern itself, destructuring leaves, defaults' targets, and
 * rest elements.
 */
export function bindingIdentifiers(pattern: BindingPattern | null | undefined): Identifier[] {
  const out: Identifier[] = [];
  const visit = (node: MaybeNode): void => {
    if (node == null) return;
    switch (node.type) {
      case "Identifier":
        out.push(node);
        break;
      case "ArrayPattern":
        for (const element of node.elements) visit(element);
        break;
      case "ObjectPattern":
        for (const property of node.properties) {
          visit(property.type === "RestElement" ? property.argument : property.value);
        }
        break;
      case "AssignmentPattern":
        visit(node.left);
        break;
      case "RestElement":
        visit(node.argument);
        break;
    }
  };
  visit(pattern);
  return out;
}

/** Every node of the given type(s) under `root`, in source order. */
export function findAll<K extends NodeType>(
  root: Node,
  types: K | readonly K[],
): NodeOfType<K>[] {
  const wanted: readonly K[] = typeof types === "string" ? [types] : types;
  const out: NodeOfType<K>[] = [];
  walk(root, {
    enter(node) {
      if (is.oneOf(node, wanted)) out.push(node);
    },
  });
  return out;
}
