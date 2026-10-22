import { assertNode } from "../traverse/walk";
import type { BindingPattern, Expression, Identifier, Node, StringLiteral } from "../types";
import { is, type MaybeNode, type Wrapper } from "./is";

/** An `Identifier`'s `name` or a string `Literal`'s `value`, null for anything else. */
export function nameOf(node: Identifier | StringLiteral): string;
export function nameOf(node: MaybeNode): string | null;
export function nameOf(node: MaybeNode): string | null {
  if (node == null) return null;
  if (node.type === "Identifier") return node.name;
  if (node.type === "Literal" && typeof node.value === "string") return node.value;
  return null;
}

/** The expression inside every {@link Wrapper} around `node`, or `node` itself. */
export function unwrap<T extends MaybeNode>(node: T): T extends Wrapper ? Expression : T {
  let current: MaybeNode = node;
  while (is.Wrapper(current)) current = current.expression;
  return current as T extends Wrapper ? Expression : T;
}

/** Every `Identifier` a binding pattern declares, in source order. */
export function bindingIdentifiers(pattern: BindingPattern | null | undefined): Identifier[] {
  const out: Identifier[] = [];
  const stack: MaybeNode[] = [pattern];
  while (stack.length > 0) {
    const node = stack.pop();
    if (node == null) continue;
    switch (node.type) {
      case "Identifier":
        out.push(node);
        break;
      case "ArrayPattern":
        for (let i = node.elements.length - 1; i >= 0; i--) stack.push(node.elements[i]);
        break;
      case "ObjectPattern":
        for (let i = node.properties.length - 1; i >= 0; i--) {
          const property = node.properties[i]!;
          stack.push(property.type === "RestElement" ? property.argument : property.value);
        }
        break;
      case "AssignmentPattern":
        stack.push(node.left);
        break;
      case "RestElement":
        stack.push(node.argument);
        break;
    }
  }
  return out;
}

/** A deep copy of a node. */
export function clone<T extends Node>(node: T): T {
  assertNode(node, "clone");
  const root = { ...node };
  const pending: object[] = [root];
  for (let item = pending.pop(); item !== undefined; item = pending.pop()) {
    if (Array.isArray(item)) {
      for (let i = 0; i < item.length; i++) {
        const value: unknown = item[i];
        if (value !== null && typeof value === "object") item[i] = copied(value, pending);
      }
    } else {
      const fields = item as Record<string, unknown>;
      for (const key in fields) {
        const value = fields[key];
        if (value !== null && typeof value === "object") fields[key] = copied(value, pending);
      }
    }
  }
  return root;
}

function copied(value: object, pending: object[]): object {
  if (Array.isArray(value)) {
    const copy = value.slice();
    pending.push(copy);
    return copy;
  }
  if (value instanceof RegExp) return new RegExp(value);
  const copy = { ...value };
  pending.push(copy);
  return copy;
}
