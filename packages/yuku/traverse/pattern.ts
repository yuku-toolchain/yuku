import { CHILD_KEYS } from "../ast/generated";
import { holesIn, parseFragment } from "../ast/template";
import type { Node, NodeType } from "../types";
import { fieldsOf } from "./walk";

type Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
type Upper =
  | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M"
  | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "_";

type NameOf<S extends string, Name extends string = ""> = S extends `${infer C}${infer Rest}`
  ? C extends Upper | (Name extends "" ? never : Digit)
    ? NameOf<Rest, `${Name}${C}`>
    : Name
  : Name;

type Skip<S extends string, Name extends string> = S extends `${Name}${infer Rest}` ? Rest : S;

type Metavariables<S extends string, Singles = never, Lists = never> =
  S extends `${string}$${infer Rest}`
    ? Rest extends `$$${infer List}`
      ? Metavariables<Skip<List, NameOf<List>>, Singles, Lists | NameOf<List>>
      : Metavariables<Skip<Rest, NameOf<Rest>>, Singles | NameOf<Rest>, Lists>
    : { singles: Exclude<Singles, "" | "_">; lists: Exclude<Lists, "" | "_"> };

/** What each metavariable matched, a node for `$NAME` and nodes for `$$$NAME`. */
export type Captures<P extends string = string> = string extends P
  ? Record<string, Node | Node[]>
  : { [K in Metavariables<P>["singles"]]: Node } & { [K in Metavariables<P>["lists"]]: Node[] };

/** A compiled pattern, which tests nodes in a walk. */
export interface Pattern<P extends string = string> {
  /** The type of the nodes it matches, the visitor key to test it under. */
  readonly type: NodeType;
  /** What the node matched, or null. */
  match(node: Node): Captures<P> | null;
}

interface Compiled {
  root: Node;
  singles: Map<Node, string>;
  lists: Map<Node, string>;
}

interface Pairing {
  node(pattern: Node, target: Node): boolean;
  list(patterns: (Node | null)[], targets: (Node | null)[]): boolean;
}

const SINGLE = /^\$([A-Z_][A-Z0-9_]*)$/;
const LIST = /^\$\$\$([A-Z_][A-Z0-9_]*)$/;
const WILDCARD = "_";
const UNCOMPARED = new Set(["type", "start", "end", "raw", "comments"]);
const PATTERNS_MAX = 512;
const STEPS_MAX = 100_000;

const compiled = new Map<string, Compiled>();

/**
 * Compiles code where `$NAME` matches one node, `$$$NAME` a run of list items, and `$_` anything
 * uncaptured. A repeated name matches equal nodes.
 */
export function pattern<P extends string>(source: P): Pattern<P> {
  const holes = compile(source);
  const type = holes.root.type;
  return {
    type,
    match(node) {
      if (node.type !== type) return null;
      const matcher = new Matcher(holes);
      return matcher.node(holes.root, node) ? (matcher.captures as Captures<P>) : null;
    },
  };
}

function compile(source: string): Compiled {
  if (typeof source !== "string") throw new TypeError("pattern: `source` must be a string");
  let holes = compiled.get(source);
  if (holes === undefined) {
    const { statement, expression } = parseFragment(source, "tsx", "pattern");
    const root = expression ?? statement;
    holes = { root, singles: holesIn(root, SINGLE, source), lists: holesIn(root, LIST, source) };
    if (holes.singles.has(root)) {
      throw new TypeError(`pattern: \`${source}\` matches any node, so visit the type instead`);
    }
    if (compiled.size === PATTERNS_MAX) compiled.clear();
    compiled.set(source, holes);
  }
  return holes;
}

class Matcher implements Pairing {
  captures: Record<string, Node | Node[]> = {};
  private steps = 0;
  private readonly holes: Compiled;

  constructor(holes: Compiled) {
    this.holes = holes;
  }

  node(pattern: Node, target: Node): boolean {
    if (++this.steps > STEPS_MAX) {
      throw new Error(`match: the pattern backtracks past ${STEPS_MAX} comparisons at one node`);
    }
    const node = unparenthesized(target);
    const single = this.holes.singles.get(pattern);
    return single === undefined ? sameShape(pattern, node, this) : this.capture(single, node);
  }

  list(patterns: (Node | null)[], targets: (Node | null)[]): boolean {
    const matchFrom = (p: number, t: number): boolean => {
      const pattern = patterns[p];
      if (pattern === undefined) return t === targets.length;
      const list = pattern === null ? undefined : this.holes.lists.get(pattern);
      if (list !== undefined) {
        for (let end = t; end <= targets.length; end++) {
          const saved = { ...this.captures };
          const nodes = targets.slice(t, end).filter((node) => node !== null);
          if (this.captureList(list, nodes) && matchFrom(p + 1, end)) return true;
          this.captures = saved;
        }
        return false;
      }
      const target = targets[t];
      if (target === undefined) return false;
      if (pattern === null || target === null) return pattern === target && matchFrom(p + 1, t + 1);
      const saved = { ...this.captures };
      if (this.node(pattern, target) && matchFrom(p + 1, t + 1)) return true;
      this.captures = saved;
      return false;
    };
    return matchFrom(0, 0);
  }

  private capture(name: string, node: Node): boolean {
    if (name === WILDCARD) return true;
    const previous = this.captures[name];
    if (previous === undefined) {
      this.captures[name] = node;
      return true;
    }
    return !Array.isArray(previous) && equal(previous, node);
  }

  private captureList(name: string, nodes: Node[]): boolean {
    if (name === WILDCARD) return true;
    const previous = this.captures[name];
    if (previous === undefined) {
      this.captures[name] = nodes;
      return true;
    }
    if (!Array.isArray(previous) || previous.length !== nodes.length) return false;
    return previous.every((node, i) => equal(node, nodes[i]!));
  }
}

function equal(a: Node, b: Node): boolean {
  const pending: [Node, Node][] = [[a, b]];
  const pairing: Pairing = {
    node(x, y) {
      pending.push([x, y]);
      return true;
    },
    list(xs, ys) {
      if (xs.length !== ys.length) return false;
      return xs.every((x, i) => {
        const y = ys[i] ?? null;
        if (x === null || y === null) return x === y;
        pending.push([x, y]);
        return true;
      });
    },
  };
  for (let pair = pending.pop(); pair !== undefined; pair = pending.pop()) {
    if (!sameShape(unparenthesized(pair[0]), unparenthesized(pair[1]), pairing)) return false;
  }
  return true;
}

function sameShape(a: Node, b: Node, pairing: Pairing): boolean {
  if (a.type !== b.type) return false;
  const childKeys = CHILD_KEYS[a.type] ?? [];
  const from = fieldsOf(a);
  const to = fieldsOf(b);
  for (const key in from) {
    if (UNCOMPARED.has(key)) continue;
    const x = from[key];
    const y = to[key];
    if (!childKeys.includes(key)) {
      if (!sameValue(x, y)) return false;
    } else if (Array.isArray(x) || Array.isArray(y)) {
      if (!pairing.list(listOf(x), listOf(y))) return false;
    } else if (x == null || y == null) {
      if (x != null || y != null) return false;
    } else if (!pairing.node(x as Node, y as Node)) {
      return false;
    }
  }
  return extraFieldsEmpty(from, to);
}

function extraFieldsEmpty(from: Record<string, unknown>, to: Record<string, unknown>): boolean {
  for (const key in to) {
    if (!(key in from) && !UNCOMPARED.has(key) && !isEmpty(to[key])) return false;
  }
  return true;
}

function unparenthesized(node: Node): Node {
  let current = node;
  while (current.type === "ParenthesizedExpression") current = current.expression;
  return current;
}

function listOf(value: unknown): (Node | null)[] {
  return Array.isArray(value) ? value : [];
}

function sameValue(a: unknown, b: unknown): boolean {
  if (a === b) return true;
  if ((a === undefined && isEmpty(b)) || (b === undefined && isEmpty(a))) return true;
  if (a instanceof RegExp || b instanceof RegExp) {
    return a instanceof RegExp && b instanceof RegExp && String(a) === String(b);
  }
  if (a === null || b === null || typeof a !== "object" || typeof b !== "object") return false;
  const left = a as Record<string, unknown>;
  const right = b as Record<string, unknown>;
  for (const key in left) if (!sameValue(left[key], right[key])) return false;
  return extraFieldsEmpty(left, right);
}

function isEmpty(value: unknown): boolean {
  return value === null || value === false || (Array.isArray(value) && value.length === 0);
}
