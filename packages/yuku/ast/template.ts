import { parse } from "../file/source-file";
import type { Comment, Identifier, Node, SourceLang } from "../types";
import { b } from "./builders";
import { is } from "./is";
import { isIdentifierName } from "./lexical";
import { clone, copyTree } from "./utils";
import { assertNode, walk } from "./walk";

type Item = Node | string | number | boolean | bigint | null | undefined;

/** A node, a name, a literal, or a list of them. */
export type TemplateValue = Item | readonly Item[];

/** Builds a node from JavaScript, an expression unless the code ends in `;`. */
export function js<T extends Node = Node>(
  strings: TemplateStringsArray,
  ...values: readonly TemplateValue[]
): T {
  return build(templateOf(strings, "jsx"), values) as T;
}

/** Builds a node from TypeScript, an expression unless the code ends in `;`. */
export function ts<T extends Node = Node>(
  strings: TemplateStringsArray,
  ...values: readonly TemplateValue[]
): T {
  return build(templateOf(strings, "tsx"), values) as T;
}

interface Template {
  root: Node;
  /** Each hole by the node it stands for, and the nodes on the way to it. */
  marks: Map<object, Hole | typeof NAME | typeof PATH>;
}

/** `"bare"` is a lone name, `"wrapped"` a node around it, `"typed"` a name with a type. */
interface Hole {
  kind: "bare" | "wrapped" | "typed";
  value: number;
  jsx: boolean;
}

interface Build {
  marks: Template["marks"];
  values: readonly TemplateValue[];
  copies: Copy[];
  placed: Set<Node>;
}

/** A copy whose fields still point into the template. */
interface Copy {
  source: object;
  target: Record<string, unknown> | unknown[];
  name: Node | null;
}

const NAME = 0;
const PATH = 1;

const HOLE = "__yuku_hole_";
const HOLE_NAME = /^__yuku_hole_(\d+)$/;
const ESCAPED = /\\+(?=`|\$\{)/g;
const SEMICOLON = /^\s*;$/;
const KINDS: Record<string, string> = { ExpressionStatement: "statement", TSTypeReference: "type" };
const NAMES: ReadonlySet<string> = new Set([
  "Identifier",
  "JSXIdentifier",
  "JSXMemberExpression",
  "JSXNamespacedName",
  "PrivateIdentifier",
  "TSQualifiedName",
  "ObjectPattern",
  "ArrayPattern",
]);

const templates = {
  jsx: new WeakMap<TemplateStringsArray, Template>(),
  tsx: new WeakMap<TemplateStringsArray, Template>(),
};

function templateOf(strings: TemplateStringsArray, lang: keyof typeof templates): Template {
  let template = templates[lang].get(strings);
  if (template === undefined) {
    template = compile(strings, lang);
    templates[lang].set(strings, template);
  }
  return template;
}

function compile(strings: TemplateStringsArray, lang: SourceLang): Template {
  const text = strings.raw
    .map((part) => part.replace(ESCAPED, (slashes) => "\\".repeat(slashes.length >> 1)))
    .reduce((code, part, i) => `${code}${HOLE}${i - 1}${part}`);
  const root = parseRoot(text, lang);
  const marks: Template["marks"] = new Map();
  const filled = new Array<boolean>(strings.length - 1).fill(false);
  walk(root, {
    enter(node, ctx) {
      if (node.type !== "Identifier" && node.type !== "JSXIdentifier") return;
      const digits = HOLE_NAME.exec(node.name)?.[1];
      if (digits === undefined) return;
      const path = [...ctx.ancestors(), node];
      const typed = node.end - node.start !== node.name.length;
      let at = path.length - 1;
      while (!typed && at > 0 && spansName(path[at - 1]!, node, text)) at--;
      const kind = typed ? "typed" : at === path.length - 1 ? "bare" : "wrapped";
      const hole: Hole = { kind, value: Number(digits), jsx: node.type === "JSXIdentifier" };
      for (let i = 0; i < path.length; i++) {
        if (i === at) marks.set(path[i]!, hole);
        else if (!marks.has(path[i]!)) marks.set(path[i]!, i === path.length - 1 ? NAME : PATH);
      }
      filled[hole.value] = true;
    },
    leave(node) {
      node.start = 0;
      node.end = 0;
      for (const comment of node.comments ?? []) {
        comment.start = 0;
        comment.end = 0;
      }
    },
  });
  const unfilled = filled.indexOf(false);
  if (unfilled >= 0) throw new TypeError(`hole ${unfilled} of a template is in text, not code`);
  return { root, marks };
}

function spansName(node: Node, name: Node, text: string): boolean {
  if (node.start !== name.start) return false;
  return node.end === name.end || SEMICOLON.test(text.slice(name.end, node.end));
}

function parseRoot(text: string, lang: SourceLang): Node {
  const options = { lang, preserveParens: false, attachComments: true };
  let file = parse(text, { ...options, sourceType: "module" });
  if (file.diagnostics.length > 0) {
    const script = parse(text, { ...options, sourceType: "commonjs" });
    if (script.diagnostics.length === 0) file = script;
  }
  const [error] = file.diagnostics;
  if (error !== undefined) throw new SyntaxError(`${error.message} in the template \`${text}\``);
  const { body } = file.program;
  const statement = body.length === 1 ? body[0] : undefined;
  if (statement === undefined) {
    throw new TypeError(`a template is one expression or statement, not ${body.length}`);
  }
  const expression = statement.type === "ExpressionStatement" && text[statement.end - 1] !== ";";
  return expression ? statement.expression : statement;
}

function build(template: Template, values: readonly TemplateValue[]): Node {
  const { marks } = template;
  const state: Build = { marks, values, copies: [], placed: new Set() };
  const result: Record<string, unknown> = {};
  state.copies.push({ source: { root: template.root }, target: result, name: null });
  for (let copy = state.copies.pop(); copy !== undefined; copy = state.copies.pop()) {
    const { source, target } = copy;
    if (Array.isArray(target)) {
      for (const item of source as unknown[]) {
        if (item === null || typeof item !== "object") {
          target.push(item);
          continue;
        }
        const mark = marks.get(item);
        if (typeof mark === "object") target.push(...fill(state, mark, item as Node, true));
        else target.push(copied(state, copy, item, mark));
      }
    } else {
      const fields = source as Record<string, unknown>;
      for (const key in fields) {
        const value = fields[key];
        if (value === null || typeof value !== "object") continue;
        const mark = marks.get(value);
        if (typeof mark === "object") target[key] = fill(state, mark, value as Node, false)[0];
        else target[key] = copied(state, copy, value, mark);
      }
    }
  }
  return result.root as Node;
}

function copied(state: Build, copy: Copy, value: object, mark: number | undefined): unknown {
  if (mark === NAME) return place(state, copy.name!);
  if (mark === PATH || Array.isArray(value)) return copyOf(state, value, copy.name);
  return copyTree(value);
}

function copyOf(state: Build, value: object, name: Node | null): object {
  const target = Array.isArray(value) ? [] : { ...value };
  state.copies.push({ source: value, target, name });
  return target;
}

function fill(state: Build, hole: Hole, carrier: Node, list: boolean): Node[] {
  const value = state.values[hole.value];
  if (Array.isArray(value) && !list) throw new TypeError("a list fills a template hole in a list");
  const items: readonly Item[] = Array.isArray(value) ? value : [value];
  const nodes = items.map((item) => standIn(state, hole, carrier, nodeOf(item, hole.jsx)));
  if (carrier.comments !== undefined && nodes.length > 0) {
    const first = nodes[0]!;
    const last = nodes[nodes.length - 1]!;
    const before = commentsAt(carrier.comments, "before");
    const after = commentsAt(carrier.comments, "after");
    if (before.length > 0) first.comments = [...before, ...(first.comments ?? [])];
    if (after.length > 0) last.comments = [...(last.comments ?? []), ...after];
  }
  return nodes;
}

function standIn(state: Build, hole: Hole, carrier: Node, node: Node): Node {
  if (hole.kind === "bare") return place(state, node);
  if (hole.kind === "typed") {
    if (node.type !== "Identifier") throw new TypeError(`a ${node.type} does not fit a typed name`);
    const renamed = copyOfHole(state, carrier, null) as Identifier;
    renamed.name = node.name;
    return renamed;
  }
  if (node.type === carrier.type) return place(state, node);
  if (carrier.type === "ExpressionStatement" ? is.Expression(node) : NAMES.has(node.type)) {
    return copyOfHole(state, carrier, node);
  }
  if (takesWhole(carrier, node)) return place(state, node);
  throw new TypeError(`a ${node.type} does not fit a ${KINDS[carrier.type] ?? carrier.type}`);
}

function takesWhole(carrier: Node, node: Node): boolean {
  switch (carrier.type) {
    case "ExpressionStatement":
      return is.Statement(node) || is.ModuleDeclaration(node);
    case "TSTypeReference":
      return is.TSType(node);
  }
  return node.type === "RestElement" || !(is.Expression(node) || is.Pattern(node));
}

function copyOfHole(state: Build, carrier: Node, name: Node | null): Node {
  const { comments: _, ...fields } = carrier;
  return copyOf(state, fields, name) as Node;
}

function place(state: Build, node: Node): Node {
  if (state.placed.has(node)) return clone(node);
  state.placed.add(node);
  return node;
}

function commentsAt(comments: readonly Comment[], position: Comment["position"]): Comment[] {
  const found = comments.filter((comment) => comment.position === position);
  return found.map((comment) => ({ ...comment }));
}

function nodeOf(item: Item, jsx: boolean): Node {
  switch (typeof item) {
    case "string": {
      const name = checkedName(item, jsx);
      return jsx ? b.JSXIdentifier({ name }) : b.Identifier({ name });
    }
    case "number":
    case "boolean":
      return b.Literal({ value: item });
    case "bigint":
      return b.Literal({ value: item, bigint: item.toString() });
    case "undefined":
      return b.Identifier({ name: "undefined" });
  }
  if (item === null) return b.Literal({ value: null });
  assertNode(item, "template");
  return item;
}

function checkedName(name: string, jsx: boolean): string {
  const plain = jsx && !name.startsWith("-") ? name.replace(/-/g, "_") : name;
  if (isIdentifierName(plain)) return name;
  throw new TypeError(`a template hole takes a name as a string, not "${name}"`);
}
