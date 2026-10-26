import { parse } from "../file/source-file";
import type {
  Comment,
  Diagnostic,
  FunctionDeclaration,
  Identifier,
  Node,
  SourceLang,
  SourceType,
  StringLiteral,
  TemplateElement,
} from "../types";
import { is, isIdentifierName } from "./utils";
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
  marks: Map<object, Mark>;
}

type Mark = Hole | Text | typeof NAME | typeof PATH;

/** `"bare"` is a lone name, `"wrapped"` a node around it, `"typed"` a name with a type. */
interface Hole {
  kind: "bare" | "wrapped" | "typed";
  value: number;
  jsx: boolean;
}

/** Text in quotes split around its holes, with the raw parts of template text. */
interface Text {
  kind: "text";
  values: number[];
  cooked: string[] | null;
  raw: string[] | null;
}

interface Build {
  marks: Template["marks"];
  values: readonly TemplateValue[];
  /** The copies still to fill, as their source, target, and the node their names become. */
  stack: unknown[];
  placed: Node[];
}

const NAME = 0;
const PATH = 1;

const HOLE = "__yuku_hole_";
const HOLE_NAME = /^__yuku_hole_(\d+)$/;
const HOLE_IN_TEXT = /__yuku_hole_(\d+)/;
const ESCAPED = /\\+(?=`|\$\{)/g;
const TEMPLATE_RAW = /\\|`|\$\{|\r/g;
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
const CONTEXTS: readonly [before: string, after: string, sourceType: SourceType][] = [
  ["async function* _() {\n", "\n}", "module"],
  ["", "", "module"],
  ["", "", "commonjs"],
];

const templates = {
  jsx: new WeakMap<TemplateStringsArray, Template>(),
  tsx: new WeakMap<TemplateStringsArray, Template>(),
};
const pending: object[] = [];

function templateOf(strings: TemplateStringsArray, lang: keyof typeof templates): Template {
  let template = templates[lang].get(strings);
  if (template === undefined) {
    template = compile(strings, lang);
    templates[lang].set(strings, template);
  }
  return template;
}

function compile(strings: TemplateStringsArray, lang: SourceLang): Template {
  const code = strings.raw
    .map((part) => part.replace(ESCAPED, (slashes) => "\\".repeat(slashes.length >> 1)))
    .reduce((joined, part, i) => `${joined}${HOLE}${i - 1}${part}`);
  const { root, text } = parseRoot(code, lang);
  const marks: Template["marks"] = new Map();
  const filled = new Array<boolean>(strings.length - 1).fill(false);
  walk(root, {
    enter(node, ctx) {
      const path = [...ctx.ancestors(), node];
      let at = path.length - 1;
      let mark: Hole | Text;
      if (node.type === "Identifier" || node.type === "JSXIdentifier") {
        const digits = HOLE_NAME.exec(node.name)?.[1];
        if (digits === undefined) return;
        const typed = node.end - node.start !== node.name.length;
        while (!typed && at > 0 && spansName(path[at - 1]!, node, text)) at--;
        const kind = typed ? "typed" : at === path.length - 1 ? "bare" : "wrapped";
        mark = { kind, value: Number(digits), jsx: node.type === "JSXIdentifier" };
        filled[mark.value] = true;
      } else {
        const found = textIn(node, ctx.parent);
        if (found === null) return;
        mark = found;
        for (const value of found.values) filled[value] = true;
      }
      for (let i = 0; i < path.length; i++) {
        if (i === at) marks.set(path[i]!, mark);
        else if (!marks.has(path[i]!)) marks.set(path[i]!, i === path.length - 1 ? NAME : PATH);
      }
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

function parseRoot(code: string, lang: SourceLang): { root: Node; text: string } {
  let error: Diagnostic | undefined;
  for (const [before, after, sourceType] of CONTEXTS) {
    const text = `${before}${code}${after}`;
    const file = parse(text, { lang, sourceType, preserveParens: false, attachComments: true });
    const { body } = file.program;
    if (file.diagnostics.length > 0 || (before !== "" && body.length !== 1)) {
      if (before === "") error ??= file.diagnostics[0];
      continue;
    }
    const statements = before === "" ? body : (body[0] as FunctionDeclaration).body!.body;
    const statement = statements.length === 1 ? statements[0] : undefined;
    if (statement === undefined) {
      throw new TypeError(`a template is one expression or statement, not ${statements.length}`);
    }
    if (statement.type !== "ExpressionStatement" || text[statement.end - 1] === ";") {
      return { root: statement, text };
    }
    const { expression, comments } = statement;
    if (comments !== undefined) expression.comments = [...comments, ...(expression.comments ?? [])];
    return { root: expression, text };
  }
  throw new SyntaxError(`${error!.message} in the template \`${code}\``);
}

function spansName(node: Node, name: Node, text: string): boolean {
  if (node.start !== name.start) return false;
  return node.end === name.end || SEMICOLON.test(text.slice(name.end, node.end));
}

function textIn(node: Node, parent: Node | null): Text | null {
  if (node.type === "TemplateElement") return split(node.value.cooked, node.value.raw);
  if (node.type !== "Literal" || typeof node.value !== "string") return null;
  if (parent?.type === "ExpressionStatement" && parent.directive != null) return null;
  return split(node.value, null);
}

function split(cooked: string | null, raw: string | null): Text | null {
  const source = raw ?? cooked!;
  if (!source.includes(HOLE)) return null;
  const parts = source.split(HOLE_IN_TEXT);
  const values = parts.filter((_, i) => i % 2 === 1).map(Number);
  const between = (text: string) => text.split(HOLE_IN_TEXT).filter((_, i) => i % 2 === 0);
  return {
    kind: "text",
    values,
    cooked: cooked === null ? null : between(cooked),
    raw: raw === null ? null : between(raw),
  };
}

function build(template: Template, values: readonly TemplateValue[]): Node {
  const { root, marks } = template;
  if (marks.size === 0) return copyTree(root);
  const state: Build = { marks, values, stack: [], placed: [] };
  const { stack } = state;
  const result = copyMarked(state, root, marks.get(root), null);
  while (stack.length > 0) {
    const name = stack.pop() as Node | null;
    const target = stack.pop() as Record<string, unknown> | unknown[];
    const source = stack.pop() as Record<string, unknown> | unknown[];
    if (Array.isArray(target)) {
      for (const item of source as unknown[]) {
        if (item === null || typeof item !== "object") {
          target.push(item);
          continue;
        }
        const mark = marks.get(item);
        if (isHole(mark)) fillList(state, mark, item as Node, target);
        else target.push(copyMarked(state, item, mark, name));
      }
    } else {
      for (const key in source) {
        const value = (source as Record<string, unknown>)[key];
        if (value === null || typeof value !== "object") continue;
        target[key] = copyMarked(state, value, marks.get(value), name);
      }
    }
  }
  return result as Node;
}

function isHole(mark: Mark | undefined): mark is Hole {
  return typeof mark === "object" && mark.kind !== "text";
}

function copyMarked(
  state: Build,
  value: object,
  mark: Mark | undefined,
  name: Node | null,
): unknown {
  if (mark === undefined) return Array.isArray(value) ? later(state, value, name) : copyTree(value);
  if (mark === PATH) return later(state, value, name);
  if (mark === NAME) return place(state, name!);
  if (mark.kind === "text") return spliced(state, value as Node, mark);
  return fillOne(state, mark, value as Node);
}

function later(state: Build, value: object, name: Node | null): object {
  const target = Array.isArray(value) ? [] : { ...value };
  state.stack.push(value, target, name);
  return target;
}

function fillOne(state: Build, hole: Hole, carrier: Node): Node {
  const value = state.values[hole.value];
  if (Array.isArray(value)) throw new TypeError("a list fills a template hole in a list");
  const node = standIn(state, hole, carrier, nodeOf(value as Item, hole.jsx));
  if (carrier.comments !== undefined) moveComments(carrier.comments, node, node);
  return node;
}

function fillList(state: Build, hole: Hole, carrier: Node, list: unknown[]): void {
  const value = state.values[hole.value];
  const from = list.length;
  const items = Array.isArray(value) ? (value as Item[]) : [value as Item];
  for (const item of items) list.push(standIn(state, hole, carrier, nodeOf(item, hole.jsx)));
  if (carrier.comments !== undefined && list.length > from) {
    moveComments(carrier.comments, list[from] as Node, list[list.length - 1] as Node);
  }
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
  return later(state, fields, name) as Node;
}

function place(state: Build, node: Node): Node {
  if (state.placed.includes(node)) return copyTree(node);
  state.placed.push(node);
  return node;
}

function spliced(state: Build, node: Node, text: Text): Node {
  const pieces = text.values.map((value) => textOf(state.values[value]));
  const cooked = text.cooked === null ? null : joined(text.cooked, pieces);
  const comments = node.comments === undefined ? undefined : copyTree(node.comments);
  if (node.type === "TemplateElement") {
    const raw = joined(text.raw!, pieces.map(templateRaw));
    const element: TemplateElement = { ...node, value: { raw, cooked } };
    if (comments !== undefined) element.comments = comments;
    return element;
  }
  const literal: StringLiteral = { type: "Literal", start: 0, end: 0, value: cooked! };
  if (comments !== undefined) literal.comments = comments;
  return literal;
}

function joined(parts: readonly string[], pieces: readonly string[]): string {
  let text = parts[0]!;
  for (let i = 0; i < pieces.length; i++) text += pieces[i]! + parts[i + 1]!;
  return text;
}

function textOf(value: TemplateValue): string {
  if (typeof value === "string" || typeof value === "number" || typeof value === "bigint") {
    return String(value);
  }
  throw new TypeError("a hole inside quotes takes a string or a number");
}

function templateRaw(text: string): string {
  return text.replace(TEMPLATE_RAW, (match) => (match === "\r" ? "\\r" : `\\${match}`));
}

function moveComments(comments: readonly Comment[], first: Node, last: Node): void {
  const before = commentsAt(comments, "before");
  const after = commentsAt(comments, "after");
  if (before.length > 0) first.comments = [...before, ...(first.comments ?? [])];
  if (after.length > 0) last.comments = [...(last.comments ?? []), ...after];
}

function commentsAt(comments: readonly Comment[], position: Comment["position"]): Comment[] {
  const found = comments.filter((comment) => comment.position === position);
  return found.map((comment) => ({ ...comment }));
}

function nodeOf(item: Item, jsx: boolean): Node {
  switch (typeof item) {
    case "string": {
      const name = checkedName(item, jsx);
      if (jsx) return { type: "JSXIdentifier", start: 0, end: 0, name };
      return { type: "Identifier", start: 0, end: 0, name };
    }
    case "number":
    case "boolean":
      return { type: "Literal", start: 0, end: 0, value: item };
    case "bigint":
      return { type: "Literal", start: 0, end: 0, value: item, bigint: item.toString() };
    case "undefined":
      return { type: "Identifier", start: 0, end: 0, name: "undefined" };
  }
  if (item === null) return { type: "Literal", start: 0, end: 0, value: null };
  assertNode(item, "template");
  return item;
}

function checkedName(name: string, jsx: boolean): string {
  const plain = jsx && !name.startsWith("-") ? name.replace(/-/g, "_") : name;
  if (isIdentifierName(plain)) return name;
  throw new TypeError(`a template hole takes a name as a string, not "${name}"`);
}

function copyTree<T extends object>(value: T): T {
  const root = shallowCopy(value);
  for (let item = pending.pop(); item !== undefined; item = pending.pop()) {
    if (Array.isArray(item)) {
      for (let i = 0; i < item.length; i++) {
        const child: unknown = item[i];
        if (child !== null && typeof child === "object") item[i] = shallowCopy(child);
      }
    } else {
      const fields = item as Record<string, unknown>;
      for (const key in fields) {
        const child = fields[key];
        if (child !== null && typeof child === "object") fields[key] = shallowCopy(child);
      }
    }
  }
  return root;
}

function shallowCopy<T extends object>(value: T): T {
  if (value instanceof RegExp) return new RegExp(value) as T;
  const copy = (Array.isArray(value) ? value.slice() : { ...value }) as T;
  pending.push(copy);
  return copy;
}
