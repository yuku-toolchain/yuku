import { parse } from "../file/source-file";
import type {
  Comment,
  Diagnostic,
  FunctionDeclaration,
  Identifier,
  JSXIdentifier,
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
  return build(planOf(strings, "jsx"), values) as T;
}

/** Builds a node from TypeScript, an expression unless the code ends in `;`. */
export function ts<T extends Node = Node>(
  strings: TemplateStringsArray,
  ...values: readonly TemplateValue[]
): T {
  return build(planOf(strings, "tsx"), values) as T;
}

/** How a build makes each object of a template, planned once per call site. */
type Plan = Copy | Fill | Splice | Value | typeof NAME;

/** An object copied with a plan for each of its fields that holds an object. */
interface Copy {
  kind: "copy";
  source: object;
  keys: string[];
  children: Plan[];
}

/** A hole, `"bare"` a lone name, `"wrapped"` a node around it, `"typed"` a name with a type. */
interface Fill {
  kind: "fill";
  shape: "bare" | "wrapped" | "typed";
  value: number;
  jsx: boolean;
  carrier: Node;
  /** The carrier's copy, when the hole fills its name. */
  inner: Copy | null;
}

/** Text in quotes split around its holes, with the raw parts of template text. */
interface Splice {
  kind: "splice";
  node: Node;
  values: number[];
  cooked: string[] | null;
  raw: string[] | null;
}

interface Value {
  kind: "value";
  value: unknown;
}

interface Build {
  values: readonly TemplateValue[];
  placed: Node[];
}

const NAME = { kind: "name" } as const;
const PATH = 1;
const DEPTH_MAX = 1_000;

const HOLE = "__yuku_hole__";
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

const plans = {
  jsx: new WeakMap<TemplateStringsArray, Plan>(),
  tsx: new WeakMap<TemplateStringsArray, Plan>(),
};
const pending: object[] = [];

function planOf(strings: TemplateStringsArray, lang: keyof typeof plans): Plan {
  let plan = plans[lang].get(strings);
  if (plan === undefined) {
    plan = compile(strings, lang);
    plans[lang].set(strings, plan);
  }
  return plan;
}

function compile(strings: TemplateStringsArray, lang: SourceLang): Plan {
  const parts = strings.raw.map((part) =>
    part.replace(ESCAPED, (slashes) => "\\".repeat(slashes.length >> 1)),
  );
  let code = parts[0]!;
  const offsets: number[] = [];
  for (let i = 1; i < parts.length; i++) {
    offsets.push(code.length);
    code += HOLE + parts[i]!;
  }
  const { root, text, shift } = parseRoot(code, lang);
  const holes = new Map(offsets.map((offset, value) => [offset + shift, value]));
  const marks = new Map<object, Fill | Splice | typeof NAME | typeof PATH>();
  const filled = new Array<boolean>(strings.length - 1).fill(false);
  walk(root, {
    enter(node, ctx) {
      const path = [...ctx.ancestors(), node];
      let at = path.length - 1;
      let mark: Fill | Splice;
      if ((node.type === "Identifier" || node.type === "JSXIdentifier") && node.name === HOLE) {
        const value = holes.get(nameAt(node, text));
        if (value === undefined) return;
        const typed = node.end - node.start !== node.name.length;
        while (!typed && at > 0 && spansName(path[at - 1]!, node, text)) at--;
        const shape = typed ? "typed" : at === path.length - 1 ? "bare" : "wrapped";
        const jsx = node.type === "JSXIdentifier";
        mark = { kind: "fill", shape, value, jsx, carrier: path[at]!, inner: null };
        filled[value] = true;
      } else {
        const found = spliceIn(node, ctx.parent, holes);
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
  return planned(root, marks);
}

function parseRoot(code: string, lang: SourceLang): { root: Node; text: string; shift: number } {
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
    const shift = before.length;
    if (statement.type !== "ExpressionStatement" || text[statement.end - 1] === ";") {
      return { root: statement, text, shift };
    }
    const { expression, comments } = statement;
    if (comments !== undefined) expression.comments = [...comments, ...(expression.comments ?? [])];
    return { root: expression, text, shift };
  }
  throw new SyntaxError(`${error!.message} in the template \`${code}\``);
}

function spansName(node: Node, name: Node, text: string): boolean {
  if (node.start !== name.start) return false;
  return node.end === name.end || SEMICOLON.test(text.slice(name.end, node.end));
}

function nameAt(node: Identifier | JSXIdentifier, text: string): number {
  const decorators = node.type === "Identifier" ? (node.decorators ?? []) : [];
  if (decorators.length === 0) return node.start;
  return text.indexOf(HOLE, decorators[decorators.length - 1]!.end);
}

function spliceIn(node: Node, parent: Node | null, holes: Map<number, number>): Splice | null {
  if (node.type === "TemplateElement") return split(node, node.value.cooked, node.value.raw, holes);
  if (node.type !== "Literal" || typeof node.value !== "string") return null;
  if (parent?.type === "ExpressionStatement" && parent.directive != null) return null;
  return split(node, node.value, null, holes);
}

function split(
  node: Node,
  cooked: string | null,
  raw: string | null,
  holes: Map<number, number>,
): Splice | null {
  const values: number[] = [];
  for (const [offset, value] of holes) {
    if (offset >= node.start && offset < node.end) values.push(value);
  }
  const parts = (raw ?? cooked!).split(HOLE);
  if (values.length === 0 || parts.length !== values.length + 1) return null;
  const cookedParts = cooked === null ? null : cooked.split(HOLE);
  return { kind: "splice", node, values, cooked: cookedParts, raw: raw === null ? null : parts };
}

function planned(root: Node, marks: Map<object, Fill | Splice | typeof NAME | typeof PATH>): Plan {
  const copies: { plan: Copy; depth: number }[] = [];
  const copy = (source: object, depth: number): Copy => {
    if (depth > DEPTH_MAX) throw new RangeError(`a template nests deeper than ${DEPTH_MAX} levels`);
    const plan: Copy = { kind: "copy", source, keys: [], children: [] };
    copies.push({ plan, depth });
    return plan;
  };
  const planFor = (value: unknown, depth: number): Plan => {
    if (value === null || typeof value !== "object" || value instanceof RegExp) {
      return { kind: "value", value };
    }
    const mark = marks.get(value);
    if (mark === undefined || mark === PATH) return copy(value, depth);
    if (mark.kind === "fill" && mark.shape !== "bare") {
      const { comments: _, ...fields } = mark.carrier;
      mark.inner = copy(fields, depth);
    }
    return mark;
  };
  const plan = planFor(root, 0);
  for (let next = copies.pop(); next !== undefined; next = copies.pop()) {
    const { source, keys, children } = next.plan;
    if (Array.isArray(source)) {
      for (const item of source) children.push(planFor(item, next.depth + 1));
      continue;
    }
    for (const key in source) {
      const value = (source as Record<string, unknown>)[key];
      if (value === null || typeof value !== "object") continue;
      keys.push(key);
      children.push(planFor(value, next.depth + 1));
    }
  }
  return plan;
}

function build(plan: Plan, values: readonly TemplateValue[]): Node {
  return make({ values, placed: [] }, plan, null) as Node;
}

function make(state: Build, plan: Plan, name: Node | null): unknown {
  switch (plan.kind) {
    case "copy": {
      const { source, keys, children } = plan;
      if (Array.isArray(source)) {
        const list: unknown[] = [];
        for (const child of children) {
          if (child.kind === "fill") fillList(state, child, list);
          else list.push(make(state, child, name));
        }
        return list;
      }
      const target: Record<string, unknown> = { ...source };
      for (let i = 0; i < keys.length; i++) target[keys[i]!] = make(state, children[i]!, name);
      return target;
    }
    case "fill":
      return fillOne(state, plan);
    case "splice":
      return spliced(state, plan);
    case "name":
      return place(state, name!);
    case "value":
      return plan.value instanceof RegExp ? new RegExp(plan.value) : plan.value;
  }
}

function fillOne(state: Build, fill: Fill): Node {
  const value = state.values[fill.value];
  if (Array.isArray(value)) throw new TypeError("a list fills a template hole in a list");
  const node = standIn(state, fill, nodeOf(value as Item, fill.jsx));
  if (fill.carrier.comments !== undefined) moveComments(fill.carrier.comments, node, node);
  return node;
}

function fillList(state: Build, fill: Fill, list: unknown[]): void {
  const value = state.values[fill.value];
  const from = list.length;
  const items = Array.isArray(value) ? (value as Item[]) : [value as Item];
  for (const item of items) list.push(standIn(state, fill, nodeOf(item, fill.jsx)));
  if (fill.carrier.comments !== undefined && list.length > from) {
    moveComments(fill.carrier.comments, list[from] as Node, list[list.length - 1] as Node);
  }
}

function standIn(state: Build, fill: Fill, node: Node): Node {
  const { carrier } = fill;
  if (fill.shape === "bare") return place(state, node);
  if (fill.shape === "typed") {
    if (node.type !== "Identifier") throw new TypeError(`a ${node.type} does not fit a typed name`);
    const renamed = make(state, fill.inner!, null) as Identifier;
    renamed.name = node.name;
    return renamed;
  }
  if (node.type === carrier.type) return place(state, node);
  if (carrier.type === "ExpressionStatement" ? is.Expression(node) : NAMES.has(node.type)) {
    return make(state, fill.inner!, node) as Node;
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

function place(state: Build, node: Node): Node {
  if (state.placed.includes(node)) return copyTree(node);
  state.placed.push(node);
  return node;
}

function spliced(state: Build, splice: Splice): Node {
  const { node } = splice;
  const pieces = splice.values.map((value) => textOf(state.values[value]));
  const cooked = splice.cooked === null ? null : joined(splice.cooked, pieces);
  const comments = node.comments === undefined ? undefined : copyTree(node.comments);
  if (node.type === "TemplateElement") {
    const raw = joined(splice.raw!, pieces.map(templateRaw));
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
