import { parse } from "../file/source-file";
import { fieldsOf, replaceAt, walk, type Slot } from "../traverse/walk";
import type { Expression, Node, Program, ProgramStatement, SourceLang } from "../types";
import { b } from "./builders";
import { CHILD_KEYS } from "./generated";
import { is } from "./is";
import { isIdentifierName } from "./lexical";
import { clone } from "./utils";

type Primitive = string | number | boolean | bigint | null | undefined;
type Item = Node | Primitive;

/** A node, a list for a list, a string for a name, or a literal value. */
export type TemplateValue = Item | readonly Item[];

interface Step {
  key: string;
  index: number | null;
}

interface Hole {
  value: number;
  name: string;
  path: Step[];
}

interface Template {
  program: Program;
  expression: boolean;
  holes: Hole[];
}

const HOLE = "__yuku_hole_";
const HOLE_PATTERN = /^__yuku_hole_(\d+)$/;
const KINDS: Record<string, string> = { ExpressionStatement: "statement", TSTypeReference: "type" };
const NAMES: ReadonlySet<string> = new Set([
  "Identifier",
  "JSXIdentifier",
  "JSXMemberExpression",
  "JSXNamespacedName",
  "PrivateIdentifier",
  "TSQualifiedName",
]);

const templates = {
  jsx: new WeakMap<TemplateStringsArray, Template>(),
  tsx: new WeakMap<TemplateStringsArray, Template>(),
};

/** Builds a node from JavaScript, an expression unless the code ends in `;`. */
export function js<T extends Node = Node>(
  strings: TemplateStringsArray,
  ...values: readonly TemplateValue[]
): T {
  return build("jsx", strings, values) as T;
}

/** Builds a node from TypeScript, an expression unless the code ends in `;`. */
export function ts<T extends Node = Node>(
  strings: TemplateStringsArray,
  ...values: readonly TemplateValue[]
): T {
  return build("tsx", strings, values) as T;
}

/** Each node a hole stands for: its name, or the largest node around it holding nothing else. */
export function holesIn(root: Node, names: RegExp, source: string): Map<Node, string> {
  const holes = new Map<Node, string>();
  walk(root, {
    enter(node, ctx) {
      if (node.type !== "Identifier" && node.type !== "JSXIdentifier") return;
      const name = names.exec(node.name)?.[1];
      if (name === undefined) return;
      let carrier: Node = node;
      const ancestors = ctx.ancestors();
      for (let i = ancestors.length - 1; i >= 0; i--) {
        const ancestor = ancestors[i]!;
        const end = source[ancestor.end - 1] === ";" ? ancestor.end - 1 : ancestor.end;
        if (ancestor.start !== node.start || end !== node.end) break;
        carrier = ancestor;
      }
      holes.set(carrier, name);
    },
  });
  return holes;
}

export function parseFragment(
  text: string,
  lang: SourceLang,
  what: string,
): { statement: ProgramStatement; expression: Expression | null } {
  const options = { lang, preserveParens: false, attachComments: true };
  let file = parse(text, { ...options, sourceType: "module" });
  if (file.diagnostics.length > 0) {
    const body = parse(text, { ...options, sourceType: "commonjs" });
    if (body.diagnostics.length === 0) file = body;
  }
  const [error] = file.diagnostics;
  if (error !== undefined) throw new SyntaxError(`${error.message} in the ${what} \`${text}\``);
  const { body } = file.program;
  const statement = body.length === 1 ? body[0] : undefined;
  if (statement === undefined) {
    throw new TypeError(`a ${what} is one expression or statement, not ${body.length}`);
  }
  const isExpression =
    statement.type === "ExpressionStatement" && text[statement.end - 1] !== ";";
  return { statement, expression: isExpression ? statement.expression : null };
}

function build(
  lang: keyof typeof templates,
  strings: TemplateStringsArray,
  values: readonly TemplateValue[],
): Node {
  const template = templates[lang].get(strings) ?? compile(lang, strings);
  const program = clone(template.program);
  const { holes } = template;
  // backwards, so filling a list never moves a hole still to fill
  for (let h = holes.length - 1; h >= 0; h--) {
    const { value, name, path } = holes[h]!;
    fill(slotAt(program, path), name, values[value]);
  }
  const [first, ...rest] = program.body;
  if (first === undefined || rest.length > 0) {
    throw new TypeError("a template builds one node, so a list cannot fill its statement");
  }
  return template.expression && first.type === "ExpressionStatement" ? first.expression : first;
}

function compile(lang: keyof typeof templates, strings: TemplateStringsArray): Template {
  const text = strings.raw
    .map((part) => part.replace(/\\+(?=`|\$\{)/g, (slashes) => "\\".repeat(slashes.length >> 1)))
    .reduce((joined, part, i) => `${joined}${HOLE}${i - 1}${part}`);
  const { statement, expression } = parseFragment(text, lang, "template");
  const carriers = holesIn(expression ?? statement, HOLE_PATTERN, text);
  const program = b.Program({ sourceType: "module", body: [statement] });
  walk(program, {
    enter(node) {
      node.start = 0;
      node.end = 0;
      for (const comment of node.comments ?? []) {
        comment.start = 0;
        comment.end = 0;
      }
    },
  });
  const holes = holesOf(program, carriers);
  for (let i = 0; i < strings.length - 1; i++) {
    if (!holes.some((hole) => hole.value === i)) {
      throw new TypeError(`hole ${i} of a template is in text, not code`);
    }
  }
  const template = { program, expression: expression !== null, holes };
  templates[lang].set(strings, template);
  return template;
}

function holesOf(program: Program, carriers: Map<Node, string>): Hole[] {
  const holes: Hole[] = [];
  const pending: [Node, Step[]][] = [[program, []]];
  for (let item = pending.pop(); item !== undefined; item = pending.pop()) {
    const [node, path] = item;
    const value = carriers.get(node);
    if (value !== undefined) {
      holes.push({ value: Number(value), name: `${HOLE}${value}`, path });
      continue;
    }
    const fields = fieldsOf(node);
    const keys = CHILD_KEYS[node.type] ?? [];
    for (let k = keys.length - 1; k >= 0; k--) {
      const key = keys[k]!;
      const value = fields[key];
      if (Array.isArray(value)) {
        for (let i = value.length - 1; i >= 0; i--) {
          if (value[i] !== null) pending.push([value[i] as Node, [...path, { key, index: i }]]);
        }
      } else if (value !== null && typeof value === "object") {
        pending.push([value as Node, [...path, { key, index: null }]]);
      }
    }
  }
  return holes;
}

function slotAt(program: Program, path: readonly Step[]): Slot {
  let parent: Node = program;
  for (let i = 0; i < path.length - 1; i++) {
    const { key, index } = path[i]!;
    const value = fieldsOf(parent)[key];
    parent = (index === null ? value : (value as Node[])[index]) as Node;
  }
  const { key, index } = path[path.length - 1]!;
  const list = index === null ? null : (fieldsOf(parent)[key] as Node[]);
  return { parent, key, list, index: index ?? 0 };
}

function fill(slot: Slot, name: string, value: TemplateValue): void {
  const fields = fieldsOf(slot.parent);
  const carrier = (slot.list === null ? fields[slot.key] : slot.list[slot.index]) as Node;
  if (!isList(value)) {
    replaceAt(slot, [standIn(carrier, name, value)]);
  } else if (slot.list === null) {
    throw new TypeError("a list fills a template hole in a list");
  } else if (value.length === 0) {
    slot.list.splice(slot.index, 1);
  } else {
    replaceAt(slot, value.map((item) => standIn(carrier, name, item)));
  }
}

function standIn(carrier: Node, name: string, item: Item): Node {
  const hole = isNamed(carrier, name) ? carrier : nameIn(carrier, name);
  const node = nodeOf(item, hole.type === "JSXIdentifier");
  if (carrier === hole || takesWhole(carrier, node)) return node;
  if (carrier.type === "ExpressionStatement" ? is.Expression(node) : NAMES.has(node.type)) {
    return named(clone(carrier), name, node);
  }
  const kind = KINDS[carrier.type] ?? carrier.type;
  throw new TypeError(`a ${node.type} does not fit a ${kind}`);
}

function takesWhole(carrier: Node, node: Node): boolean {
  if (node.type === carrier.type || node.type === "RestElement") return true;
  if (carrier.type === "ExpressionStatement") {
    return is.Statement(node) || is.ModuleDeclaration(node);
  }
  if (carrier.type === "TSTypeReference") return is.TSType(node);
  return !is.Expression(node) && !is.Pattern(node) && !NAMES.has(node.type);
}

function nameIn(carrier: Node, name: string): Node {
  let found: Node | null = null;
  walk(carrier, {
    enter(node, ctx) {
      if (isNamed(node, name)) {
        found = node;
        ctx.stop();
      }
    },
  });
  return found!;
}

function named(carrier: Node, name: string, node: Node): Node {
  let used = false;
  walk(carrier, {
    enter(current, ctx) {
      if (isNamed(current, name)) {
        ctx.replace(used ? clone(node) : node);
        ctx.skip();
        used = true;
      }
    },
  });
  return carrier;
}

function isNamed(node: Node, name: string): boolean {
  return (node.type === "Identifier" || node.type === "JSXIdentifier") && node.name === name;
}

function nodeOf(item: Item, jsx: boolean): Node {
  switch (typeof item) {
    case "string": {
      const name = checkedName(item);
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
  return item ?? b.Literal({ value: null });
}

function checkedName(name: string): string {
  if (!isIdentifierName(name)) {
    throw new TypeError(`a template hole takes a name as a string, not "${name}"`);
  }
  return name;
}

function isList(value: TemplateValue): value is readonly Item[] {
  return Array.isArray(value);
}
