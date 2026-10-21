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

type Carrier = "statement" | "property" | "type" | "node" | "jsx";

interface Step {
  key: string;
  index: number | null;
}

interface Hole {
  value: number;
  carrier: Carrier;
  path: Step[];
}

interface Template {
  program: Program;
  expression: boolean;
  holes: Hole[];
}

const HOLE = "__yuku_hole_";
const HOLE_PATTERN = /^__yuku_hole_(\d+)$/;

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
    const hole = holes[h]!;
    fill(slotAt(program, hole.path), CONVERTERS[hole.carrier], values[hole.value]);
  }
  const [first, ...rest] = program.body;
  if (first === undefined || rest.length > 0) {
    throw new TypeError("a template builds one node, so a list cannot fill its statement");
  }
  return template.expression && first.type === "ExpressionStatement" ? first.expression : first;
}

function compile(lang: keyof typeof templates, strings: TemplateStringsArray): Template {
  const text = strings.raw.reduce((joined, part, i) => `${joined}${HOLE}${i - 1}${part}`);
  const { statement, expression } = parseFragment(text, lang, "template");
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
  const holes = holesOf(program, expression !== null);
  for (let i = 0; i < strings.length - 1; i++) {
    if (!holes.some((hole) => hole.value === i)) {
      throw new TypeError(`hole ${i} of a template is in text, not code`);
    }
  }
  const template = { program, expression: expression !== null, holes };
  templates[lang].set(strings, template);
  return template;
}

function holesOf(program: Program, expression: boolean): Hole[] {
  const holes: Hole[] = [];
  const root = program.body[0];
  const pending: [Node, Step[]][] = [[program, []]];
  for (let item = pending.pop(); item !== undefined; item = pending.pop()) {
    const [node, path] = item;
    const hole = carrierOf(node, expression && node === root);
    if (hole !== null) {
      holes.push({ ...hole, path });
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

function carrierOf(node: Node, isRoot: boolean): { value: number; carrier: Carrier } | null {
  let value: number | null = null;
  let carrier: Carrier = "node";
  switch (node.type) {
    case "ExpressionStatement":
      if (!isRoot) value = holeOf(node.expression);
      carrier = "statement";
      break;
    case "Property":
      if (node.shorthand) value = holeOf(node.value);
      carrier = "property";
      break;
    case "TSTypeReference":
      if (node.typeArguments === null) value = holeOf(node.typeName);
      carrier = "type";
      break;
    case "Identifier":
      value = holeOf(node);
      break;
    case "JSXIdentifier":
      value = holeOf(node);
      carrier = "jsx";
      break;
  }
  return value === null ? null : { value, carrier };
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

function fill(slot: Slot, convert: (item: Item) => Node, value: TemplateValue): void {
  if (!isList(value)) {
    replaceAt(slot, [convert(value)]);
  } else if (slot.list === null) {
    throw new TypeError("a list fills a template hole in a list");
  } else if (value.length === 0) {
    slot.list.splice(slot.index, 1);
  } else {
    replaceAt(slot, value.map(convert));
  }
}

function holeOf(node: Node): number | null {
  if (node.type !== "Identifier" && node.type !== "JSXIdentifier") return null;
  const match = HOLE_PATTERN.exec(node.name);
  return match === null ? null : Number(match[1]);
}

const CONVERTERS: Record<Carrier, (item: Item) => Node> = {
  statement: statementOf,
  property: propertyOf,
  type: typeOf,
  node: nodeOf,
  jsx: jsxOf,
};

function nodeOf(item: Item): Node {
  switch (typeof item) {
    case "string":
      return b.Identifier({ name: checkedName(item) });
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

function jsxOf(item: Item): Node {
  return typeof item === "string" ? b.JSXIdentifier({ name: checkedName(item) }) : nodeOf(item);
}

function statementOf(item: Item): Node {
  const node = nodeOf(item);
  return is.Expression(node) ? b.ExpressionStatement({ expression: node }) : node;
}

function propertyOf(item: Item): Node {
  const node = nodeOf(item);
  if (node.type === "Identifier") {
    return b.Property({ kind: "init", key: clone(node), value: node, shorthand: true });
  }
  if (node.type === "Property" || node.type === "SpreadElement" || node.type === "RestElement") {
    return node;
  }
  throw new TypeError(`a ${node.type} does not fit a shorthand property`);
}

function typeOf(item: Item): Node {
  const node = nodeOf(item);
  if (is.TSType(node)) return node;
  if (node.type === "Identifier" || node.type === "TSQualifiedName") {
    return b.TSTypeReference({ typeName: node });
  }
  throw new TypeError(`a ${node.type} does not fit a type`);
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
