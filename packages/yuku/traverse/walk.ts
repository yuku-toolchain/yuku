import { ALIAS_GROUPS, type AliasMap, type AliasName } from "../ast/aliases";
import { CHILD_KEYS, CHILD_VISITORS } from "../ast/generated";
import { widened, type ChildVisitor } from "../core/child-keys";
import type { Comment, Node, NodeOfType, NodeType } from "../types";

export type WalkHandler<T extends Node = Node> = (node: T, ctx: WalkContext) => void;

export interface WalkHooks<T extends Node = Node> {
  enter?: WalkHandler<T>;
  leave?: WalkHandler<T>;
}

type Visitor<T extends Node> = WalkHandler<T> | WalkHooks<T>;

/** Handlers by node type or alias. Enters run universal, alias, then typed, and leaves reverse. */
export type Visitors = {
  [K in NodeType]?: Visitor<NodeOfType<K>>;
} & {
  [A in AliasName]?: Visitor<AliasMap[A]>;
} & {
  enter?: WalkHandler;
  leave?: WalkHandler;
};

/** @internal */
export type Handler = (node: Node, ctx: WalkContext) => void;

/** @internal */
export interface Handlers {
  enter: Handler[];
  leave: Handler[];
  children: ChildVisitor | null;
}

/** @internal */
export interface Frame {
  node: Node;
  key: string | null;
  list: Node[] | null;
  index: number;
  phase: number;
  handlers: Handlers;
  keys: readonly string[];
  childKey: number;
  childIndex: number;
  replacements: number;
}

export interface Slot {
  readonly parent: Node;
  readonly key: string;
  readonly list: Node[] | null;
  readonly index: number;
}

type Fields = Record<string, unknown>;

const ENTER = 0;
const CHILDREN = 1;
const LEAVE = 2;

const SIGNAL_NONE = 0;
const SIGNAL_STOP = 1;
const SIGNAL_REMOVE = 2;
const SIGNAL_REPLACE = 3;

const REPLACEMENTS_MAX = 64;
const RECURSION_MAX = 1_000;

const STOPPED = -1;
const REVISIT = -2;

const NO_HANDLERS: Handlers = { enter: [], leave: [], children: null };
const NO_KEYS: readonly string[] = [];
const NO_NODES: Node[] = [];

/** Walks a node depth-first in source order, every visitor of a list in one pass. */
export function walk(node: Node, visitors: Visitors | readonly Visitors[]): void {
  assertNode(node, "walk");
  visit(new WalkContext(new Dispatch([visitors].flat())), node, null, null, 0);
}

/** The walk's position and edits, valid only during the visit that receives it. */
export class WalkContext {
  /** @internal */
  _frames: Frame[] = [];
  /** @internal */
  _depth = 0;
  /** @internal */
  _skipped = false;
  /** @internal */
  _signal: number = SIGNAL_NONE;
  /** @internal */
  _replacement: Node[] = NO_NODES;
  /** @internal */
  _dispatch: Dispatch;

  /** @internal */
  constructor(dispatch: Dispatch) {
    this._dispatch = dispatch;
  }

  get parent(): Node | null {
    return this._depth > 1 ? this._frames[this._depth - 2]!.node : null;
  }

  get key(): string | null {
    return this.frame.key;
  }

  /** The index in an array field, null in a plain field. */
  get index(): number | null {
    return this.frame.list === null ? null : this.frame.index;
  }

  /** From the root down to the parent. */
  ancestors(): Node[] {
    return this._frames.slice(0, this._depth - 1).map((frame) => frame.node);
  }

  /** Skips the children. `leave` still runs. */
  skip(): void {
    this._skipped = true;
  }

  stop(): void {
    this._signal = SIGNAL_STOP;
  }

  /** The replacement takes the comments and is visited, unless {@link skip} is called. */
  replace(replacement: Node | readonly Node[]): void {
    const nodes = [replacement].flat();
    for (const node of nodes) assertNode(node, "replace");
    if (this._depth === 1) throw new TypeError("replace: the walk root has no place to fill");
    if (nodes.length === 0) throw new TypeError("replace: expected a node, or call remove");
    if (nodes.length > 1 && this.frame.list === null) {
      throw new TypeError("replace: several nodes need an array field");
    }
    this._replacement = nodes;
    this._signal = SIGNAL_REPLACE;
  }

  remove(): void {
    if (this._depth === 1) throw new TypeError("remove: the walk root has no place to leave");
    this._signal = SIGNAL_REMOVE;
  }

  /** Inserts a sibling the walk passes over. */
  insertBefore(sibling: Node): void {
    const frame = this.listFrame("insertBefore", sibling);
    frame.list!.splice(frame.index, 0, sibling);
    frame.index++;
  }

  /** Inserts a sibling the walk visits next. */
  insertAfter(sibling: Node): void {
    const frame = this.listFrame("insertAfter", sibling);
    frame.list!.splice(frame.index + 1, 0, sibling);
  }

  private get frame(): Frame {
    return this._frames[this._depth - 1]!;
  }

  private listFrame(operation: string, sibling: Node): Frame {
    assertNode(sibling, operation);
    const frame = this.frame;
    if (frame.list === null) throw new TypeError(`${operation}: the node is not in an array field`);
    return frame;
  }
}

function visit(
  ctx: WalkContext,
  node: Node,
  key: string | null,
  list: Node[] | null,
  index: number,
): number {
  if (ctx._depth >= RECURSION_MAX) return run(ctx, node, key, list, index);
  const frame = open(ctx, node, key, list, index);
  let count: number;
  for (;;) {
    count = 1;
    const handlers = ctx._dispatch.handlersOf(frame.node.type);
    if (call(handlers.enter, frame.node, ctx)) {
      count = settle(ctx, frame);
      if (count === REVISIT) continue;
      break;
    }
    if (ctx._skipped) ctx._skipped = false;
    else if (handlers.children !== null) {
      if (!handlers.children(fieldsOf(frame.node), visitValue, ctx)) return STOPPED;
    }
    if (call(handlers.leave, frame.node, ctx)) {
      count = settle(ctx, frame);
      if (count === REVISIT) continue;
    }
    break;
  }
  ctx._depth--;
  return count === STOPPED ? STOPPED : frame.index + count;
}

function visitValue(ctx: WalkContext, value: unknown, key: string): boolean {
  if (value === null || typeof value !== "object") return true;
  if (!Array.isArray(value)) return visit(ctx, value as Node, key, null, 0) !== STOPPED;
  for (let i = 0; i < value.length; ) {
    const item: unknown = value[i];
    if (item === null || typeof item !== "object") {
      i++;
      continue;
    }
    i = visit(ctx, item as Node, key, value as Node[], i);
    if (i === STOPPED) return false;
  }
  return true;
}

function run(
  ctx: WalkContext,
  root: Node,
  key: string | null,
  list: Node[] | null,
  index: number,
): number {
  const frames = ctx._frames;
  const base = ctx._depth;
  open(ctx, root, key, list, index);
  for (;;) {
    const frame = frames[ctx._depth - 1]!;
    const entering = frame.phase === ENTER;
    let handlers: Handler[];
    if (entering) {
      frame.handlers = ctx._dispatch.handlersOf(frame.node.type);
      handlers = frame.handlers.enter;
    } else {
      if (frame.phase === CHILDREN) {
        if (openChild(ctx, frame)) continue;
        frame.phase = LEAVE;
      }
      handlers = frame.handlers.leave;
    }
    let count = 1;
    if (call(handlers, frame.node, ctx)) {
      count = settle(ctx, frame);
      if (count === STOPPED) return STOPPED;
      if (count === REVISIT) {
        frame.phase = ENTER;
        continue;
      }
    } else if (entering) {
      frame.phase = ctx._skipped ? LEAVE : CHILDREN;
      frame.keys = CHILD_KEYS[frame.node.type] ?? NO_KEYS;
      frame.childKey = 0;
      frame.childIndex = 0;
      ctx._skipped = false;
      continue;
    }
    ctx._depth--;
    if (ctx._depth === base) return frame.index + count;
    if (frame.list !== null) frames[ctx._depth - 1]!.childIndex = frame.index + count;
  }
}

function call(handlers: Handler[], node: Node, ctx: WalkContext): boolean {
  for (let i = 0; i < handlers.length; i++) {
    handlers[i]!(node, ctx);
    if (ctx._signal !== SIGNAL_NONE) return true;
  }
  return false;
}

function settle(ctx: WalkContext, frame: Frame): number {
  const signal = ctx._signal;
  if (signal === SIGNAL_STOP) return STOPPED;
  ctx._signal = SIGNAL_NONE;
  const slot = frameSlot(ctx._frames, ctx._depth);
  if (signal === SIGNAL_REMOVE) {
    ctx._skipped = false;
    removeAt(slot);
    return 0;
  }
  const nodes = ctx._replacement;
  ctx._replacement = NO_NODES;
  replaceAt(slot, nodes);
  if (++frame.replacements > REPLACEMENTS_MAX) {
    throw new Error(`walk: a node was replaced ${REPLACEMENTS_MAX} times in one place`);
  }
  frame.node = nodes[0]!;
  if (!ctx._skipped) return REVISIT;
  ctx._skipped = false;
  return nodes.length;
}

function open(
  ctx: WalkContext,
  node: Node,
  key: string | null,
  list: Node[] | null,
  index: number,
): Frame {
  let frame = ctx._frames[ctx._depth];
  if (frame === undefined) {
    frame = {
      node,
      key,
      list,
      index,
      phase: ENTER,
      handlers: NO_HANDLERS,
      keys: NO_KEYS,
      childKey: 0,
      childIndex: 0,
      replacements: 0,
    };
    ctx._frames.push(frame);
  } else {
    frame.node = node;
    frame.key = key;
    frame.list = list;
    frame.index = index;
    frame.phase = ENTER;
    frame.replacements = 0;
  }
  ctx._depth++;
  return frame;
}

function openChild(ctx: WalkContext, frame: Frame): boolean {
  const keys = frame.keys;
  const node = fieldsOf(frame.node);
  for (; frame.childKey < keys.length; frame.childKey++, frame.childIndex = 0) {
    const key = keys[frame.childKey]!;
    const value = node[key];
    if (value === null || typeof value !== "object") continue;
    if (Array.isArray(value)) {
      for (; frame.childIndex < value.length; frame.childIndex++) {
        const item: unknown = value[frame.childIndex];
        if (item === null || typeof item !== "object") continue;
        open(ctx, item as Node, key, value as Node[], frame.childIndex);
        return true;
      }
    } else if (frame.childIndex === 0) {
      frame.childIndex = 1;
      open(ctx, value as Node, key, null, 0);
      return true;
    }
  }
  return false;
}

function frameSlot(frames: Frame[], depth: number): Slot {
  const frame = frames[depth - 1]!;
  return { parent: frames[depth - 2]!.node, key: frame.key!, list: frame.list, index: frame.index };
}

interface CompiledVisitor {
  enter: Handler | null;
  leave: Handler | null;
  aliases: { members: ReadonlySet<string>; hooks: WalkHooks }[];
  typed: Record<string, WalkHooks | undefined>;
}

const ALIAS_MEMBERS: Record<string, ReadonlySet<string> | undefined> = Object.fromEntries(
  Object.entries(ALIAS_GROUPS).map(([name, types]) => [name, new Set<string>(types)]),
);

/** @internal */
export class Dispatch {
  private visitors: CompiledVisitor[];
  private cache: Record<string, Handlers | undefined> = Object.create(null);

  constructor(visitors: readonly Visitors[]) {
    this.visitors = visitors.map(compile);
  }

  handlersOf(type: string): Handlers {
    let handlers = this.cache[type];
    if (handlers === undefined) {
      handlers = handlersFor(this.visitors, type);
      this.cache[type] = handlers;
    }
    return handlers;
  }
}

function childVisitorOf(type: string): ChildVisitor | null {
  const keys = CHILD_KEYS[type];
  if (keys === undefined || keys.length === 0) return null;
  const generated = widened.has(type) ? undefined : CHILD_VISITORS[type];
  if (generated !== undefined) return generated;
  return (node, visit, state) => {
    for (const key of keys) if (!visit(state, node[key], key)) return false;
    return true;
  };
}

function handlersFor(visitors: readonly CompiledVisitor[], type: string): Handlers {
  const enter: Handler[] = [];
  const leave: Handler[] = [];
  for (const visitor of visitors) {
    const hooks: WalkHooks[] = [];
    for (const alias of visitor.aliases) if (alias.members.has(type)) hooks.push(alias.hooks);
    const typed = visitor.typed[type];
    if (typed !== undefined) hooks.push(typed);
    const leaves: Handler[] = [];
    if (visitor.enter !== null) enter.push(visitor.enter);
    for (const hook of hooks) {
      if (hook.enter !== undefined) enter.push(hook.enter);
      if (hook.leave !== undefined) leaves.unshift(hook.leave);
    }
    if (visitor.leave !== null) leaves.push(visitor.leave);
    leave.unshift(...leaves);
  }
  return { enter, leave, children: childVisitorOf(type) };
}

function compile(visitor: Visitors): CompiledVisitor {
  if (visitor === null || typeof visitor !== "object") {
    throw new TypeError("walk: visitors must be an object of handlers by node type");
  }
  const compiled: CompiledVisitor = { enter: null, leave: null, aliases: [], typed: {} };
  for (const [name, value] of Object.entries(visitor)) {
    if (value == null) continue;
    const universal = name === "enter" || name === "leave";
    if (universal && typeof value === "function") {
      compiled[name] = value as Handler;
      continue;
    }
    const members = ALIAS_MEMBERS[name];
    if (universal || (members === undefined && !(name in CHILD_KEYS))) {
      throw new TypeError(
        universal
          ? `walk: the '${name}' visitor must be a function`
          : `walk: '${name}' is not a node type or alias`,
      );
    }
    const hooks = hooksOf(name, value);
    if (members === undefined) compiled.typed[name] = hooks;
    else compiled.aliases.push({ members, hooks });
  }
  return compiled;
}

function hooksOf(name: string, value: unknown): WalkHooks {
  if (typeof value === "function") return { enter: value as Handler };
  const hooks = value as WalkHooks;
  const valid =
    typeof value === "object" &&
    (hooks.enter == null || typeof hooks.enter === "function") &&
    (hooks.leave == null || typeof hooks.leave === "function");
  if (!valid) {
    throw new TypeError(`walk: the '${name}' visitor must be a function or { enter, leave }`);
  }
  return { enter: hooks.enter ?? undefined, leave: hooks.leave ?? undefined };
}

export function fieldsOf(node: Node): Fields {
  return node as unknown as Fields;
}

export function assertNode(node: unknown, operation: string): asserts node is Node {
  if (node === null || typeof node !== "object" || typeof (node as Node).type !== "string") {
    throw new TypeError(`${operation}: expected an AST node`);
  }
}

export function slotOf(parent: Node, node: Node): Slot | null {
  const fields = fieldsOf(parent);
  for (const key of CHILD_KEYS[parent.type] ?? []) {
    const value = fields[key];
    if (value === node) return { parent, key, list: null, index: 0 };
    if (Array.isArray(value)) {
      const index = value.indexOf(node);
      if (index >= 0) return { parent, key, list: value as Node[], index };
    }
  }
  return null;
}

export function replaceAt(slot: Slot, nodes: readonly Node[]): void {
  const fields = fieldsOf(slot.parent);
  const old = (slot.list === null ? fields[slot.key] : slot.list[slot.index]) as Node;
  if (slot.list === null) fields[slot.key] = nodes[0];
  else slot.list.splice(slot.index, 1, ...nodes);
  moveComments(old, nodes[0]!, nodes[nodes.length - 1]!);
}

function removeAt(slot: Slot): void {
  if (slot.list === null) fieldsOf(slot.parent)[slot.key] = null;
  else slot.list.splice(slot.index, 1);
}

function moveComments(old: Node, first: Node, last: Node): void {
  const comments = old.comments;
  if (comments === undefined || comments.length === 0 || first === old || last === old) return;
  const before: Comment[] = [];
  const after: Comment[] = [];
  for (const comment of comments) {
    if (comment.position === "before") before.push(comment);
    else if (comment.position === "after") after.push(comment);
  }
  if (before.length > 0) first.comments = [...before, ...(first.comments ?? [])];
  if (after.length > 0) last.comments = [...(last.comments ?? []), ...after];
  delete old.comments;
}
