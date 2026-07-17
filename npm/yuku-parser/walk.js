import { CHILD_KEYS } from "./decode.js";

export class WalkContext {
  _ancestors = [];
  _node = null;
  _key = null;
  _list = null;
  _frame = null;
  _skip = false;
  _stopped = false;
  _removed = false;
  _replacement = null;
  state;

  get node() {
    return this._node;
  }
  get parent() {
    const a = this._ancestors;
    return a.length === 0 ? null : a[a.length - 1];
  }
  get key() {
    return this._key;
  }
  get index() {
    return this._list === null ? null : this._frame.i;
  }
  ancestors() {
    return [...this._ancestors];
  }
  skip() {
    this._skip = true;
  }
  stop() {
    this._stopped = true;
  }
  replace(node) {
    if (node === null || typeof node !== "object" || typeof node.type !== "string") {
      throw new TypeError("replace: expected an AST node");
    }
    if (this.parent === null) {
      throw new TypeError("replace: cannot replace the walk root");
    }
    // synthetic nodes inherit the original span, for source maps
    if (node.start === 0 && node.end === 0) {
      node.start = this._node.start;
      node.end = this._node.end;
    }
    this._replacement = node;
  }
  remove() {
    if (this.parent === null) {
      throw new TypeError("remove: cannot remove the walk root");
    }
    this._removed = true;
  }
  insertBefore(node) {
    // advance past the insert so the current node keeps its turn
    this.#insert(node, 0);
    this._frame.i++;
  }
  insertAfter(node) {
    // lands at the next slot, so the walk visits it
    this.#insert(node, 1);
  }
  #insert(node, offset) {
    if (this._list === null) {
      throw new TypeError("insertBefore/insertAfter require a node in an array field");
    }
    this._list.splice(this._frame.i + offset, 0, node);
  }
}

function createDispatch(visitors) {
  let enter = null;
  let leave = null;
  const concrete = new Map();
  for (const [name, value] of Object.entries(visitors)) {
    if (value == null) continue;
    if (name === "enter") enter = value;
    else if (name === "leave") leave = value;
    else if (typeof value === "function") concrete.set(name, { enter: value, leave: null });
    else concrete.set(name, { enter: value.enter ?? null, leave: value.leave ?? null });
  }
  return { enter, leave, typed: (type) => concrete.get(type) };
}

function position(ctx, node, key, list, frame) {
  ctx._node = node;
  ctx._key = key;
  ctx._list = list;
  ctx._frame = frame;
}

function applyReplace(ctx, parent, key, list, frame) {
  const next = ctx._replacement;
  ctx._replacement = null;
  if (list !== null) list[frame.i] = next;
  else parent[key] = next;
  return next;
}

function applyRemove(ctx, parent, key, list, frame) {
  ctx._removed = false;
  if (list !== null) {
    list.splice(frame.i, 1);
    frame.i--;
  } else {
    parent[key] = null;
  }
}

export function walk(root, visitors, state) {
  _walk(root, visitors, state, new WalkContext());
  return root;
}

export async function walkAsync(root, visitors, state) {
  await _walkAsync(root, visitors, state, new WalkContext());
  return root;
}

export function _walk(root, visitors, state, ctx) {
  const d = createDispatch(visitors);
  ctx.state = state;
  const ancestors = ctx._ancestors;

  (function visit(node, key, list, frame) {
    let typed = d.typed(node.type);
    const parent = ctx.parent;

    position(ctx, node, key, list, frame);
    if (d.enter !== null) {
      d.enter(node, ctx);
      if (ctx._stopped) return false;
    }
    if (typed !== undefined && typed.enter !== null) {
      typed.enter(node, ctx);
      if (ctx._stopped) return false;
    }
    if (ctx._removed) {
      applyRemove(ctx, parent, key, list, frame);
      return true;
    }
    if (ctx._replacement !== null) {
      node = applyReplace(ctx, parent, key, list, frame);
      typed = d.typed(node.type);
    }

    const skipped = ctx._skip;
    ctx._skip = false;
    if (!skipped) {
      const keys = CHILD_KEYS[node.type];
      if (keys !== undefined && keys.length > 0) {
        ancestors.push(node);
        for (let k = 0; k < keys.length; k++) {
          const key2 = keys[k];
          const value = node[key2];
          if (value === null || value === undefined || typeof value !== "object") continue;
          if (Array.isArray(value)) {
            const childFrame = { i: 0 };
            for (; childFrame.i < value.length; childFrame.i++) {
              const item = value[childFrame.i];
              // pattern element holes are null
              if (item !== null && !visit(item, key2, value, childFrame)) return false;
            }
          } else if (!visit(value, key2, null, null)) {
            return false;
          }
        }
        ancestors.pop();
      }
    }

    position(ctx, node, key, list, frame);
    if (typed !== undefined && typed.leave !== null) {
      typed.leave(node, ctx);
      if (ctx._stopped) return false;
    }
    if (d.leave !== null) {
      d.leave(node, ctx);
      if (ctx._stopped) return false;
    }
    if (ctx._removed) applyRemove(ctx, parent, key, list, frame);
    else if (ctx._replacement !== null) applyReplace(ctx, parent, key, list, frame);

    return true;
  })(root, null, null, null);
}

// line-for-line twin of _walk with each handler awaited before the walk
// moves on, so keep the two visit bodies in lockstep
export async function _walkAsync(root, visitors, state, ctx) {
  const d = createDispatch(visitors);
  ctx.state = state;
  const ancestors = ctx._ancestors;

  await (async function visit(node, key, list, frame) {
    let typed = d.typed(node.type);
    const parent = ctx.parent;

    position(ctx, node, key, list, frame);
    if (d.enter !== null) {
      await d.enter(node, ctx);
      if (ctx._stopped) return false;
    }
    if (typed !== undefined && typed.enter !== null) {
      await typed.enter(node, ctx);
      if (ctx._stopped) return false;
    }
    if (ctx._removed) {
      applyRemove(ctx, parent, key, list, frame);
      return true;
    }
    if (ctx._replacement !== null) {
      node = applyReplace(ctx, parent, key, list, frame);
      typed = d.typed(node.type);
    }

    const skipped = ctx._skip;
    ctx._skip = false;
    if (!skipped) {
      const keys = CHILD_KEYS[node.type];
      if (keys !== undefined && keys.length > 0) {
        ancestors.push(node);
        for (let k = 0; k < keys.length; k++) {
          const key2 = keys[k];
          const value = node[key2];
          if (value === null || value === undefined || typeof value !== "object") continue;
          if (Array.isArray(value)) {
            const childFrame = { i: 0 };
            for (; childFrame.i < value.length; childFrame.i++) {
              const item = value[childFrame.i];
              // pattern element holes are null
              if (item !== null && !(await visit(item, key2, value, childFrame))) return false;
            }
          } else if (!(await visit(value, key2, null, null))) {
            return false;
          }
        }
        ancestors.pop();
      }
    }

    position(ctx, node, key, list, frame);
    if (typed !== undefined && typed.leave !== null) {
      await typed.leave(node, ctx);
      if (ctx._stopped) return false;
    }
    if (d.leave !== null) {
      await d.leave(node, ctx);
      if (ctx._stopped) return false;
    }
    if (ctx._removed) applyRemove(ctx, parent, key, list, frame);
    else if (ctx._replacement !== null) applyReplace(ctx, parent, key, list, frame);

    return true;
  })(root, null, null, null);
}
