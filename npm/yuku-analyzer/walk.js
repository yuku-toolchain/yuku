class WalkContext {
  #module;
  #scopes;
  #ancestors;
  _node = null;
  _key = null;
  _list = null;
  _frame = null;
  _skip = false;
  _stopped = false;
  _removed = false;
  _replacement = null;

  constructor(module, scopes, ancestors) {
    this.#module = module;
    this.#scopes = scopes;
    this.#ancestors = ancestors;
  }
  get module() {
    return this.#module;
  }
  get node() {
    return this._node;
  }
  get scope() {
    return this.#scopes[this.#scopes.length - 1];
  }
  get parent() {
    const a = this.#ancestors;
    return a.length === 0 ? null : a[a.length - 1];
  }
  get key() {
    return this._key;
  }
  get index() {
    return this._list === null ? null : this._frame.i;
  }
  get symbol() {
    return this.#module.symbolOf(this._node);
  }
  get reference() {
    return this.#module.referenceOf(this._node);
  }
  ancestors() {
    return [...this.#ancestors];
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
    this._replacement = node;
  }
  remove() {
    if (this.parent === null) {
      throw new TypeError("remove: cannot remove the walk root");
    }
    this._removed = true;
  }
  insertBefore(node) {
    // shift the cursor so the current node keeps its position and the
    // inserted sibling is not visited
    this.#insert(node, 0);
    this._frame.i++;
  }
  insertAfter(node) {
    // lands at the next cursor position, so the walk visits it
    this.#insert(node, 1);
  }
  #insert(node, offset) {
    if (this._list === null) {
      throw new TypeError("insertBefore/insertAfter require a node in an array field");
    }
    this._list.splice(this._frame.i + offset, 0, node);
  }
}

function normalize(visitor) {
  const handlers = new Map();
  let enter = null;
  let leave = null;
  for (const [key, value] of Object.entries(visitor)) {
    if (value == null) continue;
    if (key === "enter") enter = value;
    else if (key === "leave") leave = value;
    else if (typeof value === "function") handlers.set(key, { enter: value, leave: null });
    else handlers.set(key, { enter: value.enter ?? null, leave: value.leave ?? null });
  }
  return { handlers, enter, leave };
}

export function walkModule(module, visitor, root) {
  const { byNode, types } = module._scopeMap();
  const { handlers, enter, leave } = normalize(visitor);
  const start = root ?? module.ast;
  const scopes = [module.scopeOf(start)];
  const ancestors = [];
  const ctx = new WalkContext(module, scopes, ancestors);

  function position(node, key, list, frame) {
    ctx._node = node;
    ctx._key = key;
    ctx._list = list;
    ctx._frame = frame;
  }

  // swaps the current node in its parent slot and continues the walk on
  // the replacement
  function applyReplace(parent, key, list, frame) {
    const next = ctx._replacement;
    ctx._replacement = null;
    if (list !== null) list[frame.i] = next;
    else parent[key] = next;
    return next;
  }

  function applyRemove(parent, key, list, frame) {
    ctx._removed = false;
    if (list !== null) {
      list.splice(frame.i, 1);
      frame.i--;
    } else {
      parent[key] = null;
    }
  }

  (function visit(node, key, list, frame) {
    let typed = handlers.get(node.type);
    // the scope was created for the original node, a replacement keeps
    // the same lexical position, so push/pop stays balanced on it
    const scope = types.has(node.type) ? byNode.get(node) : undefined;
    if (scope !== undefined) scopes.push(scope);
    const parent = ctx.parent;

    position(node, key, list, frame);
    if (enter !== null) {
      enter(node, ctx);
      if (ctx._stopped) return false;
    }
    if (typed !== undefined && typed.enter !== null) {
      typed.enter(node, ctx);
      if (ctx._stopped) return false;
    }
    if (ctx._removed) {
      applyRemove(parent, key, list, frame);
      if (scope !== undefined) scopes.pop();
      return true;
    }
    if (ctx._replacement !== null) {
      node = applyReplace(parent, key, list, frame);
      typed = handlers.get(node.type);
    }

    const skipped = ctx._skip;
    ctx._skip = false;
    if (!skipped) {
      ancestors.push(node);
      for (const k in node) {
        // `comments` holds attached comments, which carry a `type` of
        // their own but are not AST nodes
        if (k === "type" || k === "start" || k === "end" || k === "comments") continue;
        const value = node[k];
        if (value === null || typeof value !== "object") continue;
        if (Array.isArray(value)) {
          const childFrame = { i: 0 };
          for (; childFrame.i < value.length; childFrame.i++) {
            const item = value[childFrame.i];
            if (item !== null && typeof item === "object" && typeof item.type === "string") {
              if (!visit(item, k, value, childFrame)) return false;
            }
          }
        } else if (typeof value.type === "string") {
          if (!visit(value, k, null, null)) return false;
        }
      }
      ancestors.pop();
    }

    position(node, key, list, frame);
    if (typed !== undefined && typed.leave !== null) {
      typed.leave(node, ctx);
      if (ctx._stopped) return false;
    }
    if (leave !== null) {
      leave(node, ctx);
      if (ctx._stopped) return false;
    }
    if (ctx._removed) applyRemove(parent, key, list, frame);
    else if (ctx._replacement !== null) applyReplace(parent, key, list, frame);

    if (scope !== undefined) scopes.pop();
    return true;
  })(start, null, null, null);
}
