// semantic walk over a module's decoded AST. scopes are not tracked,
// they are replayed from the native scope table: a WeakMap from
// scope-creating node to Scope, gated by a Set of node types so
// non-scope nodes pay one string lookup and nothing else.

class WalkContext {
  #module;
  #scopes;
  #ancestors;
  _node = null;
  _skip = false;
  _stopped = false;
  _replacement = null;

  constructor(module, scopes, ancestors) {
    this.#module = module;
    this.#scopes = scopes;
    this.#ancestors = ancestors;
  }
  get module() {
    return this.#module;
  }
  get scope() {
    return this.#scopes[this.#scopes.length - 1];
  }
  get parent() {
    const a = this.#ancestors;
    return a.length === 0 ? null : a[a.length - 1];
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
    this._replacement = node;
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

  function swap(container, key, replacement) {
    if (container === null) {
      throw new TypeError("replace: cannot replace the walk root");
    }
    container[key] = replacement;
    return replacement;
  }

  (function visit(node, container, key) {
    let typed = handlers.size === 0 ? undefined : handlers.get(node.type);
    // the scope was created for the original node, a replacement keeps
    // the same lexical position, so push/pop stays balanced on it
    const scope = types.has(node.type) ? byNode.get(node) : undefined;
    if (scope !== undefined) scopes.push(scope);

    ctx._node = node;
    if (enter !== null) {
      enter(node, ctx);
      if (ctx._stopped) return false;
    }
    if (typed !== undefined && typed.enter !== null) {
      typed.enter(node, ctx);
      if (ctx._stopped) return false;
    }
    if (ctx._replacement !== null) {
      node = swap(container, key, ctx._replacement);
      ctx._replacement = null;
      ctx._node = node;
      typed = handlers.size === 0 ? undefined : handlers.get(node.type);
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
          for (let i = 0; i < value.length; i++) {
            const item = value[i];
            if (item !== null && typeof item === "object" && typeof item.type === "string") {
              if (!visit(item, value, i)) return false;
            }
          }
        } else if (typeof value.type === "string") {
          if (!visit(value, node, k)) return false;
        }
      }
      ancestors.pop();
    }

    ctx._node = node;
    if (typed !== undefined && typed.leave !== null) {
      typed.leave(node, ctx);
      if (ctx._stopped) return false;
    }
    if (leave !== null) {
      leave(node, ctx);
      if (ctx._stopped) return false;
    }
    if (ctx._replacement !== null) {
      swap(container, key, ctx._replacement);
      ctx._replacement = null;
    }

    if (scope !== undefined) scopes.pop();
    return true;
  })(start, null, null);
}
