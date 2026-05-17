// Lazy proxy for transparent mutation tracking on a decoded AST.
//
// Wrapping a node returns a Proxy that intercepts writes to flip a hidden
// dirty bit on the target and bubble a subtree bit up through cached
// parent links. Reads pass through; children get proxied on first access.

export const DIRTY = Symbol.for("yuku.dirty");
export const RAW = Symbol.for("yuku.raw");
const PARENT = Symbol.for("yuku.parent");
const PROXY = Symbol.for("yuku.proxy");
const OWNER = Symbol.for("yuku.arrayOwner");

export function wrap(node, parent = null) {
  hide(node, DIRTY, { self: false, subtree: false });
  hide(node, PARENT, parent);
  return proxify(node, nodeHandler);
}

function hide(obj, sym, value) {
  Object.defineProperty(obj, sym, { value, writable: true, configurable: true });
}

function isNode(v) {
  return v != null && typeof v === "object" && typeof v.type === "string";
}

function proxify(target, handler) {
  return target[PROXY] ?? (hide(target, PROXY, new Proxy(target, handler)), target[PROXY]);
}

function dirty(node) {
  const d = node[DIRTY];
  if (!d) return;
  d.self = true;
  for (let p = node[PARENT]; p; p = p[PARENT]) {
    const pd = p[DIRTY];
    if (!pd || pd.subtree) return;
    pd.subtree = true;
  }
}

function init(node, parent) {
  if (node[DIRTY]) return;
  hide(node, DIRTY, { self: false, subtree: false });
  hide(node, PARENT, parent);
}

const nodeHandler = {
  get(t, k) {
    if (k === RAW) return t;
    const v = t[k];
    if (typeof k === "symbol") return v;
    if (Array.isArray(v)) {
      if (v[OWNER] === undefined) hide(v, OWNER, t);
      return proxify(v, arrayHandler);
    }
    if (isNode(v)) {
      init(v, t);
      return proxify(v, nodeHandler);
    }
    return v;
  },
  set(t, k, v) {
    if (typeof k !== "symbol" && t[k] !== v) dirty(t);
    t[k] = v;
    return true;
  },
  deleteProperty(t, k) {
    if (typeof k !== "symbol" && k in t) dirty(t);
    delete t[k];
    return true;
  },
};

const arrayHandler = {
  get(t, k) {
    if (k === RAW) return t;
    const v = t[k];
    if (typeof k === "symbol") return v;
    if (isNode(v)) {
      init(v, t[OWNER]);
      return proxify(v, nodeHandler);
    }
    return v;
  },
  set(t, k, v) {
    if (typeof k !== "symbol" && t[k] !== v) {
      const o = t[OWNER];
      if (o) dirty(o);
    }
    t[k] = v;
    return true;
  },
  deleteProperty(t, k) {
    if (typeof k !== "symbol" && k in t) {
      const o = t[OWNER];
      if (o) dirty(o);
    }
    delete t[k];
    return true;
  },
};
