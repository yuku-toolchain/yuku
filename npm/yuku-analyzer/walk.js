import { WalkContext, _walk } from "./engine.js";

class SemanticWalkContext extends WalkContext {
  #module;
  #scopes;
  constructor(module, scopes) {
    super();
    this.#module = module;
    this.#scopes = scopes;
  }
  get module() {
    return this.#module;
  }
  get scope() {
    return this.#scopes[this.#scopes.length - 1];
  }
  get symbol() {
    return this.#module.symbolOf(this._node);
  }
  get reference() {
    return this.#module.referenceOf(this._node);
  }
}

export function walkModule(module, visitors, root) {
  const { byNode, types } = module._scopeMap();
  const start = root ?? module.ast;
  const scopes = [module.scopeOf(start)];
  const hooks = {
    // the scope was created for the original node. a replacement keeps
    // the same lexical position, so push/pop stays balanced on it
    enter(node) {
      const scope = types.has(node.type) ? byNode.get(node) : undefined;
      if (scope === undefined) return false;
      scopes.push(scope);
      return true;
    },
    exit(pushed) {
      if (pushed) scopes.pop();
    },
  };
  _walk(start, visitors, undefined, hooks, new SemanticWalkContext(module, scopes));
}
