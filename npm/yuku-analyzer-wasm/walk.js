import { WalkContext, _walk, _walkAsync } from "yuku-ast";

class SemanticWalkContext extends WalkContext {
  #module;
  constructor(module) {
    super();
    this.#module = module;
  }
  get module() {
    return this.#module;
  }
  get scope() {
    return this.#module.scopeOf(this._node);
  }
  get symbol() {
    return this.#module.symbolOf(this._node);
  }
  get reference() {
    return this.#module.referenceOf(this._node);
  }
}

export function walkModule(module, visitors, root) {
  _walk(root ?? module.ast, visitors, undefined, new SemanticWalkContext(module));
}

export function walkModuleAsync(module, visitors, root) {
  return _walkAsync(root ?? module.ast, visitors, undefined, new SemanticWalkContext(module));
}
