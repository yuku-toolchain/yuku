import { CHILD_KEYS, findAll, WalkContext, _walk, _walkAsync } from "yuku-ast";
import { fileOptions } from "yuku-core";
import { BindingFlags, decode } from "./decode.js";

const _dec = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

class Scope {
  #sem;
  constructor(module, sem, id) {
    this.module = module;
    this.id = id;
    this.#sem = sem;
  }
  get kind() {
    return this.#sem.scope.kind(this.id);
  }
  get strict() {
    return this.#sem.scope.strict(this.id);
  }
  get node() {
    return this.#sem.scope.node(this.id);
  }
  get parent() {
    const parent = this.#sem.scope.parentId(this.id);
    return parent === null ? null : this.module.scopes[parent];
  }
  get hoistTarget() {
    return this.module.scopes[this.#sem.scope.hoistTargetId(this.id)];
  }
  get bindings() {
    return this.module._scopeBindings(this.id);
  }
  find(name) {
    for (const binding of this.bindings) if (binding.name === name) return binding;
    return null;
  }
  contains(other) {
    for (let scope = other; scope !== null; scope = scope.parent) if (scope === this) return true;
    return false;
  }
  *ancestors() {
    for (let scope = this; scope !== null; scope = scope.parent) yield scope;
  }
}

class Binding {
  #sem;
  constructor(module, sem, id) {
    this.module = module;
    this.id = id;
    this.#sem = sem;
  }
  get name() {
    return this.#sem.symbol.name(this.id);
  }
  get flags() {
    return this.#sem.symbol.flags(this.id);
  }
  get scope() {
    return this.module.scopes[this.#sem.symbol.scopeId(this.id)];
  }
  get declarations() {
    const { symbol } = this.#sem;
    const out = new Array(symbol.declCount(this.id));
    for (let i = 0; i < out.length; i++) out[i] = symbol.declNode(this.id, i);
    return out;
  }
  get references() {
    return this.module._bindingReferences(this.id);
  }
  has(mask) {
    return (this.flags & mask) !== 0;
  }
  hasAll(mask) {
    return (this.flags & mask) === mask;
  }
  // an import aliases a binding of a space one file cannot know
  visibleIn(space) {
    if (this.has(BindingFlags.Import)) return true;
    switch (space) {
      case "value":
      case "typeof":
        return this.has(BindingFlags.ValueSpace);
      case "type":
        return this.has(BindingFlags.TypeSpace);
      case "namespace":
        return this.has(BindingFlags.NamespaceSpace);
      case "any":
        return true;
    }
    throw new TypeError('`space` must be "value", "type", "namespace", "typeof", or "any"');
  }
  definition() {
    return this.module.analyzer._definitionOf(this);
  }
  findReferences() {
    return this.module.analyzer._referencesOf(this);
  }
}

class Reference {
  #sem;
  constructor(module, sem, id) {
    this.module = module;
    this.id = id;
    this.#sem = sem;
  }
  get name() {
    return this.#sem.reference.name(this.id);
  }
  get scope() {
    return this.module.scopes[this.#sem.reference.scopeId(this.id)];
  }
  get node() {
    return this.#sem.reference.node(this.id);
  }
  get space() {
    return this.#sem.reference.space(this.id);
  }
  get inTypePosition() {
    return this.#sem.reference.inTypePosition(this.id);
  }
  get isWrite() {
    return this.#sem.reference.isWrite(this.id);
  }
  get binding() {
    const binding = this.#sem.reference.symbolId(this.id);
    return binding === null ? null : this.module.bindings[binding];
  }
}

class Import {
  #sem;
  constructor(module, sem, id) {
    this.module = module;
    this.id = id;
    this.#sem = sem;
    this._resolved = null;
  }
  get kind() {
    return this.#sem.import.kind(this.id);
  }
  get name() {
    return this.kind === "named" ? this.#sem.import.name(this.id) : null;
  }
  get local() {
    const binding = this.#sem.import.symbolId(this.id);
    return binding === null ? null : this.module.bindings[binding];
  }
  get isNamespace() {
    const kind = this.kind;
    return kind === "namespace" || kind === "importEquals";
  }
  get specifier() {
    return this.#sem.import.specifier(this.id);
  }
  get typeOnly() {
    return this.#sem.import.typeOnly(this.id);
  }
  get phase() {
    return this.#sem.import.phase(this.id);
  }
  get node() {
    return this.#sem.import.node(this.id);
  }
  get _scope() {
    return this.module.scopes[this.#sem.import.scopeId(this.id)];
  }
  get resolvedModule() {
    this.module.analyzer._link(this.module);
    return this._resolved;
  }
}

class Export {
  #sem;
  constructor(module, sem, id) {
    this.module = module;
    this.id = id;
    this.#sem = sem;
    this._resolved = null;
  }
  get kind() {
    return this.#sem.export.kind(this.id);
  }
  get name() {
    const kind = this.kind;
    return kind === "named" || kind === "reExport" || kind === "namespace"
      ? this.#sem.export.name(this.id)
      : null;
  }
  get globalName() {
    return this.kind === "global" ? this.#sem.export.name(this.id) : null;
  }
  get local() {
    const binding = this.#sem.export.symbolId(this.id);
    return binding === null ? null : this.module.bindings[binding];
  }
  get specifier() {
    const kind = this.kind;
    return kind === "reExport" || kind === "namespace" || kind === "star"
      ? this.#sem.export.specifier(this.id)
      : null;
  }
  get fromName() {
    return this.kind === "reExport" ? this.#sem.export.fromName(this.id) : null;
  }
  get typeOnly() {
    return this.#sem.export.typeOnly(this.id);
  }
  get node() {
    return this.#sem.export.node(this.id);
  }
  get resolvedModule() {
    this.module.analyzer._link(this.module);
    return this._resolved;
  }
}

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
  get binding() {
    return this.#module.bindingOf(this._node);
  }
  get reference() {
    return this.#module.referenceOf(this._node);
  }
}

export class Module {
  #r;
  #sem;
  #scopes = null;
  #bindings = null;
  #references = null;
  #unresolved = null;
  #imports = null;
  #exports = null;
  #scopeBindings = null;
  #bindingReferences = null;
  #declarations = null;
  #referenceNodes = null;
  #exportMap = null;
  #starExports = null;
  #importByBinding = null;
  _deps = [];
  _dependents = [];

  constructor(analyzer, core, path, source, options = {}) {
    this.analyzer = analyzer;
    this.path = path;
    this.source = typeof source === "string" ? source : _dec.decode(source);
    this.#r = decode(
      core.analyze(this.source, fileOptions({ ...options, path })),
      this.source,
      path,
    );
    this.#sem = this.#r.semantic;
  }

  get isCurrent() {
    return this.analyzer.module(this.path) === this;
  }
  get ast() {
    return this.#r.program;
  }
  get comments() {
    return this.#r.comments;
  }
  get tokens() {
    return this.#r.tokens;
  }
  get diagnostics() {
    return this.#r.diagnostics;
  }

  get scopes() {
    return this.#scopes ?? (this.#scopes = this.#rows(Scope, this.#sem.scope.count));
  }
  get rootScope() {
    const scopes = this.scopes;
    return scopes.length > 1 && scopes[1].kind === "module" ? scopes[1] : scopes[0];
  }
  get bindings() {
    return this.#bindings ?? (this.#bindings = this.#rows(Binding, this.#sem.symbol.count));
  }
  get references() {
    return (
      this.#references ?? (this.#references = this.#rows(Reference, this.#sem.reference.count))
    );
  }
  get unresolvedReferences() {
    return (
      this.#unresolved ??
      (this.#unresolved = this.references.filter((reference) => reference.binding === null))
    );
  }
  get imports() {
    return this.#imports ?? (this.#imports = this.#rows(Import, this.#sem.import.count));
  }
  get exports() {
    return this.#exports ?? (this.#exports = this.#rows(Export, this.#sem.export.count));
  }
  get moduleFlags() {
    return this.#sem.moduleFlags;
  }
  get dependencies() {
    this.analyzer._link(this);
    return this._deps;
  }
  get dependents() {
    this.analyzer._link(this);
    return this._dependents;
  }

  bindingOf(node) {
    const index = this.#r.indexOf(node);
    if (index === undefined) return null;
    const declared = this.#declarationMap().get(index);
    if (declared !== undefined) return this.bindings[declared];
    return this.referenceOf(node)?.binding ?? null;
  }

  referenceOf(node) {
    const index = this.#r.indexOf(node);
    if (index === undefined) return null;
    const reference = this.#referenceMap().get(index);
    return reference === undefined ? null : this.references[reference];
  }

  scopeOf(node) {
    const index = this.#r.indexOf(node);
    if (index === undefined) return this.rootScope;
    return this.scopes[this.#sem.nodeScope(index)];
  }

  parentOf(node) {
    // the hashbang is synthesized in JavaScript
    if (node?.type === "Hashbang") return node === this.ast.hashbang ? this.ast : null;
    const index = this.#r.indexOf(node);
    if (index === undefined) return null;
    const parent = this.#r.parentIndex(index);
    return parent < 0 ? null : this.#r.nodeOf(parent);
  }

  *ancestors(node) {
    for (let current = node; current !== null; current = this.parentOf(current)) yield current;
  }

  nodeAt(offset) {
    let node = this.ast;
    if (!spans(node, offset)) return null;
    for (let child = childAt(node, offset); child !== null; child = childAt(node, offset)) {
      node = child;
    }
    return node;
  }

  // mirrors reference resolution in the binder
  lookup(name, { from = this.rootScope, space = "value" } = {}) {
    const argumentsBarrier = name === "arguments" && (space === "value" || space === "typeof");
    for (let scope = from; scope !== null; scope = scope.parent) {
      const found = scope.find(name);
      if (found !== null && found.visibleIn(space)) return found;
      const shared = this.#shared(scope, name, space);
      if (shared !== null) return shared;
      if (argumentsBarrier && isArgumentsScope(scope)) return null;
    }
    return null;
  }

  capturesOf(fn) {
    const index = this.#r.indexOf(fn);
    if (index === undefined) {
      throw new TypeError("capturesOf: node does not belong to this module's AST");
    }
    const { scope, symbol, reference } = this.#sem;
    const fnScope = this.#sem.nodeScope(index);
    if (scope.kind(fnScope) !== "function" || scope.nodeIndex(fnScope) !== index) {
      throw new TypeError("capturesOf: node does not create a function scope");
    }
    const start = this.#r.startOf(index);
    const end = this.#r.endOf(index);
    const captures = new Map();
    for (let i = 0; i < reference.count; i++) {
      const target = reference.symbolId(i);
      if (target === null || reference.inTypePosition(i)) continue;
      if (reference.start(i) < start || reference.end(i) > end) continue;
      let inside = false;
      for (let s = symbol.scopeId(target); s !== null; s = scope.parentId(s)) {
        if (s === fnScope) {
          inside = true;
          break;
        }
      }
      if (inside) continue;
      let capture = captures.get(target);
      if (capture === undefined) {
        capture = { binding: this.bindings[target], references: [], isWritten: false };
        captures.set(target, capture);
      }
      capture.references.push(this.references[i]);
      if (reference.isWrite(i)) capture.isWritten = true;
    }
    return [...captures.values()];
  }

  // GetExportedNames, 16.2.1.7.2.1
  exportedNames(seen = new Set()) {
    if (seen.has(this)) return [];
    seen.add(this);
    this.analyzer._link(this);
    const names = new Set(this._exportMap().keys());
    for (const name of this.analyzer._addedExports(this)) names.add(name);
    for (const star of this._starExports()) {
      if (star._resolved === null) continue;
      for (const name of star._resolved.exportedNames(seen)) {
        if (name !== "default") names.add(name);
      }
    }
    return [...names];
  }

  resolveExport(name) {
    return this.analyzer._resolveExport(this, name);
  }

  walk(visitors, root) {
    _walk(root ?? this.ast, visitors, undefined, new SemanticWalkContext(this));
  }

  walkAsync(visitors, root) {
    return _walkAsync(root ?? this.ast, visitors, undefined, new SemanticWalkContext(this));
  }

  findAll(types) {
    return findAll(this.ast, types);
  }

  _scopeBindings(scope) {
    if (this.#scopeBindings === null) {
      const lists = Array.from({ length: this.#sem.scope.count }, () => []);
      for (const binding of this.bindings) lists[binding.scope.id].push(binding);
      this.#scopeBindings = lists;
    }
    return this.#scopeBindings[scope];
  }

  _bindingReferences(binding) {
    if (this.#bindingReferences === null) {
      const lists = Array.from({ length: this.#sem.symbol.count }, () => []);
      for (const reference of this.references) {
        const target = this.#sem.reference.symbolId(reference.id);
        if (target !== null) lists[target].push(reference);
      }
      this.#bindingReferences = lists;
    }
    return this.#bindingReferences[binding];
  }

  // the export entry partition of ParseModule, 16.2.1.7.1
  _exportMap() {
    if (this.#exportMap === null) {
      const map = new Map();
      const stars = [];
      for (const record of this.exports) {
        if (record.kind === "star") stars.push(record);
        else if (record.name !== null && !map.has(record.name)) map.set(record.name, record);
      }
      this.#exportMap = map;
      this.#starExports = stars;
    }
    return this.#exportMap;
  }

  _starExports() {
    this._exportMap();
    return this.#starExports;
  }

  _importOf(binding) {
    if (this.#importByBinding === null) {
      const map = new Map();
      for (const record of this.imports) {
        const local = record.local;
        if (local !== null) map.set(local, record);
      }
      this.#importByBinding = map;
    }
    return this.#importByBinding.get(binding);
  }

  #shared(scope, name, space) {
    const next = this.#sem.scope.nextBodyId;
    const shared = scope.kind === "tsModule" ? BindingFlags.Exported : BindingFlags.EnumMember;
    for (let body = next(scope.id); body !== null && body !== scope.id; body = next(body)) {
      const found = this.scopes[body].find(name);
      if (found !== null && found.has(shared) && found.visibleIn(space)) return found;
    }
    return null;
  }

  #rows(Row, count) {
    const out = new Array(count);
    for (let i = 0; i < count; i++) out[i] = new Row(this, this.#sem, i);
    return out;
  }

  #declarationMap() {
    if (this.#declarations === null) {
      const map = new Map();
      const { symbol } = this.#sem;
      for (let s = 0; s < symbol.count; s++) {
        const count = symbol.declCount(s);
        for (let i = 0; i < count; i++) map.set(symbol.declNodeIndex(s, i), s);
      }
      this.#declarations = map;
    }
    return this.#declarations;
  }

  #referenceMap() {
    if (this.#referenceNodes === null) {
      const map = new Map();
      const { reference } = this.#sem;
      for (let i = 0; i < reference.count; i++) map.set(reference.nodeIndex(i), i);
      this.#referenceNodes = map;
    }
    return this.#referenceNodes;
  }
}

function isArgumentsScope(scope) {
  if (scope.kind === "staticBlock") return true;
  return scope.kind === "function" && scope.node.type !== "ArrowFunctionExpression";
}

function spans(node, offset) {
  return node !== null && typeof node === "object" && node.start <= offset && offset < node.end;
}

function childAt(node, offset) {
  for (const key of CHILD_KEYS[node.type] ?? []) {
    const value = node[key];
    if (Array.isArray(value)) {
      for (const child of value) if (spans(child, offset)) return child;
    } else if (spans(value, offset)) {
      return value;
    }
  }
  return null;
}
