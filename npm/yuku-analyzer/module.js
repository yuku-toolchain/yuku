// one analyzed file, its AST plus a lazily built semantic graph
import binding from "./binding.js";
import { decode, SymbolFlags } from "./decode.js";
import { walkModule } from "./walk.js";

export function langFromPath(path) {
  if (path.endsWith(".d.ts") || path.endsWith(".d.mts") || path.endsWith(".d.cts")) return "dts";
  if (path.endsWith(".tsx")) return "tsx";
  if (path.endsWith(".ts") || path.endsWith(".mts") || path.endsWith(".cts")) return "ts";
  if (path.endsWith(".jsx")) return "jsx";
  return "js";
}

export function sourceTypeFromPath(path) {
  return path.endsWith(".cjs") || path.endsWith(".cts") ? "script" : "module";
}

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
    const p = this.#sem.scope.parentId(this.id);
    return p === null ? null : this.module.scopes[p];
  }
  get hoistTarget() {
    return this.module.scopes[this.#sem.scope.hoistTargetId(this.id)];
  }
  get bindings() {
    return this.module._scopeBindings(this.id);
  }
  find(name) {
    for (const symbol of this.bindings) if (symbol.name === name) return symbol;
    return null;
  }
  contains(other) {
    for (let s = other; s; s = s.parent) if (s === this) return true;
    return false;
  }
  *ancestors() {
    for (let s = this; s; s = s.parent) yield s;
  }
}

class Symbol {
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
    const out = Array.from({ length: symbol.declCount(this.id) });
    for (let i = 0; i < out.length; i++) out[i] = symbol.declNode(this.id, i);
    return out;
  }
  get references() {
    return this.module._referencesOfSymbol(this.id);
  }
  has(mask) {
    return (this.flags & mask) !== 0;
  }
  hasAll(mask) {
    return (this.flags & mask) === mask;
  }
  definition() {
    return this.module.analyzer.definitionOf(this);
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
  get kind() {
    return this.#sem.reference.kind(this.id);
  }
  get isWrite() {
    return this.#sem.reference.isWrite(this.id);
  }
  get symbol() {
    const s = this.#sem.reference.symbolId(this.id);
    return s === null ? null : this.module.symbols[s];
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
  get #nameKind() {
    return this.#sem.import.nameKind(this.id);
  }
  get local() {
    const s = this.#sem.import.symbolId(this.id);
    return s === null ? null : this.module.symbols[s];
  }
  get name() {
    return this.#nameKind === "named" ? this.#sem.import.name(this.id) : null;
  }
  get isNamespace() {
    return this.#nameKind === "star" && this.local !== null;
  }
  get isSideEffect() {
    return this.#nameKind === "none";
  }
  get typeOnly() {
    return this.#sem.import.typeOnly(this.id);
  }
  get phase() {
    return this.#sem.import.phase(this.id);
  }
  get specifier() {
    return this.#sem.import.specifier(this.id);
  }
  get node() {
    return this.#sem.import.node(this.id);
  }
  get resolvedModule() {
    this.module.analyzer._ensureLinked();
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
  get #nameKind() {
    return this.#sem.export.nameKind(this.id);
  }
  get #fromKind() {
    return this.#sem.export.fromKind(this.id);
  }
  get name() {
    return this.#nameKind === "named" ? this.#sem.export.name(this.id) : null;
  }
  get isStar() {
    return this.#nameKind === "star";
  }
  get isExportEquals() {
    return this.#nameKind === "equals";
  }
  get globalName() {
    return this.#nameKind === "global" ? this.#sem.export.name(this.id) : null;
  }
  get typeOnly() {
    return this.#sem.export.typeOnly(this.id);
  }
  get local() {
    const s = this.#sem.export.symbolId(this.id);
    return s === null ? null : this.module.symbols[s];
  }
  get specifier() {
    return this.#fromKind === "none" ? null : this.#sem.export.specifier(this.id);
  }
  get fromName() {
    return this.#fromKind === "named" ? this.#sem.export.fromName(this.id) : null;
  }
  get isNamespaceReexport() {
    return this.#fromKind === "star";
  }
  get node() {
    return this.#sem.export.node(this.id);
  }
  get resolvedModule() {
    this.module.analyzer._ensureLinked();
    return this._resolved;
  }
}

export class Module {
  #r;
  #sem;
  #scopes = null;
  #symbols = null;
  #references = null;
  #unresolved = null;
  #imports = null;
  #exports = null;
  #scopeBindings = null;
  #symbolReferences = null;
  #declToSymbol = null;
  #nodeToReference = null;
  #exportMap = null;
  #starExports = null;
  #importBySymbol = null;
  _deps = [];
  _dependents = [];

  constructor(analyzer, path, source, options = {}) {
    this.analyzer = analyzer;
    this.path = path;
    this.source = source;
    this.#r = decode(
      binding.analyze(source, {
        lang: options.lang ?? langFromPath(path),
        sourceType: options.sourceType ?? sourceTypeFromPath(path),
        preserveParens: options.preserveParens,
        allowReturnOutsideFunction: options.allowReturnOutsideFunction,
        attachComments: options.attachComments,
      }),
      source,
    );
    this.#sem = this.#r.semantic;
  }

  get ast() {
    return this.#r.program;
  }
  get diagnostics() {
    return this.#r.diagnostics;
  }
  get comments() {
    return this.#r.comments;
  }
  get lineStarts() {
    return this.#r.lineStarts;
  }
  locOf(offset) {
    return this.#r.locOf(offset);
  }

  get scopes() {
    return (this.#scopes ??= this.#rows(Scope, this.#sem.scope.count));
  }
  get symbols() {
    return (this.#symbols ??= this.#rows(Symbol, this.#sem.symbol.count));
  }
  get references() {
    return (this.#references ??= this.#rows(Reference, this.#sem.reference.count));
  }
  get imports() {
    return (this.#imports ??= this.#rows(Import, this.#sem.import.count));
  }
  get exports() {
    return (this.#exports ??= this.#rows(Export, this.#sem.export.count));
  }
  get unresolvedReferences() {
    return (this.#unresolved ??= this.references.filter((r) => r.symbol === null));
  }

  get rootScope() {
    const scopes = this.scopes;
    if (scopes.length > 1 && scopes[1].kind === "module") return scopes[1];
    return scopes[0];
  }

  get dependencies() {
    this.analyzer._ensureLinked();
    return this._deps;
  }
  get dependents() {
    this.analyzer._ensureLinked();
    return this._dependents;
  }

  symbolOf(node) {
    const index = this.#r.indexOf(node);
    return index === undefined ? null : this._symbolByIndex(index);
  }

  referenceOf(node) {
    const index = this.#r.indexOf(node);
    return index === undefined ? null : this._referenceByIndex(index);
  }

  scopeOf(node) {
    const index = this.#r.indexOf(node);
    // a node inserted after analysis has no recorded scope
    if (index === undefined) return this.rootScope;
    return this.scopes[this.#sem.nodeScope(index)];
  }

  // structural parent, or null at the root or for a foreign node
  parentOf(node) {
    const index = this.#r.indexOf(node);
    if (index === undefined) return null;
    const parent = this.#r.parentIndex(index);
    return parent < 0 ? null : this.#r.nodeOf(parent);
  }

  resolve(name, from = this.rootScope) {
    for (let s = from; s; s = s.parent) {
      const found = s.find(name);
      if (found) return found;
    }
    return null;
  }

  // GetExportedNames, 16.2.1.7.2.1
  exportedNames(exportStarSet = new Set()) {
    if (exportStarSet.has(this)) return [];
    exportStarSet.add(this);
    this.analyzer._ensureLinked();
    const names = new Set(this._exportMap().keys());
    for (const star of this._starExports()) {
      if (star._resolved === null) continue;
      for (const name of star._resolved.exportedNames(exportStarSet)) {
        if (name !== "default") names.add(name);
      }
    }
    return [...names];
  }

  // outer bindings a function closes over, deduped by symbol
  capturesOf(fn) {
    const index = this.#r.indexOf(fn);
    if (index === undefined) {
      throw new TypeError("capturesOf: node does not belong to this module's AST");
    }
    const { scope, symbol, reference } = this.#sem;
    const fnScopeId = this.#sem.nodeScope(index);
    if (scope.kind(fnScopeId) !== "function" || scope.nodeIndex(fnScopeId) !== index) {
      throw new TypeError("capturesOf: node does not create a function scope");
    }
    const start = this.#r.startOf(index);
    const end = this.#r.endOf(index);
    const captures = new Map();
    for (let i = 0; i < reference.count; i++) {
      const symbolId = reference.symbolId(i);
      if (symbolId === null || reference.kind(i) === "type") continue;
      if (reference.start(i) < start || reference.end(i) > end) continue;
      let inside = false;
      for (let s = symbol.scopeId(symbolId); s !== null; s = scope.parentId(s)) {
        if (s === fnScopeId) {
          inside = true;
          break;
        }
      }
      if (inside) continue;
      let capture = captures.get(symbolId);
      if (capture === undefined) {
        capture = { symbol: this.symbols[symbolId], references: [], isWritten: false };
        captures.set(symbolId, capture);
      }
      capture.references.push(this.references[i]);
      if (reference.isWrite(i)) capture.isWritten = true;
    }
    return [...captures.values()];
  }

  walk(visitor, root) {
    walkModule(this, visitor, root);
  }

  findAll(types) {
    const single = typeof types === "string" ? types : null;
    const set = single === null ? new Set(types) : null;
    const out = [];
    this.walk({
      enter(node) {
        if (single === null ? set.has(node.type) : node.type === single) out.push(node);
      },
    });
    return out;
  }

  _symbolByIndex(index) {
    const declared = this.#declMap().get(index);
    if (declared !== undefined) return this.symbols[declared];
    const ref = this.#refMap().get(index);
    return ref !== undefined ? this.references[ref].symbol : null;
  }

  _referenceByIndex(index) {
    const ref = this.#refMap().get(index);
    return ref !== undefined ? this.references[ref] : null;
  }

  _scopeBindings(scopeId) {
    if (this.#scopeBindings === null) {
      const lists = Array.from({ length: this.#sem.scope.count }, () => []);
      for (const symbol of this.symbols) {
        lists[this.#sem.symbol.scopeId(symbol.id)].push(symbol);
      }
      this.#scopeBindings = lists;
    }
    return this.#scopeBindings[scopeId];
  }

  _referencesOfSymbol(symbolId) {
    if (this.#symbolReferences === null) {
      const lists = Array.from({ length: this.#sem.symbol.count }, () => []);
      for (const ref of this.references) {
        const s = this.#sem.reference.symbolId(ref.id);
        if (s !== null) lists[s].push(ref);
      }
      this.#symbolReferences = lists;
    }
    return this.#symbolReferences[symbolId];
  }

  // export-entry partition (ParseModule, 16.2.1.7.1)
  _exportMap() {
    if (this.#exportMap === null) {
      const map = new Map();
      const stars = [];
      for (const record of this.exports) {
        if (record.isStar) stars.push(record);
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

  _importOfSymbol(symbolId) {
    if (this.#importBySymbol === null) {
      const map = new Map();
      for (const record of this.imports) {
        const local = record.local;
        if (local !== null) map.set(local.id, record);
      }
      this.#importBySymbol = map;
    }
    return this.#importBySymbol.get(symbolId);
  }

  #rows(Row, count) {
    const out = Array.from({ length: count });
    for (let i = 0; i < count; i++) out[i] = new Row(this, this.#sem, i);
    return out;
  }

  #declMap() {
    if (this.#declToSymbol === null) {
      const map = new Map();
      const { symbol } = this.#sem;
      for (let s = 0; s < symbol.count; s++) {
        const len = symbol.declCount(s);
        for (let i = 0; i < len; i++) map.set(symbol.declNodeIndex(s, i), s);
      }
      this.#declToSymbol = map;
    }
    return this.#declToSymbol;
  }

  #refMap() {
    if (this.#nodeToReference === null) {
      const map = new Map();
      const { reference } = this.#sem;
      for (let i = 0; i < reference.count; i++) map.set(reference.nodeIndex(i), i);
      this.#nodeToReference = map;
    }
    return this.#nodeToReference;
  }
}

export { SymbolFlags };
