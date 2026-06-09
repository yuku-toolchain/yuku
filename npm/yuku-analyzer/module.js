import binding from "./binding.js";
import { decode, SEM, SymbolFlags, SCOPE_KINDS, NAME_KINDS, IMPORT_PHASES } from "./decode.js";
import { walkModule } from "./walk.js";

const NONE = -1;

// per-section wire layouts, generated from the zig structs
const SCOPE = SEM.scope;
const SYMBOL = SEM.symbol;
const REF = SEM.reference;
const IMP = SEM.import;
const EXP = SEM.export;

const VALUE_SPACE =
  SymbolFlags.FunctionScopedVariable |
  SymbolFlags.BlockScopedVariable |
  SymbolFlags.Function |
  SymbolFlags.Class |
  SymbolFlags.RegularEnum |
  SymbolFlags.ConstEnum |
  SymbolFlags.ValueModule;

const TYPE_SPACE =
  SymbolFlags.Class |
  SymbolFlags.RegularEnum |
  SymbolFlags.ConstEnum |
  SymbolFlags.Interface |
  SymbolFlags.TypeAlias |
  SymbolFlags.TypeParameter;

const VARIABLE = SymbolFlags.FunctionScopedVariable | SymbolFlags.BlockScopedVariable;
const IMPORT_FLAGS = SymbolFlags.Import | SymbolFlags.TypeImport;

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
  #o;
  constructor(module, id, offset) {
    this.module = module;
    this.id = id;
    this.#o = offset;
  }
  get kind() {
    return SCOPE_KINDS[this.module._.sem.scopes[this.#o + SCOPE.bits] & SCOPE.kindMask];
  }
  get strict() {
    return ((this.module._.sem.scopes[this.#o + SCOPE.bits] >> SCOPE.strictBit) & 1) !== 0;
  }
  get node() {
    return this.module._.r.nodeOf(this.module._.sem.scopes[this.#o + SCOPE.node]);
  }
  get parent() {
    const p = this.module._.sem.scopes[this.#o + SCOPE.parent];
    return p === NONE ? null : this.module.scopes[p];
  }
  get hoistTarget() {
    return this.module.scopes[this.module._.sem.scopes[this.#o + SCOPE.hoistTarget]];
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
  #o;
  constructor(module, id, offset) {
    this.module = module;
    this.id = id;
    this.#o = offset;
  }
  get name() {
    const v = this.module._.sem.symbols;
    return this.module._.r.str(v[this.#o + SYMBOL.nameStart], v[this.#o + SYMBOL.nameEnd]);
  }
  get flags() {
    return this.module._.sem.symbols[this.#o + SYMBOL.flags];
  }
  get scope() {
    return this.module.scopes[this.module._.sem.symbols[this.#o + SYMBOL.scope]];
  }
  get declarations() {
    const v = this.module._.sem.symbols;
    const start = v[this.#o + SYMBOL.declsStart];
    const len = v[this.#o + SYMBOL.declsLen];
    const out = new Array(len);
    for (let i = 0; i < len; i++) {
      out[i] = this.module._.r.nodeOf(this.module._.sem.declNodes[start + i]);
    }
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
  get isVariable() {
    return (this.flags & VARIABLE) !== 0;
  }
  get isFunction() {
    return (this.flags & SymbolFlags.Function) !== 0;
  }
  get isClass() {
    return (this.flags & SymbolFlags.Class) !== 0;
  }
  get isImported() {
    return (this.flags & IMPORT_FLAGS) !== 0;
  }
  get isExported() {
    return (this.flags & SymbolFlags.Exported) !== 0;
  }
  get isConst() {
    return (this.flags & SymbolFlags.Const) !== 0;
  }
  get isParameter() {
    return (this.flags & SymbolFlags.Parameter) !== 0;
  }
  get isCatchParam() {
    return (this.flags & SymbolFlags.CatchVariable) !== 0;
  }
  get isTypeOnly() {
    return (this.flags & SymbolFlags.TypeImport) !== 0;
  }
  get isDefaultExport() {
    return (this.flags & SymbolFlags.Default) !== 0;
  }
  get inValueSpace() {
    return (this.flags & VALUE_SPACE) !== 0;
  }
  get inTypeSpace() {
    return (this.flags & TYPE_SPACE) !== 0;
  }
  definition() {
    return this.module.analyzer.definitionOf(this);
  }
}

class Reference {
  #o;
  constructor(module, id, offset) {
    this.module = module;
    this.id = id;
    this.#o = offset;
  }
  get name() {
    const v = this.module._.sem.references;
    return this.module._.r.str(v[this.#o + REF.nameStart], v[this.#o + REF.nameEnd]);
  }
  get scope() {
    return this.module.scopes[this.module._.sem.references[this.#o + REF.scope]];
  }
  get node() {
    return this.module._.r.nodeOf(this.module._.sem.references[this.#o + REF.node]);
  }
  get kind() {
    const bits = this.module._.sem.references[this.#o + REF.bits];
    return (bits >> REF.typeBit) & 1 ? "type" : "value";
  }
  get isWrite() {
    const bits = this.module._.sem.references[this.#o + REF.bits];
    return ((bits >> REF.writeBit) & 1) !== 0;
  }
  get symbol() {
    const s = this.module._.sem.references[this.#o + REF.symbol];
    return s === NONE ? null : this.module.symbols[s];
  }
}

class Import {
  #o;
  constructor(module, offset) {
    this.module = module;
    this.#o = offset;
    this._resolved = null;
  }
  get #nameKind() {
    return NAME_KINDS[this.module._.sem.imports[this.#o + IMP.bits] & IMP.nameKindMask];
  }
  get local() {
    const s = this.module._.sem.imports[this.#o + IMP.symbol];
    return s === NONE ? null : this.module.symbols[s];
  }
  get name() {
    if (this.#nameKind !== "named") return null;
    const v = this.module._.sem.imports;
    return this.module._.r.str(v[this.#o + IMP.nameStart], v[this.#o + IMP.nameEnd]);
  }
  get isNamespace() {
    return this.#nameKind === "star" && this.local !== null;
  }
  get isSideEffect() {
    return this.#nameKind === "none";
  }
  get typeOnly() {
    return ((this.module._.sem.imports[this.#o + IMP.bits] >> IMP.typeBit) & 1) !== 0;
  }
  get phase() {
    const bits = this.module._.sem.imports[this.#o + IMP.bits];
    if (!((bits >> IMP.hasPhaseBit) & 1)) return null;
    return IMPORT_PHASES[(bits >> IMP.phaseBit) & 1];
  }
  get specifier() {
    const v = this.module._.sem.imports;
    return this.module._.r.str(v[this.#o + IMP.specifierStart], v[this.#o + IMP.specifierEnd]);
  }
  get node() {
    return this.module._.r.nodeOf(this.module._.sem.imports[this.#o + IMP.node]);
  }
  get resolvedModule() {
    this.module.analyzer._ensureLinked();
    return this._resolved;
  }
}

class Export {
  #o;
  constructor(module, offset) {
    this.module = module;
    this.#o = offset;
    this._resolved = null;
  }
  get #nameKind() {
    return NAME_KINDS[this.module._.sem.exports[this.#o + EXP.bits] & EXP.nameKindMask];
  }
  get #fromKind() {
    return NAME_KINDS[
      (this.module._.sem.exports[this.#o + EXP.bits] >> EXP.fromKindShift) & EXP.nameKindMask
    ];
  }
  get name() {
    if (this.#nameKind !== "named") return null;
    const v = this.module._.sem.exports;
    return this.module._.r.str(v[this.#o + EXP.nameStart], v[this.#o + EXP.nameEnd]);
  }
  get isStar() {
    return this.#nameKind === "star";
  }
  get typeOnly() {
    return ((this.module._.sem.exports[this.#o + EXP.bits] >> EXP.typeBit) & 1) !== 0;
  }
  get local() {
    const s = this.module._.sem.exports[this.#o + EXP.symbol];
    return s === NONE ? null : this.module.symbols[s];
  }
  get specifier() {
    if (this.#fromKind === "none") return null;
    const v = this.module._.sem.exports;
    return this.module._.r.str(v[this.#o + EXP.specifierStart], v[this.#o + EXP.specifierEnd]);
  }
  get fromName() {
    if (this.#fromKind !== "named") return null;
    const v = this.module._.sem.exports;
    return this.module._.r.str(v[this.#o + EXP.fromNameStart], v[this.#o + EXP.fromNameEnd]);
  }
  get isNamespaceReexport() {
    return this.#fromKind === "star";
  }
  get node() {
    return this.module._.r.nodeOf(this.module._.sem.exports[this.#o + EXP.node]);
  }
  get resolvedModule() {
    this.module.analyzer._ensureLinked();
    return this._resolved;
  }
}

export class Module {
  constructor(analyzer, path, source, options = {}) {
    this.analyzer = analyzer;
    this.path = path;
    this.source = source;
    const r = decode(
      binding.analyze(source, {
        lang: options.lang ?? langFromPath(path),
        sourceType: options.sourceType ?? sourceTypeFromPath(path),
        preserveParens: options.preserveParens,
        allowReturnOutsideFunction: options.allowReturnOutsideFunction,
        attachComments: options.attachComments,
      }),
      source,
    );
    this._ = {
      r,
      sem: r.semantic,
      scopes: null,
      symbols: null,
      references: null,
      imports: null,
      exports: null,
      unresolved: null,
      scopeBindings: null,
      symbolRefs: null,
      declToSymbol: null,
      nodeToRef: null,
      fnScopeByNode: null,
      scopeMap: null,
      exportMap: null,
      starExports: null,
      importBySymbol: null,
      deps: [],
      dependents: [],
    };
  }

  get ast() {
    return this._.r.program;
  }
  get diagnostics() {
    return this._.r.diagnostics;
  }
  get comments() {
    return this._.r.comments;
  }
  get lineStarts() {
    return this._.r.lineStarts;
  }
  locOf(offset) {
    return this._.r.locOf(offset);
  }
  locNear(offset, hintLine) {
    return this._.r.locNear(offset, hintLine);
  }

  get scopes() {
    if (this._.scopes === null) {
      const n = this._.sem.scopeCount;
      const out = new Array(n);
      for (let i = 0; i < n; i++) out[i] = new Scope(this, i, i * SCOPE.stride);
      this._.scopes = out;
    }
    return this._.scopes;
  }

  get rootScope() {
    const scopes = this.scopes;
    if (scopes.length > 1 && scopes[1].kind === "module") return scopes[1];
    return scopes[0];
  }

  get symbols() {
    if (this._.symbols === null) {
      const n = this._.sem.symbolCount;
      const out = new Array(n);
      for (let i = 0; i < n; i++) out[i] = new Symbol(this, i, i * SYMBOL.stride);
      this._.symbols = out;
    }
    return this._.symbols;
  }

  get references() {
    if (this._.references === null) {
      const n = this._.sem.referenceCount;
      const out = new Array(n);
      for (let i = 0; i < n; i++) out[i] = new Reference(this, i, i * REF.stride);
      this._.references = out;
    }
    return this._.references;
  }

  get unresolvedReferences() {
    if (this._.unresolved === null) {
      this._.unresolved = this.references.filter((r) => r.symbol === null);
    }
    return this._.unresolved;
  }

  get imports() {
    if (this._.imports === null) {
      const n = this._.sem.importCount;
      const out = new Array(n);
      for (let i = 0; i < n; i++) out[i] = new Import(this, i * IMP.stride);
      this._.imports = out;
    }
    return this._.imports;
  }

  get exports() {
    if (this._.exports === null) {
      const n = this._.sem.exportCount;
      const out = new Array(n);
      for (let i = 0; i < n; i++) out[i] = new Export(this, i * EXP.stride);
      this._.exports = out;
    }
    return this._.exports;
  }

  get dependencies() {
    this.analyzer._ensureLinked();
    return this._.deps;
  }
  get dependents() {
    this.analyzer._ensureLinked();
    return this._.dependents;
  }

  symbolOf(node) {
    const index = this._.r.indexOf(node);
    if (index === undefined) return null;
    const declared = this.#declMap().get(index);
    if (declared !== undefined) return this.symbols[declared];
    const ref = this.#refMap().get(index);
    return ref !== undefined ? this.references[ref].symbol : null;
  }

  referenceOf(node) {
    const index = this._.r.indexOf(node);
    if (index === undefined) return null;
    const ref = this.#refMap().get(index);
    return ref !== undefined ? this.references[ref] : null;
  }

  scopeOf(node) {
    const index = this._.r.indexOf(node);
    if (index === undefined) return this.rootScope;
    const start = this._.r.startOf(index);
    const end = this._.r.endOf(index);
    const scopes = this.scopes;
    const v = this._.sem.scopes;
    let best = scopes[0];
    let bestDepth = 0;
    for (let i = 1; i < scopes.length; i++) {
      const scopeNode = v[i * SCOPE.stride + SCOPE.node];
      if (this._.r.startOf(scopeNode) > start || this._.r.endOf(scopeNode) < end) continue;
      let depth = 0;
      for (let p = i; p !== NONE; p = v[p * SCOPE.stride + SCOPE.parent]) depth++;
      if (depth > bestDepth) {
        best = scopes[i];
        bestDepth = depth;
      }
    }
    return best;
  }

  resolve(name, from = this.rootScope) {
    for (let s = from; s; s = s.parent) {
      const found = s.find(name);
      if (found) return found;
    }
    return null;
  }

  capturesOf(fn) {
    const index = this._.r.indexOf(fn);
    if (index === undefined) {
      throw new TypeError("capturesOf: node does not belong to this module's AST");
    }
    const fnScopeId = this.#functionScopeOf(index);
    if (fnScopeId === undefined) {
      throw new TypeError("capturesOf: node does not create a function scope");
    }
    const start = this._.r.startOf(index);
    const end = this._.r.endOf(index);
    const v = this._.sem.references;
    const sv = this._.sem.scopes;
    const captures = new Map();
    for (let i = 0; i < this._.sem.referenceCount; i++) {
      const o = i * REF.stride;
      const symbolId = v[o + REF.symbol];
      if (symbolId === NONE) continue;
      if ((v[o + REF.bits] >> REF.typeBit) & 1) continue;
      const refNode = v[o + REF.node];
      const refStart = this._.r.startOf(refNode);
      if (refStart < start || this._.r.endOf(refNode) > end) continue;
      let scope = this._.sem.symbols[symbolId * SYMBOL.stride + SYMBOL.scope];
      let inside = false;
      while (scope !== NONE) {
        if (scope === fnScopeId) {
          inside = true;
          break;
        }
        scope = sv[scope * SCOPE.stride + SCOPE.parent];
      }
      if (inside) continue;
      let capture = captures.get(symbolId);
      if (capture === undefined) {
        capture = { symbol: this.symbols[symbolId], references: [], isWritten: false };
        captures.set(symbolId, capture);
      }
      capture.references.push(this.references[i]);
      if ((v[o + REF.bits] >> REF.writeBit) & 1) capture.isWritten = true;
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

  _scopeBindings(scopeId) {
    if (this._.scopeBindings === null) {
      const lists = new Array(this._.sem.scopeCount);
      for (let i = 0; i < lists.length; i++) lists[i] = [];
      for (const symbol of this.symbols) {
        lists[this._.sem.symbols[symbol.id * SYMBOL.stride + SYMBOL.scope]].push(symbol);
      }
      this._.scopeBindings = lists;
    }
    return this._.scopeBindings[scopeId];
  }

  _referencesOfSymbol(symbolId) {
    if (this._.symbolRefs === null) {
      const lists = new Array(this._.sem.symbolCount);
      for (let i = 0; i < lists.length; i++) lists[i] = [];
      for (const ref of this.references) {
        const s = this._.sem.references[ref.id * REF.stride + REF.symbol];
        if (s !== NONE) lists[s].push(ref);
      }
      this._.symbolRefs = lists;
    }
    return this._.symbolRefs[symbolId];
  }

  #declMap() {
    if (this._.declToSymbol === null) {
      const map = new Map();
      const v = this._.sem.symbols;
      for (let s = 0; s < this._.sem.symbolCount; s++) {
        const start = v[s * SYMBOL.stride + SYMBOL.declsStart];
        const len = v[s * SYMBOL.stride + SYMBOL.declsLen];
        for (let i = 0; i < len; i++) map.set(this._.sem.declNodes[start + i], s);
      }
      this._.declToSymbol = map;
    }
    return this._.declToSymbol;
  }

  #refMap() {
    if (this._.nodeToRef === null) {
      const map = new Map();
      const v = this._.sem.references;
      for (let i = 0; i < this._.sem.referenceCount; i++) {
        map.set(v[i * REF.stride + REF.node], i);
      }
      this._.nodeToRef = map;
    }
    return this._.nodeToRef;
  }

  // node index -> the function scope it creates. named function
  // expressions share the node with their expression_name scope; the
  // function scope has the higher id, so keep-last wins.
  #functionScopeOf(nodeIndex) {
    if (this._.fnScopeByNode === null) {
      const map = new Map();
      const v = this._.sem.scopes;
      for (let i = 0; i < this._.sem.scopeCount; i++) {
        if ((v[i * SCOPE.stride + SCOPE.bits] & SCOPE.kindMask) === 2) {
          map.set(v[i * SCOPE.stride + SCOPE.node], i);
        }
      }
      this._.fnScopeByNode = map;
    }
    return this._.fnScopeByNode.get(nodeIndex);
  }

  // scope-creating node object -> innermost Scope (keep-last covers
  // shared nodes: global/module on program, expression_name/function).
  _scopeMap() {
    if (this._.scopeMap === null) {
      const byNode = new WeakMap();
      const types = new Set();
      for (const scope of this.scopes) {
        const node = this._.r.nodeOf(this._.sem.scopes[scope.id * SCOPE.stride + SCOPE.node]);
        byNode.set(node, scope);
        types.add(node.type);
      }
      this._.scopeMap = { byNode, types };
    }
    return this._.scopeMap;
  }

  _exportMap() {
    if (this._.exportMap === null) {
      const map = new Map();
      const stars = [];
      for (const record of this.exports) {
        if (record.isStar) stars.push(record);
        else if (!map.has(record.name)) map.set(record.name, record);
      }
      this._.exportMap = map;
      this._.starExports = stars;
    }
    return this._.exportMap;
  }

  _starExports() {
    this._exportMap();
    return this._.starExports;
  }

  _importOfSymbol(symbolId) {
    if (this._.importBySymbol === null) {
      const map = new Map();
      for (const record of this.imports) {
        const local = record.local;
        if (local !== null) map.set(local.id, record);
      }
      this._.importBySymbol = map;
    }
    return this._.importBySymbol.get(symbolId);
  }
}

export { SymbolFlags, IMPORT_FLAGS };
