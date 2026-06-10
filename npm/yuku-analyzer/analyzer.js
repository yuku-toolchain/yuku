import { Module, IMPORT_FLAGS } from "./module.js";

const RESOLVE_EXTENSIONS = [".tsx", ".ts", ".jsx", ".js", ".mts", ".mjs", ".cts", ".cjs"];

export class Analyzer {
  #modules = new Map();
  #resolve;
  #diagnostics = [];
  #dirty = false;
  #linking = false;

  constructor(options = {}) {
    this.#resolve = options.resolve ?? defaultResolve(this.#modules);
  }

  addFile(path, source, options) {
    const module = new Module(this, path, source, options);
    this.#modules.set(path, module);
    this.#dirty = true;
    return module;
  }

  removeFile(path) {
    const removed = this.#modules.delete(path);
    if (removed) this.#dirty = true;
    return removed;
  }

  module(path) {
    return this.#modules.get(path);
  }

  get modules() {
    return this.#modules;
  }

  get diagnostics() {
    this._ensureLinked();
    return this.#diagnostics;
  }

  link() {
    this.#dirty = false;
    this.#linking = true;
    this.#diagnostics = [];
    for (const module of this.#modules.values()) {
      module._deps = [];
      module._dependents = [];
    }
    for (const module of this.#modules.values()) {
      for (const record of module.imports) {
        record._resolved = this.#resolveModule(record.specifier, module, false);
        if (record._resolved !== null) {
          this.#wire(module, record._resolved);
          if (record.name !== null) {
            const found = this.#findExport(record._resolved, record.name, new Set());
            if (found === null) {
              this.#diagnostics.push({
                severity: "error",
                message: `Module '${record.specifier}' has no export '${record.name}'`,
                module: module.path,
                start: record.node.start,
                end: record.node.end,
              });
            }
          }
        }
      }
      for (const record of module.exports) {
        if (record.specifier === null) continue;
        record._resolved = this.#resolveModule(record.specifier, module, false);
        if (record._resolved !== null) this.#wire(module, record._resolved);
      }
    }
    this.#linking = false;
  }

  definitionOf(symbol) {
    this._ensureLinked();
    let module = symbol.module;
    let current = symbol;
    const seen = new Set();
    while ((current.flags & IMPORT_FLAGS) !== 0) {
      const key = `${module.path}:${current.id}`;
      if (seen.has(key)) return null;
      seen.add(key);
      const record = module._importOfSymbol(current.id);
      if (record === undefined || record._resolved === null) return null;
      if (record.isNamespace || record.name === null) {
        return { module: record._resolved, symbol: null };
      }
      const found = this.#findExport(record._resolved, record.name, new Set());
      if (found === null) return null;
      if (found.namespace) return { module: found.module, symbol: null };
      if (found.export.local === null) return null;
      module = found.module;
      current = found.export.local;
    }
    return { module, symbol: current };
  }

  referencesOf(symbol) {
    this._ensureLinked();
    const origin = this.definitionOf(symbol) ?? { module: symbol.module, symbol };
    if (origin.symbol === null) return [];
    const out = [];
    for (const reference of origin.symbol.references) {
      out.push({ module: origin.module, reference });
    }
    for (const module of this.#modules.values()) {
      if (module === origin.module) continue;
      for (const record of module.imports) {
        if (record.local === null) continue;
        const definition = this.definitionOf(record.local);
        if (
          definition !== null &&
          definition.symbol === origin.symbol &&
          definition.module === origin.module
        ) {
          for (const reference of record.local.references) {
            out.push({ module, reference });
          }
        }
      }
    }
    return out;
  }

  _ensureLinked() {
    if (this.#dirty && !this.#linking) this.link();
  }

  #resolveModule(specifier, importer, quiet) {
    const resolved = this.#resolve(specifier, importer.path);
    if (resolved === null || resolved === undefined) return null;
    const module = this.#modules.get(resolved);
    if (module === undefined) {
      if (!quiet) {
        this.#diagnostics.push({
          severity: "warning",
          message: `Resolver returned '${resolved}' for '${specifier}' but no such file was added`,
          module: importer.path,
          start: 0,
          end: 0,
        });
      }
      return null;
    }
    return module;
  }

  #wire(from, to) {
    if (!from._deps.includes(to)) from._deps.push(to);
    if (!to._dependents.includes(from)) to._dependents.push(from);
  }

  // resolves `name` among `module`'s exports, following named
  // re-exports and `export *` chains. `default` is never satisfied
  // through `export *`, per spec. first match wins on ambiguous stars.
  #findExport(module, name, seen) {
    if (seen.has(module.path)) return null;
    seen.add(module.path);
    const direct = module._exportMap().get(name);
    if (direct !== undefined) {
      if (direct.specifier === null) return { module, export: direct, namespace: false };
      const next = direct._resolved ?? this.#resolveModule(direct.specifier, module, true);
      if (next === null) return null;
      if (direct.isNamespaceReexport) return { module: next, export: direct, namespace: true };
      return this.#findExport(next, direct.fromName, seen);
    }
    if (name === "default") return null;
    for (const star of module._starExports()) {
      const next = star._resolved ?? this.#resolveModule(star.specifier, module, true);
      if (next === null) continue;
      const found = this.#findExport(next, name, seen);
      if (found !== null) return found;
    }
    return null;
  }
}

function defaultResolve(modules) {
  return (specifier, importer) => {
    if (!specifier.startsWith(".")) return null;
    const base = joinPath(dirnameOf(importer), specifier);
    if (modules.has(base)) return base;
    for (const ext of RESOLVE_EXTENSIONS) {
      if (modules.has(base + ext)) return base + ext;
    }
    for (const ext of RESOLVE_EXTENSIONS) {
      const index = `${base}/index${ext}`;
      if (modules.has(index)) return index;
    }
    return null;
  };
}

function dirnameOf(path) {
  const i = path.lastIndexOf("/");
  return i === -1 ? "" : path.slice(0, i);
}

function joinPath(dir, relative) {
  const parts = dir === "" ? [] : dir.split("/");
  for (const part of relative.split("/")) {
    if (part === "" || part === ".") continue;
    if (part === "..") parts.pop();
    else parts.push(part);
  }
  return parts.join("/");
}
