import { Module, SymbolFlags } from "./module.js";

const RESOLVE_EXTENSIONS = [".tsx", ".ts", ".jsx", ".js", ".mts", ".mjs", ".cts", ".cjs"];

// name supplied ambiguously by multiple export * (ResolveExport, 16.2.1.7.2.2)
const AMBIGUOUS = Symbol("ambiguous");

export class Analyzer {
  #modules = new Map();
  #resolve;
  #diagnostics = [];
  #dirty = false;
  #linking = false;
  // origin defining symbol -> importing local symbols that resolve to it,
  // across every module. built once per link so referencesOf is a lookup
  // rather than a per-call walk over every import in the graph.
  #importersByOrigin = new Map();

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

  // static spec linking, reports failures as diagnostics rather than throwing
  link() {
    this.#dirty = false;
    this.#linking = true;
    this.#diagnostics = [];
    for (const module of this.#modules.values()) {
      module._deps = [];
      module._dependents = [];
      // drop earlier resolutions so removed modules cannot leak through
      for (const record of module.imports) record._resolved = null;
      for (const record of module.exports) record._resolved = null;
    }
    for (const module of this.#modules.values()) {
      for (const record of module.imports) {
        record._resolved = this.#resolveModule(record.specifier, module, false);
        if (record._resolved === null) continue;
        this.#wire(module, record._resolved);
        // namespace and side-effect imports name nothing to validate
        if (record.name !== null) {
          this.#validate(module, record, record.name, "Import");
        }
      }
      for (const record of module.exports) {
        if (record.specifier === null) continue;
        record._resolved = this.#resolveModule(record.specifier, module, false);
        if (record._resolved === null) continue;
        this.#wire(module, record._resolved);
        // export * as ns resolves trivially, bare export * conflicts deferred to use sites
        if (record.fromName !== null) {
          this.#validate(module, record, record.fromName, "Re-export");
        }
      }
    }
    this.#indexImporters();
    this.#linking = false;
  }

  // buckets every importing local under the origin symbol it resolves to,
  // computing each definitionOf once. referencesOf then reads the bucket
  // instead of re-walking the whole import graph on every call.
  #indexImporters() {
    this.#importersByOrigin = new Map();
    for (const module of this.#modules.values()) {
      for (const record of module.imports) {
        const local = record.local;
        if (local === null) continue;
        const definition = this.definitionOf(local);
        // a symbol object is unique to its (module, id), so the origin
        // symbol alone keys the bucket; the module is implied.
        if (definition === null || definition.symbol === null) continue;
        let importers = this.#importersByOrigin.get(definition.symbol);
        if (importers === undefined) {
          importers = [];
          this.#importersByOrigin.set(definition.symbol, importers);
        }
        importers.push(local);
      }
    }
  }

  // report a record whose name fails to resolve or resolves ambiguously
  #validate(module, record, name, what) {
    const resolution = this.#resolveExport(record._resolved, name, []);
    if (resolution !== null && resolution !== AMBIGUOUS) return;
    const message =
      resolution === null
        ? `Module '${record.specifier}' has no export '${name}'`
        : `${what} '${name}' of module '${record.specifier}' is ambiguous: multiple 'export *' declarations supply it`;
    this.#diagnostics.push({
      severity: "error",
      message,
      module: module.path,
      start: record.node.start,
      end: record.node.end,
    });
  }

  // follow an import binding to its defining module and symbol (ResolveExport, InitializeEnvironment 7.c)
  definitionOf(symbol) {
    this._ensureLinked();
    let module = symbol.module;
    let current = symbol;
    const seen = new Set();
    while ((current.flags & SymbolFlags.Import) !== 0) {
      const key = `${module.path}:${current.id}`;
      if (seen.has(key)) return null;
      seen.add(key);
      const record = module._importOfSymbol(current.id);
      if (record === undefined || record._resolved === null) return null;
      if (record.isNamespace || record.name === null) {
        return { module: record._resolved, symbol: null };
      }
      const resolution = this.#resolveExport(record._resolved, record.name, []);
      if (resolution === null || resolution === AMBIGUOUS) return null;
      if (resolution.namespace) return { module: resolution.module, symbol: null };
      if (resolution.symbol === null) return null;
      module = resolution.module;
      current = resolution.symbol;
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
    const importers = this.#importersByOrigin.get(origin.symbol);
    if (importers !== undefined) {
      for (const local of importers) {
        // a self-import resolving back to the origin module is already
        // covered by the direct references above
        if (local.module === origin.module) continue;
        for (const reference of local.references) {
          out.push({ module: local.module, reference });
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

  // ResolveExport, 16.2.1.7.2.2
  #resolveExport(module, exportName, resolveSet) {
    for (const entry of resolveSet) {
      if (entry.module === module && entry.exportName === exportName) return null;
    }
    resolveSet.push({ module, exportName });

    const direct = module._exportMap().get(exportName);
    if (direct !== undefined) {
      if (direct.specifier === null) {
        const local = direct.local;
        if (local !== null && (local.flags & SymbolFlags.Import) !== 0) {
          // export of an imported binding resolves through the original module (ParseModule 10.a.ii)
          const record = module._importOfSymbol(local.id);
          if (record !== undefined) {
            const imported =
              record._resolved ?? this.#resolveModule(record.specifier, module, true);
            if (imported === null) return null;
            if (record.isNamespace) return { module: imported, symbol: null, namespace: true };
            return this.#resolveExport(imported, record.name, resolveSet);
          }
        }
        return { module, symbol: local, namespace: false };
      }
      const next = direct._resolved ?? this.#resolveModule(direct.specifier, module, true);
      if (next === null) return null;
      // namespace re-export binds the namespace, named ones follow the chain
      if (direct.isNamespaceReexport) return { module: next, symbol: null, namespace: true };
      return this.#resolveExport(next, direct.fromName, resolveSet);
    }

    // default never crosses export *
    if (exportName === "default") return null;

    // identical star bindings collapse, otherwise ambiguous
    let starResolution = null;
    for (const star of module._starExports()) {
      const next = star._resolved ?? this.#resolveModule(star.specifier, module, true);
      if (next === null) continue;
      const resolution = this.#resolveExport(next, exportName, resolveSet);
      if (resolution === AMBIGUOUS) return AMBIGUOUS;
      if (resolution === null) continue;
      if (starResolution === null) {
        starResolution = resolution;
      } else if (
        resolution.module !== starResolution.module ||
        resolution.namespace !== starResolution.namespace ||
        resolution.symbol !== starResolution.symbol
      ) {
        return AMBIGUOUS;
      }
    }
    return starResolution;
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
