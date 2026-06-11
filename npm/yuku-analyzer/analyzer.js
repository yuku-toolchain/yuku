import { Module, SymbolFlags } from "./module.js";

const RESOLVE_EXTENSIONS = [".tsx", ".ts", ".jsx", ".js", ".mts", ".mjs", ".cts", ".cjs"];

// the third resolution outcome of ResolveExport (16.2.1.7.2.2): more
// than one `export *` declaration supplies the name through different
// bindings. distinct from null (not found / circular).
const AMBIGUOUS = Symbol("ambiguous");

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

  // the static half of spec linking. resolves every module request to an
  // added module, wires the dependency graph, and reports binding
  // validation failures as diagnostics rather than throwing. unresolvable
  // specifiers are skipped so a partial graph still links.
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
        // `export * as ns` (fromName null) trivially resolves and bare
        // `export *` conflicts are deferred to use sites, per spec
        if (record.fromName !== null) {
          this.#validate(module, record, record.fromName, "Re-export");
        }
      }
    }
    this.#linking = false;
  }

  // reports a record whose name does not resolve, or resolves
  // ambiguously, against its (already resolved) source module
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

  // follows an import binding to its defining module and symbol via
  // ResolveExport (InitializeEnvironment step 7.c). namespace
  // resolutions yield { module, symbol: null }; unresolved or ambiguous
  // chains yield null.
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

  // ResolveExport(exportName, resolveSet), 16.2.1.7.2.2: resolves a
  // name to the binding that defines it, following named re-export and
  // `export *` chains. returns { module, symbol, namespace } (the
  // spec's ResolvedBinding, with namespace: true for its namespace
  // [[BindingName]]), null when unresolvable or circular, or AMBIGUOUS.
  //
  // local and indirect entries live in one name-keyed map (_exportMap);
  // duplicate export names are an early error, so the spec's step 5/6
  // partition cannot change the outcome. ParseModule step 10.a.ii
  // (rewriting an export of an imported binding through its original
  // module) is applied at resolution time instead of collection time.
  #resolveExport(module, exportName, resolveSet) {
    // step 3: a repeated (module, exportName) pair is a circular import
    // request. the pair key still permits re-entering a module for a
    // different name, which renaming re-export chains legitimately do.
    for (const entry of resolveSet) {
      if (entry.module === module && entry.exportName === exportName) return null;
    }
    resolveSet.push({ module, exportName });

    const direct = module._exportMap().get(exportName);
    if (direct !== undefined) {
      if (direct.specifier === null) {
        const local = direct.local;
        if (local !== null && (local.flags & SymbolFlags.Import) !== 0) {
          // export of an imported binding: resolve through the original
          // module (the ParseModule rewrite)
          const record = module._importOfSymbol(local.id);
          if (record !== undefined) {
            const imported =
              record._resolved ?? this.#resolveModule(record.specifier, module, true);
            if (imported === null) return null;
            if (record.isNamespace) return { module: imported, symbol: null, namespace: true };
            return this.#resolveExport(imported, record.name, resolveSet);
          }
        }
        // step 5: the module provides the direct binding
        return { module, symbol: local, namespace: false };
      }
      const next = direct._resolved ?? this.#resolveModule(direct.specifier, module, true);
      if (next === null) return null;
      // step 6: `export * as ns` binds the namespace, named re-exports
      // follow the chain under the source name
      if (direct.isNamespaceReexport) return { module: next, symbol: null, namespace: true };
      return this.#resolveExport(next, direct.fromName, resolveSet);
    }

    // step 7: "default" is never satisfied through `export *`
    if (exportName === "default") return null;

    // steps 8-9: identical star bindings collapse, different ones are
    // ambiguous
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
