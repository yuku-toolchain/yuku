import { load } from "yuku-core";
import { BindingFlags } from "./decode.js";
import { Module } from "./module.js";

// probed in TypeScript's order
const EXTENSIONS = [".ts", ".tsx", ".d.ts", ".js", ".jsx", ".mts", ".mjs", ".cts", ".cjs"];

// `./a.js` imports `a.ts`
const SOURCE_EXTENSIONS = new Map([
  [".js", [".ts", ".tsx", ".d.ts"]],
  [".jsx", [".tsx"]],
  [".mjs", [".mts", ".d.mts"]],
  [".cjs", [".cts", ".d.cts"]],
]);

const AMBIGUOUS = Symbol("ambiguous");

export class Analyzer {
  #core;
  #modules = new Map();
  #resolve;
  #diagnostics = [];
  #dirty = false;
  // defining binding to the import bindings that resolve to it
  #importers = new Map();
  #exportResolutions = new Map();
  // each binding a module augmentation merges to the whole merge, the augmented binding first
  #merged = new Map();
  // the names module augmentations add to a module
  #addedExports = new Map();

  constructor(options = {}) {
    this.#core = options.core ?? load();
    this.#resolve = options.resolve ?? defaultResolve(this.#modules);
  }

  setFile(path, source, options) {
    const module = new Module(this, this.#core, path, source, options);
    this.#modules.set(path, module);
    this.#dirty = true;
    return module;
  }

  deleteFile(path) {
    const deleted = this.#modules.delete(path);
    if (deleted) this.#dirty = true;
    return deleted;
  }

  module(path) {
    return this.#modules.get(path);
  }

  get modules() {
    return this.#modules;
  }

  get diagnostics() {
    this._link();
    return this.#diagnostics;
  }

  link() {
    this.#dirty = false;
    this.#exportResolutions = new Map();
    this.#merged = new Map();
    this.#addedExports = new Map();
    const diagnostics = [];
    for (const module of this.#modules.values()) {
      diagnostics.push(...module.diagnostics);
      module._deps = [];
      module._dependents = [];
    }
    for (const module of this.#modules.values()) {
      const resolved = new Map();
      for (const record of [...module.imports, ...module.exports]) {
        record._resolved = null;
        if (record.specifier === null) continue;
        if (!resolved.has(record.specifier)) {
          resolved.set(record.specifier, this.#resolveModule(module, record, diagnostics));
        }
        record._resolved = resolved.get(record.specifier);
        if (record._resolved !== null) wire(module, record._resolved);
      }
    }
    for (const module of this.#modules.values()) {
      for (const record of module.imports) {
        if (record.kind !== "augmentation" || record._resolved === null) continue;
        for (const binding of record._scope.bindings) {
          if (binding.has(BindingFlags.Exported)) this.#augment(record._resolved, binding);
        }
      }
    }
    for (const module of this.#modules.values()) {
      for (const record of module.imports) {
        if (record._resolved !== null && record.name !== null) {
          this.#validate(module, record, record.name, "Import", diagnostics);
        }
      }
      for (const record of module.exports) {
        if (record._resolved !== null && record.fromName !== null) {
          this.#validate(module, record, record.fromName, "Re-export", diagnostics);
        }
      }
    }
    this.#importers = new Map();
    for (const module of this.#modules.values()) {
      for (const record of module.imports) {
        const origin = record.local === null ? null : this._definitionOf(record.local);
        if (origin === null || origin.binding === null) continue;
        const importers = this.#importers.get(origin.binding) ?? [];
        importers.push(record.local);
        this.#importers.set(origin.binding, importers);
      }
    }
    this.#diagnostics = diagnostics;
  }

  _link(module) {
    if (module !== undefined && !module.isCurrent) {
      throw new Error(`'${module.path}' was replaced or deleted, query its current module`);
    }
    if (this.#dirty) this.link();
  }

  _definitionOf(binding) {
    this._link(binding.module);
    const seen = new Set();
    let current = binding;
    while (current.has(BindingFlags.Import)) {
      if (seen.has(current)) return null;
      seen.add(current);
      const record = current.module._importOf(current);
      if (record === undefined || record._resolved === null) return null;
      if (record.isNamespace) return this.#definition(record._resolved, null);
      const resolution = this.#exportResolution(record._resolved, record.name);
      if (resolution === null || resolution === AMBIGUOUS) return null;
      if (resolution.namespace) return this.#definition(resolution.module, null);
      if (resolution.binding === null) return null;
      current = resolution.binding;
    }
    const target = this.#merged.get(current)?.[0] ?? current;
    return this.#definition(target.module, target);
  }

  _referencesOf(binding) {
    const definition = this._definitionOf(binding);
    const origin = definition?.binding ?? binding;
    const references = [...origin.references];
    for (const augmentation of definition?.augmentations ?? []) {
      references.push(...augmentation.references);
    }
    for (const local of this.#importers.get(origin) ?? []) references.push(...local.references);
    return references;
  }

  _addedExports(module) {
    return this.#addedExports.get(module)?.keys() ?? [];
  }

  _resolveExport(module, name) {
    this._link(module);
    const resolution = this.#exportResolution(module, name);
    if (resolution === null || resolution === AMBIGUOUS) return null;
    if (resolution.namespace) return this.#definition(resolution.module, null);
    if (resolution.binding === null) return null;
    return this.#definition(resolution.module, resolution.binding);
  }

  #definition(module, binding) {
    const merged = binding === null ? undefined : this.#merged.get(binding);
    return { module, binding, augmentations: merged === undefined ? [] : merged.slice(1) };
  }

  // a name the module lacks becomes its export
  #augment(module, binding) {
    const name = binding.has(BindingFlags.Default) ? "default" : binding.name;
    const resolution = this.#resolveExport(module, name, []);
    if (resolution === null) {
      const added = this.#addedExports.get(module) ?? new Map();
      this.#addedExports.set(module, added.set(name, binding));
    } else if (resolution !== AMBIGUOUS && resolution.binding !== null) {
      const merged = this.#merged.get(resolution.binding) ?? [resolution.binding];
      merged.push(binding);
      this.#merged.set(resolution.binding, merged).set(binding, merged);
    }
  }

  #resolveModule(module, record, diagnostics) {
    const resolved = this.#resolve(record.specifier, module.path);
    if (resolved === false) return null;
    const target = typeof resolved === "string" ? this.#modules.get(resolved) : undefined;
    if (target !== undefined) return target;
    const message =
      typeof resolved === "string"
        ? `'${record.specifier}' resolves to '${resolved}', which is not in the project`
        : `Cannot resolve '${record.specifier}'`;
    diagnostics.push(diagnostic("warning", message, module, specifierNodeOf(module, record.node)));
    return null;
  }

  #validate(module, record, name, what, diagnostics) {
    const resolution = this.#exportResolution(record._resolved, name);
    if (resolution !== null && resolution !== AMBIGUOUS) return;
    const message =
      resolution === null
        ? `Module '${record.specifier}' has no export '${name}'`
        : `${what} '${name}' of module '${record.specifier}' is ambiguous: ` +
          "multiple 'export *' declarations supply it";
    diagnostics.push(diagnostic("error", message, module, record.node));
  }

  #exportResolution(module, name) {
    let resolutions = this.#exportResolutions.get(module);
    if (resolutions === undefined) {
      resolutions = new Map();
      this.#exportResolutions.set(module, resolutions);
    }
    let resolution = resolutions.get(name);
    if (resolution === undefined) {
      resolution = this.#resolveExport(module, name, []);
      resolutions.set(name, resolution);
    }
    return resolution;
  }

  // ResolveExport, 16.2.1.7.2.2
  #resolveExport(module, name, seen) {
    for (const entry of seen) if (entry.module === module && entry.name === name) return null;
    seen.push({ module, name });

    const direct = module._exportMap().get(name);
    if (direct !== undefined) {
      if (direct.specifier === null) {
        const local = direct.local;
        const record = local?.has(BindingFlags.Import) ? module._importOf(local) : undefined;
        if (record === undefined) return { module, binding: local, namespace: false };
        if (record._resolved === null) return null;
        if (record.isNamespace) {
          return { module: record._resolved, binding: null, namespace: true };
        }
        return this.#resolveExport(record._resolved, record.name, seen);
      }
      if (direct._resolved === null) return null;
      if (direct.kind === "namespace") {
        return { module: direct._resolved, binding: null, namespace: true };
      }
      return this.#resolveExport(direct._resolved, direct.fromName, seen);
    }

    const added = this.#addedExports.get(module)?.get(name);
    if (added !== undefined) return { module: added.module, binding: added, namespace: false };

    // default never crosses export *
    if (name === "default") return null;

    let found = null;
    for (const star of module._starExports()) {
      if (star._resolved === null) continue;
      const resolution = this.#resolveExport(star._resolved, name, seen);
      if (resolution === AMBIGUOUS) return AMBIGUOUS;
      if (resolution === null) continue;
      if (found === null) {
        found = resolution;
      } else if (
        resolution.module !== found.module ||
        resolution.binding !== found.binding ||
        resolution.namespace !== found.namespace
      ) {
        return AMBIGUOUS;
      }
    }
    return found;
  }
}

function diagnostic(severity, message, module, node) {
  return {
    severity,
    message,
    path: module.path,
    start: node.start,
    end: node.end,
    labels: [],
    help: null,
  };
}

function wire(from, to) {
  if (!from._deps.includes(to)) from._deps.push(to);
  if (!to._dependents.includes(from)) to._dependents.push(from);
}

function specifierNodeOf(module, node) {
  switch (node.type) {
    case "ImportSpecifier":
    case "ImportDefaultSpecifier":
    case "ImportNamespaceSpecifier":
    case "ExportSpecifier":
      return module.parentOf(node).source;
    case "TSImportEqualsDeclaration":
      return node.moduleReference.expression;
    case "TSModuleDeclaration":
      return node.id;
    case "CallExpression":
      return node.arguments[0];
    default:
      return node.source;
  }
}

// a bare specifier is a package, and an unmatched asset such as `./app.css` is external
function defaultResolve(modules) {
  return (specifier, importer) => {
    if (!specifier.startsWith(".")) return false;
    const slash = importer.lastIndexOf("/");
    const base = joinPath(slash === -1 ? "" : importer.slice(0, slash), specifier);
    if (modules.has(base)) return base;
    const extension = /\.[^./]+$/.exec(base)?.[0] ?? null;
    for (const source of SOURCE_EXTENSIONS.get(extension) ?? []) {
      const path = base.slice(0, -extension.length) + source;
      if (modules.has(path)) return path;
    }
    for (const probe of EXTENSIONS) {
      if (modules.has(base + probe)) return base + probe;
    }
    for (const probe of EXTENSIONS) {
      if (modules.has(`${base}/index${probe}`)) return `${base}/index${probe}`;
    }
    return extension === null || EXTENSIONS.includes(extension) ? null : false;
  };
}

function joinPath(directory, relative) {
  const parts = directory === "" ? [] : directory.split("/");
  for (const part of relative.split("/")) {
    if (part === "..") parts.pop();
    else if (part !== "" && part !== ".") parts.push(part);
  }
  return parts.join("/");
}
