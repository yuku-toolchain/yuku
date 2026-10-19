import type {
  CallExpression,
  Diagnostic,
  DiagnosticSeverity,
  FileOptions,
  ImportDeclaration,
  Node,
  TSExternalModuleReference,
  TSImportEqualsDeclaration,
  TSModuleDeclaration,
} from "../types";
import { BindingFlags } from "../core/decode";
import {
  Module,
  type Binding,
  type Definition,
  type Export,
  type Import,
  type Reference,
} from "./module";

export interface ParseOptions {
  /** @default true */
  preserveParens?: boolean;
  /** @default false */
  attachComments?: boolean;
  /** Keep every token in {@link Module.tokens}. @default false */
  tokens?: boolean;
}

export interface SetFileOptions extends Omit<FileOptions, "path">, ParseOptions {}

export interface AnalyzerOptions {
  /**
   * A Yuku build to analyze with, such as one with a dialect compiled in. See
   * https://yuku.fyi/parser/dialect/#javascript.
   * @default the module yuku ships
   */
  wasm?: WebAssembly.Module;
  /**
   * Maps a specifier to a project file path, `false` for an external module, or `null` to warn
   * that it is unresolved. Defaults to relative paths probed as TypeScript does.
   */
  resolve?: (specifier: string, importer: string) => string | false | null;
}

type Resolve = NonNullable<AnalyzerOptions["resolve"]>;

interface Resolution {
  module: Module;
  binding: Binding | null;
  namespace: boolean;
}

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

type ExportResolution = Resolution | typeof AMBIGUOUS | null;

/** A set of modules and the links between them. */
export class Analyzer {
  private wasm: WebAssembly.Module | undefined;
  private moduleByPath = new Map<string, Module>();
  private resolve: Resolve;
  private projectDiagnostics: Diagnostic[] = [];
  private dirty = false;
  private importers = new Map<Binding, Binding[]>();
  private exportResolutions = new Map<Module, Map<string, ExportResolution>>();
  // the augmented binding comes first in a merge
  private merged = new Map<Binding, Binding[]>();
  private addedExports = new Map<Module, Map<string, Binding>>();

  constructor(options: AnalyzerOptions = {}) {
    this.wasm = options.wasm;
    this.resolve = options.resolve ?? defaultResolve(this.moduleByPath);
  }

  /** Analyzes a file, replacing any module at the same path. */
  setFile(path: string, source: string | Uint8Array, options?: SetFileOptions): Module {
    const module = new Module(this, path, source, { ...options, wasm: this.wasm });
    this.moduleByPath.set(path, module);
    this.dirty = true;
    return module;
  }

  /** Returns whether the file was in the project. */
  deleteFile(path: string): boolean {
    const deleted = this.moduleByPath.delete(path);
    if (deleted) this.dirty = true;
    return deleted;
  }

  module(path: string): Module | undefined {
    return this.moduleByPath.get(path);
  }

  get modules(): ReadonlyMap<string, Module> {
    return this.moduleByPath;
  }

  /** Every module's diagnostics, and the links that fail. */
  get diagnostics(): Diagnostic[] {
    this._link();
    return this.projectDiagnostics;
  }

  /** Links the project now. Every cross-file query links on demand otherwise. */
  link(): void {
    this.dirty = false;
    this.exportResolutions = new Map();
    this.merged = new Map();
    this.addedExports = new Map();
    const diagnostics: Diagnostic[] = [];
    for (const module of this.moduleByPath.values()) {
      diagnostics.push(...module.diagnostics);
      module._deps = [];
      module._dependents = [];
    }
    for (const module of this.moduleByPath.values()) {
      const resolved = new Map<string, Module | null>();
      for (const record of [...module.imports, ...module.exports]) {
        record._resolved = null;
        if (record.specifier === null) continue;
        if (!resolved.has(record.specifier)) {
          resolved.set(record.specifier, this.resolveModule(module, record, diagnostics));
        }
        record._resolved = resolved.get(record.specifier)!;
        if (record._resolved !== null) wire(module, record._resolved);
      }
    }
    for (const module of this.moduleByPath.values()) {
      for (const record of module.imports) {
        if (record.kind !== "augmentation" || record._resolved === null) continue;
        for (const binding of record._scope.bindings) {
          if (binding.has(BindingFlags.Exported)) this.augment(record._resolved, binding);
        }
      }
    }
    for (const module of this.moduleByPath.values()) {
      for (const record of module.imports) {
        if (record._resolved !== null && record.name !== null) {
          this.validate(module, record, record.name, "Import", diagnostics);
        }
      }
      for (const record of module.exports) {
        if (record._resolved !== null && record.fromName !== null) {
          this.validate(module, record, record.fromName, "Re-export", diagnostics);
        }
      }
    }
    this.importers = new Map();
    for (const module of this.moduleByPath.values()) {
      for (const record of module.imports) {
        const origin = record.local === null ? null : this._definitionOf(record.local);
        if (origin === null || origin.binding === null) continue;
        const importers = this.importers.get(origin.binding) ?? [];
        importers.push(record.local!);
        this.importers.set(origin.binding, importers);
      }
    }
    this.projectDiagnostics = diagnostics;
  }

  /** @internal */
  _link(module?: Module): void {
    if (module !== undefined && !module.isCurrent) {
      throw new Error(`'${module.path}' was replaced or deleted, query its current module`);
    }
    if (this.dirty) this.link();
  }

  /** @internal */
  _definitionOf(binding: Binding): Definition | null {
    this._link(binding.module);
    const seen = new Set<Binding>();
    let current = binding;
    while (current.has(BindingFlags.Import)) {
      if (seen.has(current)) return null;
      seen.add(current);
      const record = current.module._importOf(current);
      if (record === undefined || record._resolved === null) return null;
      if (record.isNamespace) return this.definition(record._resolved, null);
      const resolution = this.exportResolution(record._resolved, record.name!);
      if (resolution === null || resolution === AMBIGUOUS) return null;
      if (resolution.namespace) return this.definition(resolution.module, null);
      if (resolution.binding === null) return null;
      current = resolution.binding;
    }
    const target = this.merged.get(current)?.[0] ?? current;
    return this.definition(target.module, target);
  }

  /** @internal */
  _referencesOf(binding: Binding): Reference[] {
    const definition = this._definitionOf(binding);
    const origin = definition?.binding ?? binding;
    const references = [...origin.references];
    for (const augmentation of definition?.augmentations ?? []) {
      references.push(...augmentation.references);
    }
    for (const local of this.importers.get(origin) ?? []) references.push(...local.references);
    return references;
  }

  /** @internal */
  _addedExports(module: Module): Iterable<string> {
    return this.addedExports.get(module)?.keys() ?? [];
  }

  /** @internal */
  _resolveExport(module: Module, name: string): Definition | null {
    this._link(module);
    const resolution = this.exportResolution(module, name);
    if (resolution === null || resolution === AMBIGUOUS) return null;
    if (resolution.namespace) return this.definition(resolution.module, null);
    if (resolution.binding === null) return null;
    return this.definition(resolution.module, resolution.binding);
  }

  private definition(module: Module, binding: Binding | null): Definition {
    const merged = binding === null ? undefined : this.merged.get(binding);
    return { module, binding, augmentations: merged === undefined ? [] : merged.slice(1) };
  }

  private augment(module: Module, binding: Binding): void {
    const name = binding.has(BindingFlags.Default) ? "default" : binding.name;
    const resolution = this.resolveExportRecursive(module, name, []);
    if (resolution === null) {
      const added = this.addedExports.get(module) ?? new Map<string, Binding>();
      this.addedExports.set(module, added.set(name, binding));
    } else if (resolution !== AMBIGUOUS && resolution.binding !== null) {
      const merged = this.merged.get(resolution.binding) ?? [resolution.binding];
      merged.push(binding);
      this.merged.set(resolution.binding, merged).set(binding, merged);
    }
  }

  private resolveModule(
    module: Module,
    record: Import | Export,
    diagnostics: Diagnostic[],
  ): Module | null {
    const resolved = this.resolve(record.specifier!, module.path);
    if (resolved === false) return null;
    const target = typeof resolved === "string" ? this.moduleByPath.get(resolved) : undefined;
    if (target !== undefined) return target;
    const message =
      typeof resolved === "string"
        ? `'${record.specifier}' resolves to '${resolved}', which is not in the project`
        : `Cannot resolve '${record.specifier}'`;
    diagnostics.push(diagnostic("warning", message, module, specifierNodeOf(module, record.node)));
    return null;
  }

  private validate(
    module: Module,
    record: Import | Export,
    name: string,
    what: string,
    diagnostics: Diagnostic[],
  ): void {
    const resolution = this.exportResolution(record._resolved!, name);
    if (resolution !== null && resolution !== AMBIGUOUS) return;
    const message =
      resolution === null
        ? `Module '${record.specifier}' has no export '${name}'`
        : `${what} '${name}' of module '${record.specifier}' is ambiguous: ` +
          "multiple 'export *' declarations supply it";
    diagnostics.push(diagnostic("error", message, module, record.node));
  }

  private exportResolution(module: Module, name: string): ExportResolution {
    let resolutions = this.exportResolutions.get(module);
    if (resolutions === undefined) {
      resolutions = new Map();
      this.exportResolutions.set(module, resolutions);
    }
    let resolution = resolutions.get(name);
    if (resolution === undefined) {
      resolution = this.resolveExportRecursive(module, name, []);
      resolutions.set(name, resolution);
    }
    return resolution;
  }

  // ResolveExport, 16.2.1.7.2.2
  private resolveExportRecursive(
    module: Module,
    name: string,
    seen: { module: Module; name: string }[],
  ): ExportResolution {
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
        return this.resolveExportRecursive(record._resolved, record.name!, seen);
      }
      if (direct._resolved === null) return null;
      if (direct.kind === "namespace") {
        return { module: direct._resolved, binding: null, namespace: true };
      }
      return this.resolveExportRecursive(direct._resolved, direct.fromName!, seen);
    }

    const added = this.addedExports.get(module)?.get(name);
    if (added !== undefined) return { module: added.module, binding: added, namespace: false };

    // default never crosses export *
    if (name === "default") return null;

    let found: Resolution | null = null;
    for (const star of module._starExports()) {
      if (star._resolved === null) continue;
      const resolution = this.resolveExportRecursive(star._resolved, name, seen);
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

function diagnostic(
  severity: DiagnosticSeverity,
  message: string,
  module: Module,
  node: Node,
): Diagnostic {
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

function wire(from: Module, to: Module): void {
  if (!from._deps.includes(to)) from._deps.push(to);
  if (!to._dependents.includes(from)) to._dependents.push(from);
}

function specifierNodeOf(module: Module, node: Node): Node {
  switch (node.type) {
    case "ImportSpecifier":
    case "ImportDefaultSpecifier":
    case "ImportNamespaceSpecifier":
    case "ExportSpecifier":
      return (module.parentOf(node) as ImportDeclaration).source;
    case "TSImportEqualsDeclaration":
      return ((node as TSImportEqualsDeclaration).moduleReference as TSExternalModuleReference)
        .expression;
    case "TSModuleDeclaration":
      return (node as TSModuleDeclaration).id;
    case "CallExpression":
      return (node as CallExpression).arguments[0]!;
    default:
      return (node as ImportDeclaration).source;
  }
}

// bare specifiers and unmatched assets such as `./app.css` are external
function defaultResolve(modules: Map<string, Module>): Resolve {
  return (specifier, importer) => {
    if (!specifier.startsWith(".")) return false;
    const slash = importer.lastIndexOf("/");
    const base = joinPath(slash === -1 ? "" : importer.slice(0, slash), specifier);
    if (modules.has(base)) return base;
    const extension = /\.[^./]+$/.exec(base)?.[0] ?? null;
    for (const source of SOURCE_EXTENSIONS.get(extension ?? "") ?? []) {
      const path = base.slice(0, -extension!.length) + source;
      if (modules.has(path)) return path;
    }
    for (const stem of [base, `${base}/index`]) {
      for (const probe of EXTENSIONS) if (modules.has(stem + probe)) return stem + probe;
    }
    return extension === null || EXTENSIONS.includes(extension) ? null : false;
  };
}

function joinPath(directory: string, relative: string): string {
  const parts = directory === "" ? [] : directory.split("/");
  for (const part of relative.split("/")) {
    if (part === "..") parts.pop();
    else if (part !== "" && part !== ".") parts.push(part);
  }
  return parts.join("/");
}
