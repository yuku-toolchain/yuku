import { BindingFlags } from "../core/decode";
import type {
  CallExpression,
  Diagnostic,
  ImportDeclaration,
  Node,
  TSExternalModuleReference,
  TSImportEqualsDeclaration,
  TSModuleDeclaration,
} from "../types";
import type { Binding, Definition, Export, Import, Reference } from "./semantic";
import { diagnosticAt, SourceFile, type FileOptions } from "./source-file";

export interface ProjectOptions {
  /**
   * A WebAssembly build of Yuku to analyze with in place of the native addon.
   * @default the native addon where it loads, else the WebAssembly module yuku ships
   */
  wasm?: WebAssembly.Module;
  /**
   * Maps a specifier to a project file path, `false` for an external module, or `null` to warn
   * that it is unresolved. Defaults to relative paths probed as TypeScript does.
   */
  resolve?: (specifier: string, importer: string) => string | false | null;
}

type Resolve = NonNullable<ProjectOptions["resolve"]>;

interface Resolution {
  file: SourceFile;
  binding: Binding | null;
  namespace: boolean;
}

const EXTENSIONS = [".ts", ".tsx", ".d.ts", ".js", ".jsx", ".mts", ".mjs", ".cts", ".cjs"];

const SOURCE_EXTENSIONS = new Map([
  [".js", [".ts", ".tsx", ".d.ts"]],
  [".jsx", [".tsx"]],
  [".mjs", [".mts", ".d.mts"]],
  [".cjs", [".cts", ".d.cts"]],
]);

const AMBIGUOUS = Symbol("ambiguous");
const NO_FILES: readonly SourceFile[] = [];

type ExportResolution = Resolution | typeof AMBIGUOUS | null;

/** A set of files and the links between them. */
export class Project {
  private wasm: WebAssembly.Module | undefined;
  private fileByPath = new Map<string, SourceFile>();
  private resolve: Resolve;
  private projectDiagnostics: Diagnostic[] = [];
  private dirty = false;
  private importers = new Map<Binding, Binding[]>();
  private exportResolutions = new Map<SourceFile, Map<string, ExportResolution>>();
  private merged = new Map<Binding, Binding[]>();
  private addedExports = new Map<SourceFile, Map<string, Binding>>();
  private importsFrom = new Map<SourceFile, SourceFile[]>();
  private importedBy = new Map<SourceFile, SourceFile[]>();

  constructor(options: ProjectOptions = {}) {
    this.wasm = options.wasm;
    this.resolve = options.resolve ?? defaultResolve(this.fileByPath);
  }

  /** Analyzes a file, replacing any file at the same path. */
  set(path: string, source: string | Uint8Array, options: FileOptions = {}): SourceFile {
    if (typeof path !== "string" || path === "") {
      throw new TypeError("set: `path` must be a non-empty string");
    }
    const file = new SourceFile(source, { ...options, path, wasm: this.wasm }, this);
    this.fileByPath.set(path, file);
    this.dirty = true;
    return file;
  }

  /** Returns whether the file was in the project. */
  delete(path: string): boolean {
    const deleted = this.fileByPath.delete(path);
    if (deleted) this.dirty = true;
    return deleted;
  }

  get files(): ReadonlyMap<string, SourceFile> {
    return this.fileByPath;
  }

  /** Every file's diagnostics, and the links that fail. */
  get diagnostics(): readonly Diagnostic[] {
    this._link();
    return this.projectDiagnostics;
  }

  /** @internal */
  static _solo(file: SourceFile): Project {
    const project = new Project();
    project.fileByPath.set(file.path ?? "", file);
    project.dirty = true;
    return project;
  }

  /** @internal */
  _link(file?: SourceFile): void {
    if (file !== undefined && !file.isCurrent) {
      throw new Error(`'${file.path}' was replaced or deleted, query its current file`);
    }
    if (this.dirty) this.link();
  }

  /** @internal */
  _definitionOf(binding: Binding): Definition | null {
    this._link(binding.file);
    const seen = new Set<Binding>();
    let current = binding;
    while (current.has(BindingFlags.Import)) {
      if (seen.has(current)) return null;
      seen.add(current);
      const record = current.import;
      if (record === null || record._resolved === null) return null;
      if (bindsModule(record)) return this.definition(record._resolved, null);
      const resolution = this.exportResolution(record._resolved, record.name!);
      if (resolution === null || resolution === AMBIGUOUS) return null;
      if (resolution.namespace) return this.definition(resolution.file, null);
      if (resolution.binding === null) return null;
      current = resolution.binding;
    }
    const target = this.merged.get(current)?.[0] ?? current;
    return this.definition(target.file, target);
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
  _resolveExport(file: SourceFile, name: string): Definition | null {
    this._link(file);
    const resolution = this.exportResolution(file, name);
    if (resolution === null || resolution === AMBIGUOUS) return null;
    if (resolution.namespace) return this.definition(resolution.file, null);
    if (resolution.binding === null) return null;
    return this.definition(resolution.file, resolution.binding);
  }

  /** @internal */
  _dependencies(file: SourceFile): readonly SourceFile[] {
    this._link(file);
    return this.importsFrom.get(file) ?? NO_FILES;
  }

  /** @internal */
  _dependents(file: SourceFile): readonly SourceFile[] {
    this._link(file);
    return this.importedBy.get(file) ?? NO_FILES;
  }

  /** @internal */
  _exportedNames(file: SourceFile): string[] {
    this._link(file);
    return this.collectExportedNames(file, new Set());
  }

  private link(): void {
    this.dirty = false;
    this.exportResolutions = new Map();
    this.merged = new Map();
    this.addedExports = new Map();
    this.importsFrom = new Map();
    this.importedBy = new Map();
    const diagnostics: Diagnostic[] = [];
    for (const file of this.fileByPath.values()) diagnostics.push(...file.diagnostics);
    for (const file of this.fileByPath.values()) {
      const resolved = new Map<string, SourceFile | null>();
      for (const record of [...file.imports, ...file.exports]) {
        record._resolved = null;
        if (record.specifier === null) continue;
        if (!resolved.has(record.specifier)) {
          resolved.set(record.specifier, this.resolveFile(file, record, diagnostics));
        }
        record._resolved = resolved.get(record.specifier)!;
        if (record._resolved !== null) {
          addOnce(this.importsFrom, file, record._resolved);
          addOnce(this.importedBy, record._resolved, file);
        }
      }
    }
    for (const file of this.fileByPath.values()) {
      for (const record of file.imports) {
        if (record.kind !== "augmentation" || record._resolved === null) continue;
        for (const binding of record._scope.bindings) {
          if (binding.has(BindingFlags.Exported)) this.augment(record._resolved, binding);
        }
      }
    }
    for (const file of this.fileByPath.values()) {
      for (const record of file.imports) {
        if (record._resolved !== null && record.name !== null) {
          this.validate(file, record, record.name, "Import", diagnostics);
        }
      }
      for (const record of file.exports) {
        if (record._resolved !== null && record.fromName !== null) {
          this.validate(file, record, record.fromName, "Re-export", diagnostics);
        }
      }
    }
    this.importers = new Map();
    for (const file of this.fileByPath.values()) {
      for (const record of file.imports) {
        const local = record.local;
        const origin = local === null ? null : this._definitionOf(local);
        if (origin === null || origin.binding === null) continue;
        const importers = this.importers.get(origin.binding) ?? [];
        importers.push(local!);
        this.importers.set(origin.binding, importers);
      }
    }
    this.projectDiagnostics = diagnostics;
  }

  private definition(file: SourceFile, binding: Binding | null): Definition {
    const merged = binding === null ? undefined : this.merged.get(binding);
    return { file, binding, augmentations: merged === undefined ? [] : merged.slice(1) };
  }

  private augment(file: SourceFile, binding: Binding): void {
    const name = binding.has(BindingFlags.Default) ? "default" : binding.name;
    const resolution = this.resolveExportRecursive(file, name, []);
    if (resolution === null) {
      const added = this.addedExports.get(file) ?? new Map<string, Binding>();
      this.addedExports.set(file, added.set(name, binding));
    } else if (resolution !== AMBIGUOUS && resolution.binding !== null) {
      const merged = this.merged.get(resolution.binding) ?? [resolution.binding];
      merged.push(binding);
      this.merged.set(resolution.binding, merged).set(binding, merged);
    }
  }

  private resolveFile(
    file: SourceFile,
    record: Import | Export,
    diagnostics: Diagnostic[],
  ): SourceFile | null {
    const resolved = this.resolve(record.specifier!, file.path ?? "");
    if (resolved === false) return null;
    const target = typeof resolved === "string" ? this.fileByPath.get(resolved) : undefined;
    if (target !== undefined) return target;
    const message =
      typeof resolved === "string"
        ? `'${record.specifier}' resolves to '${resolved}', which is not in the project`
        : `Cannot resolve '${record.specifier}'`;
    const node = specifierNodeOf(file, record.node);
    diagnostics.push(diagnosticAt(file, node, message, { severity: "warning" }));
    return null;
  }

  private validate(
    file: SourceFile,
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
    diagnostics.push(diagnosticAt(file, record.node, message));
  }

  private exportResolution(file: SourceFile, name: string): ExportResolution {
    let resolutions = this.exportResolutions.get(file);
    if (resolutions === undefined) {
      resolutions = new Map();
      this.exportResolutions.set(file, resolutions);
    }
    let resolution = resolutions.get(name);
    if (resolution === undefined) {
      resolution = this.resolveExportRecursive(file, name, []);
      resolutions.set(name, resolution);
    }
    return resolution;
  }

  private resolveExportRecursive(
    file: SourceFile,
    name: string,
    seen: { file: SourceFile; name: string }[],
  ): ExportResolution {
    for (const entry of seen) if (entry.file === file && entry.name === name) return null;
    seen.push({ file, name });

    const direct = file._model.exportMap().get(name);
    if (direct !== undefined) {
      if (direct.specifier === null) {
        const local = direct.local;
        const record = local?.has(BindingFlags.Import) ? local.import : null;
        if (record === null) return { file, binding: local, namespace: false };
        if (record._resolved === null) return null;
        if (bindsModule(record)) {
          return { file: record._resolved, binding: null, namespace: true };
        }
        return this.resolveExportRecursive(record._resolved, record.name!, seen);
      }
      if (direct._resolved === null) return null;
      if (direct.kind === "namespace") {
        return { file: direct._resolved, binding: null, namespace: true };
      }
      return this.resolveExportRecursive(direct._resolved, direct.fromName!, seen);
    }

    const added = this.addedExports.get(file)?.get(name);
    if (added !== undefined) return { file: added.file, binding: added, namespace: false };

    if (name === "default") return null;

    let found: Resolution | null = null;
    for (const star of file._model.starExports()) {
      if (star._resolved === null) continue;
      const resolution = this.resolveExportRecursive(star._resolved, name, seen);
      if (resolution === AMBIGUOUS) return AMBIGUOUS;
      if (resolution === null) continue;
      if (found === null) {
        found = resolution;
      } else if (
        resolution.file !== found.file ||
        resolution.binding !== found.binding ||
        resolution.namespace !== found.namespace
      ) {
        return AMBIGUOUS;
      }
    }
    return found;
  }

  private collectExportedNames(file: SourceFile, seen: Set<SourceFile>): string[] {
    if (seen.has(file)) return [];
    seen.add(file);
    const names = new Set(file._model.exportMap().keys());
    for (const name of this.addedExports.get(file)?.keys() ?? []) names.add(name);
    for (const star of file._model.starExports()) {
      if (star._resolved === null) continue;
      for (const name of this.collectExportedNames(star._resolved, seen)) {
        if (name !== "default") names.add(name);
      }
    }
    return [...names];
  }
}

function addOnce(map: Map<SourceFile, SourceFile[]>, key: SourceFile, file: SourceFile): void {
  const files = map.get(key);
  if (files === undefined) map.set(key, [file]);
  else if (!files.includes(file)) files.push(file);
}

function specifierNodeOf(file: SourceFile, node: Node): Node {
  switch (node.type) {
    case "ImportSpecifier":
    case "ImportDefaultSpecifier":
    case "ImportNamespaceSpecifier":
    case "ExportSpecifier":
      return (file.parentOf(node) as ImportDeclaration).source;
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

function defaultResolve(files: Map<string, SourceFile>): Resolve {
  return (specifier, importer) => {
    if (!specifier.startsWith(".")) return false;
    const slash = importer.lastIndexOf("/");
    const base = joinPath(slash === -1 ? "" : importer.slice(0, slash), specifier);
    if (files.has(base)) return base;
    const extension = /\.[^./]+$/.exec(base)?.[0] ?? null;
    for (const source of SOURCE_EXTENSIONS.get(extension ?? "") ?? []) {
      const path = base.slice(0, -extension!.length) + source;
      if (files.has(path)) return path;
    }
    for (const stem of [base, `${base}/index`]) {
      for (const probe of EXTENSIONS) if (files.has(stem + probe)) return stem + probe;
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

function bindsModule(record: Import): boolean {
  return record.kind === "namespace" || record.kind === "importEquals";
}
