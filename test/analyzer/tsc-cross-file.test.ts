// each project's imports, re-exports, and exported names against one tsc program

import { describe, expect, test } from "bun:test";
import { existsSync } from "node:fs";
import { join, relative, resolve } from "node:path";
import ts from "typescript";
import { type Definition, Project, type SourceFile } from "yuku";
import { loadedProjects, type LoadedProject } from "../corpus";
import { SAMPLE_MAX } from "./utils/differential";

const UNRESOLVED = "unresolved";

interface Checker extends ts.TypeChecker {
  getMergedSymbol(symbol: ts.Symbol): ts.Symbol;
}

interface Linked {
  program: ts.Program;
  checker: Checker;
  project: Project;
  /** The project file tsc resolves a specifier to, or null. */
  resolveModule(specifier: string, importer: string): string | null;
  /** A path as shown, from the project root. */
  shown(path: string): string;
}

function absolute(path: string): string {
  return resolve(path).replaceAll("\\", "/");
}

function link(loaded: LoadedProject): Linked {
  const root = absolute(loaded.root);
  const options: ts.CompilerOptions = {
    allowJs: true,
    allowImportingTsExtensions: true,
    jsx: ts.JsxEmit.Preserve,
    module: ts.ModuleKind.ESNext,
    moduleResolution: ts.ModuleResolutionKind.Bundler,
    noEmit: true,
    noLib: true,
    target: ts.ScriptTarget.ESNext,
    types: [],
    // absolute, so they need no baseUrl
    paths: Object.fromEntries(
      Object.entries(loaded.project.paths ?? {}).map(([name, targets]) => [
        name,
        targets.map((target) => `${root}/${target}`),
      ]),
    ),
  };
  const host = ts.createCompilerHost(options, true);
  const files = loaded.files.map((file) => absolute(file.path));
  const inProject = new Set(files);
  const program = ts.createProgram(files, options, host);

  const resolveModule = (specifier: string, importer: string): string | null => {
    const found = ts.resolveModuleName(specifier, importer, options, host).resolvedModule;
    const path = found === undefined ? null : absolute(found.resolvedFileName);
    return path !== null && inProject.has(path) ? path : null;
  };

  const project = new Project({
    resolve: (specifier, importer) => resolveModule(specifier, importer) ?? false,
  });
  for (const file of files) project.set(file, ts.sys.readFile(file) ?? "");
  const shown = (path: string) => relative(root, path).replaceAll("\\", "/");
  const checker = program.getTypeChecker() as Checker;
  return { program, checker, project, resolveModule, shown };
}

// `path:start` per declaration, or `module:path`
function placesOf(linked: Linked, definition: Definition | null): string[] {
  if (definition === null) return [UNRESOLVED];
  if (definition.binding === null) return [`module:${linked.shown(definition.file.path!)}`];
  return [definition.binding, ...definition.augmentations].flatMap((binding) =>
    binding.declarations.map((node) => `${linked.shown(binding.file.path!)}:${node.start}`),
  );
}

function isJSDoc(declaration: ts.Node): boolean {
  return (declaration.flags & ts.NodeFlags.JSDoc) !== 0;
}

function declaresInSyntax(symbol: ts.Symbol): boolean {
  return (symbol.declarations ?? []).some((declaration) => !isJSDoc(declaration));
}

// null when JSDoc declares part of it
function tscPlacesOf(linked: Linked, symbol: ts.Symbol | undefined): string[] | null {
  if (symbol === undefined) return [UNRESOLVED];
  const isAlias = (symbol.flags & ts.SymbolFlags.Alias) !== 0;
  const target = linked.checker.getMergedSymbol(
    isAlias ? linked.checker.getAliasedSymbol(symbol) : symbol,
  );
  if (target.declarations?.some(isJSDoc)) return null;
  const places: string[] = [];
  for (const declaration of target.declarations ?? []) {
    // `X.y = …` declares `X` to tsc
    if (ts.isIdentifier(declaration)) continue;
    const file = declaration.getSourceFile();
    const path = linked.shown(absolute(file.fileName));
    if (ts.isSourceFile(declaration)) {
      places.push(`module:${path}`);
      continue;
    }
    const name = ts.getNameOfDeclaration(declaration);
    if (name !== undefined) places.push(`${path}:${name.getStart(file)}`);
  }
  return places.length === 0 ? [UNRESOLVED] : places;
}

function identifierAt(file: ts.SourceFile, position: number): ts.Identifier | undefined {
  let found: ts.Identifier | undefined;
  (function visit(node: ts.Node): void {
    if (found !== undefined || position < node.getStart(file) || position >= node.end) return;
    if (ts.isIdentifier(node) && node.getStart(file) === position) found = node;
    else ts.forEachChild(node, visit);
  })(file);
  return found;
}

function exportsByAssignment(file: SourceFile): boolean {
  if (file.exports.some((record) => record.kind === "equals")) return true;
  if (!file.moduleFlags.usesModule && !file.moduleFlags.usesExports) return false;
  if (file.exports.length > 0) return false;
  return file.imports.every((record) => record.kind === "dynamic" || record.kind === "require");
}

function compareLinks(linked: Linked, file: SourceFile, mismatches: string[]): number {
  const tsFile = linked.program.getSourceFile(file.path!)!;
  const shown = linked.shown(file.path!);
  let compared = 0;
  const check = (subject: string, ours: string[], theirs: string[] | null) => {
    if (theirs === null) return;
    compared++;
    if ([...ours].sort().join() === [...theirs].sort().join()) return;
    mismatches.push(`${shown} ${subject}: yuku ${ours}, tsc ${theirs}`);
  };

  for (const record of file.imports) {
    const local = record.local;
    if (local === null || record.resolvedFile === null) continue;
    if (exportsByAssignment(record.resolvedFile)) continue;
    const node = identifierAt(tsFile, local.declarations[0]!.start)!;
    const theirs = tscPlacesOf(linked, linked.checker.getSymbolAtLocation(node));
    check(`import ${local.name}`, placesOf(linked, local.definition()), theirs);
  }

  if (exportsByAssignment(file)) return compared;
  const symbol = linked.checker.getSymbolAtLocation(tsFile);
  const exports = symbol === undefined ? [] : linked.checker.getExportsOfModule(symbol);
  const theirNames = exports
    .filter(declaresInSyntax)
    .map((exported) => exported.name)
    // a forwarded `export =`, which no import can name
    .filter((name) => name !== "export=");
  const ourNames = file.exportedNames();
  const missing = theirNames.filter((name) => !ourNames.includes(name));
  const extra = ourNames.filter((name) => !theirNames.includes(name));
  compared++;
  if (missing.length > 0 || extra.length > 0) {
    mismatches.push(`${shown} exports: missing [${missing}], extra [${extra}]`);
  }

  for (const record of file.exports) {
    if (record.kind !== "reExport" || record.name === null) continue;
    if (record.resolvedFile === null) continue;
    const exported = exports.find((candidate) => candidate.name === record.name);
    const ours = placesOf(linked, file.resolveExport(record.name));
    check(`re-export ${record.name}`, ours, tscPlacesOf(linked, exported));
  }
  return compared;
}

function throughPackageJSON(specifier: string, importer: string, resolved: string): boolean {
  const base = absolute(join(importer, "..", specifier));
  return existsSync(join(base, "package.json")) && !resolved.startsWith(`${base}/index.`);
}

function compareResolution(linked: Linked, mismatches: string[]): number {
  const withDefaultResolver = new Project();
  for (const [path, file] of linked.project.files) withDefaultResolver.set(path, file.source);
  let compared = 0;
  for (const [path, file] of withDefaultResolver.files) {
    for (const record of [...file.imports, ...file.exports]) {
      const specifier = record.specifier;
      if (specifier === null || !specifier.startsWith(".")) continue;
      const theirs = linked.resolveModule(specifier, path);
      if (theirs === null || throughPackageJSON(specifier, path, theirs)) continue;
      compared++;
      const ours = record.resolvedFile?.path ?? null;
      if (ours === theirs) continue;
      const yuku = ours === null ? UNRESOLVED : linked.shown(ours);
      const subject = `${linked.shown(path)} '${specifier}'`;
      mismatches.push(`${subject}: yuku ${yuku}, tsc ${linked.shown(theirs)}`);
    }
  }
  return compared;
}

describe("linking agrees with tsc across each project", () => {
  for (const loaded of loadedProjects()) {
    test(
      loaded.project.name,
      () => {
        const linked = link(loaded);
        const mismatches: string[] = [];
        let compared = compareResolution(linked, mismatches);
        for (const file of linked.project.files.values()) {
          compared += compareLinks(linked, file, mismatches);
        }
        const files = loaded.files.length;
        console.log(`${loaded.project.name}: ${compared} links agreed across ${files} files`);
        expect(mismatches.slice(0, SAMPLE_MAX)).toEqual([]);
        // CommonJS links through `module.exports`, which neither side compares
        expect(compared).toBeGreaterThan(loaded.project.type === "commonjs" ? 0 : files);
      },
      600_000,
    );
  }
});
