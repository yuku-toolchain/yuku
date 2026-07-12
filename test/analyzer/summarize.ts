import {
  Analyzer,
  SymbolFlags,
  type AddFileOptions,
  type Export,
  type Import,
  type Module,
  type Reference,
  type Scope,
  type Symbol,
} from "yuku-analyzer";
import type { Node } from "yuku-parser";

export interface SummaryOptions extends AddFileOptions {
  /** Drives lang/sourceType the way a real path would. @default "input.ts" */
  path?: string;
}

/** Analyzes one file and renders its full semantic model as canonical text. */
export function summary(source: string, options: SummaryOptions = {}): string {
  const { path = "input.ts", ...rest } = options;
  const module = new Analyzer().addFile(path, source, rest);
  const lines: string[] = [];

  if (module.diagnostics.length > 0) {
    lines.push("diagnostics");
    for (const diagnostic of module.diagnostics) {
      lines.push(`  ${diagnostic.severity}: ${diagnostic.message}`);
    }
  }

  const childrenOf = groupScopeChildren(module);
  const referencesByScope = groupReferencesByScope(module);
  renderScope(module.scopes[0], 0, lines, childrenOf, referencesByScope);

  if (module.imports.length > 0) {
    lines.push("imports");
    for (const record of module.imports) lines.push(`  ${importRow(record)}`);
  }
  if (module.exports.length > 0) {
    lines.push("exports");
    for (const record of module.exports) lines.push(`  ${exportRow(record)}`);
  }

  return lines.join("\n");
}

function renderScope(
  scope: Scope,
  depth: number,
  lines: string[],
  childrenOf: Map<number, Scope[]>,
  referencesByScope: Map<number, Reference[]>,
): void {
  const pad = "  ".repeat(depth);
  const inner = "  ".repeat(depth + 1);

  lines.push(pad + scopeHeader(scope));
  for (const symbol of scope.bindings) lines.push(inner + bindingRow(symbol));
  for (const reference of referencesByScope.get(scope.id) ?? []) {
    lines.push(inner + referenceRow(reference));
  }
  for (const child of childrenOf.get(scope.id) ?? []) {
    renderScope(child, depth + 1, lines, childrenOf, referencesByScope);
  }
}

// `[strict]` marks only the scope where strictness turns on (it never turns off
// in a child), so the dump shows the transition instead of repeating it.
function scopeHeader(scope: Scope): string {
  const strict = scope.strict && !(scope.parent?.strict ?? false) ? " [strict]" : "";
  return scopeLabel(scope) + strict;
}

function scopeLabel(scope: Scope): string {
  const node = scope.node as Node & { id?: { name?: string } | null };
  switch (scope.kind) {
    case "global":
    case "module":
    case "staticBlock":
    case "tsModule":
      return scope.kind;
    case "function":
    case "expressionName":
      if (node.id?.name) return `${scope.kind} "${node.id.name}"`;
      if (node.type === "ArrowFunctionExpression") return `${scope.kind} =>`;
      return `${scope.kind} <anonymous>`;
    case "class":
      return node.id?.name ? `${scope.kind} "${node.id.name}"` : `${scope.kind} <anonymous>`;
    default:
      // block scopes come from many constructs (catch, for, switch, type
      // declarations), so the node type disambiguates siblings
      return `${scope.kind} ${node.type}`;
  }
}

function bindingRow(symbol: Symbol): string {
  const count = symbol.declarations.length;
  const merged = count > 1 ? ` ×${count}` : "";
  return `${symbol.name}#${symbol.id}  ${flagWords(symbol.flags)}${merged}`;
}

function referenceRow(reference: Reference): string {
  const target = reference.symbol ? `#${reference.symbol.id}` : "free";
  const write = reference.isWrite ? " write" : "";
  const space = reference.space === "value" ? "" : ` ${reference.space}`;
  return `${reference.name} → ${target}${write}${space}`;
}

// Decodes the raw flag bitset into canonical words. The variable kind folds its
// modifiers in (a parameter is function-scoped, `const` implies block-scoped),
// so each binding reads as one primary kind plus any qualifiers.
function flagWords(flags: number): string {
  const words: string[] = [];
  if (flags & SymbolFlags.FunctionScopedVariable) {
    if (flags & SymbolFlags.Parameter) words.push("param");
    else if (flags & SymbolFlags.CatchVariable) words.push("catch");
    else words.push("var");
  }
  if (flags & SymbolFlags.BlockScopedVariable) {
    words.push(flags & SymbolFlags.Const ? "const" : "let");
  }
  if (flags & SymbolFlags.Function) words.push("function");
  if (flags & SymbolFlags.Class) words.push("class");
  if (flags & SymbolFlags.RegularEnum) words.push("enum");
  if (flags & SymbolFlags.ConstEnum) words.push("const-enum");
  if (flags & SymbolFlags.NamespaceModule) words.push("namespace");
  if (flags & SymbolFlags.ValueModule) words.push("value-module");
  if (flags & SymbolFlags.Interface) words.push("interface");
  if (flags & SymbolFlags.TypeAlias) words.push("type");
  if (flags & SymbolFlags.TypeParameter) words.push("type-param");
  if (flags & SymbolFlags.ValueImport) words.push("import");
  if (flags & SymbolFlags.TypeImport) words.push("type-import");
  if (flags & SymbolFlags.Ambient) words.push("ambient");
  if (flags & SymbolFlags.Exported) words.push("exported");
  if (flags & SymbolFlags.Default) words.push("default");
  return words.join(" ");
}

function importRow(record: Import): string {
  const mods = (record.typeOnly ? " type" : "") + (record.phase ? ` phase:${record.phase}` : "");
  const specifier = `from "${record.specifier}"`;
  if (record.isSideEffect) return `(side-effect) ${specifier}${mods}`;
  const local = record.local ? `#${record.local.id}` : "–";
  if (record.isNamespace) return `* as ${local} ${specifier}${mods}`;
  return `${record.name} → ${local} ${specifier}${mods}`;
}

function exportRow(record: Export): string {
  const type = record.typeOnly ? " type" : "";
  if (record.isExportEquals) return "export=";
  if (record.globalName !== null) return `export as namespace ${record.globalName}`;
  if (record.isStar) return `* from "${record.specifier}"${type}`;
  if (record.specifier !== null) {
    const source = `from "${record.specifier}"`;
    if (record.isNamespaceReexport) return `* as ${record.name} ${source}${type}`;
    const from =
      record.fromName === record.name ? `${record.name}` : `${record.fromName} as ${record.name}`;
    return `${from} ${source}${type}`;
  }
  const local = record.local ? `#${record.local.id}` : "(anonymous)";
  return `${record.name} → ${local}${type}`;
}

function groupScopeChildren(module: Module): Map<number, Scope[]> {
  const children = new Map<number, Scope[]>();
  for (const scope of module.scopes) {
    const parent = scope.parent;
    if (parent === null) continue;
    const list = children.get(parent.id) ?? [];
    list.push(scope);
    children.set(parent.id, list);
  }
  return children;
}

function groupReferencesByScope(module: Module): Map<number, Reference[]> {
  const byScope = new Map<number, Reference[]>();
  for (const reference of module.references) {
    const list = byScope.get(reference.scope.id) ?? [];
    list.push(reference);
    byScope.set(reference.scope.id, list);
  }
  return byScope;
}

const FUNCTION_TYPES = [
  "FunctionDeclaration",
  "FunctionExpression",
  "ArrowFunctionExpression",
] as const;

/** Dumps the free variables of every function in the file, in source order. */
export function captures(source: string, options: SummaryOptions = {}): string {
  const { path = "input.ts", ...rest } = options;
  const module = new Analyzer().addFile(path, source, rest);
  const lines: string[] = [];
  for (const fn of module.findAll(FUNCTION_TYPES)) {
    const caps = module
      .capturesOf(fn)
      .map((c) => `${c.symbol.name}#${c.symbol.id}${c.isWritten ? "(w)" : ""}`);
    lines.push(`${functionLabel(fn)}  captures: ${caps.length > 0 ? caps.join(", ") : "(none)"}`);
  }
  return lines.join("\n");
}

function functionLabel(node: Node): string {
  const named = node as Node & { id?: { name?: string } | null };
  if (named.id?.name) return `function "${named.id.name}"`;
  if (node.type === "ArrowFunctionExpression") return "arrow";
  return "function <anonymous>";
}

/** Builds a multi-file analyzer from a path→source map (no auto-link). */
export function project(files: Record<string, string>): Analyzer {
  const analyzer = new Analyzer();
  for (const [path, source] of Object.entries(files)) analyzer.addFile(path, source);
  return analyzer;
}

// renders the graph-level model. link diagnostics, the dependency edges, and
// each module's exported names with `export *` chains followed.
export function links(files: Record<string, string>): string {
  const analyzer = project(files);
  analyzer.link();
  const lines: string[] = ["diagnostics"];
  if (analyzer.diagnostics.length === 0) lines.push("  (none)");
  for (const diagnostic of analyzer.diagnostics) {
    lines.push(`  ${diagnostic.module}: ${diagnostic.message}`);
  }
  lines.push("graph");
  for (const module of analyzer.modules.values()) {
    const deps = module.dependencies.map((d) => d.path).join(", ") || "(none)";
    lines.push(`  ${module.path} → ${deps}`);
  }
  lines.push("exportedNames");
  for (const module of analyzer.modules.values()) {
    lines.push(`  ${module.path}: ${module.exportedNames().join(", ") || "(none)"}`);
  }
  return lines.join("\n");
}

function bindingOf(analyzer: Analyzer, path: string, name: string): Symbol {
  const module = analyzer.module(path);
  if (module === undefined) throw new Error(`no module ${path}`);
  const symbol = module.resolve(name, module.rootScope, "any");
  if (symbol === null) throw new Error(`no binding ${name} in ${path}`);
  return symbol;
}

/** Formats `definitionOf` for the binding named `name` in module `path`. */
export function definition(analyzer: Analyzer, path: string, name: string): string {
  const def = analyzer.definitionOf(bindingOf(analyzer, path, name));
  if (def === null) return "(none)";
  return `${def.module.path}:${def.symbol ? def.symbol.name : "(namespace)"}`;
}

/** Formats `referencesOf` for the binding named `name` in module `path`. */
export function references(analyzer: Analyzer, path: string, name: string): string {
  return analyzer
    .referencesOf(bindingOf(analyzer, path, name))
    .map((r) => `${r.module.path}:${r.reference.name}${r.reference.isWrite ? "(w)" : ""}`)
    .join(", ");
}
