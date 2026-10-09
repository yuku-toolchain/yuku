import {
  analyze,
  Analyzer,
  BindingFlags,
  type AnalyzeOptions,
  type Binding,
  type Export,
  type Import,
  type Module,
  type Reference,
  type Scope,
} from "yuku-analyzer";
import type { Node } from "yuku-parser";

function analyzeOne(source: string, options: AnalyzeOptions): Module {
  return analyze(source, { path: "input.ts", ...options });
}

export function summary(source: string, options: AnalyzeOptions = {}): string {
  const module = analyzeOne(source, options);
  const lines: string[] = [];

  if (module.diagnostics.length > 0) {
    lines.push("diagnostics");
    for (const diagnostic of module.diagnostics) {
      lines.push(`  ${diagnostic.severity}: ${diagnostic.message}`);
    }
  }

  const childrenOf = groupScopeChildren(module);
  const referencesByScope = groupReferencesByScope(module);
  renderScope(module.scopes[0]!, 0, lines, childrenOf, referencesByScope);

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
  for (const binding of scope.bindings) lines.push(inner + bindingRow(binding));
  for (const reference of referencesByScope.get(scope.id) ?? []) {
    lines.push(inner + referenceRow(reference));
  }
  for (const child of childrenOf.get(scope.id) ?? []) {
    renderScope(child, depth + 1, lines, childrenOf, referencesByScope);
  }
}

// `[strict]` marks only where strictness turns on
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
      // the node type tells sibling block scopes apart
      return `${scope.kind} ${node.type}`;
  }
}

function bindingRow(binding: Binding): string {
  const count = binding.declarations.length;
  const merged = count > 1 ? ` ×${count}` : "";
  return `${binding.name}#${binding.id}  ${flagWords(binding.flags)}${merged}`;
}

function referenceRow(reference: Reference): string {
  const target = reference.binding ? `#${reference.binding.id}` : "free";
  const write = reference.isWrite ? " write" : "";
  const space = reference.space === "value" ? "" : ` ${reference.space}`;
  return `${reference.name} → ${target}${write}${space}`;
}

// one kind plus qualifiers per binding
function flagWords(flags: number): string {
  const words: string[] = [];
  if (flags & BindingFlags.FunctionScopedVariable) {
    if (flags & BindingFlags.Parameter) words.push("param");
    else if (flags & BindingFlags.CatchVariable) words.push("catch");
    else words.push("var");
  }
  if (flags & BindingFlags.BlockScopedVariable) {
    words.push(flags & BindingFlags.Const ? "const" : "let");
  }
  if (flags & BindingFlags.Function) words.push("function");
  if (flags & BindingFlags.Class) words.push("class");
  if (flags & BindingFlags.RegularEnum) words.push("enum");
  if (flags & BindingFlags.ConstEnum) words.push("const-enum");
  if (flags & BindingFlags.NamespaceModule) words.push("namespace");
  if (flags & BindingFlags.ValueModule) words.push("value-module");
  if (flags & BindingFlags.Interface) words.push("interface");
  if (flags & BindingFlags.TypeAlias) words.push("type");
  if (flags & BindingFlags.TypeParameter) words.push("type-param");
  if (flags & BindingFlags.ValueImport) words.push("import");
  if (flags & BindingFlags.TypeImport) words.push("type-import");
  if (flags & BindingFlags.Ambient) words.push("ambient");
  if (flags & BindingFlags.Exported) words.push("exported");
  if (flags & BindingFlags.Default) words.push("default");
  return words.join(" ");
}

function importRow(record: Import): string {
  const mods = (record.typeOnly ? " type" : "") + (record.phase ? ` phase:${record.phase}` : "");
  const specifier = `from "${record.specifier}"`;
  if (record.kind === "sideEffect") return `(side-effect) ${specifier}${mods}`;
  if (record.kind === "augmentation") return `(augmentation) ${specifier}${mods}`;
  const local = record.local ? `#${record.local.id}` : "–";
  if (record.isNamespace) return `* as ${local} ${specifier}${mods}`;
  return `${record.name} → ${local} ${specifier}${mods}`;
}

function exportRow(record: Export): string {
  const type = record.typeOnly ? " type" : "";
  if (record.kind === "equals") return "export=";
  if (record.globalName !== null) return `export as namespace ${record.globalName}`;
  if (record.kind === "star") return `* from "${record.specifier}"${type}`;
  if (record.specifier !== null) {
    const source = `from "${record.specifier}"`;
    if (record.kind === "namespace") return `* as ${record.name} ${source}${type}`;
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

export function captures(source: string, options: AnalyzeOptions = {}): string {
  const module = analyzeOne(source, options);
  const lines: string[] = [];
  for (const fn of module.findAll(FUNCTION_TYPES)) {
    const caps = module
      .capturesOf(fn)
      .map((c) => `${c.binding.name}#${c.binding.id}${c.isWritten ? "(w)" : ""}`);
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

export function project(files: Record<string, string>): Analyzer {
  const analyzer = new Analyzer();
  for (const [path, source] of Object.entries(files)) analyzer.setFile(path, source);
  return analyzer;
}

export function links(files: Record<string, string>): string {
  const analyzer = project(files);
  analyzer.link();
  const lines: string[] = ["diagnostics"];
  if (analyzer.diagnostics.length === 0) lines.push("  (none)");
  for (const diagnostic of analyzer.diagnostics) {
    lines.push(`  ${diagnostic.path}: ${diagnostic.message}`);
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

function bindingOf(analyzer: Analyzer, path: string, name: string): Binding {
  const module = analyzer.module(path);
  if (module === undefined) throw new Error(`no module ${path}`);
  const binding = module.lookup(name, { space: "any" });
  if (binding === null) throw new Error(`no binding ${name} in ${path}`);
  return binding;
}

export function definition(analyzer: Analyzer, path: string, name: string): string {
  const def = bindingOf(analyzer, path, name).definition();
  if (def === null) return "(none)";
  const defined = `${def.module.path}:${def.binding ? def.binding.name : "(namespace)"}`;
  const augmented = def.augmentations.map((binding) => `${binding.module.path}:${binding.name}`);
  return [defined, ...augmented].join(" + ");
}

export function references(analyzer: Analyzer, path: string, name: string): string {
  return bindingOf(analyzer, path, name)
    .findReferences()
    .map((r) => `${r.module.path}:${r.name}${r.isWrite ? "(w)" : ""}`)
    .join(", ");
}
