import {
  BindingFlags,
  parse,
  Project,
  type Binding,
  type Export,
  type FileOptions,
  type Import,
  type Node,
  type ParseOptions,
  type Reference,
  type Scope,
  type SourceFile,
} from "yuku";
import { nodesOf } from "../../ast-helpers-for-test";

function parseOne(source: string, options: ParseOptions): SourceFile {
  return parse(source, { semantic: true, path: "input.ts", ...options });
}

export function summary(source: string, options: ParseOptions = {}): string {
  const file = parseOne(source, options);
  const lines: string[] = [];

  if (file.diagnostics.length > 0) {
    lines.push("diagnostics");
    for (const diagnostic of file.diagnostics) {
      lines.push(`  ${diagnostic.severity}: ${diagnostic.message}`);
    }
  }

  const childrenByScope = groupBy(file.scopes, (scope) => scope.parent?.id);
  const referencesByScope = groupBy(file.references, (reference) => reference.scope.id);
  const render = (scope: Scope, depth: number): void => {
    const inner = "  ".repeat(depth + 1);
    lines.push("  ".repeat(depth) + scopeHeader(scope));
    for (const binding of scope.bindings) lines.push(inner + bindingRow(binding));
    for (const reference of referencesByScope.get(scope.id) ?? []) {
      lines.push(inner + referenceRow(reference));
    }
    for (const child of childrenByScope.get(scope.id) ?? []) render(child, depth + 1);
  };
  render(file.scopes[0]!, 0);

  if (file.imports.length > 0) {
    lines.push("imports");
    for (const record of file.imports) lines.push(`  ${importRow(record)}`);
  }
  if (file.exports.length > 0) {
    lines.push("exports");
    for (const record of file.exports) lines.push(`  ${exportRow(record)}`);
  }

  return lines.join("\n");
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
  if (record.kind === "dynamic" || record.kind === "require") {
    return `(${record.kind}) ${specifier}${mods}`;
  }
  const local = record.local ? `#${record.local.id}` : "–";
  if (record.kind === "namespace" || record.kind === "importEquals") {
    return `* as ${local} ${specifier}${mods}`;
  }
  return `${record.name} → ${local} ${specifier}${mods}`;
}

function exportRow(record: Export): string {
  const type = record.typeOnly ? " type" : "";
  if (record.kind === "equals") return "export=";
  if (record.kind === "global") return `export as namespace ${record.name}`;
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

function groupBy<T>(items: readonly T[], keyOf: (item: T) => number | undefined): Map<number, T[]> {
  const groups = new Map<number, T[]>();
  for (const item of items) {
    const key = keyOf(item);
    if (key === undefined) continue;
    const group = groups.get(key);
    if (group === undefined) groups.set(key, [item]);
    else group.push(item);
  }
  return groups;
}

const FUNCTION_TYPES = [
  "FunctionDeclaration",
  "FunctionExpression",
  "ArrowFunctionExpression",
] as const;

export function captures(source: string, options: ParseOptions = {}): string {
  const file = parseOne(source, options);
  const lines: string[] = [];
  for (const node of nodesOf(file.program, ...FUNCTION_TYPES)) {
    const captured = file
      .capturesOf(node)
      .map(({ binding, isWritten }) => `${binding.name}#${binding.id}${isWritten ? "(w)" : ""}`);
    const list = captured.length > 0 ? captured.join(", ") : "(none)";
    lines.push(`${functionLabel(node)}  captures: ${list}`);
  }
  return lines.join("\n");
}

function functionLabel(node: Node): string {
  const named = node as Node & { id?: { name?: string } | null };
  if (named.id?.name) return `function "${named.id.name}"`;
  if (node.type === "ArrowFunctionExpression") return "arrow";
  return "function <anonymous>";
}

export function projectOf(files: Record<string, string>, options?: FileOptions): Project {
  const project = new Project();
  for (const [path, source] of Object.entries(files)) project.set(path, source, options);
  return project;
}

export function links(files: Record<string, string>): string {
  const linked = projectOf(files);
  const lines: string[] = ["diagnostics"];
  if (linked.diagnostics.length === 0) lines.push("  (none)");
  for (const diagnostic of linked.diagnostics) {
    lines.push(`  ${diagnostic.path}: ${diagnostic.message}`);
  }
  lines.push("graph");
  for (const file of linked.files.values()) {
    const dependencies = file.dependencies.map((dependency) => dependency.path);
    lines.push(`  ${file.path} → ${dependencies.join(", ") || "(none)"}`);
  }
  lines.push("exportedNames");
  for (const file of linked.files.values()) {
    lines.push(`  ${file.path}: ${file.exportedNames().join(", ") || "(none)"}`);
  }
  return lines.join("\n");
}

function bindingOf(project: Project, path: string, name: string): Binding {
  const file = project.files.get(path);
  if (file === undefined) throw new Error(`no file ${path}`);
  const binding = file.rootScope.lookup(name, "any");
  if (binding === null) throw new Error(`no binding ${name} in ${path}`);
  return binding;
}

export function definition(project: Project, path: string, name: string): string {
  const found = bindingOf(project, path, name).definition();
  if (found === null) return "(none)";
  const defined = `${found.file.path}:${found.binding ? found.binding.name : "(namespace)"}`;
  const augmented = found.augmentations.map((binding) => `${binding.file.path}:${binding.name}`);
  return [defined, ...augmented].join(" + ");
}

export function references(project: Project, path: string, name: string): string {
  return bindingOf(project, path, name)
    .findReferences()
    .map((use) => `${use.file.path}:${use.name}${use.isWrite ? "(w)" : ""}`)
    .join(", ");
}
