import { b } from "../ast/builders";
import { isEvalOrArguments, isValidIdentifier } from "../ast/lexical";
import { bindingIdentifiers } from "../ast/utils";
import { BindingFlags } from "../core/decode";
import { replaceAt, slotOf, type Slot } from "../traverse/walk";
import type { Diagnostic, ExportNamedDeclaration, Identifier, JSXIdentifier, Node } from "../types";
import type { Binding } from "./semantic";
import { diagnosticAt, type DiagnosticDetails, type SourceFile } from "./source-file";

interface ExportSite {
  statement: ExportNamedDeclaration;
  declaration: Node;
  slot: Slot;
}

export function rename(binding: Binding, name: string): Diagnostic[] {
  if (!isValidIdentifier(name)) {
    throw new TypeError(`rename: '${name}' is not a valid identifier`);
  }
  const previous = binding.name;
  if (name === previous) return [];
  const identifiers = binding.declarations.filter((node) => node.type === "Identifier");
  const sites = exportSites(binding, identifiers);
  const diagnostics = [...unsupported(binding, sites), ...conflicts(binding, name)];
  if (diagnostics.length > 0 || sites === null) return diagnostics;

  for (const identifier of identifiers) renameNode(binding.file, identifier, name);
  for (const reference of binding.references) renameNode(binding.file, reference.node, name);
  for (let i = sites.length - 1; i >= 0; i--) keepExportNames(sites[i]!, previous, name);
  return [];
}

function unsupported(binding: Binding, sites: ExportSite[] | null): Diagnostic[] {
  const { file, name } = binding;
  const diagnostics: Diagnostic[] = [];
  const report = (node: Node, reason: string) => {
    diagnostics.push(diagnosticAt(file, node, `Cannot rename '${name}', ${reason}`));
  };
  for (const node of binding.declarations) {
    if (binding.has(BindingFlags.EnumMember)) {
      report(node, "which is an enum member read by name");
    } else if (node.type !== "Identifier") {
      report(node, `which a ${node.type} declares`);
    } else if (file.parentOf(node)?.type === "TSParameterProperty") {
      report(node, "which is also a class member");
    }
  }
  const [declaration] = binding.declarations;
  const exported = binding.has(BindingFlags.Exported) && !binding.has(BindingFlags.Default);
  const kept = sites !== null && sites.length > 0 && !exportsImplicitly(file);
  if (declaration !== undefined && exported && !kept) {
    report(declaration, "which also names its export");
  }
  return diagnostics;
}

function conflicts(binding: Binding, name: string): Diagnostic[] {
  const { file, scope } = binding;
  const diagnostics: Diagnostic[] = [];
  const report = (node: Node, outcome: string, details: DiagnosticDetails = {}) => {
    const message = `Renaming '${binding.name}' to '${name}' would ${outcome}`;
    diagnostics.push(diagnosticAt(file, node, message, details));
  };

  const [declaration] = binding.declarations;
  if (declaration !== undefined) {
    if (isEvalOrArguments(name) && scope.strict) {
      report(declaration, `declare '${name}' in strict mode code`);
    }
    const existing = scope.find(name);
    if (existing !== null) report(declaration, `redeclare '${name}'`, declaredHere(existing));
  }
  for (const reference of binding.references) {
    const found = reference.scope.lookup(name, reference.space);
    if (found !== null && found.scope !== scope && scope.contains(found.scope)) {
      report(reference.node, `let another '${name}' shadow this use`, declaredHere(found));
    }
    if (reference.node.type === "JSXIdentifier" && isIntrinsicTag(file, reference.node, name)) {
      report(reference.node, "read this tag as an intrinsic element", {
        help: "Start the name in uppercase",
      });
    }
  }
  for (const reference of file.references) {
    if (reference.name !== name || !scope.contains(reference.scope)) continue;
    if (!binding.visibleIn(reference.space)) continue;
    const target = reference.binding;
    if (target === null || !scope.contains(target.scope)) {
      report(reference.node, `capture this use of '${name}'`, declaredHere(binding));
    }
  }
  return diagnostics;
}

function declaredHere(binding: Binding): DiagnosticDetails {
  const message = `'${binding.name}' is declared here`;
  const labels = binding.declarations
    .slice(0, 1)
    .map(({ start, end }) => ({ start, end, message }));
  return { labels };
}

function exportSites(binding: Binding, identifiers: Identifier[]): ExportSite[] | null {
  if (!binding.has(BindingFlags.Exported) || binding.has(BindingFlags.Default)) return [];
  const { file } = binding;
  const sites: ExportSite[] = [];
  for (const identifier of identifiers) {
    const site = exportOf(file, identifier);
    if (site === undefined) continue;
    if (site === null) return null;
    if (!sites.some(({ statement }) => statement === site.statement)) sites.push(site);
  }
  return sites;
}

function exportOf(file: SourceFile, identifier: Node): ExportSite | null | undefined {
  let declaration: Node = identifier;
  for (const ancestor of file.ancestors(identifier)) {
    if (ancestor.type === "Program" || ancestor.type === "TSModuleBlock") return undefined;
    if (ancestor.type === "ExportNamedDeclaration") {
      const slot = slotOf(file.program, ancestor);
      return slot === null ? null : { statement: ancestor, declaration, slot };
    }
    declaration = ancestor;
  }
  return undefined;
}

// an export list would stop a declaration file exporting its unexported declarations
function exportsImplicitly(file: SourceFile): boolean {
  return file.rootScope.bindings.some(
    (binding) =>
      binding.has(BindingFlags.Exported) &&
      binding.declarations.every((node) => exportOf(file, node) === undefined),
  );
}

function isIntrinsicTag(file: SourceFile, node: JSXIdentifier, name: string): boolean {
  const parent = file.parentOf(node);
  const isTag = parent?.type === "JSXOpeningElement" || parent?.type === "JSXClosingElement";
  return isTag && !/^[A-Z_$]/.test(name);
}

function renameNode(file: SourceFile, node: Identifier | JSXIdentifier, name: string): void {
  if (node.type === "Identifier") {
    const parent = file.parentOf(node);
    const holder = parent?.type === "AssignmentPattern" ? file.parentOf(parent) : parent;
    if (holder?.type === "Property" && holder.shorthand) holder.shorthand = false;
  }
  node.name = name;
}

function keepExportNames(site: ExportSite, previous: string, name: string): void {
  const { statement, declaration, slot } = site;
  const list = b.ExportNamedDeclaration({
    specifiers: declaredNames(declaration).map((local) =>
      b.ExportSpecifier({
        local: b.Identifier({ name: local }),
        exported: b.Identifier({ name: local === name ? previous : local }),
      }),
    ),
  });
  if (statement.exportKind !== undefined) list.exportKind = statement.exportKind;
  replaceAt(slot, [declaration, list]);
}

function declaredNames(declaration: Node): string[] {
  if (declaration.type === "VariableDeclaration") {
    return declaration.declarations.flatMap((declarator) =>
      bindingIdentifiers(declarator.id).map((id) => id.name),
    );
  }
  return "id" in declaration && declaration.id?.type === "Identifier" ? [declaration.id.name] : [];
}
