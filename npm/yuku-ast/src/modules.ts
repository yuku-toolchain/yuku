import type {
  Declaration,
  ExportAllDeclaration,
  ExportDefaultDeclaration,
  ExportNamedDeclaration,
  ExportSpecifier,
  ImportDeclaration,
  ImportDeclarationSpecifier,
  ImportPhase,
  Program,
} from "@yuku-toolchain/types";
import { bindingIdentifiers, nameOf } from "./utils.ts";

/** One imported binding, one record per specifier. */
export interface CollectedImport {
  /** The module specifier string. */
  source: string;
  /** The local binding name. */
  local: string;
  /** The imported name: `"default"`, `"*"` for namespaces, or the export name. */
  imported: string;
  /** True for `import type` and `import { type x }`. */
  typeOnly: boolean;
  /** Stage 3 phase modifier, or null. */
  phase: ImportPhase | null;
  specifier: ImportDeclarationSpecifier;
  declaration: ImportDeclaration;
}

/**
 * The imported bindings of one declaration, one record per specifier.
 * Type-only imports are included with `typeOnly` set, side-effect
 * imports introduce no bindings and yield no records. Composes with a
 * walk: `ImportDeclaration(node) { records.push(...collectImportDeclaration(node)) }`.
 */
export function collectImportDeclaration(
  declaration: ImportDeclaration,
): CollectedImport[] {
  const source = declaration.source.value;
  const declarationTypeOnly = declaration.importKind === "type";
  return declaration.specifiers.map((specifier) => ({
    source,
    local: specifier.local.name,
    imported:
      specifier.type === "ImportSpecifier"
        ? nameOf(specifier.imported)
        : specifier.type === "ImportNamespaceSpecifier"
          ? "*"
          : "default",
    typeOnly:
      declarationTypeOnly ||
      (specifier.type === "ImportSpecifier" && specifier.importKind === "type"),
    phase: declaration.phase ?? null,
    specifier,
    declaration,
  }));
}

/** Every imported binding of a program, in source order. */
export function collectImports(program: Program): CollectedImport[] {
  return program.body.flatMap((statement) =>
    statement.type === "ImportDeclaration"
      ? collectImportDeclaration(statement)
      : [],
  );
}

/** One exported name, one record per binding or specifier. */
export interface CollectedExport {
  /** The exported name, or null for bare `export *`. */
  exported: string | null;
  /** The local name backing the export, or null for re-exports and anonymous defaults. */
  local: string | null;
  /** The re-export source specifier, or null for local exports. */
  source: string | null;
  /** True for `export type` and `export { type x }`. */
  typeOnly: boolean;
  node:
    | ExportNamedDeclaration
    | ExportDefaultDeclaration
    | ExportAllDeclaration
    | ExportSpecifier;
}

/**
 * The exports of one declaration: declaration forms expand to one
 * record per bound name, specifiers to one record each, and `export *`
 * to a single record with a null `exported`. Composes with a walk.
 */
export function collectExportDeclaration(
  statement:
    ExportNamedDeclaration | ExportDefaultDeclaration | ExportAllDeclaration,
): CollectedExport[] {
  const out: CollectedExport[] = [];
  switch (statement.type) {
    case "ExportNamedDeclaration": {
      const typeOnly = statement.exportKind === "type";
      if (statement.declaration !== null) {
        for (const name of declaredNames(statement.declaration)) {
          out.push({
            exported: name,
            local: name,
            source: null,
            typeOnly,
            node: statement,
          });
        }
        break;
      }
      const source = statement.source === null ? null : statement.source.value;
      for (const specifier of statement.specifiers) {
        out.push({
          exported: nameOf(specifier.exported),
          local: source === null ? nameOf(specifier.local) : null,
          source,
          typeOnly: typeOnly || specifier.exportKind === "type",
          node: specifier,
        });
      }
      break;
    }
    case "ExportDefaultDeclaration": {
      const declaration = statement.declaration;
      out.push({
        exported: "default",
        local:
          declaration.type === "Identifier"
            ? declaration.name
            : "id" in declaration
              ? (declaration.id?.name ?? null)
              : null,
        source: null,
        typeOnly: false,
        node: statement,
      });
      break;
    }
    case "ExportAllDeclaration": {
      out.push({
        exported:
          statement.exported === null ? null : nameOf(statement.exported),
        local: null,
        source: statement.source.value,
        typeOnly: statement.exportKind === "type",
        node: statement,
      });
      break;
    }
  }
  return out;
}

/** Every export of a program, in source order. */
export function collectExports(program: Program): CollectedExport[] {
  return program.body.flatMap((statement) =>
    statement.type === "ExportNamedDeclaration" ||
    statement.type === "ExportDefaultDeclaration" ||
    statement.type === "ExportAllDeclaration"
      ? collectExportDeclaration(statement)
      : [],
  );
}

function declaredNames(declaration: Declaration): string[] {
  if (declaration.type === "VariableDeclaration") {
    return declaration.declarations.flatMap((declarator) =>
      bindingIdentifiers(declarator.id).map((id) => id.name),
    );
  }
  if (declaration.type === "TSModuleDeclaration") {
    const name = nameOf(declaration.id);
    return name === null ? [] : [name];
  }
  return declaration.id === null ? [] : [declaration.id.name];
}
