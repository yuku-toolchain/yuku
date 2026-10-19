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
} from "../types";
import { bindingIdentifiers, nameOf } from "./utils";

/** One imported binding. */
export interface CollectedImport {
  /** The module specifier. */
  source: string;
  /** The local binding name. */
  local: string;
  /** The export name, `"default"`, or `"*"` for a namespace. */
  imported: string;
  /** Whether it is `import type` or `import { type x }`. */
  typeOnly: boolean;
  /** Stage 3 phase modifier, or null. */
  phase: ImportPhase | null;
  specifier: ImportDeclarationSpecifier;
  declaration: ImportDeclaration;
}

/** The bindings one declaration imports, none for a side-effect import. */
export function collectImportDeclaration(declaration: ImportDeclaration): CollectedImport[] {
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
    statement.type === "ImportDeclaration" ? collectImportDeclaration(statement) : [],
  );
}

/** One exported name. */
export interface CollectedExport {
  /** Null for a bare `export *`. */
  exported: string | null;
  /** Null for a re-export or an anonymous default. */
  local: string | null;
  /** The module a re-export takes from. */
  source: string | null;
  /** Whether it is `export type` or `export { type x }`. */
  typeOnly: boolean;
  node: ExportNamedDeclaration | ExportDefaultDeclaration | ExportAllDeclaration | ExportSpecifier;
}

/** The names one export declaration exports, one per bound name or specifier. */
export function collectExportDeclaration(
  statement: ExportNamedDeclaration | ExportDefaultDeclaration | ExportAllDeclaration,
): CollectedExport[] {
  switch (statement.type) {
    case "ExportNamedDeclaration": {
      const typeOnly = statement.exportKind === "type";
      if (statement.declaration !== null) {
        return declaredNames(statement.declaration).map((name) => ({
          exported: name,
          local: name,
          source: null,
          typeOnly,
          node: statement,
        }));
      }
      const source = statement.source === null ? null : statement.source.value;
      return statement.specifiers.map((specifier) => ({
        exported: nameOf(specifier.exported),
        local: source === null ? nameOf(specifier.local) : null,
        source,
        typeOnly: typeOnly || specifier.exportKind === "type",
        node: specifier,
      }));
    }
    case "ExportDefaultDeclaration": {
      const declaration = statement.declaration;
      return [
        {
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
        },
      ];
    }
    case "ExportAllDeclaration":
      return [
        {
          exported: statement.exported === null ? null : nameOf(statement.exported),
          local: null,
          source: statement.source.value,
          typeOnly: statement.exportKind === "type",
          node: statement,
        },
      ];
  }
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
