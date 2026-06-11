//! Module import/export records.
//!
//! Walks the top level of an analyzed module and produces the flat
//! records a cross-file linker consumes: which names this module pulls
//! in from which specifiers, and which names it exposes. The shapes
//! mirror the spec's ImportEntry/ExportEntry model (ImportEntries,
//! 16.2.2.2, and ExportEntries, 16.2.3.4): `default` is not a special
//! kind, it is the export *name* "default".
//!
//! Records reference local bindings by `SymbolId`, so a linker joins an
//! importing module's record to the defining module's record and lands
//! directly on the defining symbol.

const std = @import("std");
const ast = @import("../ast.zig");
const binder = @import("binder.zig");
const sc = @import("scope.zig");

const Allocator = std.mem.Allocator;
const SymbolId = binder.SymbolId;
const SymbolTable = binder.SymbolTable;

/// What an import or re-export takes from a module, or what an export
/// exposes.
pub const NameKind = enum(u3) {
    /// A specific export name. Default imports/exports use the name
    /// "default", exactly as the spec models them.
    named,
    /// The module namespace object (`* as ns`) or all names (`export *`).
    star,
    /// No binding at all: a side-effect import (`import "m"`).
    none,
    /// The module's entire export value: TS `export = expr`. Exports
    /// only; consumed by `import x = require("m")`.
    equals,
    /// A UMD global alias: TS `export as namespace N` exposes the
    /// module under the global name `N`. Exports only; not reachable
    /// from the import graph.
    global,
};

/// One imported binding (or a side-effect import) of this module.
pub const ImportRecord = struct {
    /// The local binding symbol. `.none` for side-effect imports.
    symbol: SymbolId,
    /// What is taken from the source module.
    name_kind: NameKind,
    /// The imported name when `name_kind` is `.named` ("default" for
    /// default imports). `.empty` otherwise.
    name: ast.String,
    /// Decoded module specifier text.
    specifier: ast.String,
    /// `.type` for `import type` and `import { type x }` bindings.
    kind: ast.ImportOrExportKind,
    /// Stage 3 phase modifier, or `null` for a regular import.
    phase: ?ast.ImportPhase,
    /// The specifier node, or the whole declaration for side-effect
    /// imports.
    node: ast.NodeIndex,
};

/// One exported name of this module.
pub const ExportRecord = struct {
    /// `.named` for every named export including "default". `.star`
    /// only for `export * from "m"` without an alias. `.equals` for
    /// `export =` and `.global` for `export as namespace`.
    name_kind: NameKind,
    /// The exported name when `name_kind` is `.named`, or the global
    /// name when `.global`. `.empty` for `export *` and `export =`.
    name: ast.String,
    /// The local symbol backing the export. `.none` for re-exports,
    /// anonymous default exports, and expression default or `export =`
    /// exports that do not resolve to a module-scope binding.
    symbol: SymbolId,
    /// What a re-export takes from its source module. `.none` when the
    /// export is local (no `from` clause).
    from_kind: NameKind,
    /// The source-module name when `from_kind` is `.named`.
    from_name: ast.String,
    /// Decoded specifier of the `from` clause. `.empty` when local.
    specifier: ast.String,
    /// `.type` for type-only exports.
    kind: ast.ImportOrExportKind,
    /// The specifier or declaration node, for spans and diagnostics.
    node: ast.NodeIndex,
};

/// The collected records of one module, allocated in the tree's arena.
pub const ModuleRecords = struct {
    imports: []const ImportRecord,
    exports: []const ExportRecord,

    pub const empty: ModuleRecords = .{ .imports = &.{}, .exports = &.{} };
};

/// Collects import/export records from the top level of `tree`.
pub fn collect(tree: *ast.Tree, table: *const SymbolTable) Allocator.Error!ModuleRecords {
    std.debug.assert(tree.root != .null);

    var collector = Collector{
        .tree = tree,
        .table = table,
        .allocator = tree.allocator(),
        // spec-true name for default imports/exports. the pool dedups,
        // so repeated analyses cost nothing.
        .default_name = try tree.addString("default"),
        // scripts have no module scope, their top level is the global
        .top_scope = if (tree.isModule()) .module else .root,
    };

    const program = tree.data(tree.root).program;
    for (tree.extra(program.body)) |statement| {
        try collector.statement(statement);
    }

    return .{
        .imports = collector.imports.items,
        .exports = collector.exports.items,
    };
}

const Collector = struct {
    tree: *ast.Tree,
    table: *const SymbolTable,
    allocator: Allocator,
    default_name: ast.String,
    /// The scope top-level bindings live in: `.module` for modules,
    /// `.root` (global) for scripts.
    top_scope: sc.ScopeId,
    imports: std.ArrayList(ImportRecord) = .empty,
    exports: std.ArrayList(ExportRecord) = .empty,

    fn statement(self: *Collector, index: ast.NodeIndex) Allocator.Error!void {
        switch (self.tree.data(index)) {
            .import_declaration => |decl| try self.importDeclaration(decl, index),
            .ts_import_equals_declaration => |decl| try self.importEquals(decl, index),
            .export_named_declaration => |decl| try self.exportNamed(decl, index),
            .export_default_declaration => |decl| try self.exportDefault(decl, index),
            .export_all_declaration => |decl| try self.exportAll(decl, index),
            .ts_export_assignment => |decl| try self.exportAssignment(decl, index),
            .ts_namespace_export_declaration => |decl| try self.namespaceExport(decl, index),
            else => {},
        }
    }

    fn importDeclaration(
        self: *Collector,
        decl: ast.ImportDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        const specifier = self.stringOf(decl.source);
        const specifiers = self.tree.extra(decl.specifiers);

        if (specifiers.len == 0) {
            try self.imports.append(self.allocator, .{
                .symbol = .none,
                .name_kind = .none,
                .name = .empty,
                .specifier = specifier,
                .kind = decl.import_kind,
                .phase = decl.phase,
                .node = index,
            });
            return;
        }

        for (specifiers) |spec_index| {
            switch (self.tree.data(spec_index)) {
                .import_specifier => |spec| {
                    // `import type { x }` makes every binding type-only.
                    // `import { type x }` marks just this one.
                    const kind: ast.ImportOrExportKind =
                        if (decl.import_kind == .type) .type else spec.import_kind;
                    try self.imports.append(self.allocator, .{
                        .symbol = self.localSymbol(spec.local),
                        .name_kind = .named,
                        .name = self.stringOf(spec.imported),
                        .specifier = specifier,
                        .kind = kind,
                        .phase = decl.phase,
                        .node = spec_index,
                    });
                },
                .import_default_specifier => |spec| {
                    try self.imports.append(self.allocator, .{
                        .symbol = self.localSymbol(spec.local),
                        .name_kind = .named,
                        .name = self.default_name,
                        .specifier = specifier,
                        .kind = decl.import_kind,
                        .phase = decl.phase,
                        .node = spec_index,
                    });
                },
                .import_namespace_specifier => |spec| {
                    try self.imports.append(self.allocator, .{
                        .symbol = self.localSymbol(spec.local),
                        .name_kind = .star,
                        .name = .empty,
                        .specifier = specifier,
                        .kind = decl.import_kind,
                        .phase = decl.phase,
                        .node = spec_index,
                    });
                },
                // error-recovery trees can hold unexpected children
                else => {},
            }
        }
    }

    /// `import x = require("m")` binds the whole module, like a
    /// namespace import. The qualified-name form (`import a = B.C`)
    /// aliases a namespace, not a module, and is not a graph edge.
    fn importEquals(
        self: *Collector,
        decl: ast.TSImportEqualsDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        if (self.tree.data(decl.module_reference) != .ts_external_module_reference) return;
        const reference = self.tree.data(decl.module_reference).ts_external_module_reference;

        try self.imports.append(self.allocator, .{
            .symbol = self.localSymbol(decl.id),
            .name_kind = .star,
            .name = .empty,
            .specifier = self.stringOf(reference.expression),
            .kind = decl.import_kind,
            .phase = null,
            .node = index,
        });
    }

    fn exportNamed(
        self: *Collector,
        decl: ast.ExportNamedDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        if (decl.declaration != .null) {
            std.debug.assert(decl.specifiers.len == 0);
            try self.exportDeclarationNames(decl.declaration, decl.export_kind, index);
            return;
        }

        const has_source = decl.source != .null;
        const specifier = if (has_source) self.stringOf(decl.source) else ast.String.empty;

        for (self.tree.extra(decl.specifiers)) |spec_index| {
            if (self.tree.data(spec_index) != .export_specifier) continue;
            const spec = self.tree.data(spec_index).export_specifier;
            const kind: ast.ImportOrExportKind =
                if (decl.export_kind == .type) .type else spec.export_kind;
            const local_name = self.stringOf(spec.local);

            if (has_source) {
                // `export { x as y } from "m"`: nothing local is bound
                try self.exports.append(self.allocator, .{
                    .name_kind = .named,
                    .name = self.stringOf(spec.exported),
                    .symbol = .none,
                    .from_kind = .named,
                    .from_name = local_name,
                    .specifier = specifier,
                    .kind = kind,
                    .node = spec_index,
                });
            } else {
                try self.exports.append(self.allocator, .{
                    .name_kind = .named,
                    .name = self.stringOf(spec.exported),
                    .symbol = self.moduleBinding(local_name),
                    .from_kind = .none,
                    .from_name = .empty,
                    .specifier = .empty,
                    .kind = kind,
                    .node = spec_index,
                });
            }
        }
    }

    /// `export <decl>`: every bound name of the declaration is exported
    /// under its own name (spec: ExportEntries of `export Declaration`).
    fn exportDeclarationNames(
        self: *Collector,
        declaration: ast.NodeIndex,
        kind: ast.ImportOrExportKind,
        node: ast.NodeIndex,
    ) Allocator.Error!void {
        switch (self.tree.data(declaration)) {
            .variable_declaration => |decl| {
                for (self.tree.extra(decl.declarators)) |declarator_index| {
                    const declarator = self.tree.data(declarator_index).variable_declarator;
                    try self.exportBoundNames(declarator.id, kind, node, 0);
                }
            },
            .function => |func| {
                if (func.id != .null) try self.exportLocal(func.id, kind, node);
            },
            .class => |cls| {
                if (cls.id != .null) try self.exportLocal(cls.id, kind, node);
            },
            .ts_type_alias_declaration => |decl| try self.exportLocal(decl.id, .type, node),
            .ts_interface_declaration => |decl| try self.exportLocal(decl.id, .type, node),
            .ts_enum_declaration => |decl| try self.exportLocal(decl.id, kind, node),
            .ts_module_declaration => |decl| {
                // ambient `declare module "m"` has a string-literal id,
                // which is not an exported binding
                if (decl.id != .null and self.tree.data(decl.id) == .binding_identifier) {
                    try self.exportLocal(decl.id, kind, node);
                }
            },
            else => {},
        }
    }

    /// walks a binding pattern, exporting every bound identifier.
    /// recursion is bounded by source pattern nesting depth.
    fn exportBoundNames(
        self: *Collector,
        pattern: ast.NodeIndex,
        kind: ast.ImportOrExportKind,
        node: ast.NodeIndex,
        depth: u32,
    ) Allocator.Error!void {
        std.debug.assert(depth < 256);
        if (pattern == .null) return;

        switch (self.tree.data(pattern)) {
            .binding_identifier => try self.exportLocal(pattern, kind, node),
            .assignment_pattern => |p| {
                try self.exportBoundNames(p.left, kind, node, depth + 1);
            },
            .binding_rest_element => |p| {
                try self.exportBoundNames(p.argument, kind, node, depth + 1);
            },
            .array_pattern => |p| {
                for (self.tree.extra(p.elements)) |element| {
                    try self.exportBoundNames(element, kind, node, depth + 1);
                }
                try self.exportBoundNames(p.rest, kind, node, depth + 1);
            },
            .object_pattern => |p| {
                for (self.tree.extra(p.properties)) |property| {
                    if (self.tree.data(property) == .binding_property) {
                        const value = self.tree.data(property).binding_property.value;
                        try self.exportBoundNames(value, kind, node, depth + 1);
                    }
                }
                try self.exportBoundNames(p.rest, kind, node, depth + 1);
            },
            else => {},
        }
    }

    fn exportLocal(
        self: *Collector,
        binding: ast.NodeIndex,
        kind: ast.ImportOrExportKind,
        node: ast.NodeIndex,
    ) Allocator.Error!void {
        std.debug.assert(self.tree.data(binding) == .binding_identifier);
        const name = self.tree.data(binding).binding_identifier.name;
        try self.exports.append(self.allocator, .{
            .name_kind = .named,
            .name = name,
            .symbol = self.moduleBinding(name),
            .from_kind = .none,
            .from_name = .empty,
            .specifier = .empty,
            .kind = kind,
            .node = node,
        });
    }

    fn exportDefault(
        self: *Collector,
        decl: ast.ExportDefaultDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        // anonymous defaults and arbitrary expressions bind nothing
        const symbol: SymbolId = switch (self.tree.data(decl.declaration)) {
            .function => |func| if (func.id != .null)
                self.localSymbol(func.id)
            else
                .none,
            .class => |cls| if (cls.id != .null)
                self.localSymbol(cls.id)
            else
                .none,
            .identifier_reference => |id| self.moduleBinding(id.name),
            else => .none,
        };

        try self.exports.append(self.allocator, .{
            .name_kind = .named,
            .name = self.default_name,
            .symbol = symbol,
            .from_kind = .none,
            .from_name = .empty,
            .specifier = .empty,
            .kind = .value,
            .node = index,
        });
    }

    fn exportAll(
        self: *Collector,
        decl: ast.ExportAllDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        const specifier = self.stringOf(decl.source);

        if (decl.exported != .null) {
            // `export * as ns from "m"`: a named export of the namespace
            try self.exports.append(self.allocator, .{
                .name_kind = .named,
                .name = self.stringOf(decl.exported),
                .symbol = .none,
                .from_kind = .star,
                .from_name = .empty,
                .specifier = specifier,
                .kind = decl.export_kind,
                .node = index,
            });
        } else {
            try self.exports.append(self.allocator, .{
                .name_kind = .star,
                .name = .empty,
                .symbol = .none,
                .from_kind = .star,
                .from_name = .empty,
                .specifier = specifier,
                .kind = decl.export_kind,
                .node = index,
            });
        }
    }

    /// `export = expr` (legacy TS, CommonJS-style): the expression is
    /// the module's entire export value. Like default exports, only an
    /// identifier expression resolves to a module-scope binding.
    fn exportAssignment(
        self: *Collector,
        decl: ast.TSExportAssignment,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        const symbol: SymbolId = switch (self.tree.data(decl.expression)) {
            .identifier_reference => |id| self.moduleBinding(id.name),
            else => .none,
        };

        try self.exports.append(self.allocator, .{
            .name_kind = .equals,
            .name = .empty,
            .symbol = symbol,
            .from_kind = .none,
            .from_name = .empty,
            .specifier = .empty,
            .kind = .value,
            .node = index,
        });
    }

    /// `export as namespace N` (legacy TS, UMD): exposes the module
    /// under the global name `N`. The name is a global alias, not a
    /// local binding, so no symbol backs it.
    fn namespaceExport(
        self: *Collector,
        decl: ast.TSNamespaceExportDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        try self.exports.append(self.allocator, .{
            .name_kind = .global,
            .name = self.stringOf(decl.id),
            .symbol = .none,
            .from_kind = .none,
            .from_name = .empty,
            .specifier = .empty,
            .kind = .value,
            .node = index,
        });
    }

    /// the symbol bound by a specifier's local `binding_identifier`.
    /// import bindings land directly in the module scope, so a direct
    /// scope lookup finds them.
    fn localSymbol(self: *const Collector, binding: ast.NodeIndex) SymbolId {
        std.debug.assert(binding != .null);
        std.debug.assert(self.tree.data(binding) == .binding_identifier);
        return self.moduleBinding(self.tree.data(binding).binding_identifier.name);
    }

    /// looks up `name` in the top-level scope, including hoisted vars.
    fn moduleBinding(self: *const Collector, name: ast.String) SymbolId {
        const text = self.tree.string(name);
        // error-recovery trees can reference names that never bound
        return self.table.findInScopeOrHoisted(self.top_scope, text) orelse .none;
    }

    /// name text handle of a ModuleExportName-shaped node: an
    /// identifier, a binding identifier, or a string literal.
    fn stringOf(self: *const Collector, node: ast.NodeIndex) ast.String {
        std.debug.assert(node != .null);
        return switch (self.tree.data(node)) {
            .identifier_name => |id| id.name,
            .identifier_reference => |id| id.name,
            .binding_identifier => |id| id.name,
            .string_literal => |lit| lit.value,
            else => .empty,
        };
    }
};
