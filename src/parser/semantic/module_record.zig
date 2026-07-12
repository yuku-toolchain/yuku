//! Module import and export records.

const std = @import("std");
const ast = @import("../ast.zig");
const binder = @import("binder.zig");
const sc = @import("scope.zig");

const Allocator = std.mem.Allocator;
const SymbolId = binder.SymbolId;
const Semantic = binder.Semantic;

/// One dependency edge of the module, static or dynamic.
pub const Import = struct {
    /// The import form.
    kind: Kind,
    /// Imported name of a `.named` record, "default" for default imports.
    name: ast.String = .empty,
    /// Local binding, `.none` when the form binds nothing.
    symbol: SymbolId = .none,
    /// Decoded module specifier.
    specifier: ast.String,
    /// True for type only imports.
    type_only: bool = false,
    /// Phase modifier, `null` for regular imports.
    phase: ?ast.ImportPhase = null,
    /// The smallest node identifying the record. The import specifier,
    /// the call expression for `.dynamic` and `.require`, or the whole
    /// declaration for `.side_effect` and `.import_equals`.
    node: ast.NodeIndex,

    pub const Kind = enum(u3) {
        /// `import d from "m"` and `import { a as b } from "m"`
        named,
        /// `import * as ns from "m"`
        namespace,
        /// `import "m"`
        side_effect,
        /// `import x = require("m")`
        import_equals,
        /// `import("m")` at any depth
        dynamic,
        /// `require("m")` at any depth, only when `require` is a free name
        require,
    };
};

/// One exported name of the module.
pub const Export = struct {
    /// The export form.
    kind: Kind,
    /// Exported name, the global name for `.global`, empty otherwise.
    name: ast.String = .empty,
    /// Backing local binding, `.none` when nothing local backs the export.
    symbol: SymbolId = .none,
    /// Name taken from the source module by a `.re_export`.
    from_name: ast.String = .empty,
    /// Decoded specifier of the `from` clause, empty for local exports.
    specifier: ast.String = .empty,
    /// True for type only exports.
    type_only: bool = false,
    /// The smallest node identifying the record. The export specifier,
    /// the binding identifier of an exported declaration, or the whole
    /// declaration.
    node: ast.NodeIndex,

    pub const Kind = enum(u3) {
        /// `export { x }` and exported declarations, `export default` included
        named,
        /// `export { a as b } from "m"`
        re_export,
        /// `export * as ns from "m"`
        namespace,
        /// `export * from "m"`
        star,
        /// `export = expr`
        equals,
        /// `export as namespace N`
        global,
    };
};

/// CommonJS classification signals, from free value references at any depth.
pub const Flags = packed struct(u8) {
    /// A free `require` reference appears.
    uses_require: bool = false,
    /// A free `module` reference appears.
    uses_module: bool = false,
    /// A free `exports` reference appears.
    uses_exports: bool = false,
    /// `import.meta` appears.
    uses_import_meta: bool = false,
    _: u4 = 0,
};

/// The records of one module, allocated in the tree arena.
pub const Records = struct {
    imports: []const Import,
    exports: []const Export,
    flags: Flags,

    pub const empty: Records = .{
        .imports = &.{},
        .exports = &.{},
        .flags = .{},
    };
};

/// Collects the records of an analyzed tree. Static records come first
/// in source order, then dynamic and require records in source order.
pub fn collect(tree: *ast.Tree, sem: *const Semantic) Allocator.Error!Records {
    std.debug.assert(tree.root != .null);

    var collector = Collector{
        .tree = tree,
        .sem = sem,
        .allocator = tree.allocator(),
        .default_name = try tree.addString("default"),
        // scripts bind top level names in the global scope
        .top_scope = if (tree.isModule()) .module else .root,
    };

    for (tree.extra(tree.data(tree.root).program.body)) |statement| {
        try collector.statement(statement);
    }
    try collector.sweep();

    return .{
        .imports = collector.imports.items,
        .exports = collector.exports.items,
        .flags = collector.flags,
    };
}

const Collector = struct {
    tree: *ast.Tree,
    sem: *const Semantic,
    allocator: Allocator,
    default_name: ast.String,
    top_scope: sc.ScopeId,
    imports: std.ArrayList(Import) = .empty,
    exports: std.ArrayList(Export) = .empty,
    flags: Flags = .{},

    fn addImport(self: *Collector, record: Import) Allocator.Error!void {
        try self.imports.append(self.allocator, record);
    }

    fn addExport(self: *Collector, record: Export) Allocator.Error!void {
        try self.exports.append(self.allocator, record);
    }

    fn statement(self: *Collector, index: ast.NodeIndex) Allocator.Error!void {
        switch (self.tree.data(index)) {
            .import_declaration => |decl| try self.importDeclaration(decl, index),
            .ts_import_equals_declaration => |decl| try self.importEquals(decl, index),
            .export_named_declaration => |decl| try self.exportNamed(decl),
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
        const type_only = decl.import_kind == .type;
        const specifiers = self.tree.extra(decl.specifiers);

        if (specifiers.len == 0) {
            return self.addImport(.{
                .kind = .side_effect,
                .specifier = specifier,
                .type_only = type_only,
                .phase = decl.phase,
                .node = index,
            });
        }

        for (specifiers) |spec_index| {
            switch (self.tree.data(spec_index)) {
                .import_specifier => |spec| try self.addImport(.{
                    .kind = .named,
                    .name = self.stringOf(spec.imported),
                    .symbol = self.bindingSymbol(spec.local),
                    .specifier = specifier,
                    .type_only = type_only or spec.import_kind == .type,
                    .phase = decl.phase,
                    .node = spec_index,
                }),
                .import_default_specifier => |spec| try self.addImport(.{
                    .kind = .named,
                    .name = self.default_name,
                    .symbol = self.bindingSymbol(spec.local),
                    .specifier = specifier,
                    .type_only = type_only,
                    .phase = decl.phase,
                    .node = spec_index,
                }),
                .import_namespace_specifier => |spec| try self.addImport(.{
                    .kind = .namespace,
                    .symbol = self.bindingSymbol(spec.local),
                    .specifier = specifier,
                    .type_only = type_only,
                    .phase = decl.phase,
                    .node = spec_index,
                }),
                else => {},
            }
        }
    }

    /// Only the external module reference form is a dependency edge.
    /// The qualified name form aliases a namespace, not a module.
    fn importEquals(
        self: *Collector,
        decl: ast.TSImportEqualsDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        if (self.tree.data(decl.module_reference) != .ts_external_module_reference) return;
        const reference = self.tree.data(decl.module_reference).ts_external_module_reference;
        try self.addImport(.{
            .kind = .import_equals,
            .symbol = self.bindingSymbol(decl.id),
            .specifier = self.stringOf(reference.expression),
            .type_only = decl.import_kind == .type,
            .node = index,
        });
    }

    fn exportNamed(
        self: *Collector,
        decl: ast.ExportNamedDeclaration,
    ) Allocator.Error!void {
        if (decl.declaration != .null) {
            return self.declarationNames(decl.declaration, decl.export_kind == .type);
        }

        const has_source = decl.source != .null;
        const specifier = if (has_source) self.stringOf(decl.source) else ast.String.empty;

        for (self.tree.extra(decl.specifiers)) |spec_index| {
            if (self.tree.data(spec_index) != .export_specifier) continue;
            const spec = self.tree.data(spec_index).export_specifier;
            const type_only = decl.export_kind == .type or spec.export_kind == .type;

            if (has_source) {
                try self.addExport(.{
                    .kind = .re_export,
                    .name = self.stringOf(spec.exported),
                    .from_name = self.stringOf(spec.local),
                    .specifier = specifier,
                    .type_only = type_only,
                    .node = spec_index,
                });
            } else {
                try self.addExport(.{
                    .kind = .named,
                    .name = self.stringOf(spec.exported),
                    .symbol = self.moduleBinding(self.stringOf(spec.local)),
                    .type_only = type_only,
                    .node = spec_index,
                });
            }
        }
    }

    /// Every name bound by an exported declaration is exported under
    /// its own name.
    fn declarationNames(
        self: *Collector,
        declaration: ast.NodeIndex,
        type_only: bool,
    ) Allocator.Error!void {
        switch (self.tree.data(declaration)) {
            .variable_declaration => |decl| {
                for (self.tree.extra(decl.declarators)) |declarator| {
                    const id = self.tree.data(declarator).variable_declarator.id;
                    try self.patternNames(id, type_only, 0);
                }
            },
            .function => |func| {
                if (func.id != .null) try self.exportLocal(func.id, type_only);
            },
            .class => |cls| {
                if (cls.id != .null) try self.exportLocal(cls.id, type_only);
            },
            .ts_type_alias_declaration => |decl| try self.exportLocal(decl.id, true),
            .ts_interface_declaration => |decl| try self.exportLocal(decl.id, true),
            .ts_enum_declaration => |decl| try self.exportLocal(decl.id, type_only),
            .ts_module_declaration => |decl| {
                // ambient module ids are string literals, not bindings
                if (decl.id != .null and self.tree.data(decl.id) == .binding_identifier) {
                    try self.exportLocal(decl.id, type_only);
                }
            },
            else => {},
        }
    }

    /// Exports every identifier bound by a binding pattern.
    fn patternNames(
        self: *Collector,
        pattern: ast.NodeIndex,
        type_only: bool,
        depth: u32,
    ) Allocator.Error!void {
        std.debug.assert(depth < 256);
        if (pattern == .null) return;

        switch (self.tree.data(pattern)) {
            .binding_identifier => try self.exportLocal(pattern, type_only),
            .assignment_pattern => |p| {
                try self.patternNames(p.left, type_only, depth + 1);
            },
            .binding_rest_element => |p| {
                try self.patternNames(p.argument, type_only, depth + 1);
            },
            .array_pattern => |p| {
                for (self.tree.extra(p.elements)) |element| {
                    try self.patternNames(element, type_only, depth + 1);
                }
                try self.patternNames(p.rest, type_only, depth + 1);
            },
            .object_pattern => |p| {
                for (self.tree.extra(p.properties)) |property| {
                    if (self.tree.data(property) == .binding_property) {
                        const value = self.tree.data(property).binding_property.value;
                        try self.patternNames(value, type_only, depth + 1);
                    }
                }
                try self.patternNames(p.rest, type_only, depth + 1);
            },
            else => {},
        }
    }

    fn exportLocal(
        self: *Collector,
        binding: ast.NodeIndex,
        type_only: bool,
    ) Allocator.Error!void {
        const name = self.tree.data(binding).binding_identifier.name;
        try self.addExport(.{
            .kind = .named,
            .name = name,
            .symbol = self.moduleBinding(name),
            .type_only = type_only,
            .node = binding,
        });
    }

    /// Only named declarations and identifiers bind a symbol.
    fn exportDefault(
        self: *Collector,
        decl: ast.ExportDefaultDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        const symbol: SymbolId = switch (self.tree.data(decl.declaration)) {
            .function => |func| if (func.id != .null) self.bindingSymbol(func.id) else .none,
            .class => |cls| if (cls.id != .null) self.bindingSymbol(cls.id) else .none,
            .identifier_reference => |id| self.moduleBinding(id.name),
            else => .none,
        };
        try self.addExport(.{
            .kind = .named,
            .name = self.default_name,
            .symbol = symbol,
            .node = index,
        });
    }

    fn exportAll(
        self: *Collector,
        decl: ast.ExportAllDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        const named = decl.exported != .null;
        try self.addExport(.{
            .kind = if (named) .namespace else .star,
            .name = if (named) self.stringOf(decl.exported) else .empty,
            .specifier = self.stringOf(decl.source),
            .type_only = decl.export_kind == .type,
            .node = index,
        });
    }

    fn exportAssignment(
        self: *Collector,
        decl: ast.TSExportAssignment,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        const symbol: SymbolId = switch (self.tree.data(decl.expression)) {
            .identifier_reference => |id| self.moduleBinding(id.name),
            else => .none,
        };
        try self.addExport(.{ .kind = .equals, .symbol = symbol, .node = index });
    }

    fn namespaceExport(
        self: *Collector,
        decl: ast.TSNamespaceExportDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        try self.addExport(.{
            .kind = .global,
            .name = self.stringOf(decl.id),
            .node = index,
        });
    }

    /// Collects the records and flags that can appear at any depth.
    fn sweep(self: *Collector) Allocator.Error!void {
        for (0..self.tree.nodes.len) |i| {
            const index: ast.NodeIndex = @enumFromInt(@as(u32, @intCast(i)));
            switch (self.tree.data(index)) {
                .import_expression => |expr| try self.dynamicImport(expr, index),
                .call_expression => |call| try self.requireCall(call, index),
                .meta_property => |meta| self.metaProperty(meta),
                else => {},
            }
        }

        for (self.sem.references) |ref| {
            if (ref.symbol != .none or ref.flags.space.inTypePosition()) continue;
            const name = self.tree.string(ref.name);
            if (std.mem.eql(u8, name, "require")) {
                self.flags.uses_require = true;
            } else if (std.mem.eql(u8, name, "module")) {
                self.flags.uses_module = true;
            } else if (std.mem.eql(u8, name, "exports")) {
                self.flags.uses_exports = true;
            }
        }
    }

    /// Only a literal specifier is a statically known edge.
    fn dynamicImport(
        self: *Collector,
        expr: ast.ImportExpression,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        const specifier = self.literal(expr.source) orelse return;
        try self.addImport(.{
            .kind = .dynamic,
            .specifier = specifier,
            .phase = expr.phase,
            .node = index,
        });
    }

    /// A local binding named `require` makes the call an ordinary call.
    fn requireCall(
        self: *Collector,
        call: ast.CallExpression,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        if (call.callee == .null) return;
        if (self.tree.data(call.callee) != .identifier_reference) return;
        const callee = self.tree.data(call.callee).identifier_reference;
        if (!std.mem.eql(u8, self.tree.string(callee.name), "require")) return;
        if (self.sem.symbolOf(call.callee) != null) return;

        const arguments = self.tree.extra(call.arguments);
        if (arguments.len != 1) return;
        const specifier = self.literal(arguments[0]) orelse return;

        try self.addImport(.{ .kind = .require, .specifier = specifier, .node = index });
    }

    fn metaProperty(self: *Collector, meta: ast.MetaProperty) void {
        if (meta.meta == .null or self.tree.data(meta.meta) != .identifier_name) return;
        const head = self.tree.data(meta.meta).identifier_name;
        if (std.mem.eql(u8, self.tree.string(head.name), "import")) {
            self.flags.uses_import_meta = true;
        }
    }

    /// The symbol bound by a binding identifier in the top level scope.
    fn bindingSymbol(self: *const Collector, binding: ast.NodeIndex) SymbolId {
        return self.moduleBinding(self.tree.data(binding).binding_identifier.name);
    }

    /// Looks up a name in the top level scope, hoisted vars included.
    fn moduleBinding(self: *const Collector, name: ast.String) SymbolId {
        return self.sem.binding(self.top_scope, self.tree.string(name)) orelse .none;
    }

    /// String literal text, or null for anything computed.
    fn literal(self: *const Collector, node: ast.NodeIndex) ?ast.String {
        if (node == .null) return null;
        return switch (self.tree.data(node)) {
            .string_literal => |lit| lit.value,
            else => null,
        };
    }

    /// Name text of an identifier or string literal shaped node.
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
