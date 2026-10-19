//! Module import and export records.

const std = @import("std");
const ast = @import("../ast.zig");
const ecmascript = @import("../ecmascript.zig");
const binder = @import("binder.zig");
const sc = @import("scope.zig");

const Allocator = std.mem.Allocator;
const SymbolId = binder.SymbolId;
const Semantic = binder.Semantic;

/// One static or dynamic dependency of the module.
pub const Import = struct {
    kind: Kind,
    /// The imported name of a `.named` record, "default" for a default import.
    name: ast.String = .empty,
    /// The local binding, or `.none`.
    symbol: SymbolId = .none,
    /// The decoded module specifier.
    specifier: ast.String,
    /// True for type only imports.
    type_only: bool = false,
    /// The phase modifier, or `null`.
    phase: ?ast.ImportPhase = null,
    /// The specifier, call, or declaration the record comes from.
    node: ast.NodeIndex,
    /// The body of an `.augmentation`.
    scope: sc.ScopeId = .none,

    pub const Kind = enum(u3) {
        /// `import d from "m"` and `import { a as b } from "m"`
        named,
        /// `import * as ns from "m"`
        namespace,
        /// `import "m"` and `export {} from "m"`
        side_effect,
        /// `import x = require("m")`
        import_equals,
        /// `import("m")` at any depth
        dynamic,
        /// `require("m")` at any depth, only when `require` is a free name
        require,
        /// `declare module "m" {}` in a module, merging its declarations into `m`
        augmentation,
    };
};

/// One exported name of the module.
pub const Export = struct {
    kind: Kind,
    /// The exported name, or the global name of a `.global`.
    name: ast.String = .empty,
    /// The local binding, or `.none`.
    symbol: SymbolId = .none,
    /// The name a `.re_export` takes from the source module.
    from_name: ast.String = .empty,
    /// The decoded specifier of the `from` clause.
    specifier: ast.String = .empty,
    /// True for type only exports.
    type_only: bool = false,
    /// The specifier, binding identifier, or declaration the record comes from.
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

/// Module format signals from free value references and `import.meta`.
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

/// The records of one module, in the tree arena.
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

/// Collects the records of an analyzed tree, static ones first, each group in source order.
pub fn collect(tree: *ast.Tree, sem: *const Semantic) Allocator.Error!Records {
    std.debug.assert(tree.root != .null);

    var collector = Collector{
        .tree = tree,
        .sem = sem,
        .allocator = tree.allocator(),
        .default_name = try tree.addString("default"),
        .top_scope = if (tree.isModule()) .module else .root,
        .exports_implicitly = binder.exportsImplicitly(tree),
    };

    const body = tree.extra(tree.data(tree.root).program.body);
    for (body) |statement| try collector.statement(statement);
    try collector.sweep();
    // in a script, `declare module "m"` declares `m` instead
    if (!collector.flags.uses_import_meta and !binder.hasModuleSyntax(tree, body)) {
        collector.dropAugmentations();
    }

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
    exports_implicitly: bool,
    imports: std.ArrayList(Import) = .empty,
    exports: std.ArrayList(Export) = .empty,
    flags: Flags = .{},

    fn statement(self: *Collector, index: ast.NodeIndex) Allocator.Error!void {
        switch (self.tree.data(index)) {
            .import_declaration => |decl| try self.importDeclaration(decl, index),
            .ts_import_equals_declaration => |decl| try self.importEquals(decl, index),
            .export_named_declaration => |decl| try self.exportNamed(decl, index),
            .export_default_declaration => |decl| try self.exportDefault(decl, index),
            .export_all_declaration => |decl| try self.exports.append(self.allocator, .{
                .kind = if (decl.exported != .null) .namespace else .star,
                .name = if (decl.exported != .null) self.stringOf(decl.exported) else .empty,
                .specifier = self.stringOf(decl.source),
                .type_only = decl.export_kind == .type,
                .node = index,
            }),
            .ts_export_assignment => |decl| try self.exports.append(self.allocator, .{
                .kind = .equals,
                .symbol = switch (self.tree.data(decl.expression)) {
                    .identifier_reference => |id| self.moduleBinding(id.name),
                    else => .none,
                },
                .node = index,
            }),
            .ts_namespace_export_declaration => |decl| try self.exports.append(self.allocator, .{
                .kind = .global,
                .name = self.stringOf(decl.id),
                .node = index,
            }),
            .ts_module_declaration => |decl| {
                if (self.literal(decl.id)) |specifier| {
                    if (decl.body != .null) try self.imports.append(self.allocator, .{
                        .kind = .augmentation,
                        .specifier = specifier,
                        .type_only = true,
                        .node = index,
                        .scope = self.sem.scopeOf(decl.body),
                    });
                }
                if (self.exports_implicitly) try self.declarationNames(index, true);
            },
            // type only, as `export declare` is
            else => if (self.exports_implicitly) try self.declarationNames(index, true),
        }
    }

    fn dropAugmentations(self: *Collector) void {
        var kept: usize = 0;
        for (self.imports.items) |record| {
            if (record.kind == .augmentation) continue;
            self.imports.items[kept] = record;
            kept += 1;
        }
        self.imports.shrinkRetainingCapacity(kept);
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
            return self.imports.append(self.allocator, .{
                .kind = .side_effect,
                .specifier = specifier,
                .type_only = type_only,
                .phase = decl.phase,
                .node = index,
            });
        }

        for (specifiers) |spec_index| {
            switch (self.tree.data(spec_index)) {
                .import_specifier => |spec| try self.imports.append(self.allocator, .{
                    .kind = .named,
                    .name = self.stringOf(spec.imported),
                    .symbol = self.bindingSymbol(spec.local),
                    .specifier = specifier,
                    .type_only = type_only or spec.import_kind == .type,
                    .phase = decl.phase,
                    .node = spec_index,
                }),
                .import_default_specifier => |spec| try self.imports.append(self.allocator, .{
                    .kind = .named,
                    .name = self.default_name,
                    .symbol = self.bindingSymbol(spec.local),
                    .specifier = specifier,
                    .type_only = type_only,
                    .phase = decl.phase,
                    .node = spec_index,
                }),
                .import_namespace_specifier => |spec| try self.imports.append(self.allocator, .{
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

    // the qualified name form aliases a namespace, not a module
    fn importEquals(
        self: *Collector,
        decl: ast.TSImportEqualsDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        if (self.tree.data(decl.module_reference) != .ts_external_module_reference) return;
        const reference = self.tree.data(decl.module_reference).ts_external_module_reference;
        try self.imports.append(self.allocator, .{
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
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        if (decl.declaration != .null) {
            const type_only = decl.export_kind == .type;
            if (self.tree.data(decl.declaration) == .ts_import_equals_declaration) {
                const alias = self.tree.data(decl.declaration).ts_import_equals_declaration;
                try self.importEquals(alias, decl.declaration);
                return self.exportLocal(alias.id, type_only or alias.import_kind == .type);
            }
            return self.declarationNames(decl.declaration, type_only);
        }

        const has_source = decl.source != .null;
        const specifier = if (has_source) self.stringOf(decl.source) else ast.String.empty;

        if (has_source and decl.specifiers.len == 0) {
            return self.imports.append(self.allocator, .{
                .kind = .side_effect,
                .specifier = specifier,
                .type_only = decl.export_kind == .type,
                .node = index,
            });
        }

        for (self.tree.extra(decl.specifiers)) |spec_index| {
            if (self.tree.data(spec_index) != .export_specifier) continue;
            const spec = self.tree.data(spec_index).export_specifier;

            try self.exports.append(self.allocator, .{
                .kind = if (has_source) .re_export else .named,
                .name = self.stringOf(spec.exported),
                .symbol = if (has_source) .none else self.moduleBinding(self.stringOf(spec.local)),
                .from_name = if (has_source) self.stringOf(spec.local) else .empty,
                .specifier = specifier,
                .type_only = decl.export_kind == .type or spec.export_kind == .type,
                .node = spec_index,
            });
        }
    }

    fn declarationNames(
        self: *Collector,
        declaration: ast.NodeIndex,
        type_only: bool,
    ) Allocator.Error!void {
        switch (self.tree.data(declaration)) {
            .variable_declaration => |decl| {
                for (self.tree.extra(decl.declarators)) |declarator| {
                    const id = self.tree.data(declarator).variable_declarator.id;
                    try self.patternNames(id, type_only);
                }
            },
            inline .function, .class => |decl| {
                if (decl.id != .null) try self.exportLocal(decl.id, type_only);
            },
            inline .ts_type_alias_declaration, .ts_interface_declaration => |decl| {
                try self.exportLocal(decl.id, true);
            },
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

    fn patternNames(
        self: *Collector,
        pattern: ast.NodeIndex,
        type_only: bool,
    ) Allocator.Error!void {
        if (pattern == .null) return;

        switch (self.tree.data(pattern)) {
            .binding_identifier => try self.exportLocal(pattern, type_only),
            .assignment_pattern => |p| try self.patternNames(p.left, type_only),
            .binding_rest_element => |p| try self.patternNames(p.argument, type_only),
            .array_pattern => |p| {
                for (self.tree.extra(p.elements)) |element| {
                    try self.patternNames(element, type_only);
                }
                try self.patternNames(p.rest, type_only);
            },
            .object_pattern => |p| {
                for (self.tree.extra(p.properties)) |property| {
                    if (self.tree.data(property) == .binding_property) {
                        const value = self.tree.data(property).binding_property.value;
                        try self.patternNames(value, type_only);
                    }
                }
                try self.patternNames(p.rest, type_only);
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
        try self.exports.append(self.allocator, .{
            .kind = .named,
            .name = name,
            .symbol = self.moduleBinding(name),
            .type_only = type_only,
            .node = binding,
        });
    }

    fn exportDefault(
        self: *Collector,
        decl: ast.ExportDefaultDeclaration,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        const symbol: SymbolId = switch (self.tree.data(decl.declaration)) {
            inline .function, .class => |d| if (d.id != .null) self.bindingSymbol(d.id) else .none,
            .identifier_reference => |id| self.moduleBinding(id.name),
            else => .none,
        };
        try self.exports.append(self.allocator, .{
            .kind = .named,
            .name = self.default_name,
            .symbol = symbol,
            .node = index,
        });
    }

    fn sweep(self: *Collector) Allocator.Error!void {
        for (0..self.tree.nodes.len) |i| {
            const index: ast.NodeIndex = @fromBackingInt(@intCast(i));
            switch (self.tree.data(index)) {
                .import_expression => |expr| try self.dynamicImport(expr, index),
                .call_expression => |call| try self.requireCall(call, index),
                .meta_property => |meta| self.metaProperty(meta),
                else => {},
            }
        }

        for (self.sem.references) |ref| {
            if (ref.symbol != .none or ref.flags.type_position) continue;
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

    fn dynamicImport(
        self: *Collector,
        expr: ast.ImportExpression,
        index: ast.NodeIndex,
    ) Allocator.Error!void {
        const specifier = self.literal(expr.source) orelse return;
        try self.imports.append(self.allocator, .{
            .kind = .dynamic,
            .specifier = specifier,
            .phase = expr.phase,
            .node = index,
        });
    }

    // a local binding named `require` makes the call an ordinary call
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

        try self.imports.append(self.allocator, .{
            .kind = .require,
            .specifier = specifier,
            .node = index,
        });
    }

    fn metaProperty(self: *Collector, meta: ast.MetaProperty) void {
        if (meta.meta == .null or self.tree.data(meta.meta) != .identifier_name) return;
        const head = self.tree.data(meta.meta).identifier_name;
        if (std.mem.eql(u8, self.tree.string(head.name), "import")) {
            self.flags.uses_import_meta = true;
        }
    }

    fn bindingSymbol(self: *const Collector, binding: ast.NodeIndex) SymbolId {
        return self.moduleBinding(self.tree.data(binding).binding_identifier.name);
    }

    fn moduleBinding(self: *const Collector, name: ast.String) SymbolId {
        return self.sem.binding(self.top_scope, self.tree.string(name)) orelse .none;
    }

    fn literal(self: *const Collector, node: ast.NodeIndex) ?ast.String {
        if (node == .null) return null;
        switch (self.tree.data(ecmascript.unwrapParens(self.tree, node))) {
            .string_literal => |lit| return lit.value,
            .template_literal => |template| {
                if (template.expressions.len > 0) return null;
                std.debug.assert(template.quasis.len == 1);
                const quasi = self.tree.data(self.tree.extra(template.quasis)[0]).template_element;
                if (quasi.is_cooked_undefined) return null;
                return quasi.cooked;
            },
            else => return null,
        }
    }

    fn stringOf(self: *const Collector, node: ast.NodeIndex) ast.String {
        std.debug.assert(node != .null);
        return switch (self.tree.data(node)) {
            inline .identifier_name, .identifier_reference, .binding_identifier => |id| id.name,
            .string_literal => |lit| lit.value,
            else => .empty,
        };
    }
};
