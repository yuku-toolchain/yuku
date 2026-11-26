//! Module import and export records.

const std = @import("std");
const ast = @import("../ast.zig");
const static_semantics = @import("spec").static_semantics;
const binder = @import("binder.zig");
const sc = @import("scope.zig");

const Allocator = std.mem.Allocator;
const NodeIndex = ast.NodeIndex;
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
    node: NodeIndex,
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
    node: NodeIndex,

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
};

/// Collects the records of an analyzed tree, static ones first, each group in source order.
pub fn collect(tree: *ast.Tree, sem: *const Semantic) Allocator.Error!Records {
    var collector = Collector{
        .tree = tree,
        .sem = sem,
        .allocator = tree.allocator(),
        .default_name = try tree.addString("default"),
        .top_scope = sem.scopeOf(tree.root),
        .exports_implicitly = binder.exportsImplicitly(tree),
    };

    const body = tree.extra(tree.data(tree.root).program.body);
    for (body) |statement| try collector.statement(statement);
    try collector.sweep();
    // in a script, `declare module "m"` declares `m` instead
    if (!collector.flags.uses_import_meta and !binder.hasModuleSyntax(tree, body)) {
        var kept: usize = 0;
        for (collector.imports.items) |record| {
            if (record.kind == .augmentation) continue;
            collector.imports.items[kept] = record;
            kept += 1;
        }
        collector.imports.shrinkRetainingCapacity(kept);
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

    fn statement(self: *Collector, index: NodeIndex) Allocator.Error!void {
        switch (self.tree.data(index)) {
            .import_declaration => |decl| try self.importDeclaration(decl, index),
            .ts_import_equals_declaration => |decl| try self.importEquals(decl, index),
            .export_named_declaration => |decl| try self.exportNamed(decl, index),
            .export_default_declaration => |decl| {
                const declaration = self.tree.data(decl.declaration);
                const symbol: SymbolId = switch (declaration) {
                    inline .function, .class, .ts_interface_declaration => |d| self.symbolAt(d.id),
                    .identifier_reference => |id| self.moduleBinding(id.name),
                    else => .none,
                };
                try self.exports.append(self.allocator, .{
                    .kind = .named,
                    .name = self.default_name,
                    .symbol = symbol,
                    .type_only = declaration == .ts_interface_declaration,
                    .node = index,
                });
            },
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

    fn importDeclaration(
        self: *Collector,
        decl: ast.ImportDeclaration,
        index: NodeIndex,
    ) Allocator.Error!void {
        const specifier = self.stringOf(decl.source);
        const type_only = decl.import_kind == .type;
        const specifiers = self.tree.extra(decl.specifiers);
        if (specifiers.len == 0) return self.imports.append(self.allocator, .{
            .kind = .side_effect,
            .specifier = specifier,
            .type_only = type_only,
            .phase = decl.phase,
            .node = index,
        });

        for (specifiers) |spec_index| switch (self.tree.data(spec_index)) {
            inline .import_specifier,
            .import_default_specifier,
            .import_namespace_specifier,
            => |spec, tag| try self.imports.append(self.allocator, .{
                .kind = if (tag == .import_namespace_specifier) .namespace else .named,
                .name = switch (tag) {
                    .import_specifier => self.stringOf(spec.imported),
                    .import_default_specifier => self.default_name,
                    else => .empty,
                },
                .symbol = self.symbolAt(spec.local),
                .specifier = specifier,
                .type_only = type_only or (tag == .import_specifier and spec.import_kind == .type),
                .phase = decl.phase,
                .node = spec_index,
            }),
            else => {},
        };
    }

    // the qualified name form aliases a namespace, not a module
    fn importEquals(
        self: *Collector,
        decl: ast.TSImportEqualsDeclaration,
        index: NodeIndex,
    ) Allocator.Error!void {
        const reference = switch (self.tree.data(decl.module_reference)) {
            .ts_external_module_reference => |reference| reference,
            else => return,
        };
        try self.imports.append(self.allocator, .{
            .kind = .import_equals,
            .symbol = self.symbolAt(decl.id),
            .specifier = self.stringOf(reference.expression),
            .type_only = decl.import_kind == .type,
            .node = index,
        });
    }

    fn exportNamed(
        self: *Collector,
        decl: ast.ExportNamedDeclaration,
        index: NodeIndex,
    ) Allocator.Error!void {
        const type_only = decl.export_kind == .type;
        if (decl.declaration != .null) switch (self.tree.data(decl.declaration)) {
            .ts_import_equals_declaration => |alias| {
                try self.importEquals(alias, decl.declaration);
                return self.exportLocal(alias.id, type_only or alias.import_kind == .type);
            },
            else => return self.declarationNames(decl.declaration, type_only),
        };

        const has_source = decl.source != .null;
        const specifier = if (has_source) self.stringOf(decl.source) else ast.String.empty;
        if (has_source and decl.specifiers.len == 0) return self.imports.append(self.allocator, .{
            .kind = .side_effect,
            .specifier = specifier,
            .type_only = type_only,
            .node = index,
        });

        for (self.tree.extra(decl.specifiers)) |spec_index| {
            const spec = switch (self.tree.data(spec_index)) {
                .export_specifier => |spec| spec,
                else => continue,
            };
            const local = self.stringOf(spec.local);
            try self.exports.append(self.allocator, .{
                .kind = if (has_source) .re_export else .named,
                .name = self.stringOf(spec.exported),
                .symbol = if (has_source) .none else self.moduleBinding(local),
                .from_name = if (has_source) local else .empty,
                .specifier = specifier,
                .type_only = type_only or spec.export_kind == .type,
                .node = spec_index,
            });
        }
    }

    fn declarationNames(
        self: *Collector,
        declaration: NodeIndex,
        type_only: bool,
    ) Allocator.Error!void {
        switch (self.tree.data(declaration)) {
            .variable_declaration => |decl| for (self.tree.extra(decl.declarators)) |declarator| {
                const id = self.tree.data(declarator).variable_declarator.id;
                try self.patternNames(id, type_only);
            },
            inline .function, .class, .ts_enum_declaration => |decl| {
                if (decl.id != .null) try self.exportLocal(decl.id, type_only);
            },
            inline .ts_type_alias_declaration, .ts_interface_declaration => |decl| {
                try self.exportLocal(decl.id, true);
            },
            .ts_module_declaration => |decl| {
                var head = decl.id;
                if (head == .null) return;
                while (self.tree.data(head) == .ts_qualified_name) {
                    head = self.tree.data(head).ts_qualified_name.left;
                }
                // ambient module ids are string literals, not bindings
                if (self.tree.data(head) == .binding_identifier) {
                    try self.exportLocal(head, type_only);
                }
            },
            else => {},
        }
    }

    fn patternNames(self: *Collector, root: NodeIndex, type_only: bool) Allocator.Error!void {
        var stack: std.ArrayList(NodeIndex) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, root);

        while (stack.pop()) |pattern| {
            if (pattern == .null) continue;
            switch (self.tree.data(pattern)) {
                .binding_identifier => try self.exportLocal(pattern, type_only),
                .assignment_pattern => |p| try stack.append(self.allocator, p.left),
                .binding_rest_element => |p| try stack.append(self.allocator, p.argument),
                .binding_property => |p| try stack.append(self.allocator, p.value),
                inline .array_pattern, .object_pattern => |p, tag| {
                    try stack.append(self.allocator, p.rest);
                    const nested = if (tag == .array_pattern) p.elements else p.properties;
                    var children = std.mem.reverseIterator(self.tree.extra(nested));
                    while (children.next()) |child| try stack.append(self.allocator, child);
                },
                else => {},
            }
        }
    }

    fn exportLocal(self: *Collector, binding: NodeIndex, type_only: bool) Allocator.Error!void {
        const name = self.tree.data(binding).binding_identifier.name;
        try self.exports.append(self.allocator, .{
            .kind = .named,
            .name = name,
            .symbol = self.moduleBinding(name),
            .type_only = type_only,
            .node = binding,
        });
    }

    fn sweep(self: *Collector) Allocator.Error!void {
        const tree = self.tree;
        for (tree.nodes.items(.data), 0..) |data, i| {
            const index: NodeIndex = @fromBackingInt(@intCast(i));
            switch (data) {
                .import_expression => |expr| if (self.literal(expr.source)) |specifier| {
                    try self.imports.append(self.allocator, .{
                        .kind = .dynamic,
                        .specifier = specifier,
                        .phase = expr.phase,
                        .node = index,
                    });
                },
                .call_expression => |call| if (self.requireSpecifier(call)) |specifier| {
                    try self.imports.append(self.allocator, .{
                        .kind = .require,
                        .specifier = specifier,
                        .node = index,
                    });
                },
                .meta_property => |meta| {
                    if (meta.meta == .null or tree.data(meta.meta) != .identifier_name) continue;
                    const head = tree.string(tree.data(meta.meta).identifier_name.name);
                    if (std.mem.eql(u8, head, "import")) self.flags.uses_import_meta = true;
                },
                else => {},
            }
        }

        for (self.sem.references) |ref| {
            if (ref.symbol != .none or ref.flags.type_position) continue;
            const name = tree.string(ref.name);
            inline for (.{ "require", "module", "exports" }) |free| {
                if (std.mem.eql(u8, name, free)) @field(self.flags, "uses_" ++ free) = true;
            }
        }
    }

    // the specifier of a `require("m")` call, which a local binding named `require` makes an
    // ordinary call
    fn requireSpecifier(self: *const Collector, call: ast.CallExpression) ?ast.String {
        if (call.callee == .null) return null;
        const callee = switch (self.tree.data(call.callee)) {
            .identifier_reference => |callee| callee,
            else => return null,
        };
        if (!std.mem.eql(u8, self.tree.string(callee.name), "require")) return null;
        if (self.sem.symbolOf(call.callee) != null) return null;
        const arguments = self.tree.extra(call.arguments);
        return if (arguments.len == 1) self.literal(arguments[0]) else null;
    }

    fn symbolAt(self: *const Collector, binding: NodeIndex) SymbolId {
        if (binding == .null) return .none;
        return self.moduleBinding(self.tree.data(binding).binding_identifier.name);
    }

    fn moduleBinding(self: *const Collector, name: ast.String) SymbolId {
        return self.sem.binding(self.top_scope, self.tree.string(name)) orelse .none;
    }

    fn literal(self: *const Collector, node: NodeIndex) ?ast.String {
        if (node == .null) return null;
        const unwrapped = static_semantics.unwrapParens(self.tree, node);
        return static_semantics.stringLiteralLike(self.tree, unwrapped);
    }

    fn stringOf(self: *const Collector, node: NodeIndex) ast.String {
        return switch (self.tree.data(node)) {
            inline .identifier_name, .identifier_reference, .binding_identifier => |id| id.name,
            .string_literal => |lit| lit.value,
            else => .empty,
        };
    }
};
