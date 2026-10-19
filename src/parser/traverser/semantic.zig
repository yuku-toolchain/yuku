const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");
const sc = @import("../semantic/scope.zig");
const bi = @import("../semantic/binder.zig");

const Allocator = std.mem.Allocator;
const NodePath = wk.NodePath;

pub const Scope = sc.Scope;
pub const ScopeId = sc.ScopeId;
pub const ScopeTree = sc.ScopeTree;
pub const ScopeTracker = sc.ScopeTracker;
pub const SymbolId = bi.SymbolId;
pub const ReferenceId = bi.ReferenceId;
pub const Symbol = bi.Symbol;
pub const Reference = bi.Reference;
pub const Semantic = bi.Semantic;
pub const SymbolTracker = bi.SymbolTracker;
pub const Mode = bi.Mode;

/// Traverser context that tracks the node path, scopes, and symbols.
pub const Ctx = struct {
    tree: *const ast.Tree,
    mode: Mode,
    path: NodePath = .{},
    scope: ScopeTracker,
    symbols: SymbolTracker,
    type_position_depth: u32 = 0,
    // empty in a `check` walk
    node_scopes: []ScopeId,
    node_parents: []ast.NodeIndex,

    pub fn init(tree: *ast.Tree, mode: Mode) Allocator.Error!Ctx {
        const node_count: usize = switch (mode) {
            .check => 0,
            .analyze => tree.nodes.len,
        };
        const node_scopes = try tree.allocator().alloc(ScopeId, node_count);
        @memset(node_scopes, .root);
        const node_parents = try tree.allocator().alloc(ast.NodeIndex, node_count);
        @memset(node_parents, .null);
        return .{
            .tree = tree,
            .mode = mode,
            .scope = try ScopeTracker.init(tree),
            .symbols = try SymbolTracker.init(tree, mode),
            .node_scopes = node_scopes,
            .node_parents = node_parents,
        };
    }

    /// The `Semantic` model of a finished `analyze` walk.
    pub fn finalize(self: *Ctx) Allocator.Error!Semantic {
        std.debug.assert(self.mode == .analyze);
        std.debug.assert(self.path.depth() == 0);
        return self.symbols.finalize(self.scope.toScopeTree(), self.node_scopes, self.node_parents);
    }

    /// Whether the walk is inside a TypeScript type-only subtree.
    pub inline fn inTypePosition(self: *const Ctx) bool {
        return self.type_position_depth > 0;
    }

    /// Whether the walk is inside a TypeScript namespace body.
    pub inline fn inTsNamespace(self: *const Ctx) bool {
        var it = self.scope.ancestors(self.scope.current);
        while (it.next()) |id| {
            if (self.scope.get(id).kind == .ts_module) return true;
        }
        return false;
    }

    pub inline fn enter(
        self: *Ctx,
        comptime tag: ast.NodeTag,
        index: ast.NodeIndex,
        data: ast.NodeData,
    ) Allocator.Error!void {
        self.path.push(index);
        const parent = self.path.parent() orelse .null;
        try self.scope.enter(tag, index, parent, data);

        switch (self.mode) {
            .check => {},
            .analyze => {
                self.node_scopes[@backingInt(index)] = self.scope.current;
                self.node_parents[@backingInt(index)] = parent;
            },
        }

        if (data.isTypeContext()) self.type_position_depth += 1;

        try self.symbols.setBindingContext(tag, data, parent, &self.scope);
    }

    pub inline fn post_enter(
        self: *Ctx,
        comptime tag: ast.NodeTag,
        index: ast.NodeIndex,
        data: ast.NodeData,
    ) Allocator.Error!void {
        try self.symbols.declareBindings(tag, index, data, &self.scope, self.inTypePosition());
        switch (self.mode) {
            .check => {},
            .analyze => {
                var flags: Reference.Flags = .{};
                if (tag == .identifier_reference) {
                    flags = refPosition(self.tree, &self.path, self.inTypePosition());
                    flags.write = isWriteTarget(self.tree, &self.path);
                }
                try self.symbols.addReferences(tag, index, data, &self.scope, flags);
            },
        }
    }

    pub inline fn exit(
        self: *Ctx,
        comptime tag: ast.NodeTag,
        index: ast.NodeIndex,
        data: ast.NodeData,
    ) void {
        self.symbols.exit(tag, data);
        self.scope.exit(tag, index);
        if (data.isTypeContext()) {
            std.debug.assert(self.type_position_depth > 0);
            self.type_position_depth -= 1;
        }
        self.path.pop();
    }
};

/// Walks the tree tracking the node path, scopes, and symbols, and returns the `Semantic` model.
pub fn traverse(comptime V: type, tree: *ast.Tree, visitor: *V) Allocator.Error!Semantic {
    std.debug.assert(tree.root != .null);
    var ctx = try Ctx.init(tree, .analyze);
    try wk.walk(Ctx, V, visitor, &ctx, .{ .whole_tree = true });
    return ctx.finalize();
}

fn refPosition(
    tree: *const ast.Tree,
    path: *const NodePath,
    in_type_position: bool,
) Reference.Flags {
    std.debug.assert(path.depth() > 0);

    var qualified_left = false;
    var child = path.ancestor(0) orelse return .{};
    var n: usize = 1;
    while (path.ancestor(n)) |parent| : (n += 1) {
        switch (tree.data(parent)) {
            .ts_qualified_name => {
                qualified_left = true;
                child = parent;
                continue;
            },
            // `implements ns.I`
            .member_expression => |m| if (in_type_position and m.object == child) {
                qualified_left = true;
                child = parent;
                continue;
            },
            .call_expression => |c| if (in_type_position and c.callee == child) {
                child = parent;
                continue;
            },
            // a computed key of a type member is a value use, `[key]: string`
            inline .ts_property_signature, .ts_method_signature, .binding_property => |member| {
                if (member.computed and member.key == child) {
                    if (in_type_position) return .{ .space = .typeof, .type_position = true };
                    return .{};
                }
            },
            .ts_type_query => return .{ .space = .typeof, .type_position = true },
            .export_specifier => |s| {
                if (s.local != child) return .{};
                const in_type_export = if (path.ancestor(n + 1)) |declaration|
                    tree.data(declaration).export_named_declaration.export_kind == .type
                else
                    false;
                return .{
                    .space = .any,
                    .type_position = s.export_kind == .type or in_type_export,
                };
            },
            .export_default_declaration, .ts_export_assignment => return .{ .space = .any },
            // tsc resolves an alias target as a namespace
            .ts_import_equals_declaration => |decl| {
                if (decl.module_reference != child) return .{ .space = .any };
                return .{ .space = .namespace, .type_position = decl.import_kind == .type };
            },
            else => {},
        }
        if (!in_type_position) return .{};
        return .{ .space = if (qualified_left) .namespace else .type, .type_position = true };
    }
    return .{};
}

// https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype
fn isWriteTarget(tree: *const ast.Tree, path: *const NodePath) bool {
    std.debug.assert(path.depth() > 0);

    var child = path.ancestor(0) orelse return false;
    var n: usize = 1;
    while (path.ancestor(n)) |parent| : (n += 1) {
        std.debug.assert(parent != child);
        switch (tree.data(parent)) {
            .assignment_expression => |a| return a.left == child,
            .update_expression => |u| return u.argument == child,
            // the declaration form's leaves are binding identifiers, never references
            inline .for_in_statement, .for_of_statement => |f| return f.left == child,
            .array_pattern, .object_pattern => child = parent,
            .binding_property => |bp| {
                if (bp.value != child) return false;
                child = parent;
            },
            .assignment_pattern => |ap| {
                if (ap.left != child) return false;
                child = parent;
            },
            .binding_rest_element => |r| {
                if (r.argument != child) return false;
                child = parent;
            },
            .parenthesized_expression, .ts_non_null_expression => child = parent,
            inline .ts_as_expression, .ts_satisfies_expression, .ts_type_assertion => |e| {
                if (e.expression != child) return false;
                child = parent;
            },
            else => return false,
        }
    }
    return false;
}
