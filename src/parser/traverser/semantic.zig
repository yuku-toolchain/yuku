const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");
const sc = @import("../semantic/scope.zig");
const bi = @import("../semantic/binder.zig");
const NodePath = wk.NodePath;

const Allocator = std.mem.Allocator;

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

/// Walk context combining the path stack, scope tracker, and symbol
/// tracker. Visitor hooks receive `*Ctx` and use it to inspect the
/// surrounding source structure (`scope`, `path`, `inTypePosition`) or
/// to query and contribute symbols (`symbols`).
pub const Ctx = struct {
    tree: *const ast.Tree,
    path: wk.NodePath = .{},
    scope: ScopeTracker,
    symbols: SymbolTracker,
    // depth of ts type-only context. inspect via `inTypePosition()`.
    type_position_depth: u32 = 0,
    // per-node tables carried into the final `Semantic`
    node_scopes: []ScopeId,
    node_parents: []ast.NodeIndex,

    pub fn init(tree: *ast.Tree) Allocator.Error!Ctx {
        const node_scopes = try tree.allocator().alloc(ScopeId, tree.nodes.len);
        @memset(node_scopes, .root);
        const node_parents = try tree.allocator().alloc(ast.NodeIndex, tree.nodes.len);
        @memset(node_parents, .null);
        return .{
            .tree = tree,
            .scope = try ScopeTracker.init(tree),
            .symbols = try SymbolTracker.init(tree),
            .node_scopes = node_scopes,
            .node_parents = node_parents,
        };
    }

    /// True when the walker is currently inside a TS type-only subtree.
    pub inline fn inTypePosition(self: *const Ctx) bool {
        return self.type_position_depth > 0;
    }

    /// True when the walker is currently inside a TS namespace body.
    pub inline fn inTsNamespace(self: *const Ctx) bool {
        var it = self.scope.ancestors(self.scope.current);
        while (it.next()) |id| {
            if (self.scope.get(id).kind == .ts_module) return true;
        }
        return false;
    }

    pub inline fn enter(self: *Ctx, index: ast.NodeIndex, data: ast.NodeData) Allocator.Error!void {
        self.path.push(index);
        try self.scope.enter(index, data);

        self.node_scopes[@intFromEnum(index)] = self.scope.current;
        self.node_parents[@intFromEnum(index)] = self.path.parent() orelse .null;

        if (data.isTypeContext()) self.type_position_depth += 1;

        try self.symbols.setBindingContext(data, &self.scope);
    }

    pub inline fn post_enter(
        self: *Ctx,
        index: ast.NodeIndex,
        data: ast.NodeData,
    ) Allocator.Error!void {
        const is_write = data == .identifier_reference and
            isWriteTarget(self.tree, &self.path);
        try self.symbols.declareBindings(index, data, &self.scope, .{
            .in_type_position = self.inTypePosition(),
            .is_write = is_write,
        });
    }

    pub inline fn exit(self: *Ctx, data: ast.NodeData) void {
        self.symbols.exit(data);
        self.scope.exit(data);
        if (data.isTypeContext()) {
            std.debug.assert(self.type_position_depth > 0);
            self.type_position_depth -= 1;
        }
        self.path.pop();
    }
};

/// https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype
pub fn isWriteTarget(tree: *const ast.Tree, path: *const NodePath) bool {
    std.debug.assert(path.depth() > 0);

    var child = path.ancestor(0) orelse return false;
    var n: usize = 1;
    while (path.ancestor(n)) |parent| : (n += 1) {
        std.debug.assert(parent != child);
        switch (tree.data(parent)) {
            .assignment_expression => |a| return a.left == child,
            .update_expression => |u| return u.argument == child,
            // assignment form only. the declaration form holds a
            // variable_declaration whose leaves are binding identifiers,
            // never references.
            .for_in_statement => |f| return f.left == child,
            .for_of_statement => |f| return f.left == child,
            .array_pattern => child = parent,
            .object_pattern => child = parent,
            // computed keys are reads
            .binding_property => |bp| {
                if (bp.value != child) return false;
                child = parent;
            },
            // the default value is a read
            .assignment_pattern => |ap| {
                if (ap.left != child) return false;
                child = parent;
            },
            .binding_rest_element => |r| {
                if (r.argument != child) return false;
                child = parent;
            },
            // transparent wrappers: `(a) += 1`, `a!++`, `(a as T) = b`
            .parenthesized_expression => child = parent,
            .ts_non_null_expression => child = parent,
            .ts_as_expression => |e| {
                if (e.expression != child) return false;
                child = parent;
            },
            .ts_satisfies_expression => |e| {
                if (e.expression != child) return false;
                child = parent;
            },
            .ts_type_assertion => |e| {
                if (e.expression != child) return false;
                child = parent;
            },
            // member objects, call arguments, initializers: reads
            else => return false,
        }
    }
    return false;
}

/// Walks the tree with full path, scope, and symbol tracking. Returns
/// the complete `Semantic` model, with scopes, symbols, and references
/// fully resolved and cross-indexed.
pub fn traverse(comptime V: type, tree: *ast.Tree, visitor: *V) Allocator.Error!Semantic {
    std.debug.assert(tree.root != .null);
    var ctx = try Ctx.init(tree);
    var layer = wk.Layer(Ctx, V){ .inner = visitor };
    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);
    return ctx.symbols.finalize(
        ctx.scope.toScopeTree(),
        ctx.node_scopes,
        ctx.node_parents,
    );
}
