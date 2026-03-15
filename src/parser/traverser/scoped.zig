const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");
const sc = @import("tracker/scope.zig");

const Allocator = std.mem.Allocator;

pub const ScopeId = sc.ScopeId;
pub const Scope = sc.Scope;
pub const ScopeTree = sc.ScopeTree;
pub const ScopeTracker = sc.ScopeTracker;

/// Traverser context that tracks path and JavaScript lexical scopes.
pub const Ctx = struct {
    tree: *const ast.TreeBuilder,
    path: wk.NodePath = .{},
    scope: ScopeTracker,

    pub fn init(tree: *ast.TreeBuilder) Allocator.Error!Ctx {
        return .{ .tree = tree, .scope = try ScopeTracker.init(tree) };
    }

    pub fn enter(self: *Ctx, index: ast.NodeIndex, data: ast.NodeData) Allocator.Error!void {
        self.path.push(index);
        try self.scope.enter(index, data);
    }

    pub fn exit(self: *Ctx, data: ast.NodeData) void {
        self.scope.exit(data);
        self.path.pop();
    }
};

/// Walks the tree with path and scope tracking. Returns a `ScopeTree`.
pub fn traverse(comptime V: type, tree: *ast.TreeBuilder, visitor: *V) Allocator.Error!ScopeTree {
    var ctx = try Ctx.init(tree);

    var layer = wk.Layer(Ctx, V){ .inner = visitor };

    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);

    return ctx.scope.toScopeTree();
}
