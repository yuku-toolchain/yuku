const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");
const sc = @import("../semantic/scope.zig");

const Allocator = std.mem.Allocator;

pub const ScopeId = sc.ScopeId;
pub const Scope = sc.Scope;
pub const ScopeTree = sc.ScopeTree;
pub const ScopeTracker = sc.ScopeTracker;

/// Traverser context that tracks the node path and lexical scopes.
pub const Ctx = struct {
    tree: *const ast.Tree,
    path: wk.NodePath = .{},
    scope: ScopeTracker,

    pub fn init(tree: *ast.Tree) Allocator.Error!Ctx {
        return .{ .tree = tree, .scope = try ScopeTracker.init(tree) };
    }

    pub inline fn enter(
        self: *Ctx,
        comptime tag: ast.NodeTag,
        index: ast.NodeIndex,
        data: ast.NodeData,
    ) Allocator.Error!void {
        self.path.push(index);
        try self.scope.enter(tag, index, self.path.parent() orelse .null, data);
    }

    pub inline fn exit(
        self: *Ctx,
        comptime tag: ast.NodeTag,
        index: ast.NodeIndex,
        _: ast.NodeData,
    ) void {
        self.scope.exit(tag, index);
        self.path.pop();
    }
};

/// Walks the tree tracking the node path and scopes, and returns the `ScopeTree`.
pub fn traverse(comptime V: type, tree: *ast.Tree, visitor: *V) Allocator.Error!ScopeTree {
    std.debug.assert(tree.root != .null);
    var ctx = try Ctx.init(tree);
    try wk.walk(Ctx, V, visitor, &ctx, .{ .whole_tree = false });
    return ctx.scope.toScopeTree();
}
