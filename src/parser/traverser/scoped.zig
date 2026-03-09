const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");
const st = @import("scope_tracker.zig");

const Allocator = std.mem.Allocator;
const NodeTag = std.meta.Tag(ast.NodeData);

pub const ScopeId = st.ScopeId;
pub const Scope = st.Scope;
pub const ScopeTree = st.ScopeTree;
pub const ScopeTracker = st.ScopeTracker;

pub const Ctx = struct {
    tree: *const ast.ParseTree,
    path: wk.NodePath = .{},
    scope: ScopeTracker,

    pub fn init(tree: *const ast.ParseTree, allocator: Allocator) Allocator.Error!Ctx {
        return .{ .tree = tree, .scope = try ScopeTracker.init(tree, allocator) };
    }

    pub fn onEnter(self: *Ctx, index: ast.NodeIndex, tag: NodeTag) Allocator.Error!void {
        try self.scope.enter(index, tag);
    }

    pub fn onExit(self: *Ctx, _: ast.NodeIndex, tag: NodeTag) void {
        self.scope.exit(tag);
    }

    /// Returns the completed scope tree and releases resources that are
    /// no longer needed. Call after traversal is done.
    pub fn toScopeTree(self: *Ctx) Allocator.Error!ScopeTree {
        return self.scope.toScopeTree();
    }

    /// Release all owned memory without producing a scope tree.
    pub fn deinit(self: *Ctx) void {
        self.scope.deinit();
    }
};

/// Run a scoped traversal over the parse tree.
///
/// Walks the AST while automatically tracking scope enter/exit. The visitor
/// receives a `*scoped.Ctx` as its context, giving access to scope
/// information at every node via `ctx.scope`. Returns the completed `ScopeTree`.
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V, allocator: Allocator) Allocator.Error!ScopeTree {
    var ctx = try Ctx.init(tree, allocator);
    errdefer ctx.deinit();
    try wk.walk(Ctx, V, visitor, &ctx);
    return try ctx.toScopeTree();
}
