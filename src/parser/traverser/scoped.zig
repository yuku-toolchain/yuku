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

    /// Called by the walker when entering a node.
    pub fn onEnter(self: *Ctx, index: ast.NodeIndex, tag: NodeTag) Allocator.Error!void {
        try self.scope.enter(index, tag);
    }

    /// Called by the walker when exiting a node.
    pub fn onExit(self: *Ctx, _: ast.NodeIndex, tag: NodeTag) void {
        self.scope.exit(tag);
    }

    /// Returns the id of the currently active scope.
    pub inline fn currentScopeId(self: *const Ctx) ScopeId {
        return self.scope.currentScopeId();
    }

    /// Returns the id of the nearest hoist-target scope (where `var` lands).
    pub inline fn currentHoistScopeId(self: *const Ctx) ScopeId {
        return self.scope.currentHoistScopeId();
    }

    /// Returns the currently active scope.
    pub inline fn currentScope(self: *const Ctx) Scope {
        return self.scope.currentScope();
    }

    /// Returns a mutable pointer to the currently active scope.
    pub inline fn currentScopePtr(self: *Ctx) *Scope {
        return self.scope.currentScopePtr();
    }

    /// Returns the scope for a given id.
    pub inline fn getScope(self: *const Ctx, id: ScopeId) Scope {
        return self.scope.getScope(id);
    }

    /// Returns a mutable pointer to the scope for a given id.
    pub inline fn getScopePtr(self: *Ctx, id: ScopeId) *Scope {
        return self.scope.getScopePtr(id);
    }

    /// Returns whether the current scope is in strict mode.
    pub inline fn isStrict(self: *const Ctx) bool {
        return self.scope.isStrict();
    }

    /// Returns an iterator over ancestor scopes starting from `start`, walking up to root.
    pub fn ancestors(self: *const Ctx, start: ScopeId) ScopeTree.AncestorIterator {
        return self.scope.ancestors(start);
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
/// information at every node. Returns the completed `ScopeTree`.
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V, allocator: Allocator) Allocator.Error!ScopeTree {
    var ctx = try Ctx.init(tree, allocator);
    errdefer ctx.deinit();
    try wk.walk(Ctx, V, visitor, &ctx);
    return try ctx.toScopeTree();
}
