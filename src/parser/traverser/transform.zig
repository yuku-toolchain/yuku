//! Transform traverser whose visitor hooks mutate the tree in place through
//! `ctx.tree`. The walker re-reads a node after enter, so never point a
//! node's child back to its own index.

const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");

const Allocator = std.mem.Allocator;

pub const Ctx = struct {
    tree: *ast.Tree,
    path: wk.NodePath = .{},

    pub fn enter(self: *Ctx, index: ast.NodeIndex, _: ast.NodeData) Allocator.Error!void {
        self.path.push(index);
    }

    pub fn exit(self: *Ctx, _: ast.NodeIndex, _: ast.NodeData) void {
        self.path.pop();
    }
};

/// Walks the tree with path tracking and mutation support.
pub fn traverse(comptime V: type, tree: *ast.Tree, visitor: *V) Allocator.Error!void {
    std.debug.assert(tree.root != .null);
    var ctx = Ctx{ .tree = tree };

    var layer = wk.Layer(Ctx, V){ .inner = visitor };

    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);
}
