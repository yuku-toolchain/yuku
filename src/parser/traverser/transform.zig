const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");
const mt = @import("mutable_tree.zig");

const Allocator = std.mem.Allocator;

pub const MutableTree = mt.MutableTree;

/// Traverser context for AST transformations.
///
/// Tracks the path and provides mutation through the `MutableTree`.
/// The walker re-reads node data after enter hooks, so replaced nodes
/// have their new children walked automatically.
pub const Ctx = struct {
    tree: *MutableTree,
    path: wk.NodePath = .{},

    pub fn enter(self: *Ctx, index: ast.NodeIndex, _: ast.NodeData) Allocator.Error!void {
        self.path.push(index);
    }

    pub fn exit(self: *Ctx, _: ast.NodeData) void {
        self.path.pop();
    }

    /// Replaces the node at `index` with `data`.
    ///
    /// The walker re-reads after enter, so replaced nodes have their
    /// new children walked automatically.
    pub inline fn replaceWith(self: *Ctx, index: ast.NodeIndex, data: ast.NodeData) void {
        self.tree.setData(index, data);
    }
};

/// Walks the tree with path tracking and mutation support.
pub fn traverse(comptime V: type, tree: *MutableTree, visitor: *V) Allocator.Error!void {
    var ctx = Ctx{ .tree = tree };

    var layer = wk.Layer(Ctx, V){ .inner = visitor };

    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);
}
