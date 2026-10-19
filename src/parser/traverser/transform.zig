//! Traversal whose hooks may mutate the tree through `ctx.tree`. The walk re-reads a node
//! after its enter hooks, so a node's child must never point back to the node.

const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");

const Allocator = std.mem.Allocator;

pub const Ctx = wk.PathCtx(*ast.Tree);

/// Walks the tree tracking the node path, letting hooks mutate it.
pub fn traverse(comptime V: type, tree: *ast.Tree, visitor: *V) Allocator.Error!void {
    std.debug.assert(tree.root != .null);
    var ctx = Ctx{ .tree = tree };
    var layer = wk.Layer(Ctx, V){ .inner = visitor };
    try wk.walk(Ctx, @TypeOf(layer), &layer, &ctx);
}
