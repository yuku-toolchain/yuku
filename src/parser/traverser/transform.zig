const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");

const Allocator = std.mem.Allocator;

pub const Ctx = wk.PathCtx(*ast.Tree);

/// Walks the tree tracking the node path, letting hooks mutate it.
pub fn traverse(comptime V: type, tree: *ast.Tree, visitor: *V) Allocator.Error!void {
    std.debug.assert(tree.root != .null);
    var ctx = Ctx{ .tree = tree };
    try wk.walk(Ctx, V, visitor, &ctx, .{ .whole_tree = false });
}
