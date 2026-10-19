const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");

const Allocator = std.mem.Allocator;

/// Traverser context that tracks the path from the root to the current node.
pub const Ctx = wk.PathCtx(*const ast.Tree);

/// Walks the tree tracking only the node path.
pub fn traverse(comptime V: type, tree: *const ast.Tree, visitor: *V) Allocator.Error!void {
    std.debug.assert(tree.root != .null);
    var ctx = Ctx{ .tree = tree };
    var layer = wk.Layer(Ctx, V){ .inner = visitor };
    try wk.walk(Ctx, @TypeOf(layer), &layer, &ctx);
}
