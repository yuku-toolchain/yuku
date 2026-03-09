const ast = @import("../ast.zig");
const wk = @import("walk.zig");

const walk = wk.walk;
const Allocator = std.mem.Allocator;
const std = @import("std");

pub const Ctx = struct {
    tree: *const ast.ParseTree,
    path: wk.NodePath = .{},
};

/// Run a basic traversal over the parse tree.
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V) Allocator.Error!void {
    var ctx = Ctx{ .tree = tree };
    try walk(Ctx, V, visitor, &ctx);
}
