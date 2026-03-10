const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");

const Allocator = std.mem.Allocator;

// simplest traverser context. only tracks the path from root to
// the current node. no scope or symbol tracking.
pub const Ctx = struct {
    tree: *const ast.ParseTree,
    path: wk.NodePath = .{},

    pub fn enter(self: *Ctx, index: ast.NodeIndex, _: ast.NodeData) Allocator.Error!void {
        self.path.push(index);
    }

    pub fn exit(self: *Ctx, _: ast.NodeData) void {
        self.path.pop();
    }
};

pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V) Allocator.Error!void {
    var ctx = Ctx{ .tree = tree };
    var layer = wk.Layer(Ctx, V){ .inner = visitor };
    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);
}
