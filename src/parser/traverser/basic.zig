const ast = @import("../ast.zig");
const walk = @import("walk.zig").walk;

pub const BasicCtx = struct {
    tree: *const ast.ParseTree,
};

pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V) void {
    var ctx = BasicCtx{ .tree = tree };

    walk(BasicCtx, V, visitor, &ctx);
}
