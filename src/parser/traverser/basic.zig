const ast = @import("../ast.zig");
const walk = @import("walk.zig").walk;

pub const Ctx = struct {
    tree: *const ast.ParseTree,

    pub inline fn nodeText(self: *const Ctx, start: u32, len: u16) []const u8 {
        return self.tree.source[start..][0..len];
    }
};

pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V) void {
    var ctx = Ctx{ .tree = tree };

    walk(Ctx, V, visitor, &ctx);
}
