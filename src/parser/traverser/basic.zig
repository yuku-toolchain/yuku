const std = @import("std");
const ast = @import("../ast.zig");
const walk = @import("walk.zig").walk;

const Allocator = std.mem.Allocator;

/// Minimal context for walking the AST without scope tracking.
pub const BasicCtx = struct {
    tree: *const ast.ParseTree,
};

/// Run a basic traversal over the parse tree (no scope tracking).
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V) Allocator.Error!void {
    var ctx = BasicCtx{ .tree = tree };
    try walk(BasicCtx, V, visitor, &ctx);
}
