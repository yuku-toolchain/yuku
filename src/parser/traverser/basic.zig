const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");

const walk = wk.walk;
const Allocator = std.mem.Allocator;

/// Minimal context for walking the AST without scope tracking.
pub const BasicCtx = struct {
    tree: *const ast.ParseTree,
    allocator: Allocator,
    path: wk.NodePath = .{},
};

/// Run a basic traversal over the parse tree (no scope tracking).
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V, allocator: Allocator) Allocator.Error!void {
    var ctx = BasicCtx{ .tree = tree, .allocator = allocator };
    try ctx.path.stack.ensureTotalCapacity(allocator, 64);
    try walk(BasicCtx, V, visitor, &ctx);
}
