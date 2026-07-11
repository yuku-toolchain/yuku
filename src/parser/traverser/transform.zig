//! Transform traverser: walks a `Tree` and lets visitor hooks mutate
//! the AST in place through `ctx.tree` (`setData`, `addNode`, `addExtra`).
//!
//! The walker re-reads node data after every enter, so replacing a
//! node's data also walks the replacement's children. For the same
//! reason, never point a node's child back to its own index: move the
//! original data to a new node first, then point the wrapper at it.

const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");

const Allocator = std.mem.Allocator;

pub const Ctx = struct {
    tree: *ast.Tree,
    path: wk.NodePath = .{},

    pub fn enter(self: *Ctx, index: ast.NodeIndex, _: ast.NodeData) Allocator.Error!void {
        self.path.push(index);
    }

    pub fn exit(self: *Ctx, _: ast.NodeData) void {
        self.path.pop();
    }
};

/// Walks the tree with path tracking and mutation support.
pub fn traverse(comptime V: type, tree: *ast.Tree, visitor: *V) Allocator.Error!void {
    std.debug.assert(tree.root != .null);
    var ctx = Ctx{ .tree = tree };

    var layer = wk.Layer(Ctx, V){ .inner = visitor };

    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);
}
