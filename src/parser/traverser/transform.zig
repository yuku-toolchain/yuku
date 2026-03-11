//! # Transform Traverser
//!
//! Walks a `ParseTreeMut` and lets visitor hooks mutate the AST in place.
//! The context exposes the mutable tree directly via `ctx.tree`, giving
//! access to all read and write operations: `getData`, `getSpan`, `setData`,
//! `setSpan`, `addNode`, and `addExtra`.
//!
//! ## Replacing a node
//!
//! The simplest transform is replacing a node's data in its `enter_*` hook.
//! The walker re-reads node data after every enter, so the replacement's
//! children are walked automatically:
//!
//! ```
//! pub fn enter_binary_expression(
//!     _: *MyVisitor,
//!     expr: ast.BinaryExpression,
//!     index: ast.NodeIndex,
//!     ctx: *transform.Ctx,
//! ) traverser.Action {
//!     if (expr.operator == .add) {
//!         ctx.tree.setData(index, .{ .binary_expression = .{
//!             .left = expr.left,
//!             .right = expr.right,
//!             .operator = .multiply,
//!         } });
//!     }
//!     return .proceed;
//! }
//! ```
//!
//! ## Creating new nodes
//!
//! Use `addNode` to append a new node to the tree and get its index.
//! Use `addExtra` to allocate a child list (for nodes with `IndexRange` fields).
//! Both are safe to call during traversal.
//!
//! A common pattern is wrapping a node: copy the original data to a new node,
//! then replace the current node with a wrapper pointing to the new one:
//!
//! ```
//! // Move the original data to a new node.
//! const inner = try ctx.tree.addNode(
//!     .{ .binary_expression = expr },
//!     ctx.tree.getSpan(index),
//! );
//!
//! // Replace the current node with a wrapper.
//! ctx.tree.setData(index, .{ .parenthesized_expression = .{
//!     .expression = inner,
//! } });
//!
//! // Skip so the walker does not descend into the new children,
//! // which would re-trigger this hook on the moved inner node.
//! return .skip;
//! ```
//!
//! ## Avoiding self-references
//!
//! Never point a node's child back to its own index. The walker re-reads
//! after enter, so a self-referential node causes infinite recursion:
//!
//! ```
//! // WRONG: creates a cycle (node points to itself).
//! const wrapper = try ctx.tree.addNode(
//!     .{ .parenthesized_expression = .{ .expression = index } },
//!     span,
//! );
//! ctx.tree.setData(index, ctx.tree.getData(wrapper));
//!
//! // RIGHT: move original data to a new node, point wrapper to it.
//! const inner = try ctx.tree.addNode(original_data, span);
//! ctx.tree.setData(index, .{ .parenthesized_expression = .{
//!     .expression = inner,
//! } });
//! ```

const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");

const Allocator = std.mem.Allocator;

pub const Ctx = struct {
    tree: *ast.ParseTreeMut,
    path: wk.NodePath = .{},

    pub fn enter(self: *Ctx, index: ast.NodeIndex, _: ast.NodeData) Allocator.Error!void {
        self.path.push(index);
    }

    pub fn exit(self: *Ctx, _: ast.NodeData) void {
        self.path.pop();
    }
};

/// Walks the tree with path tracking and mutation support.
pub fn traverse(comptime V: type, tree: *ast.ParseTreeMut, visitor: *V) Allocator.Error!void {
    var ctx = Ctx{ .tree = tree };

    var layer = wk.Layer(Ctx, V){ .inner = visitor };

    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);
}
