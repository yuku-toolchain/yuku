const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const traverser = parser.traverser;
const transform = traverser.transform;

pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();

    const source = "const a = x + y";

    var builder = try parser.build(std.heap.page_allocator, source, .{});

    var t = TransformVisit{};
    try transform.traverse(TransformVisit, &builder, &t);

    var tree = builder.toTree(.{});
    defer tree.deinit();

    const json = try parser.estree.toJSON(&tree, allocator, .{});
    std.debug.print("{s}\n", .{json});
}

const TransformVisit = struct {
    pub fn enter_binary_expression(
        _: *TransformVisit,
        expr: ast.BinaryExpression,
        index: ast.NodeIndex,
        ctx: *transform.Ctx,
    ) !traverser.Action {
        const span = ctx.tree.getSpan(index);

        const inner = try ctx.tree.createNode(
            .{ .binary_expression = .{
                .left = expr.left,
                .right = expr.right,
                .operator = .multiply,
            } },
            span,
        );

        if (expr.operator == .add) {
            ctx.tree.replaceData(index, .{ .parenthesized_expression = .{
                .expression = inner,
            } });

            ctx.tree.replaceSpan(index, .{ .start = span.start - 1, .end = span.end + 1 });
        }

        return .proceed;
    }

    pub fn enter_binding_identifier(
        _: *TransformVisit,
        _: ast.BindingIdentifier,
        index: ast.NodeIndex,
        ctx: *transform.Ctx,
    ) !traverser.Action {
        ctx.tree.replaceData(index, .{ .binding_identifier = .{ .name = try ctx.tree.internString("new_name") } });

        return .proceed;
    }
};
