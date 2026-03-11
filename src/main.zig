const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const traverser = parser.traverser;

pub fn main(init: std.process.Init) !void {
    const Io = init.io;
    const allocator = init.arena.allocator();

    const source = "const a = x + y";

    var tree = try parser.parse(std.heap.page_allocator, source, .{});
    defer tree.deinit();

    // before transform
    const json_before = try parser.estree.toJSON(&tree, allocator, .{});
    std.debug.print("=== Before ===\n{s}\n\n", .{json_before});

    // transform: change + to *
    var t = PlusToMul{};
    try traverser.transform.traverse(PlusToMul, &tree, &t);

    // after transform — same tree, same toJSON
    const json_after = try parser.estree.toJSON(&tree, allocator, .{});
    std.debug.print("=== After ===\n{s}\n", .{json_after});

    _ = Io;
}

/// Demo transform: replaces every `+` with `*`.
const PlusToMul = struct {
    pub fn enter_binary_expression(
        _: *PlusToMul,
        expr: ast.BinaryExpression,
        index: ast.NodeIndex,
        ctx: *traverser.transform.Ctx,
    ) traverser.Action {
        if (expr.operator == .add) {
            ctx.replaceWith(index, .{ .binary_expression = .{
                .left = expr.left,
                .right = expr.right,
                .operator = .multiply,
            } });
        }
        return .proceed;
    }
};
