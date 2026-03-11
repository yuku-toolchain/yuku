const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const traverser = parser.traverser;
const tranform = traverser.transform;

pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();

    const source = "const a = x + y";

    var mut_tree = try parser.parseMut(std.heap.page_allocator, source, .{});

    var t = PlusToMul{};
    try tranform.traverse(PlusToMul, &mut_tree, &t);

    var tree = mut_tree.finalize();
    defer tree.deinit();

    const json = try parser.estree.toJSON(&tree, allocator, .{});
    std.debug.print("{s}\n", .{json});
}

const PlusToMul = struct {
    pub fn enter_binary_expression(
        _: *PlusToMul,
        expr: ast.BinaryExpression,
        index: ast.NodeIndex,
        ctx: *tranform.Ctx,
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
