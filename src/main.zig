const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const basic = parser.traverser.basic;
const codegen = parser.codegen;
const Action = parser.traverser.Action;

const source =
    \\abstract class Shape {
    \\  abstract area(): number;
    \\}
    \\
    \\function clamp(x: number): number {
    \\  return x;
    \\}
    \\
;

const Visitor = struct {
    pub fn enter_class(_: *Visitor, c: ast.Class, _: ast.NodeIndex, ctx: *basic.Ctx) !Action {
        std.debug.print("class {s}\n", .{name(ctx.tree, c.id)});
        return .proceed;
    }

    pub fn enter_function(_: *Visitor, f: ast.Function, _: ast.NodeIndex, ctx: *basic.Ctx) !Action {
        if (f.id == .null) return .proceed;
        std.debug.print("function {s}\n", .{name(ctx.tree, f.id)});
        return .proceed;
    }
};

fn name(tree: *const ast.Tree, id: ast.NodeIndex) []const u8 {
    if (id == .null) return "(anonymous)";
    return tree.string(tree.data(id).binding_identifier.name);
}

pub fn main() !void {
    const gpa = std.heap.smp_allocator;

    var tree = try parser.parse(gpa, source, .{ .lang = .ts });
    defer tree.deinit();

    var visitor: Visitor = .{};
    try basic.traverse(Visitor, &tree, &visitor);

    const js = try codegen.strip(gpa, &tree, .{});
    defer js.deinit(gpa);
    std.debug.print("\n{s}\n", .{js.code});
}
