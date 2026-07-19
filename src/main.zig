const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const basic = parser.traverser.basic;
const codegen = parser.codegen;
const Action = parser.traverser.Action;

const source =
    \\ const {...{}} = cool;
;

const Visitor = struct {
    pub fn enter_object_pattern(_: *Visitor, _: ast.ObjectPattern, _: ast.NodeIndex, _: *basic.Ctx) !Action {
        std.debug.print("yes", .{});

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

    for (tree.diagnostics.items) |d| {
        std.debug.print("{s}\n", .{d.message});
    }

    var visitor: Visitor = .{};
    try basic.traverse(Visitor, &tree, &visitor);

    const js = try codegen.generate(gpa, &tree, .{ .strip = true });
    defer js.deinit(gpa);
    std.debug.print("\n{s}\n", .{js.code});
}
