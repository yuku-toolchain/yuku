const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const basic = parser.traverser.basic;

const source =
    \\function greet(name: string): string {
    \\    return `hello ${name}`;
    \\}
;

const Visitor = struct {
    pub fn enter_binding_identifier(
        _: *Visitor,
        id: ast.BindingIdentifier,
        _: ast.NodeIndex,
        ctx: *basic.Ctx,
    ) parser.traverser.Action {
        std.debug.print("binding {s}\n", .{ctx.tree.string(id.name)});
        return .proceed;
    }
};

pub fn main() !void {
    const gpa = std.heap.smp_allocator;

    var tree = try parser.parse(gpa, source, .{ .lang = .ts });
    defer tree.deinit();

    for (tree.diagnostics.items) |diagnostic| {
        std.debug.print("{s}\n", .{diagnostic.message});
    }

    var visitor: Visitor = .{};
    try basic.traverse(Visitor, &tree, &visitor);

    const output = try parser.codegen.generate(gpa, &tree, .{ .strip = true });
    defer output.deinit(gpa);
    std.debug.print("\n{s}\n", .{output.code});
}
