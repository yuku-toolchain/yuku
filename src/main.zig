const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const Action = parser.traverser.Action;
const Ctx = parser.traverser.basic.Ctx;

const Printer = struct {
    pub fn enter_node(_: *Printer, data: ast.NodeData, _: ast.NodeIndex, ctx: *Ctx) Action {
        for (1..ctx.path.depth()) |_| std.debug.print("  ", .{});
        std.debug.print("{s}\n", .{@tagName(data)});
        return .proceed;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\class C {
        \\  get async() { return 1; }
        \\  set get(v) {}
        \\  async get() { return 2; }
        \\  static async *gen() {}
        \\}
    ;

    var tree = try parser.parse(allocator, source, .{ .lang = .ts });

    const diagnostics = tree.diagnostics.items;
    if (diagnostics.len == 0) {
        std.debug.print("diagnostics: none\n\n", .{});
    } else {
        std.debug.print("diagnostics ({d}):\n", .{diagnostics.len});
        for (diagnostics) |d| {
            std.debug.print("  {s}: {s} ({d}..{d})\n", .{ d.severity.toString(), d.message, d.span.start, d.span.end });
        }
        std.debug.print("\n", .{});
    }

    std.debug.print("ast:\n", .{});
    var printer: Printer = .{};
    try parser.traverser.basic.traverse(Printer, &tree, &printer);

    const result = try parser.codegen.print(allocator, &tree, .{});
    std.debug.print("\ngenerated:\n{s}\n", .{result.code});
}
