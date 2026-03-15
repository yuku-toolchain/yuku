const std = @import("std");
const parser = @import("parser");

const semantic = parser.semantic;

pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();

    const source = "const a = x + y";

    var tree = try parser.parse(std.heap.page_allocator, source, .{});

    var result = try semantic.analyze(&tree, allocator);
    defer result.deinit();
}
