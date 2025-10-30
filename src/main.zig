const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Token = @import("token.zig").Token;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const content = @embedFile("test.js");

    var parser = try Parser.init(allocator, content);

    const ast = try parser.parse();

    std.debug.print("{any}", .{ast});
}
