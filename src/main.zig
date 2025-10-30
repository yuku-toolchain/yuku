const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Token = @import("token.zig").Token;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const content = @embedFile("test.js");

    const start = std.time.nanoTimestamp();

    var parser = try Parser.init(allocator, content);

    _ =  try parser.parse();

    const end = std.time.nanoTimestamp();

    const elapsed = end - start;

    const elapsed_ms = @as(f64, @floatFromInt(elapsed)) / 1_000_000.0;

    std.debug.print("{d:.2}ms\n\n", .{elapsed_ms});

    // std.log.info("{f}", .{std.json.fmt(ast, .{ .whitespace = .indent_2 })});
}
