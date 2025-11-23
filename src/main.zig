const std = @import("std");
const js = @import("js");

pub fn main() !void {
    const content = @embedFile("test.js");

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var parser = try js.Parser.init(allocator, content);

    const start = std.time.nanoTimestamp();

    _ = try parser.parse();

    const end = std.time.nanoTimestamp();

    const time_ns = end - start;
    const time_ns_float = @as(f64, @floatFromInt(time_ns));
    const time_ms = time_ns_float / 1_00_00_00.0;

    // std.log.info("\n\n{f}\n\n", .{std.json.fmt(result, .{ .whitespace = .indent_2 })});

    std.debug.print("{d:.2}ms\n", .{time_ms});
}
