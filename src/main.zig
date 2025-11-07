const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Token = @import("token.zig").Token;
const printError = @import("print-error.zig").printError;

pub fn main() !void {
    const content = @embedFile("test.js");

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const start = std.time.nanoTimestamp();

    var parser = try Parser.init(allocator, content);

    const result = try parser.parse();

    const end = std.time.nanoTimestamp();

    const elapsed = end - start;

    const elapsed_ms = @as(f64, @floatFromInt(elapsed)) / 1_000_000.0;

    std.debug.print("{d:.3}ms\n\n", .{elapsed_ms});

    if (result.hasErrors()) {
        for (result.errors) |parse_err| {
            printError(content, parse_err);
        }
    }

    // std.log.info("{f}", .{std.json.fmt(result, .{ .whitespace = .indent_2 })});
}
