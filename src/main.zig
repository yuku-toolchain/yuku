const std = @import("std");
const Parser = @import("parser.zig").Parser;
const ParseResult = @import("parser.zig").ParseResult;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const printError = @import("print-error.zig").printError;

fn countLines(content: []const u8) usize {
    var count: usize = 1;
    for (content) |c| {
        if (c == '\n') count += 1;
    }
    return count;
}

fn formatNumber(num: f64) void {
    if (num >= 1_000_000.0) {
        std.debug.print("{d:.2}M", .{num / 1_000_000.0});
    } else if (num >= 1_000.0) {
        std.debug.print("{d:.2}K", .{num / 1_000.0});
    } else {
        std.debug.print("{d:.0}", .{num});
    }
}

pub fn main() !void {
    const content = @embedFile("test.js");

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const iterations = 10;
    var times = std.ArrayList(i128).empty;
    defer times.deinit(allocator);

    std.debug.print("Running {d} iterations...\n", .{iterations});

    var i: usize = 0;
    var first_result: ParseResult = undefined;

    while (i < iterations) : (i += 1) {
        var parser = try Parser.init(allocator, content);

        const start = std.time.nanoTimestamp();
        const result = try parser.parse();
        const end = std.time.nanoTimestamp();

        if (i == 0) {
            first_result = result;
        }

        try times.append(allocator, end - start);

        if (i == 0 and result.hasErrors()) {
            std.debug.print("\nErrors found:\n", .{});
            for (result.errors) |parse_err| {
                printError(content, parse_err);
            }
            std.debug.print("\n", .{});
        }
    }

    var total: i128 = 0;
    var min: i128 = times.items[0];
    var max: i128 = times.items[0];

    for (times.items) |t| {
        total += t;
        if (t < min) min = t;
        if (t > max) max = t;
    }

    const avg = @divTrunc(total, iterations);

    const avg_ms = @as(f64, @floatFromInt(avg)) / 1_000_000.0;
    const min_ms = @as(f64, @floatFromInt(min)) / 1_000_000.0;
    const max_ms = @as(f64, @floatFromInt(max)) / 1_000_000.0;

    const lines = countLines(content);
    const size_kb = @as(f64, @floatFromInt(content.len)) / 1024.0;

    const lines_per_sec = @as(f64, @floatFromInt(lines)) / (avg_ms / 1000.0);
    const mb_per_sec = (size_kb / 1024.0) / (avg_ms / 1000.0);

    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
    std.debug.print("File:    {d} bytes ({d:.2} KB)\n", .{ content.len, size_kb });
    std.debug.print("Lines:   {d}\n", .{lines});
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
    std.debug.print("Min:     {d:.3} ms\n", .{min_ms});
    std.debug.print("Max:     {d:.3} ms\n", .{max_ms});
    std.debug.print("Average: {d:.3} ms\n", .{avg_ms});
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
    std.debug.print("Throughput:\n", .{});
    std.debug.print("  ", .{});
    formatNumber(lines_per_sec);
    std.debug.print(" lines/sec\n", .{});
    std.debug.print("  {d:.2} MB/sec\n", .{mb_per_sec});
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});

    // std.log.info("\n\n{f}", .{std.json.fmt(first_result, .{ .whitespace = .indent_2 })});
}
