const std = @import("std");
const js = @import("js");
const printError = @import("print-error.zig").printError;

pub fn main() !void {
    const content = @embedFile("test.js");
    const allocator = std.heap.page_allocator;

    var times = try std.ArrayList(i128).initCapacity(allocator, iterations);
    defer times.deinit(allocator);

    var first_result: ?js.ParseResult = null;

    for (0..iterations) |i| {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        var parser = try js.Parser.init(arena.allocator(), content);

        const start = std.time.nanoTimestamp();
        const result = try parser.parse();
        const end = std.time.nanoTimestamp();

        try times.append(allocator, end - start);

        if (i == 0) {
            first_result = result;
            if (result.hasErrors()) {
                std.debug.print("\nErrors:\n", .{});
                for (result.errors) |parse_err| {
                    printError(content, parse_err);
                }
                std.debug.print("\n", .{});
            }
        }
    }

    var total: i128 = 0;
    var min: i128 = times.items[0];
    var max: i128 = times.items[0];

    for (times.items) |time| {
        total += time;
        if (time < min) min = time;
        if (time > max) max = time;
    }

    const avg = @divTrunc(total, iterations);

    const avg_ms = @as(f64, @floatFromInt(avg)) / ns_to_ms;
    const min_ms = @as(f64, @floatFromInt(min)) / ns_to_ms;
    const max_ms = @as(f64, @floatFromInt(max)) / ns_to_ms;

    const size_kb = @as(f64, @floatFromInt(content.len)) / 1024.0;
    const mb_per_sec = (size_kb / 1024.0) / (avg_ms / 1000.0);

    std.debug.print("Min: {d:.3} ms | Max: {d:.3} ms | Avg: {d:.3} ms\n", .{ min_ms, max_ms, avg_ms });
    std.debug.print("Throughput: {d:.2} MB/sec\n", .{mb_per_sec});
}

const iterations = 10;
const ns_to_ms = 1_000_000.0;

fn formatNumber(num: f64) void {
    if (num >= 1_000_000.0) {
        std.debug.print("{d:.2}M", .{num / 1_000_000.0});
    } else if (num >= 1_000.0) {
        std.debug.print("{d:.2}K", .{num / 1_000.0});
    } else {
        std.debug.print("{d:.0}", .{num});
    }
}
