const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Tokenize = @import("js").Tokenizer;

pub fn main() !void {
    const content = @embedFile("test.js");

    {
        var lexer = Lexer.init(content);
        while ((try lexer.nextToken()).type != .EOF) {}
    }

    const num_runs = 10;
    var total_time: u64 = 0;
    var total_tokens: usize = 0;

    for (0..num_runs) |_| {
        var timer = try std.time.Timer.start();
        var lexer = Lexer.init(content);
        var token_count: usize = 0;

        while (true) {
            const token = try lexer.nextToken();
            if (token.type == .EOF) break;
            token_count += 1;
        }

        total_time += timer.read();
        total_tokens = token_count;
    }

    const avg_time_ns = total_time / num_runs;
    const avg_time_ms = @as(f64, @floatFromInt(avg_time_ns)) / 1_000_000.0;
    const mb_per_sec = (@as(f64, @floatFromInt(content.len)) / @as(f64, @floatFromInt(avg_time_ns))) * 1_000.0;

    std.debug.print("=== Lexer Benchmark ===\n", .{});
    std.debug.print("File size: {d} bytes ({d:.2} MB)\n", .{ content.len, @as(f64, @floatFromInt(content.len)) / 1_000_000.0 });
    std.debug.print("Tokens: {d}\n", .{total_tokens});
    std.debug.print("Runs: {d}\n", .{num_runs});
    std.debug.print("Avg time: {d:.2}ms\n", .{avg_time_ms});
    std.debug.print("Speed: {d:.1} MB/s\n", .{mb_per_sec});
    std.debug.print("Tokens/sec: {d:.0}\n", .{@as(f64, @floatFromInt(total_tokens)) / (avg_time_ms / 1000.0)});
}
