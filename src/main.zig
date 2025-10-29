const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;

// pub fn main() !void {
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//     const allocator = arena.allocator();

//     const content = "let عمر = 25;";

//     var lexer = try Lexer.init(allocator, content);
//     defer lexer.deinit();

//     while (true) {
//         const token = try lexer.nextToken();
//         std.debug.print("{s}", .{token.lexeme});
//         if (token.type == .EOF) break;
//     }
// }

const Position = struct {
    line: usize,
    column: usize,
};

fn getPosition(content: []const u8, offset: usize) Position {
    var line: usize = 1;
    var column: usize = 1;
    var i: usize = 0;

    while (i < offset and i < content.len) : (i += 1) {
        if (content[i] == '\n') {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    return Position{ .line = line, .column = column };
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const content = @embedFile("test.js");

    const num_runs = 10;
    var total_time: i128 = 0;
    var total_tokens: usize = 0;

    for (0..num_runs) |_| {
        var lexer = try Lexer.init(allocator, content);
        defer lexer.deinit();
        var token_count: usize = 0;
        var last_token: ?Token = null;

        const start = std.time.nanoTimestamp();

        while (true) {
            const token = lexer.nextToken() catch |err| {
                if (last_token) |tok| {
                    const start_pos = getPosition(content, tok.span.start);
                    std.debug.print("Error: {any} at line {d}, column {d}\n", .{
                        err,
                        start_pos.line,
                        start_pos.column,
                    });
                } else {
                    std.debug.print("Error: {any} at start of file\n", .{err});
                }
                break;
            };
            if (token.type == .EOF) break;
            token_count += 1;
            last_token = token;
        }

        const end = std.time.nanoTimestamp();
        const elapsed = end - start;
        total_time += elapsed;
        total_tokens = token_count;
    }

    const avg_time_ns = @divTrunc(total_time, @as(i128, num_runs));

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
