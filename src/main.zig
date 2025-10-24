const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;

pub fn main() !void {
    var timer = try std.time.Timer.start();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const source: []const u8 = "*10.10";

    var lexer = Lexer.init(allocator, source);

    _ = try lexer.nextToken();

    const token = try lexer.nextToken();

    std.debug.print("{any}\n", .{token.type});
    std.debug.print("{s}\n", .{token.lexeme});
    std.debug.print("{any}\n", .{token.span});

    const elapsed = timer.read();
    std.debug.print("Time taken: {d}ns ({d}μs)\n", .{ elapsed, elapsed / 1000 });
}
