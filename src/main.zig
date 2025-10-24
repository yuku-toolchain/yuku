const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;

pub fn main() !void {
    var timer = try std.time.Timer.start();

    const source: []const u8 = "1.23s+10 Hello";

    var lexer = Lexer.init(source);

    const token = try lexer.nextToken();

    std.debug.print("{any}\n", .{token.type});
    std.debug.print("{s}\n", .{token.lexeme});
    std.debug.print("{any}\n", .{token.span});

    const elapsed = timer.read();
    std.debug.print("Time taken: {d}ns ({d}μs)\n", .{ elapsed, elapsed / 1000 });
}
