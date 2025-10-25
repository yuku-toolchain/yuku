const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;

pub fn main() !void {
    var timer = try std.time.Timer.start();

    const source: []const u8 = "+ - * / % ** ++ -- += -= *= /= %= **=";

    var lexer = Lexer.init(source);

    while (true) {
        const token = try lexer.nextToken();
        std.debug.print("{any}, {s}, {any}\n", .{token.type, token.lexeme, token.span});
        if(token.type == .EOF){
            break;
        }
    }

    const elapsed = timer.read();
    std.debug.print("Time taken: {d}ns ({d}μs)\n", .{ elapsed, elapsed / 1000 });
}
