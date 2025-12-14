const std = @import("std");

pub fn parseJSNumeric(str: []const u8) !f64 {
    if (str.len == 0) return error.InvalidCharacter;

    var buf: [128]u8 = undefined;
    var len: usize = 0;
    for (str) |c| {
        if (c != '_') {
            if (len >= buf.len) return error.Overflow;
            buf[len] = c;
            len += 1;
        }
    }
    const s = buf[0..len];
    if (s.len == 0) return error.InvalidCharacter;

    if (s.len >= 2 and s[0] == '0') {
        switch (s[1]) {
            'x', 'X' => return @floatFromInt(try std.fmt.parseInt(i64, s[2..], 16)),
            'b', 'B' => return @floatFromInt(try std.fmt.parseInt(i64, s[2..], 2)),
            'o', 'O' => return @floatFromInt(try std.fmt.parseInt(i64, s[2..], 8)),
            '0'...'7' => {
                for (s[1..]) |c| {
                    if (c < '0' or c > '7') break;
                } else {
                    return @floatFromInt(try std.fmt.parseInt(i64, s[1..], 8));
                }
            },
            else => {},
        }
    }

    return std.fmt.parseFloat(f64, s) catch @floatFromInt(try std.fmt.parseInt(i64, s, 10));
}
