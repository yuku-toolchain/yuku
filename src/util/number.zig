const std = @import("std");

pub fn parseJSNumeric(str: []const u8) !f64 {
    return std.fmt.parseFloat(f64, str) catch {
        const int = try std.fmt.parseInt(i64, str, 0);
        return @floatFromInt(int);
    };
}
