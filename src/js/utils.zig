const std = @import("std");

pub inline fn isUseStrict(str: []const u8) bool {
    return std.mem.eql(u8, "use strict", str);
}
