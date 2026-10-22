const std = @import("std");

pub const CodePoint = struct { len: u3, value: u21 };

pub fn codePointAt(str: []const u8, i: usize) error{InvalidUtf8}!CodePoint {
    std.debug.assert(i < str.len);
    const len = std.unicode.utf8ByteSequenceLength(str[i]) catch return error.InvalidUtf8;
    if (str.len - i < len) return error.InvalidUtf8;
    const value = std.unicode.utf8Decode(str[i..][0..len]) catch return error.InvalidUtf8;
    return .{ .len = @intCast(len), .value = value };
}

/// Returns the code unit of a WTF-8 lone surrogate (`ED A0..BF 80..BF`) at `s[i]`, else
/// null.
pub fn loneSurrogateAt(s: []const u8, i: usize) ?u21 {
    if (s[i] != 0xED or i + 2 >= s.len) return null;
    if (s[i + 1] < 0xA0 or s[i + 1] > 0xBF or s[i + 2] & 0xC0 != 0x80) return null;
    return (@as(u21, s[i] & 0x0F) << 12) |
        (@as(u21, s[i + 1] & 0x3F) << 6) |
        @as(u21, s[i + 2] & 0x3F);
}

/// Appends `cp` as WTF-8, where a surrogate takes three bytes of its own.
pub fn appendCodePoint(
    out: *std.ArrayList(u8),
    alloc: std.mem.Allocator,
    cp: u21,
) error{OutOfMemory}!void {
    std.debug.assert(cp <= 0x10FFFF);
    if (cp >= 0xD800 and cp <= 0xDFFF) {
        try out.appendSlice(alloc, &[3]u8{
            0xE0 | @as(u8, @intCast(cp >> 12)),
            0x80 | @as(u8, @intCast((cp >> 6) & 0x3F)),
            0x80 | @as(u8, @intCast(cp & 0x3F)),
        });
    } else {
        var buf: [4]u8 = undefined;
        const n = std.unicode.utf8Encode(cp, &buf) catch unreachable;
        try out.appendSlice(alloc, buf[0..n]);
    }
}

/// Rewrites each pair of WTF-8 surrogates from `start` on as the one code point they encode.
pub fn joinSurrogatePairs(out: *std.ArrayList(u8), start: usize) void {
    const items = out.items;
    var read = start;
    var write = start;
    while (read < items.len) {
        if (pairAt(items, read)) |cp| {
            write += std.unicode.utf8Encode(cp, items[write..][0..4]) catch unreachable;
            read += 6;
        } else {
            items[write] = items[read];
            write += 1;
            read += 1;
        }
    }
    out.shrinkRetainingCapacity(write);
}

fn pairAt(s: []const u8, i: usize) ?u21 {
    const high = loneSurrogateAt(s, i) orelse return null;
    if (high > 0xDBFF or i + 3 >= s.len) return null;
    const low = loneSurrogateAt(s, i + 3) orelse return null;
    if (low < 0xDC00) return null;
    return 0x10000 + (high - 0xD800) * 0x400 + (low - 0xDC00);
}

const testing = std.testing;

test "loneSurrogateAt reads only a well-formed surrogate" {
    try testing.expectEqual(@as(?u21, 0xD83D), loneSurrogateAt("\xED\xA0\xBD", 0));
    try testing.expectEqual(@as(?u21, 0xDFFF), loneSurrogateAt("\xED\xBF\xBF", 0));
    try testing.expectEqual(@as(?u21, null), loneSurrogateAt("\xED\x9F\xBF", 0));
    try testing.expectEqual(@as(?u21, null), loneSurrogateAt("\xED\xF0\x9F\x98", 0));
    try testing.expectEqual(@as(?u21, null), loneSurrogateAt("\xED\xA0\x41", 0));
    try testing.expectEqual(@as(?u21, null), loneSurrogateAt("\xED\xA0", 0));
}

test "codePointAt decodes every sequence length" {
    const cases = [_]struct { source: []const u8, len: u3, value: u21 }{
        .{ .source = "hello", .len = 1, .value = 'h' },
        .{ .source = "ñ", .len = 2, .value = 0x00F1 },
        .{ .source = "€", .len = 3, .value = 0x20AC },
        .{ .source = "𐍈", .len = 4, .value = 0x10348 },
    };
    for (cases) |case| {
        const cp = try codePointAt(case.source, 0);
        try testing.expectEqual(case.len, cp.len);
        try testing.expectEqual(case.value, cp.value);
    }
}

test "codePointAt rejects an invalid lead byte and truncated sequences" {
    for ([_][]const u8{ "\xFF", "\xC3", "\xE2", "\xF0\x90" }) |source| {
        try testing.expectError(error.InvalidUtf8, codePointAt(source, 0));
    }
}
