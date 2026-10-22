//! https://tc39.es/ecma262/#sec-line-terminators

const std = @import("std");

/// Returns 3 if a U+2028 LINE SEPARATOR or U+2029 PARAGRAPH SEPARATOR starts at `pos`, else 0.
pub fn unicodeSeparatorLen(source: []const u8, pos: usize) u8 {
    if (pos + 2 < source.len and source[pos] == 0xE2 and source[pos + 1] == 0x80) {
        if (source[pos + 2] == 0xA8 or source[pos + 2] == 0xA9) {
            return 3;
        }
    }
    return 0;
}

/// https://tc39.es/ecma262/#prod-LineTerminatorSequence
pub fn lineBreakLen(source: []const u8, pos: usize) u8 {
    std.debug.assert(pos < source.len);
    return switch (source[pos]) {
        '\n' => 1,
        '\r' => if (pos + 1 < source.len and source[pos + 1] == '\n') 2 else 1,
        0xE2 => unicodeSeparatorLen(source, pos),
        else => 0,
    };
}

const testing = std.testing;

test "unicodeSeparatorLen" {
    try testing.expectEqual(@as(u8, 3), unicodeSeparatorLen("\xE2\x80\xA8", 0));
    try testing.expectEqual(@as(u8, 3), unicodeSeparatorLen("\xE2\x80\xA9", 0));
    try testing.expectEqual(@as(u8, 0), unicodeSeparatorLen("\xE2\x80\xAA", 0));
    try testing.expectEqual(@as(u8, 0), unicodeSeparatorLen("\xE2\x80", 0));
}

test "lineBreakLen" {
    try testing.expectEqual(@as(u8, 1), lineBreakLen("\n", 0));
    try testing.expectEqual(@as(u8, 1), lineBreakLen("\r", 0));
    try testing.expectEqual(@as(u8, 2), lineBreakLen("\r\n", 0));
    try testing.expectEqual(@as(u8, 1), lineBreakLen("\r\n", 1));
    try testing.expectEqual(@as(u8, 0), lineBreakLen("a", 0));
    try testing.expectEqual(@as(u8, 3), lineBreakLen("\xE2\x80\xA8", 0));
    try testing.expectEqual(@as(u8, 3), lineBreakLen("\xE2\x80\xA9", 0));
    try testing.expectEqual(@as(u8, 0), lineBreakLen("€", 0));
}
