const std = @import("std");

pub const UnicodeId = @import("unicode-id.zig");
pub const StringInterner = @import("string_interner.zig");

pub const CodePoint = struct { len: u3, value: u21 };

pub const Utf8Error = error{InvalidUtf8};

pub fn codePointAt(str: []const u8, i: u32) Utf8Error!CodePoint {
    const len = std.unicode.utf8ByteSequenceLength(str[i]) catch return error.InvalidUtf8;
    const codepoint = switch (len) {
        1 => str[i],
        2 => std.unicode.utf8Decode2(.{ str[i], str[i + 1] }),
        3 => std.unicode.utf8Decode3(.{ str[i], str[i + 1], str[i + 2] }),
        4 => std.unicode.utf8Decode4(.{ str[i], str[i + 1], str[i + 2], str[i + 3] }),
        else => unreachable,
    };
    return .{ .len = @intCast(len), .value = codepoint catch return error.InvalidUtf8 };
}

pub fn isOctalDigit(digit: u8) bool {
    return digit >= '0' and digit <= '7';
}

pub fn isMultiByteSpace(cp: u21) bool {
    return switch (cp) {
        '\u{FEFF}',
        '\u{00A0}',
        '\u{2000}',
        '\u{2001}'...'\u{200A}',
        '\u{202F}',
        '\u{205F}',
        '\u{3000}',
        '\u{1680}',
        => true,
        else => false,
    };
}
