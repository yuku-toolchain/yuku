const std = @import("std");

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

/// ECMAScript LineTerminator: LF, CR, LS (U+2028), PS (U+2029)
pub fn lineTerminatorLen(source: []const u8, pos: usize) u8 {
    if (pos >= source.len) return 0;
    const c = source[pos];
    if (c == '\n' or c == '\r') return 1;
    if (c == 0xE2 and pos + 2 < source.len and source[pos + 1] == 0x80) {
        if (source[pos + 2] == 0xA8 or source[pos + 2] == 0xA9) return 3;
    }
    return 0;
}

/// Check if byte at position is start of a line terminator
pub inline fn isLineTerminator(source: []const u8, pos: usize) bool {
    return lineTerminatorLen(source, pos) > 0;
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

pub fn parseOctal(input: []const u8, start: usize) struct { value: u21, end: usize } {
    var value: u16 = 0;
    var i = start;
    const max: usize = if (input[start] <= '3') 3 else 2;
    var count: usize = 0;
    while (i < input.len and count < max) : (i += 1) {
        if (input[i] >= '0' and input[i] <= '7') {
            value = (value << 3) | (input[i] - '0');
            count += 1;
        } else break;
    }
    return .{ .value = @intCast(value), .end = i };
}

/// exactly 2 hex digits (for \xHH escape sequences)
pub fn parseHex2(input: []const u8, start: usize) ?struct { value: u21, end: usize } {
    if (start + 2 > input.len) return null;

    const hi = hexVal(input[start]) orelse return null;
    const lo = hexVal(input[start + 1]) orelse return null;

    return .{ .value = (@as(u21, hi) << 4) | lo, .end = start + 2 };
}

/// exactly 4 hex digits (for \uHHHH escape sequences)
pub fn parseHex4(input: []const u8, start: usize) ?struct { value: u21, end: usize } {
    if (start + 4 > input.len) return null;

    var value: u21 = 0;
    for (0..4) |j| {
        const d = hexVal(input[start + j]) orelse return null;
        value = (value << 4) | d;
    }

    return .{ .value = value, .end = start + 4 };
}

/// 1-6 hex digits until closing brace or max digits (for \u{H...} escape sequences)
pub fn parseHexVariable(input: []const u8, start: usize, max_digits: usize) ?struct { value: u21, end: usize, has_digits: bool } {
    var value: u21 = 0;
    var i = start;
    var count: usize = 0;
    var has_digits = false;

    const max = @min(max_digits, 6);

    while (i < input.len and count < max) {
        if (hexVal(input[i])) |d| {
            value = (value << 4) | d;
            has_digits = true;
            count += 1;
            i += 1;
        } else {
            break;
        }
    }

    if (!has_digits) return null;
    return .{ .value = value, .end = i, .has_digits = has_digits };
}

pub fn hexVal(c: u8) ?u8 {
    return if (c >= '0' and c <= '9') c - '0' else if (c >= 'a' and c <= 'f') c - 'a' + 10 else if (c >= 'A' and c <= 'F') c - 'A' + 10 else null;
}

pub fn buildUtf16PosMap(allocator: std.mem.Allocator, source: []const u8) ![]u32 {
    var map = try allocator.alloc(u32, source.len + 1);
    var byte_pos: usize = 0;
    var utf16_pos: u32 = 0;

    while (byte_pos < source.len) {
        map[byte_pos] = utf16_pos;
        const len = std.unicode.utf8ByteSequenceLength(source[byte_pos]) catch 1;
        utf16_pos += if (len == 4) 2 else 1; // surrogate pair for 4-byte sequences
        for (1..len) |i| {
            if (byte_pos + i < source.len) map[byte_pos + i] = utf16_pos;
        }
        byte_pos += len;
    }
    map[source.len] = utf16_pos;
    return map;
}
