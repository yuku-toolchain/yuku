const std = @import("std");

pub const CodePoint = struct { len: u3, value: u21 };

pub const Utf8Error = error{InvalidUtf8};

pub fn codePointAt(str: []const u8, i: u32) Utf8Error!CodePoint {
    const len = std.unicode.utf8ByteSequenceLength(str[i]) catch return error.InvalidUtf8;

    const codepoint: u21 = switch (len) {
        1 => str[i],
        2 => std.unicode.utf8Decode2(.{ str[i], str[i + 1] }) catch return error.InvalidUtf8,
        3 => std.unicode.utf8Decode3(.{ str[i], str[i + 1], str[i + 2] }) catch return error.InvalidUtf8,
        4 => std.unicode.utf8Decode4(.{ str[i], str[i + 1], str[i + 2], str[i + 3] }) catch return error.InvalidUtf8,
        else => unreachable,
    };

    return .{ .len = @intCast(len), .value = codepoint };
}

pub fn isOctalDigit(digit: u8) bool {
    return digit >= '0' and digit <= '7';
}

/// returns the length of ascii line terminators (\n, \r, \r\n)
/// returns 0 if no ascii line terminator is found at the position
pub fn asciiLineTerminatorLen(source: []const u8, pos: usize) u8 {
    if (pos >= source.len) return 0;

    const c = source[pos];

    // LF
    if (c == '\n') return 1;

    // CR or CRLF
    if (c == '\r') {
        return if (pos + 1 < source.len and source[pos + 1] == '\n') 2 else 1;
    }

    return 0;
}

pub fn lineTerminatorLen(source: []const u8, pos: usize) u8 {
    const ascii_len = asciiLineTerminatorLen(source, pos);

    if (ascii_len > 0) return ascii_len;

    return unicodeSeparatorLen(source, pos);
}

/// check if the byte sequence at `pos` is U+2028 (Line Separator) or U+2029 (Paragraph Separator)
pub fn unicodeSeparatorLen(source: []const u8, pos: usize) u8 {
    if (pos + 2 < source.len and source[pos] == 0xE2 and source[pos + 1] == 0x80) {
        if (source[pos + 2] == 0xA8 or source[pos + 2] == 0xA9) {
            return 3;
        }
    }
    return 0;
}

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
    var value: u21 = 0;
    var i = start;
    const max: usize = if (input[start] <= '3') 3 else 2;
    var count: usize = 0;

    while (i < input.len and count < max) : (i += 1) {
        if (isOctalDigit(input[i])) {
            value = (value << 3) | (input[i] - '0');
            count += 1;
        } else break;
    }

    return .{ .value = value, .end = i };
}

pub inline fn parseHex2(input: []const u8, start: usize) ?struct { value: u21, end: usize } {
    if (start + 2 > input.len) return null;

    const hi = hexVal(input[start]) orelse return null;
    const lo = hexVal(input[start + 1]) orelse return null;

    return .{ .value = (@as(u21, hi) << 4) | lo, .end = start + 2 };
}

pub inline fn parseHex4(input: []const u8, start: usize) ?struct { value: u21, end: usize } {
    if (start + 4 > input.len) return null;

    var value: u21 = 0;
    for (0..4) |j| {
        const d = hexVal(input[start + j]) orelse return null;
        value = (value << 4) | d;
    }

    return .{ .value = value, .end = start + 4 };
}

/// validates the value is <= 0x10FFFF
pub fn parseHexVariable(input: []const u8, start: usize, max_digits: usize) ?struct { value: u21, end: usize, has_digits: bool } {
    var value: u32 = 0;
    var i = start;
    var count: usize = 0;

    while (i < input.len and count < max_digits) {
        const d = hexVal(input[i]) orelse break;

        value = (value << 4) | d;

        if (value > 0x10FFFF) return null;

        count += 1;
        i += 1;
    }

    const has_digits = count > 0;
    if (!has_digits) return null;

    return .{ .value = @intCast(value), .end = i, .has_digits = has_digits };
}

/// parse a unicode escape after the `\u` prefix.
/// `start` points at `{` (braced form) or the first hex digit (4-digit form).
/// returns the decoded code point and position after the escape.
pub fn parseUnicodeEscape(input: []const u8, start: usize) ?struct { value: u21, end: usize } {
    if (start < input.len and input[start] == '{') {
        const digit_start = start + 1;

        const brace_end = std.mem.findScalarPos(u8, input, digit_start, '}') orelse return null;

        const r = parseHexVariable(input, digit_start, brace_end - digit_start) orelse return null;

        if (r.end != brace_end) return null;

        return .{ .value = r.value, .end = brace_end + 1 };
    }

    const r = parseHex4(input, start) orelse return null;

    return .{ .value = r.value, .end = r.end };
}

inline fn hexVal(c: u8) ?u8 {
    return if (c >= '0' and c <= '9') c - '0' else if (c >= 'a' and c <= 'f') c - 'a' + 10 else if (c >= 'A' and c <= 'F') c - 'A' + 10 else null;
}

pub inline fn isSurrogateRange(cp: u21) bool {
    return cp >= 0xD800 and cp <= 0xDFFF;
}

pub inline fn isHighSurrogate(cp: u21) bool {
    return cp >= 0xD800 and cp <= 0xDBFF;
}

pub inline fn isLowSurrogate(cp: u21) bool {
    return cp >= 0xDC00 and cp <= 0xDFFF;
}
