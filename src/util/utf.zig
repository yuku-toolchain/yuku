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

/// check if the byte sequence at `pos` is U+2028 (Line Separator) or U+2029 (Paragraph Separator)
pub fn unicodeSeparatorLen(source: []const u8, pos: usize) u8 {
    if (pos + 2 < source.len and source[pos] == 0xE2 and source[pos + 1] == 0x80) {
        if (source[pos + 2] == 0xA8 or source[pos + 2] == 0xA9) {
            return 3;
        }
    }
    return 0;
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

// --- Tests ---

const testing = std.testing;

test "codePointAt - ASCII" {
    const s = "hello";
    const cp = try codePointAt(s, 0);
    try testing.expectEqual(@as(u3, 1), cp.len);
    try testing.expectEqual(@as(u21, 'h'), cp.value);
}

test "codePointAt - 2-byte UTF-8" {
    const s = "ñ";
    const cp = try codePointAt(s, 0);
    try testing.expectEqual(@as(u3, 2), cp.len);
    try testing.expectEqual(@as(u21, 0x00F1), cp.value);
}

test "codePointAt - 3-byte UTF-8" {
    const s = "€";
    const cp = try codePointAt(s, 0);
    try testing.expectEqual(@as(u3, 3), cp.len);
    try testing.expectEqual(@as(u21, 0x20AC), cp.value);
}

test "codePointAt - 4-byte UTF-8" {
    const s = "𐍈";
    const cp = try codePointAt(s, 0);
    try testing.expectEqual(@as(u3, 4), cp.len);
    try testing.expectEqual(@as(u21, 0x10348), cp.value);
}

test "codePointAt - invalid leading byte" {
    const s = &[_]u8{0xFF};
    try testing.expectError(error.InvalidUtf8, codePointAt(s, 0));
}

test "isOctalDigit" {
    for ('0'..'8') |c| {
        try testing.expect(isOctalDigit(@intCast(c)));
    }
    try testing.expect(!isOctalDigit('8'));
    try testing.expect(!isOctalDigit('9'));
    try testing.expect(!isOctalDigit('a'));
}

test "unicodeSeparatorLen - line separator U+2028" {
    const s = &[_]u8{ 0xE2, 0x80, 0xA8 };
    try testing.expectEqual(@as(u8, 3), unicodeSeparatorLen(s, 0));
}

test "unicodeSeparatorLen - paragraph separator U+2029" {
    const s = &[_]u8{ 0xE2, 0x80, 0xA9 };
    try testing.expectEqual(@as(u8, 3), unicodeSeparatorLen(s, 0));
}

test "unicodeSeparatorLen - not a separator" {
    const s = &[_]u8{ 0xE2, 0x80, 0xAA };
    try testing.expectEqual(@as(u8, 0), unicodeSeparatorLen(s, 0));
}

test "unicodeSeparatorLen - too short" {
    const s = &[_]u8{ 0xE2, 0x80 };
    try testing.expectEqual(@as(u8, 0), unicodeSeparatorLen(s, 0));
}

test "isMultiByteSpace" {
    try testing.expect(isMultiByteSpace(0xFEFF)); // BOM
    try testing.expect(isMultiByteSpace(0x00A0)); // NBSP
    try testing.expect(isMultiByteSpace(0x2000)); // EN QUAD
    try testing.expect(isMultiByteSpace(0x2005)); // FOUR-PER-EM SPACE
    try testing.expect(isMultiByteSpace(0x3000)); // IDEOGRAPHIC SPACE
    try testing.expect(isMultiByteSpace(0x1680)); // OGHAM SPACE MARK
    try testing.expect(!isMultiByteSpace(' '));
    try testing.expect(!isMultiByteSpace('A'));
}

test "parseOctal - single digit" {
    const r = parseOctal("5xyz", 0);
    try testing.expectEqual(@as(u21, 5), r.value);
    try testing.expectEqual(@as(usize, 1), r.end);
}

test "parseOctal - three digits starting with 0-3" {
    const r = parseOctal("377", 0);
    try testing.expectEqual(@as(u21, 0o377), r.value);
    try testing.expectEqual(@as(usize, 3), r.end);
}

test "parseOctal - two digits starting with 4-7" {
    const r = parseOctal("77", 0);
    try testing.expectEqual(@as(u21, 0o77), r.value);
    try testing.expectEqual(@as(usize, 2), r.end);
}

test "parseOctal - stops at non-octal" {
    const r = parseOctal("129", 0);
    try testing.expectEqual(@as(u21, 0o12), r.value);
    try testing.expectEqual(@as(usize, 2), r.end);
}

test "parseHex2 - valid" {
    const r = parseHex2("FF", 0).?;
    try testing.expectEqual(@as(u21, 0xFF), r.value);
    try testing.expectEqual(@as(usize, 2), r.end);
}

test "parseHex2 - lowercase" {
    const r = parseHex2("ab", 0).?;
    try testing.expectEqual(@as(u21, 0xAB), r.value);
}

test "parseHex2 - too short" {
    try testing.expect(parseHex2("F", 0) == null);
}

test "parseHex2 - invalid hex char" {
    try testing.expect(parseHex2("GG", 0) == null);
}

test "parseHex4 - valid" {
    const r = parseHex4("00E9rest", 0).?;
    try testing.expectEqual(@as(u21, 0x00E9), r.value);
    try testing.expectEqual(@as(usize, 4), r.end);
}

test "parseHex4 - too short" {
    try testing.expect(parseHex4("00E", 0) == null);
}

test "parseHexVariable - valid" {
    const r = parseHexVariable("1F600", 0, 6).?;
    try testing.expectEqual(@as(u21, 0x1F600), r.value);
    try testing.expectEqual(@as(usize, 5), r.end);
}

test "parseHexVariable - exceeds max codepoint" {
    try testing.expect(parseHexVariable("FFFFFF", 0, 6) == null);
}

test "parseHexVariable - no digits" {
    try testing.expect(parseHexVariable("xyz", 0, 6) == null);
}

test "parseHexVariable - respects max_digits" {
    const r = parseHexVariable("ABCDEF", 0, 2).?;
    try testing.expectEqual(@as(u21, 0xAB), r.value);
    try testing.expectEqual(@as(usize, 2), r.end);
}

test "parseUnicodeEscape - 4-digit form" {
    const r = parseUnicodeEscape("00E9rest", 0).?;
    try testing.expectEqual(@as(u21, 0x00E9), r.value);
    try testing.expectEqual(@as(usize, 4), r.end);
}

test "parseUnicodeEscape - braced form" {
    const r = parseUnicodeEscape("{1F600}!", 0).?;
    try testing.expectEqual(@as(u21, 0x1F600), r.value);
    try testing.expectEqual(@as(usize, 7), r.end);
}

test "parseUnicodeEscape - braced form single digit" {
    const r = parseUnicodeEscape("{A}z", 0).?;
    try testing.expectEqual(@as(u21, 0xA), r.value);
    try testing.expectEqual(@as(usize, 3), r.end);
}

test "parseUnicodeEscape - missing closing brace" {
    try testing.expect(parseUnicodeEscape("{1F600", 0) == null);
}

test "parseUnicodeEscape - invalid hex in 4-digit" {
    try testing.expect(parseUnicodeEscape("GHIJ", 0) == null);
}

test "hexVal" {
    try testing.expectEqual(@as(u8, 0), hexVal('0').?);
    try testing.expectEqual(@as(u8, 9), hexVal('9').?);
    try testing.expectEqual(@as(u8, 10), hexVal('a').?);
    try testing.expectEqual(@as(u8, 15), hexVal('f').?);
    try testing.expectEqual(@as(u8, 10), hexVal('A').?);
    try testing.expectEqual(@as(u8, 15), hexVal('F').?);
    try testing.expect(hexVal('g') == null);
    try testing.expect(hexVal('G') == null);
    try testing.expect(hexVal(' ') == null);
}
