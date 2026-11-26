//! https://tc39.es/ecma262/#sec-literals-string-literals

const std = @import("std");
const util = @import("util");
const line_terminators = @import("line_terminators.zig");

pub const Decoded = struct { value: u21, end: usize };

/// https://tc39.es/ecma262/#prod-LegacyOctalEscapeSequence
pub fn parseOctal(input: []const u8, start: usize) Decoded {
    std.debug.assert(start < input.len);
    const digits_max: usize = if (input[start] <= '3') 3 else 2;
    var value: u21 = 0;
    var i = start;
    while (i < input.len and i - start < digits_max and input[i] -% '0' < 8) : (i += 1) {
        value = (value << 3) | (input[i] - '0');
    }
    return .{ .value = value, .end = i };
}

/// https://tc39.es/ecma262/#prod-HexEscapeSequence
pub fn parseHex(input: []const u8, start: usize, len: usize) ?Decoded {
    if (len == 0 or start + len > input.len) return null;
    var value: u32 = 0;
    for (input[start..][0..len]) |c| {
        value = (value << 4) | (std.fmt.charToDigit(c, 16) catch return null);
        if (value > 0x10FFFF) return null;
    }
    return .{ .value = @intCast(value), .end = start + len };
}

/// https://tc39.es/ecma262/#prod-UnicodeEscapeSequence
pub fn parseUnicodeEscape(input: []const u8, start: usize) ?Decoded {
    if (start >= input.len or input[start] != '{') return parseHex(input, start, 4);
    const close = std.mem.findScalarPos(u8, input, start + 1, '}') orelse return null;
    const code_point = parseHex(input, start + 1, close - start - 1) orelse return null;
    return .{ .value = code_point.value, .end = close + 1 };
}

/// https://tc39.es/ecma262/#sec-identifier-names-static-semantics-stringvalue
pub fn decodeIdentifierEscapes(raw: []const u8, buf: *[256]u8) []const u8 {
    var out: usize = 0;
    var i: usize = 0;
    while (i < raw.len) {
        var unit: [4]u8 = .{ raw[i], 0, 0, 0 };
        var unit_len: usize = 1;
        if (raw[i] == '\\' and i + 1 < raw.len and raw[i + 1] == 'u') {
            const parsed = parseUnicodeEscape(raw, i + 2) orelse return raw;
            unit_len = std.unicode.utf8Encode(parsed.value, &unit) catch return raw;
            i = parsed.end;
        } else i += 1;
        if (out + unit_len > buf.len) return raw;
        @memcpy(buf[out..][0..unit_len], unit[0..unit_len]);
        out += unit_len;
    }
    return buf[0..out];
}

/// https://tc39.es/ecma262/#sec-static-semantics-sv
/// https://tc39.es/ecma262/#sec-static-semantics-tv
pub fn decodeStringEscapes(
    gpa: std.mem.Allocator,
    raw: []const u8,
    out: *std.ArrayList(u8),
) error{OutOfMemory}!void {
    const start = out.items.len;
    defer if (std.mem.findScalarPos(u8, out.items, start, 0xED) != null) {
        util.Utf.joinSurrogatePairs(out, start);
    };
    var i: usize = 0;
    while (std.mem.findAnyPos(u8, raw, i, "\\\r")) |special| {
        try out.appendSlice(gpa, raw[i..special]);
        if (raw[special] == '\r') {
            try out.append(gpa, '\n');
            i = special + line_terminators.lineBreakLen(raw, special);
            continue;
        }
        i = special + 1;
        if (i == raw.len) return;
        const escape: ?Decoded = switch (raw[i]) {
            '0'...'7' => parseOctal(raw, i),
            'x' => parseHex(raw, i + 1, 2),
            'u' => parseUnicodeEscape(raw, i + 1),
            else => null,
        };
        if (escape) |code_point| {
            try util.Utf.appendCodePoint(out, gpa, code_point.value);
            i = code_point.end;
            continue;
        }
        const break_len = line_terminators.lineBreakLen(raw, i);
        if (break_len == 0) try out.append(gpa, switch (raw[i]) {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'b' => 0x08,
            'f' => 0x0C,
            'v' => 0x0B,
            else => |c| c,
        });
        i += @max(1, break_len);
    }
    try out.appendSlice(gpa, raw[i..]);
}

/// https://tc39.es/ecma262/#sec-string-literals-early-errors
pub fn hasOctalEscape(str: []const u8) bool {
    var pos: usize = 0;
    while (std.mem.findScalarPos(u8, str, pos, '\\')) |i| {
        const next = i + 1;
        if (next >= str.len) break;
        switch (str[next]) {
            '1'...'9' => return true,
            '0' => if (next + 1 < str.len and std.ascii.isDigit(str[next + 1])) return true,
            else => {},
        }
        pos = next + 1;
    }
    return false;
}

const testing = std.testing;

fn expectDecoded(value: u21, end: usize, actual: ?Decoded) !void {
    const decoded = actual orelse return error.TestExpectedDecoded;
    try testing.expectEqual(value, decoded.value);
    try testing.expectEqual(end, decoded.end);
}

test "parseOctal takes three digits after 0 to 3, two after 4 to 7" {
    try expectDecoded(5, 1, parseOctal("5xyz", 0));
    try expectDecoded(0o377, 3, parseOctal("377", 0));
    try expectDecoded(0o77, 2, parseOctal("77", 0));
    try expectDecoded(0o12, 2, parseOctal("129", 0));
    try expectDecoded(0o1, 1, parseOctal("18", 0));
    try expectDecoded(0o7, 1, parseOctal("7/", 0));
}

test "parseHex reads an exact digit count, up to U+10FFFF" {
    try expectDecoded(0xFF, 2, parseHex("FF", 0, 2));
    try expectDecoded(0xAB, 2, parseHex("ab", 0, 2));
    try expectDecoded(0xAB, 2, parseHex("ABCDEF", 0, 2));
    try expectDecoded(0x00E9, 4, parseHex("00E9rest", 0, 4));
    try expectDecoded(0x1F600, 5, parseHex("1F600", 0, 5));
    try expectDecoded(0x09AFAF, 6, parseHex("09afAF", 0, 6));
    const invalid = [_]struct { []const u8, usize }{
        .{ "F", 2 },   .{ "GG", 2 },     .{ "gg", 2 },  .{ "  ", 2 },
        .{ "00E", 4 }, .{ "FFFFFF", 6 }, .{ "xyz", 3 }, .{ "FF", 0 },
    };
    for (invalid) |case| try testing.expect(parseHex(case[0], 0, case[1]) == null);
}

test "parseUnicodeEscape reads the braced and four digit forms" {
    try expectDecoded(0x00E9, 4, parseUnicodeEscape("00E9rest", 0));
    try expectDecoded(0x1F600, 7, parseUnicodeEscape("{1F600}!", 0));
    try expectDecoded(0xA, 3, parseUnicodeEscape("{A}z", 0));
    try testing.expect(parseUnicodeEscape("{1F600", 0) == null);
    try testing.expect(parseUnicodeEscape("{}", 0) == null);
    try testing.expect(parseUnicodeEscape("{110000}", 0) == null);
    try testing.expect(parseUnicodeEscape("GHIJ", 0) == null);
}

test "decodeIdentifierEscapes decodes into the buffer, or returns the name as written" {
    var buf: [256]u8 = undefined;
    try testing.expectEqualStrings("café", decodeIdentifierEscapes("c\\u0061f\\u{e9}", &buf));
    try testing.expectEqualStrings("\\u00zz", decodeIdentifierEscapes("\\u00zz", &buf));
    const long: [254]u8 = @splat('a');
    try testing.expectEqualStrings(long ++ "é", decodeIdentifierEscapes(long ++ "\\u00e9", &buf));
    const outgrows = long ++ "a\\u00e9";
    try testing.expectEqualStrings(outgrows, decodeIdentifierEscapes(outgrows, &buf));
}

fn expectDecode(input: []const u8, expected: []const u8) !void {
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(testing.allocator);
    try decodeStringEscapes(testing.allocator, input, &out);
    try testing.expectEqualSlices(u8, expected, out.items);
}

test "decodeStringEscapes passes text without escapes through" {
    try expectDecode("hello world", "hello world");
    try expectDecode("", "");
}

test "decodeStringEscapes single character escapes" {
    try expectDecode("a\\nb\\tc", "a\nb\tc");
    try expectDecode("\\r\\n", "\r\n");
    try expectDecode("\\b\\f\\v", "\x08\x0C\x0B");
    try expectDecode("\\\\", "\\");
    try expectDecode("\\'", "'");
    try expectDecode("\\\"", "\"");
    try expectDecode("a\\0b", "a\x00b");
}

test "decodeStringEscapes octal escapes" {
    try expectDecode("\\01", "\x01");
    try expectDecode("\\101", "A");
    try expectDecode("\\7", "\x07");
    try expectDecode("\\377", "\xC3\xBF");
}

test "decodeStringEscapes hex and unicode escapes" {
    try expectDecode("\\x41", "A");
    try expectDecode("\\x61\\x62\\x63", "abc");
    try expectDecode("\\xff", "\xC3\xBF");
    try expectDecode("\\u0041", "A");
    try expectDecode("\\u00E9", "\xC3\xA9");
    try expectDecode("\\u4E16\\u754C", "\xE4\xB8\x96\xE7\x95\x8C");
    try expectDecode("\\u{41}", "A");
    try expectDecode("\\u{1F600}", "\xF0\x9F\x98\x80");
    try expectDecode("\\u{0}", "\x00");
}

test "decodeStringEscapes joins a surrogate pair however it is written" {
    try expectDecode("\\uD83D\\uDCA9", "\xF0\x9F\x92\xA9");
    try expectDecode("\\uD83D\\\xED\xB0\x80", "\xF0\x9F\x90\x80");
    try expectDecode("\xED\xA0\xBD\\uDC00", "\xF0\x9F\x90\x80");
    try expectDecode("\\uD83D\\\n\\uDC00", "\xF0\x9F\x90\x80");
    try expectDecode("\\u{D83D}\\u{DC00}", "\xF0\x9F\x90\x80");
    try expectDecode("\\uDC00\\uD83D", "\xED\xB0\x80\xED\xA0\xBD");
}

test "decodeStringEscapes keeps a lone surrogate as wtf-8" {
    try expectDecode("\\uD800", "\xED\xA0\x80");
    try expectDecode("\\uDC00", "\xED\xB0\x80");
}

test "decodeStringEscapes line continuations and raw line breaks" {
    try expectDecode("hello\\\nworld", "helloworld");
    try expectDecode("hello\\\r\nworld", "helloworld");
    try expectDecode("hello\\\rworld", "helloworld");
    try expectDecode("a\rb", "a\nb");
    try expectDecode("a\r\nb", "a\nb");
    try expectDecode("\r\n\r", "\n\n");
}

test "decodeStringEscapes mixed escapes and a trailing backslash" {
    try expectDecode("hello\\nworld\\t\\u0021", "hello\nworld\t!");
    try expectDecode("\\x48\\x65\\x6C\\x6C\\x6F", "Hello");
    try expectDecode("abc\\", "abc");
}
