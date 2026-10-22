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
    while (i < input.len and i - start < digits_max and isOctalDigit(input[i])) : (i += 1) {
        value = (value << 3) | (input[i] - '0');
    }
    return .{ .value = value, .end = i };
}

/// https://tc39.es/ecma262/#prod-HexEscapeSequence
pub inline fn parseHex2(input: []const u8, start: usize) ?Decoded {
    if (start + 2 > input.len) return null;
    const hi = hexVal(input[start]) orelse return null;
    const lo = hexVal(input[start + 1]) orelse return null;
    return .{ .value = (@as(u21, hi) << 4) | lo, .end = start + 2 };
}

inline fn parseHex4(input: []const u8, start: usize) ?Decoded {
    if (start + 4 > input.len) return null;
    var value: u21 = 0;
    for (input[start..][0..4]) |c| value = (value << 4) | (hexVal(c) orelse return null);
    return .{ .value = value, .end = start + 4 };
}

fn parseHexVariable(input: []const u8, start: usize, digits_max: usize) ?Decoded {
    var value: u32 = 0;
    var i = start;
    while (i < input.len and i - start < digits_max) : (i += 1) {
        value = (value << 4) | (hexVal(input[i]) orelse break);
        if (value > 0x10FFFF) return null;
    }
    if (i == start) return null;
    return .{ .value = @intCast(value), .end = i };
}

/// https://tc39.es/ecma262/#prod-UnicodeEscapeSequence
pub fn parseUnicodeEscape(input: []const u8, start: usize) ?Decoded {
    if (start < input.len and input[start] == '{') {
        const brace_end = std.mem.findScalarPos(u8, input, start + 1, '}') orelse return null;
        const r = parseHexVariable(input, start + 1, brace_end - start - 1) orelse return null;
        if (r.end != brace_end) return null;
        return .{ .value = r.value, .end = brace_end + 1 };
    }
    return parseHex4(input, start);
}

/// https://tc39.es/ecma262/#sec-identifier-names-static-semantics-stringvalue
pub fn decodeIdentifierEscapes(raw: []const u8, buf: *[256]u8) []const u8 {
    var out: usize = 0;
    var i: usize = 0;
    while (i < raw.len) {
        if (raw[i] == '\\' and i + 1 < raw.len and raw[i + 1] == 'u') {
            const parsed = parseUnicodeEscape(raw, i + 2) orelse return raw;
            if (parsed.value < 0x80) {
                if (out >= buf.len) return raw;
                buf[out] = @intCast(parsed.value);
                out += 1;
            } else {
                const n = std.unicode.utf8Encode(@intCast(parsed.value), buf[out..]) catch
                    return raw;
                out += n;
            }
            i = parsed.end;
        } else {
            if (out >= buf.len) return raw;
            buf[out] = raw[i];
            out += 1;
            i += 1;
        }
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
    while (i < raw.len) {
        const run_start = i;

        while (i < raw.len and raw[i] != '\\' and raw[i] != '\r') : (i += 1) {}

        if (i > run_start) try out.appendSlice(gpa, raw[run_start..i]);
        if (i >= raw.len) break;

        if (raw[i] == '\r') {
            try out.append(gpa, '\n');
            i += line_terminators.lineBreakLen(raw, i);
            continue;
        }
        i += 1;
        if (i >= raw.len) break;
        switch (raw[i]) {
            'n', 'r', 't', 'b', 'f', 'v' => |c| {
                try out.append(gpa, switch (c) {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'b' => 0x08,
                    'f' => 0x0C,
                    'v' => 0x0B,
                    else => unreachable,
                });
                i += 1;
            },
            '0'...'7' => {
                const r = parseOctal(raw, i);
                try util.Utf.appendCodePoint(out, gpa, r.value);
                i = r.end;
            },
            'x', 'u' => |c| {
                const parsed = if (c == 'x')
                    parseHex2(raw, i + 1)
                else
                    parseUnicodeEscape(raw, i + 1);
                if (parsed) |r| {
                    try util.Utf.appendCodePoint(out, gpa, r.value);
                    i = r.end;
                } else {
                    try out.append(gpa, c);
                    i += 1;
                }
            },
            else => {
                const break_len = line_terminators.lineBreakLen(raw, i);
                if (break_len > 0) {
                    i += break_len;
                } else {
                    try out.append(gpa, raw[i]);
                    i += 1;
                }
            },
        }
    }
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

fn isOctalDigit(digit: u8) bool {
    return digit >= '0' and digit <= '7';
}

inline fn hexVal(c: u8) ?u8 {
    return std.fmt.charToDigit(c, 16) catch null;
}

const testing = std.testing;

test "isOctalDigit" {
    for ('0'..'8') |c| try testing.expect(isOctalDigit(@intCast(c)));
    try testing.expect(!isOctalDigit('8'));
    try testing.expect(!isOctalDigit('9'));
    try testing.expect(!isOctalDigit('a'));
}

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
}

test "parseHex2 and parseHex4 read a fixed digit count" {
    try expectDecoded(0xFF, 2, parseHex2("FF", 0));
    try expectDecoded(0xAB, 2, parseHex2("ab", 0));
    try testing.expect(parseHex2("F", 0) == null);
    try testing.expect(parseHex2("GG", 0) == null);
    try expectDecoded(0x00E9, 4, parseHex4("00E9rest", 0));
    try testing.expect(parseHex4("00E", 0) == null);
}

test "parseHexVariable stops at the digit limit and past U+10FFFF" {
    try expectDecoded(0x1F600, 5, parseHexVariable("1F600", 0, 6));
    try expectDecoded(0xAB, 2, parseHexVariable("ABCDEF", 0, 2));
    try testing.expect(parseHexVariable("FFFFFF", 0, 6) == null);
    try testing.expect(parseHexVariable("xyz", 0, 6) == null);
}

test "parseUnicodeEscape reads the braced and four digit forms" {
    try expectDecoded(0x00E9, 4, parseUnicodeEscape("00E9rest", 0));
    try expectDecoded(0x1F600, 7, parseUnicodeEscape("{1F600}!", 0));
    try expectDecoded(0xA, 3, parseUnicodeEscape("{A}z", 0));
    try testing.expect(parseUnicodeEscape("{1F600", 0) == null);
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
