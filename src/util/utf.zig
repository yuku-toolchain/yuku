const std = @import("std");
const xhtml_entities = @import("xhtml_entities.zig");

pub const CodePoint = struct { len: u3, value: u21 };

pub const Utf8Error = error{InvalidUtf8};

pub fn codePointAt(str: []const u8, i: usize) Utf8Error!CodePoint {
    std.debug.assert(i < str.len);
    const len = std.unicode.utf8ByteSequenceLength(str[i]) catch return error.InvalidUtf8;
    if (str.len - i < len) return error.InvalidUtf8;

    const codepoint: u21 = switch (len) {
        1 => str[i],
        2 => std.unicode.utf8Decode2(.{ str[i], str[i + 1] }) catch return error.InvalidUtf8,
        3 => std.unicode.utf8Decode3(.{ str[i], str[i + 1], str[i + 2] }) catch
            return error.InvalidUtf8,
        4 => std.unicode.utf8Decode4(.{ str[i], str[i + 1], str[i + 2], str[i + 3] }) catch
            return error.InvalidUtf8,
        else => unreachable,
    };

    return .{ .len = @intCast(len), .value = codepoint };
}

pub fn isOctalDigit(digit: u8) bool {
    return digit >= '0' and digit <= '7';
}

/// Returns 3 if a U+2028 LINE SEPARATOR or U+2029 PARAGRAPH SEPARATOR starts at `pos`, else 0.
pub fn unicodeSeparatorLen(source: []const u8, pos: usize) u8 {
    if (pos + 2 < source.len and source[pos] == 0xE2 and source[pos + 1] == 0x80) {
        if (source[pos + 2] == 0xA8 or source[pos + 2] == 0xA9) {
            return 3;
        }
    }
    return 0;
}

/// The byte length of the line terminator at `pos`, or 0. CRLF is one terminator.
pub fn lineBreakLen(source: []const u8, pos: usize) u8 {
    std.debug.assert(pos < source.len);
    return switch (source[pos]) {
        '\n' => 1,
        '\r' => if (pos + 1 < source.len and source[pos + 1] == '\n') 2 else 1,
        0xE2 => unicodeSeparatorLen(source, pos),
        else => 0,
    };
}

pub const Decoded = struct { value: u21, end: usize };

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

/// Parses a Unicode escape after its `\u` prefix. `start` points at `{` for the braced form
/// or at the first of four hex digits.
pub fn parseUnicodeEscape(input: []const u8, start: usize) ?Decoded {
    if (start < input.len and input[start] == '{') {
        const brace_end = std.mem.findScalarPos(u8, input, start + 1, '}') orelse return null;
        const r = parseHexVariable(input, start + 1, brace_end - start - 1) orelse return null;
        if (r.end != brace_end) return null;
        return .{ .value = r.value, .end = brace_end + 1 };
    }
    return parseHex4(input, start);
}

/// Decodes the Unicode escapes of an identifier into `buf`, or returns `raw` when they do
/// not fit or are malformed.
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

/// Appends the cooked value of a string or template body to `out`, as WTF-8.
pub fn decodeStringEscapes(
    gpa: std.mem.Allocator,
    raw: []const u8,
    out: *std.ArrayList(u8),
) error{OutOfMemory}!void {
    const start = out.items.len;
    defer if (std.mem.findScalarPos(u8, out.items, start, 0xED) != null) {
        joinSurrogatePairs(out, start);
    };
    var i: usize = 0;
    while (i < raw.len) {
        const run_start = i;

        while (i < raw.len and raw[i] != '\\' and raw[i] != '\r') : (i += 1) {}

        if (i > run_start) try out.appendSlice(gpa, raw[run_start..i]);
        if (i >= raw.len) break;

        // a raw CR or CRLF cooks to LF
        if (raw[i] == '\r') {
            try out.append(gpa, '\n');
            i += 1;
            if (i < raw.len and raw[i] == '\n') i += 1;
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
            '0' => {
                if (i + 1 < raw.len and std.ascii.isDigit(raw[i + 1])) {
                    const r = parseOctal(raw, i);
                    try appendCodePoint(out, gpa, r.value);
                    i = r.end;
                } else {
                    try out.append(gpa, 0);
                    i += 1;
                }
            },
            '1'...'7' => {
                const r = parseOctal(raw, i);
                try appendCodePoint(out, gpa, r.value);
                i = r.end;
            },
            'x' => {
                if (parseHex2(raw, i + 1)) |r| {
                    try appendCodePoint(out, gpa, r.value);
                    i = r.end;
                } else {
                    try out.append(gpa, 'x');
                    i += 1;
                }
            },
            'u' => {
                if (parseUnicodeEscape(raw, i + 1)) |r| {
                    try appendCodePoint(out, gpa, r.value);
                    i = r.end;
                } else {
                    try out.append(gpa, 'u');
                    i += 1;
                }
            },
            '\n' => i += 1,
            '\r' => {
                i += 1;
                if (i < raw.len and raw[i] == '\n') i += 1;
            },
            else => {
                const separator_len = unicodeSeparatorLen(raw, i);
                if (separator_len > 0) {
                    i += separator_len;
                } else {
                    try out.append(gpa, raw[i]);
                    i += 1;
                }
            },
        }
    }
}

/// Appends JSX text or a JSX attribute string to `out`, entities decoded.
pub fn decodeJSXEntities(
    gpa: std.mem.Allocator,
    raw: []const u8,
    out: *std.ArrayList(u8),
) error{OutOfMemory}!void {
    var run_start: usize = 0;
    var i: usize = 0;
    while (std.mem.findScalarPos(u8, raw, i, '&')) |amp| {
        const entity = jsxEntityAt(raw, amp) orelse {
            i = amp + 1;
            continue;
        };
        try out.appendSlice(gpa, raw[run_start..amp]);
        try appendCodePoint(out, gpa, entity.value);
        run_start = entity.end;
        i = entity.end;
    }
    try out.appendSlice(gpa, raw[run_start..]);
}

fn jsxEntityAt(raw: []const u8, amp: usize) ?Decoded {
    std.debug.assert(raw[amp] == '&');
    var i = amp + 1;
    var value: u32 = 0;
    if (i < raw.len and raw[i] == '#') {
        i += 1;
        const base: u8 = if (i < raw.len and raw[i] == 'x') 16 else 10;
        if (base == 16) i += 1;
        const digits = i;
        while (i < raw.len) : (i += 1) {
            const digit = std.fmt.charToDigit(raw[i], base) catch break;
            // saturates past the last code point, which stays as written
            value = @min(value * base + digit, 0x110000);
        }
        if (i == digits or value > 0x10FFFF) return null;
    } else {
        const name = i;
        while (i < raw.len and std.ascii.isAlphanumeric(raw[i])) i += 1;
        value = xhtml_entities.code_points.get(raw[name..i]) orelse return null;
    }
    if (i == raw.len or raw[i] != ';') return null;
    return .{ .value = @intCast(value), .end = i + 1 };
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

// adjacent surrogates are one code point, escaped or not
fn joinSurrogatePairs(out: *std.ArrayList(u8), start: usize) void {
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

// utf-8, or wtf-8 for a lone surrogate
fn appendCodePoint(
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

/// Whether a string contains a legacy octal escape (`\0n`, `\1` to `\7`) or a non-octal
/// decimal escape (`\8`, `\9`).
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

inline fn hexVal(c: u8) ?u8 {
    if (c >= '0' and c <= '9') return c - '0';
    if (c >= 'a' and c <= 'f') return c - 'a' + 10;
    if (c >= 'A' and c <= 'F') return c - 'A' + 10;
    return null;
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

test "isOctalDigit" {
    for ('0'..'8') |c| try testing.expect(isOctalDigit(@intCast(c)));
    try testing.expect(!isOctalDigit('8'));
    try testing.expect(!isOctalDigit('9'));
    try testing.expect(!isOctalDigit('a'));
}

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
