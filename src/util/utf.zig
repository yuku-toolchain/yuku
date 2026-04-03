const std = @import("std");

pub const CodePoint = struct { len: u3, value: u21 };

pub const Utf8Error = error{InvalidUtf8};

pub fn codePointAt(str: []const u8, i: usize) Utf8Error!CodePoint {
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
pub fn parseHexVariable(input: []const u8, start: usize, max_digits: usize) ?struct { value: u21, end: usize } {
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

    if (count == 0) return null;

    return .{ .value = @intCast(value), .end = i };
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

/// resolves \uHHHH / \u{HHHH} escapes in an identifier to their UTF-8 form.
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
                const n = std.unicode.utf8Encode(@intCast(parsed.value), buf[out..]) catch return raw;
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

/// decodes all JavaScript string escape sequences from `raw` into `out`.
/// handles standard escapes (\n, \t, etc.), hex (\xHH), unicode (\uHHHH, \u{H}),
/// octal, line continuations, and CR normalization for template values.
pub fn decodeStringEscapes(raw: []const u8, out: *std.ArrayList(u8), alloc: std.mem.Allocator) error{OutOfMemory}!void {
    var i: usize = 0;
    while (i < raw.len) {
        const run_start = i;

        while (i < raw.len and raw[i] != '\\' and raw[i] != '\r') : (i += 1) {}

        if (i > run_start) try out.appendSlice(alloc, raw[run_start..i]);
        if (i >= raw.len) break;

        // CR normalization per ECMAScript TV semantics:
        // \r\n -> \n, standalone \r -> \n
        if (raw[i] == '\r') {
            try out.append(alloc, '\n');
            i += 1;
            if (i < raw.len and raw[i] == '\n') i += 1;
            continue;
        }
        i += 1; // skip backslash
        if (i >= raw.len) break;
        switch (raw[i]) {
            'n' => { try out.append(alloc, '\n'); i += 1; },
            'r' => { try out.append(alloc, '\r'); i += 1; },
            't' => { try out.append(alloc, '\t'); i += 1; },
            'b' => { try out.append(alloc, 0x08); i += 1; },
            'f' => { try out.append(alloc, 0x0C); i += 1; },
            'v' => { try out.append(alloc, 0x0B); i += 1; },
            '0' => {
                if (i + 1 < raw.len and std.ascii.isDigit(raw[i + 1])) {
                    const r = parseOctal(raw, i);
                    try appendCodePoint(out, alloc, r.value);
                    i = r.end;
                } else {
                    try out.append(alloc, 0);
                    i += 1;
                }
            },
            '1'...'7' => {
                const r = parseOctal(raw, i);
                try appendCodePoint(out, alloc, r.value);
                i = r.end;
            },
            'x' => {
                if (parseHex2(raw, i + 1)) |r| {
                    try appendCodePoint(out, alloc, r.value);
                    i = r.end;
                } else {
                    try out.append(alloc, 'x');
                    i += 1;
                }
            },
            'u' => {
                if (parseUnicodeEscape(raw, i + 1)) |r| {
                    var cp = r.value;
                    var end = r.end;
                    // combine surrogate pair if both halves are present
                    if (cp >= 0xD800 and cp <= 0xDBFF and
                        end + 1 < raw.len and raw[end] == '\\' and raw[end + 1] == 'u')
                    {
                        if (parseUnicodeEscape(raw, end + 2)) |r2| {
                            if (r2.value >= 0xDC00 and r2.value <= 0xDFFF) {
                                // decode surrogate pair: U+10000 + (high - 0xD800) * 0x400 + (low - 0xDC00)
                                cp = 0x10000 + (@as(u21, cp) - 0xD800) * 0x400 + (@as(u21, r2.value) - 0xDC00);
                                end = r2.end;
                            }
                        }
                    }
                    try appendCodePoint(out, alloc, cp);
                    i = end;
                } else {
                    try out.append(alloc, 'u');
                    i += 1;
                }
            },
            '\n' => i += 1, // line continuation \<LF>
            '\r' => {
                i += 1;
                if (i < raw.len and raw[i] == '\n') i += 1; // line continuation \<CR><LF>
            },
            else => {
                const us_len = unicodeSeparatorLen(raw, i);
                if (us_len > 0) {
                    i += us_len; // line continuation \<LS> or \<PS>
                } else {
                    try out.append(alloc, raw[i]); // unknown escape: pass through as-is
                    i += 1;
                }
            },
        }
    }
}

/// writes a Unicode code point as UTF-8. lone surrogates use WTF-8 encoding.
fn appendCodePoint(out: *std.ArrayList(u8), alloc: std.mem.Allocator, cp: u21) error{OutOfMemory}!void {
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

/// checks whether a string contains a legacy octal escape
/// (\0n, \1-\7) or non-octal decimal escape (\8, \9).
pub fn hasOctalEscape(str: []const u8) bool {
    var pos: usize = 0;
    while (std.mem.findScalarPos(u8, str, pos, '\\')) |i| {
        const next = i + 1;
        if (next >= str.len) break;
        switch (str[next]) {
            '1'...'9' => return true,
            '0' => if (next + 1 < str.len and str[next + 1] >= '0' and str[next + 1] <= '9') return true,
            else => {},
        }
        pos = next + 1;
    }
    return false;
}

inline fn hexVal(c: u8) ?u8 {
    return if (c >= '0' and c <= '9') c - '0' else if (c >= 'a' and c <= 'f') c - 'a' + 10 else if (c >= 'A' and c <= 'F') c - 'A' + 10 else null;
}

const testing = std.testing;

test "codePointAt ascii" {
    const s = "hello";
    const cp = try codePointAt(s, 0);
    try testing.expectEqual(@as(u3, 1), cp.len);
    try testing.expectEqual(@as(u21, 'h'), cp.value);
}

test "codePointAt 2 byte utf8" {
    const s = "ñ";
    const cp = try codePointAt(s, 0);
    try testing.expectEqual(@as(u3, 2), cp.len);
    try testing.expectEqual(@as(u21, 0x00F1), cp.value);
}

test "codePointAt 3 byte utf8" {
    const s = "€";
    const cp = try codePointAt(s, 0);
    try testing.expectEqual(@as(u3, 3), cp.len);
    try testing.expectEqual(@as(u21, 0x20AC), cp.value);
}

test "codePointAt 4 byte utf8" {
    const s = "𐍈";
    const cp = try codePointAt(s, 0);
    try testing.expectEqual(@as(u3, 4), cp.len);
    try testing.expectEqual(@as(u21, 0x10348), cp.value);
}

test "codePointAt invalid leading byte" {
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

test "unicodeSeparatorLen line separator u+2028" {
    const s = &[_]u8{ 0xE2, 0x80, 0xA8 };
    try testing.expectEqual(@as(u8, 3), unicodeSeparatorLen(s, 0));
}

test "unicodeSeparatorLen paragraph separator u+2029" {
    const s = &[_]u8{ 0xE2, 0x80, 0xA9 };
    try testing.expectEqual(@as(u8, 3), unicodeSeparatorLen(s, 0));
}

test "unicodeSeparatorLen not a separator" {
    const s = &[_]u8{ 0xE2, 0x80, 0xAA };
    try testing.expectEqual(@as(u8, 0), unicodeSeparatorLen(s, 0));
}

test "unicodeSeparatorLen too short" {
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

test "parseOctal single digit" {
    const r = parseOctal("5xyz", 0);
    try testing.expectEqual(@as(u21, 5), r.value);
    try testing.expectEqual(@as(usize, 1), r.end);
}

test "parseOctal three digits starting with 0 to 3" {
    const r = parseOctal("377", 0);
    try testing.expectEqual(@as(u21, 0o377), r.value);
    try testing.expectEqual(@as(usize, 3), r.end);
}

test "parseOctal two digits starting with 4 to 7" {
    const r = parseOctal("77", 0);
    try testing.expectEqual(@as(u21, 0o77), r.value);
    try testing.expectEqual(@as(usize, 2), r.end);
}

test "parseOctal stops at non octal" {
    const r = parseOctal("129", 0);
    try testing.expectEqual(@as(u21, 0o12), r.value);
    try testing.expectEqual(@as(usize, 2), r.end);
}

test "parseHex2 valid" {
    const r = parseHex2("FF", 0).?;
    try testing.expectEqual(@as(u21, 0xFF), r.value);
    try testing.expectEqual(@as(usize, 2), r.end);
}

test "parseHex2 lowercase" {
    const r = parseHex2("ab", 0).?;
    try testing.expectEqual(@as(u21, 0xAB), r.value);
}

test "parseHex2 too short" {
    try testing.expect(parseHex2("F", 0) == null);
}

test "parseHex2 invalid hex char" {
    try testing.expect(parseHex2("GG", 0) == null);
}

test "parseHex4 valid" {
    const r = parseHex4("00E9rest", 0).?;
    try testing.expectEqual(@as(u21, 0x00E9), r.value);
    try testing.expectEqual(@as(usize, 4), r.end);
}

test "parseHex4 too short" {
    try testing.expect(parseHex4("00E", 0) == null);
}

test "parseHexVariable valid" {
    const r = parseHexVariable("1F600", 0, 6).?;
    try testing.expectEqual(@as(u21, 0x1F600), r.value);
    try testing.expectEqual(@as(usize, 5), r.end);
}

test "parseHexVariable exceeds max codepoint" {
    try testing.expect(parseHexVariable("FFFFFF", 0, 6) == null);
}

test "parseHexVariable no digits" {
    try testing.expect(parseHexVariable("xyz", 0, 6) == null);
}

test "parseHexVariable respects max digits" {
    const r = parseHexVariable("ABCDEF", 0, 2).?;
    try testing.expectEqual(@as(u21, 0xAB), r.value);
    try testing.expectEqual(@as(usize, 2), r.end);
}

test "parseUnicodeEscape 4 digit form" {
    const r = parseUnicodeEscape("00E9rest", 0).?;
    try testing.expectEqual(@as(u21, 0x00E9), r.value);
    try testing.expectEqual(@as(usize, 4), r.end);
}

test "parseUnicodeEscape braced form" {
    const r = parseUnicodeEscape("{1F600}!", 0).?;
    try testing.expectEqual(@as(u21, 0x1F600), r.value);
    try testing.expectEqual(@as(usize, 7), r.end);
}

test "parseUnicodeEscape braced form single digit" {
    const r = parseUnicodeEscape("{A}z", 0).?;
    try testing.expectEqual(@as(u21, 0xA), r.value);
    try testing.expectEqual(@as(usize, 3), r.end);
}

test "parseUnicodeEscape missing closing brace" {
    try testing.expect(parseUnicodeEscape("{1F600", 0) == null);
}

test "parseUnicodeEscape invalid hex in 4 digit" {
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

// decodeStringEscapes tests

fn decode(input: []const u8) ![]const u8 {
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(testing.allocator);
    try decodeStringEscapes(input, &out, testing.allocator);
    return try testing.allocator.dupe(u8, out.items);
}

fn expectDecode(input: []const u8, expected: []const u8) !void {
    const result = try decode(input);
    defer testing.allocator.free(result);
    try testing.expectEqualSlices(u8, expected, result);
}

test "decodeStringEscapes no escapes" {
    try expectDecode("hello world", "hello world");
}

test "decodeStringEscapes empty" {
    try expectDecode("", "");
}

test "decodeStringEscapes standard escapes" {
    try expectDecode("a\\nb\\tc", "a\nb\tc");
    try expectDecode("\\r\\n", "\r\n");
    try expectDecode("\\b\\f\\v", &[_]u8{ 0x08, 0x0C, 0x0B });
}

test "decodeStringEscapes backslash and quote escapes" {
    try expectDecode("\\\\", "\\");
    try expectDecode("\\'", "'");
    try expectDecode("\\\"", "\"");
}

test "decodeStringEscapes null escape" {
    try expectDecode("a\\0b", "a\x00b");
}

test "decodeStringEscapes octal escapes" {
    // \0 followed by digit is octal
    try expectDecode("\\01", &[_]u8{0o1});
    // \1-\7 are octal
    try expectDecode("\\101", "A"); // 0o101 = 65 = 'A'
    try expectDecode("\\7", &[_]u8{7});
    // multi-digit octal
    try expectDecode("\\377", &[_]u8{ 0xC3, 0xBF }); // 0o377 = 255 = U+00FF -> 2-byte UTF-8
}

test "decodeStringEscapes hex escapes" {
    try expectDecode("\\x41", "A");
    try expectDecode("\\x61\\x62\\x63", "abc");
    try expectDecode("\\xff", &[_]u8{ 0xC3, 0xBF }); // U+00FF -> 2-byte UTF-8
}

test "decodeStringEscapes unicode escapes" {
    try expectDecode("\\u0041", "A");
    try expectDecode("\\u00E9", "\xC3\xA9"); // e with acute
    try expectDecode("\\u4E16\\u754C", "\xE4\xB8\x96\xE7\x95\x8C"); // 世界
}

test "decodeStringEscapes braced unicode escapes" {
    try expectDecode("\\u{41}", "A");
    try expectDecode("\\u{1F600}", "\xF0\x9F\x98\x80"); // grinning face emoji
    try expectDecode("\\u{0}", &[_]u8{0});
}

test "decodeStringEscapes surrogate pairs" {
    // \uD83D\uDCA9 = U+1F4A9 (pile of poo)
    try expectDecode("\\uD83D\\uDCA9", "\xF0\x9F\x92\xA9");
}

test "decodeStringEscapes lone high surrogate" {
    const result = try decode("\\uD800");
    defer testing.allocator.free(result);
    // WTF-8: ED A0 80
    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqual(@as(u8, 0xED), result[0]);
    try testing.expectEqual(@as(u8, 0xA0), result[1]);
    try testing.expectEqual(@as(u8, 0x80), result[2]);
}

test "decodeStringEscapes lone low surrogate" {
    const result = try decode("\\uDC00");
    defer testing.allocator.free(result);
    // WTF-8: ED B0 80
    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqual(@as(u8, 0xED), result[0]);
    try testing.expectEqual(@as(u8, 0xB0), result[1]);
    try testing.expectEqual(@as(u8, 0x80), result[2]);
}

test "decodeStringEscapes line continuation lf" {
    try expectDecode("hello\\\nworld", "helloworld");
}

test "decodeStringEscapes line continuation crlf" {
    try expectDecode("hello\\\r\nworld", "helloworld");
}

test "decodeStringEscapes line continuation cr" {
    try expectDecode("hello\\\rworld", "helloworld");
}

test "decodeStringEscapes raw cr normalization" {
    // standalone \r -> \n
    try expectDecode("a\rb", "a\nb");
    // \r\n -> \n
    try expectDecode("a\r\nb", "a\nb");
    // mixed
    try expectDecode("\r\n\r", "\n\n");
}

test "decodeStringEscapes mixed escapes" {
    try expectDecode("hello\\nworld\\t\\u0021", "hello\nworld\t!");
    try expectDecode("\\x48\\x65\\x6C\\x6C\\x6F", "Hello");
}

test "decodeStringEscapes trailing backslash" {
    try expectDecode("abc\\", "abc");
}
