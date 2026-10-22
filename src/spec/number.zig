//! https://tc39.es/ecma262/#sec-numeric-types-number-tostring

const std = @import("std");

const string_bytes_max = 25;
const digits_max = 17;

/// Whether `text` is exactly what ECMAScript's `Number::toString` writes for a finite number.
pub fn isCanonicalString(text: []const u8) bool {
    if (text.len == 0) return false;
    if (text.len > string_bytes_max) return false;
    if (text[0] != '-' and !std.ascii.isDigit(text[0])) return false;
    const value = std.fmt.parseFloat(f64, text) catch return false;
    if (!std.math.isFinite(value)) return false;
    var buffer: [string_bytes_max]u8 = undefined;
    return std.mem.eql(u8, toString(value, &buffer), text);
}

fn toString(value: f64, buffer: *[string_bytes_max]u8) []const u8 {
    if (std.math.isNan(value)) return "NaN";
    if (std.math.isInf(value)) return if (value > 0) "Infinity" else "-Infinity";
    if (value == 0) return "0";

    var scientific_buffer: [std.fmt.float.min_buffer_size]u8 = undefined;
    const scientific = std.fmt.float.render(&scientific_buffer, @abs(value), .{
        .mode = .scientific,
        .precision = null,
    }) catch unreachable;

    var digits: [digits_max]u8 = undefined;
    var digits_len: u32 = 0;
    var index: u32 = 0;
    while (scientific[index] != 'e') : (index += 1) {
        if (scientific[index] == '.') continue;
        digits[digits_len] = scientific[index];
        digits_len += 1;
    }
    std.debug.assert(digits_len > 0);
    std.debug.assert(digits[0] != '0');
    const exponent = std.fmt.parseInt(i32, scientific[index + 1 ..], 10) catch unreachable;

    var length: u32 = 0;
    if (value < 0) {
        buffer[0] = '-';
        length = 1;
    }
    const k: i32 = @intCast(digits_len);
    const n = exponent + 1;
    if (k <= n and n <= 21) {
        length += write(buffer, length, digits[0..digits_len]);
        length += fill(buffer, length, @intCast(n - k));
    } else if (0 < n and n <= 21) {
        const whole: u32 = @intCast(n);
        length += write(buffer, length, digits[0..whole]);
        length += write(buffer, length, ".");
        length += write(buffer, length, digits[whole..digits_len]);
    } else if (-6 < n and n <= 0) {
        length += write(buffer, length, "0.");
        length += fill(buffer, length, @intCast(-n));
        length += write(buffer, length, digits[0..digits_len]);
    } else {
        length += write(buffer, length, digits[0..1]);
        if (digits_len > 1) {
            length += write(buffer, length, ".");
            length += write(buffer, length, digits[1..digits_len]);
        }
        length += write(buffer, length, if (n - 1 < 0) "e-" else "e+");
        const written = std.fmt.bufPrint(buffer[length..], "{d}", .{@abs(n - 1)}) catch unreachable;
        length += @intCast(written.len);
    }
    std.debug.assert(length <= string_bytes_max);
    return buffer[0..length];
}

fn write(buffer: *[string_bytes_max]u8, at: u32, bytes: []const u8) u32 {
    std.debug.assert(at + bytes.len <= string_bytes_max);
    @memcpy(buffer[at..][0..bytes.len], bytes);
    return @intCast(bytes.len);
}

fn fill(buffer: *[string_bytes_max]u8, at: u32, zeros: u32) u32 {
    std.debug.assert(at + zeros <= string_bytes_max);
    @memset(buffer[at..][0..zeros], '0');
    return zeros;
}

test toString {
    const cases = [_]struct { f64, []const u8 }{
        .{ 0, "0" },
        .{ -0.0, "0" },
        .{ 1, "1" },
        .{ -1, "-1" },
        .{ 100, "100" },
        .{ 0.5, "0.5" },
        .{ 123.456, "123.456" },
        .{ @as(f64, 0.1) + @as(f64, 0.2), "0.30000000000000004" },
        .{ 0.000001, "0.000001" },
        .{ -0.000001234567890123456, "-0.000001234567890123456" },
        .{ 0.0000001, "1e-7" },
        .{ 1.5e-7, "1.5e-7" },
        .{ 1.2345678901234568e20, "123456789012345680000" },
        .{ 1e21, "1e+21" },
        .{ 1.5e300, "1.5e+300" },
        .{ 5e-324, "5e-324" },
        .{ -1.2345678901234567e-308, "-1.2345678901234567e-308" },
        .{ 1.7976931348623157e308, "1.7976931348623157e+308" },
        .{ std.math.inf(f64), "Infinity" },
        .{ -std.math.inf(f64), "-Infinity" },
        .{ std.math.nan(f64), "NaN" },
    };
    for (cases) |case| {
        var buffer: [string_bytes_max]u8 = undefined;
        try std.testing.expectEqualStrings(case[1], toString(case[0], &buffer));
    }
}

test isCanonicalString {
    for ([_][]const u8{ "0", "1", "-1", "0.5", "16", "1e+21", "1e-7", "5e-324" }) |text| {
        try std.testing.expect(isCanonicalString(text));
    }
    const not_canonical = [_][]const u8{
        "",    "-0",  "01",   "1.0",      "+1",  ".5",  "1e21",   "0x10",
        "1_0", " 1",  "1 ",   "Infinity", "NaN", "inf", "1e+999", "a",
        "1e1", "1E1", "0.50", "00",
    };
    for (not_canonical) |text| {
        try std.testing.expect(!isCanonicalString(text));
    }
}
