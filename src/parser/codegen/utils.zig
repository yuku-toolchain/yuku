const std = @import("std");
const lexical = @import("../lexical.zig");

pub inline fn isWordOp(op: []const u8) bool {
    return std.ascii.isLower(op[0]);
}

pub inline fn isIdCont(c: u8) bool {
    return c >= 0x80 or lexical.isIdentifierChar(c);
}

pub fn scriptEscape(s: []const u8, i: usize) ?[]const u8 {
    return switch (s[i]) {
        '<' => if (scriptOpenAt(s, i)) "<\\" else null,
        '>' => if (i >= 2 and s[i - 1] == '-' and s[i - 2] == '-') "\\>" else null,
        else => null,
    };
}

fn scriptOpenAt(s: []const u8, i: usize) bool {
    const rest = s[i + 1 ..];
    if (std.mem.startsWith(u8, rest, "!--")) return true;
    if (rest.len < 7 or rest[0] != '/' or !std.ascii.eqlIgnoreCase(rest[1..7], "script"))
        return false;
    // `</script` ends the tag only before whitespace, `/`, `>`, or the end
    return rest.len == 7 or std.ascii.isWhitespace(rest[7]) or rest[7] == '/' or rest[7] == '>';
}

pub fn stripUnderscores(raw: []const u8, buf: []u8) ?[]const u8 {
    if (std.mem.findScalar(u8, raw, '_') == null) return raw;
    if (raw.len > buf.len) return null;
    return buf[0 .. raw.len - std.mem.replace(u8, raw, "_", "", buf)];
}

pub fn isMinimalInteger(raw: []const u8) bool {
    if (raw.len == 0 or raw[raw.len - 1] == '0') return false;
    for (raw) |c| if (!std.ascii.isDigit(c)) return false;
    return true;
}

pub fn shortestDecimal(s: []const u8, scratch: []u8) []const u8 {
    const exp_at = std.mem.findAny(u8, s, "eE") orelse s.len;
    const mantissa = s[0..exp_at];
    const int_part, const frac_part = std.mem.cutScalar(u8, mantissa, '.') orelse .{ mantissa, "" };

    var exp: i64 = 0;
    if (exp_at < s.len) {
        exp = std.fmt.parseInt(i64, s[exp_at + 1 ..], 10) catch return s;
        // past the bound the value is 0 or Infinity
        if (exp > 100_000 or exp < -100_000) return s;
    }
    exp -= @intCast(frac_part.len);

    var dig: [128]u8 = undefined;
    const all = std.mem.print(&dig, "{s}{s}", .{ int_part, frac_part }) catch return s;
    const significant = std.mem.trimStart(u8, all, "0");
    if (significant.len == 0) return "0";
    const d = std.mem.trimEnd(u8, significant, "0");
    exp += @intCast(significant.len - d.len);

    const fixed = writeFixed(scratch, d, exp);
    const exp_len = d.len + 1 + std.fmt.count("{d}", .{exp});
    const out = if (fixed == null or exp_len < fixed.?.len)
        (std.mem.print(scratch, "{s}e{d}", .{ d, exp }) catch return s)
    else
        fixed.?;
    return if (out.len <= s.len) out else s;
}

fn writeFixed(scratch: []u8, d: []const u8, exp: i64) ?[]const u8 {
    const shift: usize = @intCast(@abs(exp));
    return (if (exp >= 0)
        std.mem.print(scratch, "{[d]s:0<[width]}", .{ .d = d, .width = d.len + shift })
    else if (shift < d.len)
        std.mem.print(scratch, "{s}.{s}", .{ d[0 .. d.len - shift], d[d.len - shift ..] })
    else
        std.mem.print(scratch, ".{[d]s:0>[width]}", .{ .d = d, .width = shift })) catch null;
}

pub fn isJsdocBody(value: []const u8) bool {
    var it = std.mem.splitScalar(u8, value, '\n');
    _ = it.next();
    var multi = false;
    while (it.next()) |line| {
        multi = true;
        const t = std.mem.trimStart(u8, line, " \t");
        if (t.len != 0 and t[0] != '*') return false;
    }
    return multi;
}

pub fn isSignificantBlockComment(value: []const u8) bool {
    if (value.len == 0) return false;
    if (value[0] == '!' or value[0] == '*') return true;
    // `/* @__PURE__ */` may be spaced, `/*!` and `/**` never are
    const body = std.mem.trimStart(u8, value, &std.ascii.whitespace);
    if (body.len > 0 and (body[0] == '@' or body[0] == '#')) return true;
    return std.mem.find(u8, value, "@license") != null or
        std.mem.find(u8, value, "@preserve") != null or
        std.mem.find(u8, value, "@cc_on") != null;
}
