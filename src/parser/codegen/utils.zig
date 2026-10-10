const std = @import("std");
const util = @import("util");

pub fn isWordOp(op: []const u8) bool {
    return std.mem.eql(u8, op, "in") or std.mem.eql(u8, op, "instanceof") or
        std.mem.eql(u8, op, "typeof") or std.mem.eql(u8, op, "void") or
        std.mem.eql(u8, op, "delete");
}

/// True when `c` can continue an ASCII identifier or is a non-ASCII byte.
pub inline fn isIdCont(c: u8) bool {
    return c == '$' or c >= 0x80 or util.UnicodeId.canContinueId(c);
}

pub fn isIdentifierName(s: []const u8) bool {
    if (s.len == 0) return false;
    var i: usize = 0;
    while (i < s.len) {
        const cp = util.Utf.codePointAt(s, i) catch return false;
        const ok = if (cp.value == '$' or cp.value == '_')
            true
        else if (i == 0)
            util.UnicodeId.canStartId(cp.value)
        else
            util.UnicodeId.canContinueId(cp.value);
        if (!ok) return false;
        i += cp.len;
    }
    return true;
}

/// The escape that keeps `s[i]` from ending an inline `<script>`, a backslash the value
/// ignores before `</script`, `<!--`, or `-->`.
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
    var len: usize = 0;
    for (raw) |c| if (c != '_') {
        buf[len] = c;
        len += 1;
    };
    return buf[0..len];
}

/// Digits without a trailing zero, already the shortest spelling.
pub fn isMinimalInteger(raw: []const u8) bool {
    std.debug.assert(raw.len > 0);
    if (raw[raw.len - 1] == '0') return false;
    for (raw) |c| if (!std.ascii.isDigit(c)) return false;
    return true;
}

/// The shortest equal spelling of decimal `s`, rewritten as text with no float round-trip.
/// `scratch` and `s` must not alias.
pub fn shortestDecimal(s: []const u8, scratch: []u8) []const u8 {
    const exp_at = std.mem.findAny(u8, s, "eE") orelse s.len;
    const mantissa = s[0..exp_at];
    const dot_at = std.mem.findScalar(u8, mantissa, '.') orelse mantissa.len;
    const int_part = mantissa[0..dot_at];
    const frac_part = if (dot_at < mantissa.len) mantissa[dot_at + 1 ..] else "";

    // value is <int_part><frac_part> * 10^(exp - frac_len), past the bound it is 0 or Infinity
    var exp: i64 = blk: {
        if (exp_at == s.len) break :blk 0;
        const e = std.fmt.parseInt(i64, s[exp_at + 1 ..], 10) catch return s;
        if (e > 100_000 or e < -100_000) return s;
        break :blk e;
    };
    exp -= @intCast(frac_part.len);

    var dig: [128]u8 = undefined;
    if (int_part.len + frac_part.len > dig.len) return s;
    @memcpy(dig[0..int_part.len], int_part);
    @memcpy(dig[int_part.len..][0..frac_part.len], frac_part);
    var d = dig[0 .. int_part.len + frac_part.len];

    var lead: usize = 0;
    while (lead < d.len and d[lead] == '0') lead += 1;
    if (lead == d.len) return "0";
    d = d[lead..];
    while (d[d.len - 1] == '0') {
        d = d[0 .. d.len - 1];
        exp += 1;
    }

    if (exp == 0) {
        @memcpy(scratch[0..d.len], d);
        return if (d.len <= s.len) scratch[0..d.len] else s;
    }

    const exp_len = d.len + 1 + std.fmt.count("{d}", .{exp});
    const out = if (exp_len < fixedLen(d.len, exp, scratch.len))
        (std.mem.print(scratch, "{s}e{d}", .{ d, exp }) catch return s)
    else
        writeFixed(scratch, d, exp) orelse return s;
    return if (out.len <= s.len) out else s;
}

// maxInt when the fixed form would exceed cap
fn fixedLen(dlen: usize, exp: i64, cap: usize) usize {
    if (exp > 0) {
        const e: usize = @intCast(exp);
        return if (e <= cap) dlen + e else std.math.maxInt(usize);
    }
    const f: usize = @intCast(-exp);
    if (f < dlen) return dlen + 1;
    return if (1 + f <= cap) 1 + f else std.math.maxInt(usize);
}

fn writeFixed(scratch: []u8, d: []const u8, exp: i64) ?[]const u8 {
    if (exp > 0) {
        const e: usize = @intCast(exp);
        if (d.len + e > scratch.len) return null;
        @memcpy(scratch[0..d.len], d);
        @memset(scratch[d.len..][0..e], '0');
        return scratch[0 .. d.len + e];
    }
    const f: usize = @intCast(-exp);
    if (f < d.len) {
        const head = d.len - f;
        if (d.len + 1 > scratch.len) return null;
        @memcpy(scratch[0..head], d[0..head]);
        scratch[head] = '.';
        @memcpy(scratch[head + 1 ..][0..f], d[head..]);
        return scratch[0 .. d.len + 1];
    }
    const zeros = f - d.len;
    if (1 + f > scratch.len) return null;
    scratch[0] = '.';
    @memset(scratch[1..][0..zeros], '0');
    @memcpy(scratch[1 + zeros ..][0..d.len], d);
    return scratch[0 .. 1 + f];
}

/// True when the value spans lines and every line after the first is blank or starts with `*`.
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

/// True when a block comment is a legal header or annotation.
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
