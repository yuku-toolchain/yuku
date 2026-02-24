const std = @import("std");
const ast = @import("ast.zig");
const Parser = @import("parser.zig").Parser;
const utf = @import("util").Utf;

pub const PropName = struct {
    name: []const u8,
    span: ast.Span,

    /// compare the StringValue of this PropName against an expected ASCII name.
    pub fn eql(self: PropName, expected: []const u8) bool {
        return eqlStringValue(self.name, expected);
    }
};

/// https://tc39.es/ecma262/#sec-static-semantics-propname
pub fn propName(parser: *const Parser, key: ast.NodeIndex) ?PropName {
    const key_data = parser.getData(key);
    switch (key_data) {
        .identifier_name => |id| {
            return .{
                .name = parser.getSourceText(id.name_start, id.name_len),
                .span = parser.getSpan(key),
            };
        },
        .string_literal => |str| {
            const raw = parser.getSourceText(str.raw_start, str.raw_len);
            if (raw.len < 2) return null;
            return .{
                .name = raw[1 .. raw.len - 1],
                .span = parser.getSpan(key),
            };
        },
        // currently only handles identifier_name and string_literal,
        // which is sufficient for the checks in class.zig, extend when needed.
        else => return null,
    }
}

/// https://tc39.es/ecma262/#sec-static-semantics-stringvalue
fn eqlStringValue(source: []const u8, expected: []const u8) bool {
    if (std.mem.indexOfScalar(u8, source, '\\') == null) {
        return std.mem.eql(u8, source, expected);
    }

    var si: usize = 0;
    var ei: usize = 0;

    while (si < source.len) {
        if (ei >= expected.len) return false;

        if (source[si] == '\\' and si + 1 < source.len and source[si + 1] == 'u') {
            const r = utf.parseUnicodeEscape(source, si + 2) orelse return false;
            if (r.value > 0x7F) return false; // expected is ASCII
            if (@as(u8, @intCast(r.value)) != expected[ei]) return false;
            si = r.end;
            ei += 1;
        } else {
            if (source[si] != expected[ei]) return false;
            si += 1;
            ei += 1;
        }
    }

    return ei == expected.len;
}
