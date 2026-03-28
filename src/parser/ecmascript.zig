const std = @import("std");
const ast = @import("ast.zig");
const Parser = @import("parser.zig").Parser;
const utf = @import("util").Utf;

pub const PropName = struct {
    name: []const u8,
    span: ast.Span,
    is_string_literal: bool,

    /// compares the StringValue against an expected name.
    /// uses escape-resolving comparison for string literal keys.
    pub fn eql(self: PropName, expected: []const u8) bool {
        if (self.is_string_literal) return eqlStringValue(self.name, expected);
        return std.mem.eql(u8, self.name, expected);
    }
};

/// https://tc39.es/ecma262/#sec-static-semantics-propname
pub fn propName(parser: *const Parser, key: ast.NodeIndex) ?PropName {
    switch (parser.b.getData(key)) {
        .identifier_name => |id| return .{
            .name = parser.b.getString(id.name),
            .span = parser.b.getSpan(key),
            .is_string_literal = false,
        },
        .string_literal => |str| {
            const name = str.value(&parser.b);
            if (name.len == 0) return null;
            return .{
                .name = name,
                .span = parser.b.getSpan(key),
                .is_string_literal = true,
            };
        },
        else => return null,
    }
}

/// https://tc39.es/ecma262/#sec-static-semantics-stringvalue
pub fn eqlStringValue(source: []const u8, expected: []const u8) bool {
    if (std.mem.findScalar(u8, source, '\\') == null) {
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

/// https://tc39.es/ecma262/#sec-static-semantics-issimpleparameterlist
pub fn findNonSimpleParameter(tree: *const ast.Tree, params: ast.FormalParameters) ?ast.NodeIndex {
    if (params.rest != .null) return params.rest;

    for (tree.getExtra(params.items)) |param_idx| {
        const pattern = tree.getData(param_idx).formal_parameter.pattern;
        switch (tree.getData(pattern)) {
            .binding_identifier => {},
            else => return pattern,
        }
    }

    return null;
}
