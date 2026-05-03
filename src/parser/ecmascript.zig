const std = @import("std");
const ast = @import("ast.zig");

pub const PropName = struct {
    name: []const u8,
    span: ast.Span,
    is_string_literal: bool,

    pub fn eql(self: PropName, expected: []const u8) bool {
        return std.mem.eql(u8, self.name, expected);
    }
};

/// https://tc39.es/ecma262/#sec-static-semantics-propname
pub fn propName(tree: *const ast.Tree, key: ast.NodeIndex) ?PropName {
    switch (tree.data(key)) {
        .identifier_name => |id| return .{
            .name = tree.string(id.name),
            .span = tree.span(key),
            .is_string_literal = false,
        },
        .string_literal => |str| {
            const name = tree.string(str.value);
            if (name.len == 0) return null;
            return .{
                .name = name,
                .span = tree.span(key),
                .is_string_literal = true,
            };
        },
        else => return null,
    }
}

/// https://tc39.es/ecma262/#sec-static-semantics-issimpleparameterlist
pub fn findNonSimpleParameter(tree: *const ast.Tree, params: ast.FormalParameters) ?ast.NodeIndex {
    if (params.rest != .null) return params.rest;

    for (tree.extra(params.items)) |param_idx| {
        const pattern = tree.data(param_idx).formal_parameter.pattern;
        switch (tree.data(pattern)) {
            // `this: T` is not a runtime parameter, erased at emit, so the
            // ECMAScript "simple parameter list" rule does not apply to it.
            .binding_identifier, .ts_this_parameter => {},
            else => return pattern,
        }
    }

    return null;
}
