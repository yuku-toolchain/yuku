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

/// https://tc39.es/ecma262/#sec-static-semantics-containsexpression
///
/// The first expression contained in the parameter list, a default
/// initializer or a computed property key. Plain destructuring without
/// either contains no expression.
pub fn findParameterExpression(tree: *const ast.Tree, params: ast.FormalParameters) ?ast.NodeIndex {
    for (tree.extra(params.items)) |param_idx| {
        const pattern = switch (tree.data(param_idx)) {
            .formal_parameter => |fp| fp.pattern,
            .ts_parameter_property => |pp| pp.parameter,
            else => param_idx,
        };
        if (findPatternExpression(tree, pattern)) |found| return found;
    }
    if (params.rest != .null) return findPatternExpression(tree, params.rest);
    return null;
}

/// https://tc39.es/ecma262/#sec-static-semantics-containsexpression
///
/// The first expression contained in one binding pattern, see
/// `findParameterExpression`. Safe to call with `.null`.
pub fn findPatternExpression(tree: *const ast.Tree, pattern: ast.NodeIndex) ?ast.NodeIndex {
    if (pattern == .null) return null;
    switch (tree.data(pattern)) {
        // the initializer is the contained expression
        .assignment_pattern => return pattern,
        .binding_rest_element => |rest| return findPatternExpression(tree, rest.argument),
        .array_pattern => |arr| {
            for (tree.extra(arr.elements)) |element| {
                if (findPatternExpression(tree, element)) |found| return found;
            }
            return findPatternExpression(tree, arr.rest);
        },
        .object_pattern => |obj| {
            for (tree.extra(obj.properties)) |prop_idx| {
                switch (tree.data(prop_idx)) {
                    .binding_property => |prop| {
                        if (prop.computed) return prop.key;
                        if (findPatternExpression(tree, prop.value)) |found| return found;
                    },
                    else => {},
                }
            }
            // object rest is never inspected, BindingRestProperty is
            // always `...BindingIdentifier`, so it cannot contain an
            // expression (15.1.2 ObjectBindingPattern productions)
            return null;
        },
        else => return null,
    }
}

/// https://tc39.es/ecma262/#sec-static-semantics-issimpleparameterlist
pub fn findNonSimpleParameter(tree: *const ast.Tree, params: ast.FormalParameters) ?ast.NodeIndex {
    if (params.rest != .null) return params.rest;

    for (tree.extra(params.items)) |param_idx| {
        const pattern = switch (tree.data(param_idx)) {
            .formal_parameter => |fp| fp.pattern,
            .ts_parameter_property => |pp| pp.parameter,
            else => return param_idx,
        };
        switch (tree.data(pattern)) {
            // `this: T` is not a runtime parameter, erased at emit, so the
            // ECMAScript "simple parameter list" rule does not apply to it.
            .binding_identifier, .ts_this_parameter => {},
            else => return pattern,
        }
    }

    return null;
}
