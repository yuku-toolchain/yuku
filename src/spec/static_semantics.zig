const std = @import("std");
const ast = @import("parser").ast;

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
    const name, const is_string_literal = switch (tree.data(key)) {
        .identifier_name => |id| .{ tree.string(id.name), false },
        .string_literal => |str| .{ tree.string(str.value), true },
        else => return null,
    };
    if (is_string_literal and name.len == 0) return null;
    return .{ .name = name, .span = tree.span(key), .is_string_literal = is_string_literal };
}

/// https://tc39.es/ecma262/#sec-static-semantics-containsexpression
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
pub fn findPatternExpression(tree: *const ast.Tree, pattern: ast.NodeIndex) ?ast.NodeIndex {
    if (pattern == .null) return null;
    switch (tree.data(pattern)) {
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
            // a BindingRestProperty is always `...BindingIdentifier`
            return null;
        },
        else => return null,
    }
}

/// The value of a string literal, or of a template literal without substitutions.
pub fn stringLiteralLike(tree: *const ast.Tree, node: ast.NodeIndex) ?ast.String {
    std.debug.assert(node != .null);
    switch (tree.data(node)) {
        .string_literal => |literal| return literal.value,
        .template_literal => |template| {
            if (template.expressions.len > 0) return null;
            std.debug.assert(template.quasis.len == 1);
            const quasi = tree.data(tree.extra(template.quasis)[0]).template_element;
            if (quasi.is_cooked_undefined) return null;
            return quasi.cooked;
        },
        else => return null,
    }
}

/// The expression inside any number of parentheses.
pub fn unwrapParens(tree: *const ast.Tree, node: ast.NodeIndex) ast.NodeIndex {
    std.debug.assert(node != .null);
    var current = node;
    // at most one paren node per syntactic level, so nesting depth bounds the chain
    var depth: u32 = 0;
    while (true) : (depth += 1) {
        std.debug.assert(depth < 1_000_000);
        switch (tree.data(current)) {
            .parenthesized_expression => |p| current = p.expression,
            else => return current,
        }
    }
}

/// https://tc39.es/ecma262/#sec-initializers-in-forin-statement-heads
pub fn isAnnexBForInHead(
    tree: *const ast.Tree,
    kind: ast.VariableKind,
    declarator_id: ast.NodeIndex,
    is_for_in: bool,
) bool {
    if (!is_for_in) return false;
    if (kind != .@"var") return false;
    if (tree.isTs()) return false;
    if (tree.isModule()) return false;
    return tree.data(declarator_id) == .binding_identifier;
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
            // `this: T` is erased at emit, so it keeps the list simple
            .binding_identifier, .ts_this_parameter => {},
            else => return pattern,
        }
    }

    return null;
}
