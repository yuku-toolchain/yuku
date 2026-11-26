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
pub fn containsExpression(tree: *const ast.Tree, pattern: ast.NodeIndex) bool {
    if (pattern == .null) return false;
    switch (tree.data(pattern)) {
        .assignment_pattern => return true,
        .formal_parameter => |parameter| return containsExpression(tree, parameter.pattern),
        .ts_parameter_property => |property| return containsExpression(tree, property.parameter),
        .binding_rest_element => |rest| return containsExpression(tree, rest.argument),
        inline .formal_parameters, .array_pattern => |list, tag| {
            const items = if (tag == .array_pattern) list.elements else list.items;
            for (tree.extra(items)) |item| {
                if (containsExpression(tree, item)) return true;
            }
            return containsExpression(tree, list.rest);
        },
        .object_pattern => |object| {
            for (tree.extra(object.properties)) |property| {
                if (containsExpression(tree, property)) return true;
            }
            // a BindingRestProperty is always `...BindingIdentifier`
            return false;
        },
        .binding_property => |property| return property.computed or
            containsExpression(tree, property.value),
        else => return false,
    }
}

pub fn stringLiteralLike(tree: *const ast.Tree, node: ast.NodeIndex) ?ast.String {
    std.debug.assert(node != .null);
    switch (tree.data(node)) {
        .string_literal => |literal| return literal.value,
        .template_literal => |template| {
            if (template.expressions.len > 0 or template.quasis.len != 1) return null;
            const quasi = tree.data(tree.extra(template.quasis)[0]).template_element;
            if (quasi.is_cooked_undefined) return null;
            return quasi.cooked;
        },
        else => return null,
    }
}

pub fn unwrapParens(tree: *const ast.Tree, node: ast.NodeIndex) ast.NodeIndex {
    std.debug.assert(node != .null);
    var current = node;
    while (true) {
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
    return is_for_in and kind == .@"var" and !tree.isTs() and !tree.isModule() and
        tree.data(declarator_id) == .binding_identifier;
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
