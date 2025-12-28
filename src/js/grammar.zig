const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;
const ast = @import("ast.zig");

const object = @import("syntax/object.zig");
const expressions = @import("syntax/expressions.zig");
const array = @import("syntax/array.zig");

/// parse an expression within a cover grammar context without validation.
/// validation is deferred until the top-level context is known.
pub inline fn parseExpressionInCover(parser: *Parser, precedence: u8) Error!?ast.NodeIndex {
    return expressions.parseExpression(parser, precedence, .{ .in_cover = true });
}

/// validate that an expression doesn't contain CoverInitializedName.
pub fn validateNoCoverInitializedSyntax(parser: *Parser, expr: ast.NodeIndex) Error!bool {
    const data = parser.getData(expr);

    switch (data) {
        .object_expression => |obj| {
            const properties = parser.getExtra(obj.properties);
            for (properties) |prop| {
                if (ast.isNull(prop)) continue;

                const prop_data = parser.getData(prop);

                switch (prop_data) {
                    .object_property => |obj_prop| {
                        if (obj_prop.shorthand and isCoverInitializedName(parser, obj_prop.value)) {
                            try reportCoverInitializedNameError(parser, prop);
                            return false;
                        }

                        if (!try validateNoCoverInitializedSyntax(parser, obj_prop.value)) {
                            return false;
                        }
                    },
                    .spread_element => |spread| {
                        if (!try validateNoCoverInitializedSyntax(parser, spread.argument)) {
                            return false;
                        }
                    },
                    else => {},
                }
            }
        },
        .array_expression => |arr| {
            const elements = parser.getExtra(arr.elements);
            for (elements) |elem| {
                if (ast.isNull(elem)) continue;
                if (!try validateNoCoverInitializedSyntax(parser, elem)) {
                    return false;
                }
            }
        },
        .object_property => |obj_prop| {
            if (obj_prop.shorthand and isCoverInitializedName(parser, obj_prop.value)) {
                try reportCoverInitializedNameError(parser, expr);
                return false;
            }
        },
        .spread_element => |spread| {
            return validateNoCoverInitializedSyntax(parser, spread.argument);
        },
        .parenthesized_expression => |paren| {
            return validateNoCoverInitializedSyntax(parser, paren.expression);
        },
        .sequence_expression => |seq| {
            for (parser.getExtra(seq.expressions)) |e| {
                if (!try validateNoCoverInitializedSyntax(parser, e)) return false;
            }
        },
        else => {},
    }

    return true;
}

/// check if a node is a CoverInitializedName (assignment expression with = operator).
/// CoverInitializedName: { a = 1 } where the value is AssignmentExpression
pub inline fn isCoverInitializedName(parser: *Parser, node: ast.NodeIndex) bool {
    const data = parser.getData(node);
    return data == .assignment_expression and data.assignment_expression.operator == .assign;
}

pub inline fn reportCoverInitializedNameError(parser: *Parser, node: ast.NodeIndex) Error!void {
    try parser.report(
        parser.getSpan(node),
        "Shorthand property cannot have a default value in object expression",
        .{ .help = "Use '{ a: a = 1 }' syntax or this is only valid in destructuring patterns." },
    );
}

pub const PatternContext = enum {
    /// binding patterns for function parameters, variable declarations, etc.
    binding,
    /// assignable patterns for assignment expressions.
    assignable,
};

/// convert an expression node to a destructuring pattern (mutates in-place).
/// the context determines what syntax is allowed.
pub fn expressionToPattern(
    parser: *Parser,
    expr: ast.NodeIndex,
    context: PatternContext,
    // if return null, there is a error reported, so caller do 'orelse return null'
) Error!?void {
    const data = parser.getData(expr);

    switch (data) {
        .identifier_reference => |id| {
            parser.setData(expr, .{ .binding_identifier = .{
                .name_start = id.name_start,
                .name_len = id.name_len,
            } });
        },

        .assignment_expression => |assign| {
            if (assign.operator != .assign) {
                try parser.report(
                    parser.getSpan(expr),
                    "Invalid assignment operator in destructuring pattern",
                    .{ .help = "Only '=' is allowed in destructuring defaults, not compound operators like '+='." },
                );
                return null;
            }

            try expressionToPattern(parser, assign.left, context) orelse return null;

            parser.setData(expr, .{ .assignment_pattern = .{
                .left = assign.left,
                .right = assign.right,
            } });
        },

        .array_expression => |arr| {
            try array.toArrayPattern(parser, expr, arr.elements, parser.getSpan(expr), context) orelse return null;
        },

        .object_expression => |obj| {
            try object.toObjectPattern(parser, expr, obj.properties, parser.getSpan(expr), context) orelse return null;
        },

        .chain_expression => {
            try parser.report(
                parser.getSpan(expr),
                "Optional chaining is not allowed in destructuring pattern",
                .{ .help = "Optional chaining ('?.') cannot be used as an assignment target in destructuring patterns." },
            );

            return null;
        },

        .member_expression => {
            if (context != .assignable) {
                try parser.report(
                    parser.getSpan(expr),
                    "Member expression is not allowed in binding pattern",
                    .{ .help = "Function parameters and variable declarations can only bind to identifiers, not member expressions like 'obj.prop' or 'obj[key]'. Use a simple identifier instead." },
                );
                return null;
            }
        },

        .parenthesized_expression => |paren| {
            if (context != .assignable) {
                try parser.report(
                    parser.getSpan(expr),
                    "Parentheses are not allowed in this binding pattern",
                    .{ .help = "Remove the extra parentheses. Binding patterns can only be identifiers, destructuring patterns, or assignment patterns, not parenthesized expressions." },
                );
                return null;
            }

            if (!expressions.isSimpleAssignmentTarget(parser, paren.expression)) {
                try parser.report(
                    parser.getSpan(paren.expression),
                    "Parenthesized expression in destructuring pattern must be a simple assignment target",
                    .{ .help = "Only identifiers or member expressions (without optional chaining) are allowed inside parentheses in destructuring patterns." },
                );
                return null;
            }

            try expressionToPattern(parser, paren.expression, context) orelse return null;

            parser.setData(expr, parser.getData(paren.expression));
            parser.setSpan(expr, parser.getSpan(paren.expression));
        },

        .binding_identifier, .array_pattern, .object_pattern, .assignment_pattern => {},

        else => {
            try parser.report(
                parser.getSpan(expr),
                "Invalid element in destructuring pattern",
                .{ .help = "Expected an identifier, array pattern, object pattern, or assignment pattern." },
            );
            return null;
        },
    }
}
