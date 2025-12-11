const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;
const ast = @import("ast.zig");

const object = @import("syntax/object.zig");
const expressions = @import("syntax/expressions.zig");
const array = @import("syntax/array.zig");

/// parse an element within a cover grammar context (used for nested arrays/objects).
/// without validation. validation is deferred until the top-level context is known:
/// - if parent becomes an expression -> validate at top level
/// - if parent becomes a pattern -> no validation needed
pub fn parseCoverElement(parser: *Parser) Error!?ast.NodeIndex {
    return expressions.parseExpression(parser, 2, .{ .enable_validation = false });
}

/// validate that an expression doesn't contain CoverInitializedName.
pub fn validateNoInvalidCoverSyntax(parser: *Parser, expr: ast.NodeIndex) Error!bool {
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

                        if (!try validateNoInvalidCoverSyntax(parser, obj_prop.value)) {
                            return false;
                        }
                    },
                    .spread_element => |spread| {
                        if (!try validateNoInvalidCoverSyntax(parser, spread.argument)) {
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
                if (!try validateNoInvalidCoverSyntax(parser, elem)) {
                    return false;
                }
            }
        },
        .spread_element => |spread| {
            return validateNoInvalidCoverSyntax(parser, spread.argument);
        },
        .parenthesized_expression => |paren| {
            return validateNoInvalidCoverSyntax(parser, paren.expression);
        },
        .sequence_expression => |seq| {
            for (parser.getExtra(seq.expressions)) |e| {
                if (!try validateNoInvalidCoverSyntax(parser, e)) return false;
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

/// Convert an expression node to a binding pattern.
/// - IdentifierReference -> BindingIdentifier
/// - ArrayExpression -> ArrayPattern
/// - ObjectExpression -> ObjectPattern
/// - AssignmentExpression -> AssignmentPattern
pub fn expressionToPattern(parser: *Parser, expr: ast.NodeIndex) Error!?ast.NodeIndex {
    const data = parser.getData(expr);

    switch (data) {
        .identifier_reference => |id| {
            parser.setData(expr, .{ .binding_identifier = .{
                .name_start = id.name_start,
                .name_len = id.name_len,
            } });
            return expr;
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

            const left_pattern = try expressionToPattern(parser, assign.left) orelse return null;

            parser.setData(expr, .{ .assignment_pattern = .{
                .left = left_pattern,
                .right = assign.right,
            } });
            return expr;
        },

        .array_expression => |arr| {
            return array.toArrayPattern(parser, expr, arr.elements);
        },

        .object_expression => |obj| {
            return object.toObjectPattern(parser, expr, obj.properties);
        },

        .member_expression => |member| {
            if (member.optional) {
                try parser.report(
                    parser.getSpan(expr),
                    "Optional chaining is not allowed in destructuring pattern",
                    .{ .help = "Optional chaining ('?.') cannot be used as an assignment target in destructuring patterns." },
                );
                return null;
            }

            return expr;
        },

        .parenthesized_expression => |paren| {
            if (!expressions.isSimpleAssignmentTarget(parser, paren.expression)) {
                try parser.report(
                    parser.getSpan(paren.expression),
                    "Parenthesized expression in destructuring pattern must be a simple assignment target",
                    .{ .help = "Only identifiers or member expressions (without optional chaining) are allowed inside parentheses in destructuring patterns." },
                );
                return null;
            }

            const inner_pattern = try expressionToPattern(parser, paren.expression) orelse return null;

            const inner_pattern_data = parser.getData(inner_pattern);
            const inner_pattern_span = parser.getSpan(inner_pattern);

            parser.setData(expr, inner_pattern_data);
            parser.setSpan(expr, inner_pattern_span);

            return inner_pattern;
        },

        .binding_identifier, .array_pattern, .object_pattern, .assignment_pattern => {
            return expr;
        },

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
