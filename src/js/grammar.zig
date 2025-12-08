const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;
const ast = @import("ast.zig");

const object = @import("syntax/object.zig");
const expressions = @import("syntax/expressions.zig");
const array = @import("syntax/array.zig");

/// parse an element within a cover grammar context (used for nested arrays/objects).
///
/// keeps nested arrays/objects as "covers" by converting them to expressions
/// without validation. validation is deferred until the top-level context is known:
/// - if parent becomes an expression -> validate at top level
/// - if parent becomes a pattern -> no validation needed
pub fn parseCoverElement(parser: *Parser) Error!?ast.NodeIndex {
    return switch (parser.current_token.type) {
        .left_bracket => blk: {
            const cover = try array.parseCover(parser) orelse return null;
            break :blk array.coverToExpressionUnchecked(parser, cover);
        },
        .left_brace => blk: {
            const cover = try object.parseCover(parser) orelse return null;
            break :blk object.coverToExpressionUnchecked(parser, cover);
        },
        else => expressions.parseExpression(parser, 2),
    };
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
                        // recurse into property value
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
    const span = parser.getSpan(expr);

    switch (data) {
        // Identifier -> BindingIdentifier
        .identifier_reference => |id| {
            return try parser.addNode(
                .{ .binding_identifier = .{ .name_start = id.name_start, .name_len = id.name_len } },
                span,
            );
        },

        // AssignmentExpression -> AssignmentPattern
        .assignment_expression => |assign| {
            if (assign.operator != .assign) {
                try parser.report(
                    span,
                    "Invalid assignment operator in destructuring pattern",
                    .{ .help = "Only '=' is allowed in destructuring defaults, not compound operators like '+='." },
                );
                return null;
            }

            const left_pattern = try expressionToPattern(parser, assign.left) orelse return null;
            return try parser.addNode(
                .{ .assignment_pattern = .{ .left = left_pattern, .right = assign.right } },
                span,
            );
        },

        // ArrayExpression -> ArrayPattern
        .array_expression => |arr| {
            return array.toArrayPattern(parser, parser.getExtra(arr.elements), span);
        },

        // ObjectExpression -> ObjectPattern (recursive)
        .object_expression => |obj| {
            return object.toObjectPattern(parser, parser.getExtra(obj.properties), span);
        },

        // already a pattern (can happen with nested patterns)
        .binding_identifier, .array_pattern, .object_pattern, .assignment_pattern => {
            return expr;
        },

        else => {
            try parser.report(
                span,
                "Invalid element in destructuring pattern",
                .{ .help = "Expected an identifier, array pattern, object pattern, or assignment pattern." },
            );
            return null;
        },
    }
}
