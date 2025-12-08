const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");

const expressions = @import("expressions.zig");

/// result from parsing array cover grammar: [a, b, ...c]
pub const ArrayCover = struct {
    elements: []const ast.NodeIndex,
    start: u32,
    end: u32,
};

/// parse an element within a cover grammar context (used for nested arrays/objects).
///
/// keeps nested arrays/objects as "covers" by converting them to expressions
/// without validation. validation is deferred until the top-level context is known:
/// - if parent becomes an expression -> validate at top level
/// - if parent becomes a pattern -> no validation needed
pub fn parseCoverElement(parser: *Parser) Error!?ast.NodeIndex {
    const object = @import("object.zig");
    return switch (parser.current_token.type) {
        .left_bracket => blk: {
            const cover = try parseCover(parser) orelse return null;
            break :blk coverToExpressionUnchecked(parser, cover);
        },
        .left_brace => blk: {
            const cover = try object.parseCover(parser) orelse return null;
            break :blk object.coverToExpressionUnchecked(parser, cover);
        },
        else => expressions.parseExpression(parser, 2),
    };
}

/// parse array literal permissively using cover grammar: [a, b, ...c]
/// https://tc39.es/ecma262/#sec-array-initializer (covers ArrayAssignmentPattern)
pub fn parseCover(parser: *Parser) Error!?ArrayCover {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume [

    const checkpoint = parser.scratch_a.begin();
    errdefer parser.scratch_a.reset(checkpoint);

    var end = start + 1;

    while (parser.current_token.type != .right_bracket and parser.current_token.type != .eof) {
        // elision (holes): [,,,]
        if (parser.current_token.type == .comma) {
            try parser.scratch_a.append(parser.allocator(), ast.null_node);
            try parser.advance();
            continue;
        }

        // spread: [...x]
        if (parser.current_token.type == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance();
            const argument = try parseCoverElement(parser) orelse {
                parser.scratch_a.reset(checkpoint);
                return null;
            };
            const spread_end = parser.getSpan(argument).end;
            const spread = try parser.addNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );
            try parser.scratch_a.append(parser.allocator(), spread);
            end = spread_end;
        } else {
            // regular element - parse as cover element
            const element = try parseCoverElement(parser) orelse {
                parser.scratch_a.reset(checkpoint);
                return null;
            };
            try parser.scratch_a.append(parser.allocator(), element);
            end = parser.getSpan(element).end;
        }

        // comma or end
        if (parser.current_token.type == .comma) {
            try parser.advance();
        } else if (parser.current_token.type != .right_bracket) {
            try parser.report(
                parser.current_token.span,
                "Expected ',' or ']' in array",
                .{ .help = "Add a comma between elements or close the array with ']'." },
            );
            parser.scratch_a.reset(checkpoint);
            return null;
        }
    }

    if (parser.current_token.type != .right_bracket) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated array",
            .{ .help = "Add a closing ']' to complete the array." },
        );
        parser.scratch_a.reset(checkpoint);
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance(); // consume ]

    return .{
        .elements = parser.scratch_a.take(checkpoint),
        .start = start,
        .end = end,
    };
}

/// convert array cover to ArrayExpression with validation (top-level use).
///
/// validates recursively that does not contain CoverInitializedName.
pub fn coverToExpression(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    const object = @import("object.zig");
    for (cover.elements) |elem| {
        if (ast.isNull(elem)) continue;
        if (!try object.validateNoInvalidCoverSyntax(parser, elem)) return null;
    }
    return coverToExpressionUnchecked(parser, cover);
}

/// convert array cover to ArrayExpression without validation (nested use).
/// used for nested arrays that might later become patterns when parent converts.
fn coverToExpressionUnchecked(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    return try parser.addNode(
        .{ .array_expression = .{ .elements = try parser.addExtra(cover.elements) } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// convert array cover to ArrayPattern.
///
/// recursively converts all elements from expressions to patterns.
/// CoverInitializedName ({ a = 1 }) becomes valid AssignmentPattern here.
pub fn coverToPattern(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    return toArrayPattern(parser, cover.elements, .{ .start = cover.start, .end = cover.end });
}

/// Convert an expression node to a binding pattern.
///
/// transformations:
/// - IdentifierReference -> BindingIdentifier
/// - ArrayExpression -> ArrayPattern (recursive)
/// - ObjectExpression -> ObjectPattern (recursive)
/// - AssignmentExpression -> AssignmentPattern (for defaults)
pub fn expressionToPattern(parser: *Parser, expr: ast.NodeIndex) Error!?ast.NodeIndex {
    const object = @import("object.zig");
    const data = parser.getData(expr);
    const span = parser.getSpan(expr);

    switch (data) {
        // Identifier → BindingIdentifier
        .identifier_reference => |id| {
            return try parser.addNode(
                .{ .binding_identifier = .{ .name_start = id.name_start, .name_len = id.name_len } },
                span,
            );
        },

        // AssignmentExpression → AssignmentPattern
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

        // ArrayExpression → ArrayPattern (recursive)
        .array_expression => |arr| {
            return toArrayPattern(parser, parser.getExtra(arr.elements), span);
        },

        // ObjectExpression → ObjectPattern (recursive)
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

/// for converting array elements to ArrayPattern.
fn toArrayPattern(parser: *Parser, elements: []const ast.NodeIndex, span: ast.Span) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_b.begin();
    errdefer parser.scratch_b.reset(checkpoint);

    var rest: ast.NodeIndex = ast.null_node;

    for (elements, 0..) |elem, i| {
        if (ast.isNull(elem)) {
            try parser.scratch_b.append(parser.allocator(), ast.null_node);
            continue;
        }

        const elem_data = parser.getData(elem);
        const elem_span = parser.getSpan(elem);

        if (elem_data == .spread_element) {
            // rest must be last (no non-hole elements after)
            const has_more = for (elements[i + 1 ..]) |next| {
                if (!ast.isNull(next)) break true;
            } else false;

            if (has_more) {
                try parser.report(elem_span, "Rest element must be the last element", .{
                    .help = "No elements can follow the rest element in a destructuring pattern.",
                });
                parser.scratch_b.reset(checkpoint);
                return null;
            }

            const pattern = try expressionToPattern(parser, elem_data.spread_element.argument) orelse {
                parser.scratch_b.reset(checkpoint);
                return null;
            };

            rest = try parser.addNode(.{ .binding_rest_element = .{ .argument = pattern } }, elem_span);
            continue;
        }

        const pattern = try expressionToPattern(parser, elem) orelse {
            parser.scratch_b.reset(checkpoint);
            return null;
        };
        try parser.scratch_b.append(parser.allocator(), pattern);
    }

    return try parser.addNode(
        .{ .array_pattern = .{ .elements = try parser.addExtra(parser.scratch_b.take(checkpoint)), .rest = rest } },
        span,
    );
}
