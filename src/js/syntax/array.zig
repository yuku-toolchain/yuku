const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");

const grammar = @import("../grammar.zig");
const expressions = @import("expressions.zig");

/// result from parsing array cover grammar: [a, b, ...c]
pub const ArrayCover = struct {
    elements: []const ast.NodeIndex,
    start: u32,
    end: u32,
};

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
            const argument = try grammar.parseCoverElement(parser) orelse {
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
            const element = try grammar.parseCoverElement(parser) orelse {
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
/// validates that does not contain CoverInitializedName.
pub fn coverToExpression(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    for (cover.elements) |elem| {
        if (ast.isNull(elem)) continue;
        if (!try grammar.validateNoInvalidCoverSyntax(parser, elem)) return null;
    }
    return coverToExpressionUnchecked(parser, cover);
}

/// convert array cover to ArrayExpression without validation (nested use).
/// used for nested arrays that might later become patterns when parent converts.
pub fn coverToExpressionUnchecked(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    return try parser.addNode(
        .{ .array_expression = .{ .elements = try parser.addExtra(cover.elements) } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// convert array cover to ArrayPattern.
/// converts all elements from expressions to patterns.
/// CoverInitializedName ({ a = 1 }) becomes valid AssignmentPattern here.
pub fn coverToPattern(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    return toArrayPattern(parser, cover.elements, .{ .start = cover.start, .end = cover.end });
}

/// for converting array elements to ArrayPattern.
pub fn toArrayPattern(parser: *Parser, elements: []const ast.NodeIndex, span: ast.Span) Error!?ast.NodeIndex {
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

            const pattern = try grammar.expressionToPattern(parser, elem_data.spread_element.argument) orelse {
                parser.scratch_b.reset(checkpoint);
                return null;
            };

            rest = try parser.addNode(.{ .binding_rest_element = .{ .argument = pattern } }, elem_span);
            continue;
        }

        const pattern = try grammar.expressionToPattern(parser, elem) orelse {
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
