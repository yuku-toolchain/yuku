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

    const checkpoint = parser.scratch_cover.begin();
    errdefer parser.scratch_cover.reset(checkpoint);

    var end = start + 1;

    while (parser.current_token.type != .right_bracket and parser.current_token.type != .eof) {
        // elision (holes): [,,,]
        if (parser.current_token.type == .comma) {
            try parser.scratch_cover.append(parser.allocator(), ast.null_node);
            try parser.advance();
            continue;
        }

        // spread: [...x]
        if (parser.current_token.type == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance();
            const argument = try grammar.parseCoverExpression(parser, 2) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };
            const spread_end = parser.getSpan(argument).end;
            const spread = try parser.addNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );
            try parser.scratch_cover.append(parser.allocator(), spread);
            end = spread_end;
        } else {
            // regular element - parse as cover element
            const element = try grammar.parseCoverExpression(parser, 2) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };
            try parser.scratch_cover.append(parser.allocator(), element);
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
            parser.scratch_cover.reset(checkpoint);
            return null;
        }
    }

    if (parser.current_token.type != .right_bracket) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated array",
            .{ .help = "Add a closing ']' to complete the array." },
        );
        parser.scratch_cover.reset(checkpoint);
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance(); // consume ]

    return .{
        .elements = parser.scratch_cover.take(checkpoint),
        .start = start,
        .end = end,
    };
}

/// convert array cover to ArrayExpression.
/// validates that does not contain CoverInitializedName when validate=true.
pub fn coverToExpression(parser: *Parser, cover: ArrayCover, validate: bool) Error!?ast.NodeIndex {
    if (validate) {
        for (cover.elements) |elem| {
            if (ast.isNull(elem)) continue;
            if (!try grammar.validateNoInvalidCoverSyntax(parser, elem)) return null;
        }
    }
    return try parser.addNode(
        .{ .array_expression = .{ .elements = try parser.addExtra(cover.elements) } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// convert array cover to ArrayPattern.
pub fn coverToPattern(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    const elements_range = try parser.addExtra(cover.elements);
    return toArrayPatternImpl(parser, null, elements_range, .{ .start = cover.start, .end = cover.end });
}

/// convert ArrayExpression to ArrayPattern (mutates in-place).
pub fn toArrayPattern(parser: *Parser, expr_node: ast.NodeIndex, elements_range: ast.IndexRange) Error!?ast.NodeIndex {
    return toArrayPatternImpl(parser, expr_node, elements_range, undefined);
}

fn toArrayPatternImpl(parser: *Parser, mutate_node: ?ast.NodeIndex, elements_range: ast.IndexRange, span: ast.Span) Error!?ast.NodeIndex {
    const elements = parser.getExtra(elements_range);

    var rest: ast.NodeIndex = ast.null_node;
    var elements_len = elements_range.len;

    for (elements, 0..) |elem, i| {
        if (ast.isNull(elem)) continue;

        const elem_data = parser.getData(elem);
        if (elem_data == .spread_element) {
            // validate rest element is last (ignoring trailing holes)
            const has_more = for (elements[i + 1 ..]) |next| {
                if (!ast.isNull(next)) break true;
            } else false;

            if (has_more) {
                try parser.report(parser.getSpan(elem), "Rest element must be the last element", .{
                    .help = "No elements can follow the rest element in a destructuring pattern.",
                });
                return null;
            }

            const pattern = try grammar.expressionToPattern(parser, elem_data.spread_element.argument) orelse return null;

            parser.setData(elem, .{ .binding_rest_element = .{ .argument = pattern } });
            rest = elem;
            elements_len = @intCast(i);
            break;
        }

        _ = try grammar.expressionToPattern(parser, elem) orelse return null;
    }

    const pattern_data: ast.NodeData = .{ .array_pattern = .{
        .elements = .{ .start = elements_range.start, .len = elements_len },
        .rest = rest,
    } };

    if (mutate_node) |node| {
        parser.setData(node, pattern_data);
        return node;
    }

    return try parser.addNode(pattern_data, span);
}
