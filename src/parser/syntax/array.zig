const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;

const grammar = @import("../grammar.zig");

/// result from parsing array cover grammar: [a, b, ...c]
pub const ArrayCover = struct {
    elements: ast.IndexRange,
    start: u32,
    end: u32,
};

/// parse array literal permissively using cover grammar: [a, b, ...c]
/// https://tc39.es/ecma262/#sec-array-initializer (covers ArrayAssignmentPattern)
pub fn parseCover(parser: *Parser) Error!?ArrayCover {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume [

    const checkpoint = parser.scratch_cover.begin();
    defer parser.scratch_cover.reset(checkpoint);

    var end = start + 1;

    while (parser.current_token.tag != .right_bracket and parser.current_token.tag != .eof) {
        // elision (holes): [,,,]
        if (parser.current_token.tag == .comma) {
            try parser.scratch_cover.append(parser.allocator(), ast.null_node);
            try parser.advance() orelse return null;
            continue;
        }

        // spread: [...x]
        if (parser.current_token.tag == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance() orelse return null;
            const argument = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse return null;
            const spread_end = parser.getSpan(argument).end;
            const spread = try parser.addNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );
            try parser.scratch_cover.append(parser.allocator(), spread);
            end = spread_end;
        } else {
            // regular element - parse as cover element
            const element = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse return null;
            try parser.scratch_cover.append(parser.allocator(), element);
            end = parser.getSpan(element).end;
        }

        // comma or end
        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
            // then it's a trailing comma
            if (parser.current_token.tag == .right_bracket) {
                parser.state.cover_has_trailing_comma = start;
            }
        } else if (parser.current_token.tag != .right_bracket) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected ',' or ']' in array",
                .{ .help = "Add a comma between elements or close the array with ']'." },
            );
            return null;
        }
    }

    if (parser.current_token.tag != .right_bracket) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated array",
            .{
                .help = "Add a closing ']' to complete the array.",
                .labels = try parser.makeLabels(&.{parser.label(.{ .start = start, .end = start + 1 }, "Opened here")}),
            },
        );
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume ]

    const elements = try parser.addExtraFromScratch(&parser.scratch_cover, checkpoint);

    return .{
        .elements = elements,
        .start = start,
        .end = end,
    };
}

/// convert array cover to ArrayExpression.
/// validates that the expression does not contain CoverInitializedName when validate=true.
pub fn coverToExpression(parser: *Parser, cover: ArrayCover, validate: bool) Error!?ast.NodeIndex {
    const array_expression = try parser.addNode(
        .{ .array_expression = .{ .elements = cover.elements } },
        .{ .start = cover.start, .end = cover.end },
    );

    if (validate) try grammar.validateNoCoverInitializedSyntax(parser, array_expression);

    return array_expression;
}

/// convert array cover to ArrayPattern.
pub fn coverToPattern(parser: *Parser, cover: ArrayCover, comptime context: grammar.PatternContext) Error!ast.NodeIndex {
    return toArrayPatternImpl(parser, null, cover.elements, .{ .start = cover.start, .end = cover.end }, context);
}

/// convert ArrayExpression to ArrayPattern (mutates in-place).
pub fn toArrayPattern(parser: *Parser, expr_node: ast.NodeIndex, elements_range: ast.IndexRange, span: ast.Span, comptime context: grammar.PatternContext) Error!void {
    _ = try toArrayPatternImpl(parser, expr_node, elements_range, span, context);
}

fn toArrayPatternImpl(parser: *Parser, mutate_node: ?ast.NodeIndex, elements_range: ast.IndexRange, span: ast.Span, comptime context: grammar.PatternContext) Error!ast.NodeIndex {
    const elements = parser.getExtra(elements_range);

    var rest: ast.NodeIndex = ast.null_node;
    var elements_len = elements_range.len;

    for (elements, 0..) |elem, i| {
        if (ast.isNull(elem)) continue;

        const elem_data = parser.getData(elem);

        if (elem_data == .spread_element) {
            if (parser.state.cover_has_trailing_comma == span.start) {
                try parser.report(span, "Rest element cannot have a trailing comma in array destructuring.", .{
                    .help = "Remove the trailing comma after the rest element",
                });

                parser.state.cover_has_trailing_comma = null;
            }

            if (i != elements_len - 1) {
                try parser.report(parser.getSpan(elem), "Rest element must be the last element", .{
                    .help = "No elements can follow the rest element in a destructuring pattern.",
                });
            }

            // spread_element to binding_rest_element
            try grammar.expressionToPattern(parser, elem, context);
            rest = elem;
            elements_len = @intCast(i);
            break;
        }

        try grammar.expressionToPattern(parser, elem, context);
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
