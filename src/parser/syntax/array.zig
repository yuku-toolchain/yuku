const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;

const grammar = @import("../grammar.zig");
const expressions = @import("expressions.zig");

/// Elements parsed by the array cover grammar, before they become an expression or a pattern.
pub const ArrayCover = struct {
    elements: ast.IndexRange,
    start: u32,
    end: u32,
};

/// Parses an array literal permissively, covering ArrayAssignmentPattern as well.
/// https://tc39.es/ecma262/#sec-array-initializer
pub fn parseCover(parser: *Parser) Error!?ArrayCover {
    std.debug.assert(parser.current_token.tag == .left_bracket);
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume [

    const checkpoint = parser.scratch_cover.begin();
    defer parser.scratch_cover.reset(checkpoint);

    var end = start + 1;

    while (parser.current_token.tag != .right_bracket and parser.current_token.tag != .eof) {
        if (parser.current_token.tag == .comma) {
            try parser.scratch_cover.append(parser.allocator(), .null);
            try parser.advance() orelse return null;
            continue;
        }

        const element = if (parser.current_token.tag == .spread)
            try expressions.parseSpreadElement(parser) orelse return null
        else
            try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        try parser.scratch_cover.append(parser.allocator(), element);
        end = parser.tree.span(element).end;

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
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
                .labels = try parser.labels(&.{
                    parser.label(.{ .start = start, .end = start + 1 }, "Opened here"),
                }),
            },
        );
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume ]

    const elements = try parser.flushToExtras(&parser.scratch_cover, checkpoint);

    return .{
        .elements = elements,
        .start = start,
        .end = end,
    };
}

/// Converts an array cover to an ArrayExpression.
pub fn coverToExpression(parser: *Parser, cover: ArrayCover) Error!ast.NodeIndex {
    return parser.tree.addNode(
        .{ .array_expression = .{ .elements = cover.elements } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// Converts an array cover to an ArrayPattern.
pub fn coverToPattern(
    parser: *Parser,
    cover: ArrayCover,
    comptime context: grammar.PatternContext,
) Error!ast.NodeIndex {
    return toArrayPatternImpl(
        parser,
        null,
        cover.elements,
        .{ .start = cover.start, .end = cover.end },
        context,
    );
}

/// Converts an ArrayExpression node to an ArrayPattern in place.
pub fn toArrayPattern(
    parser: *Parser,
    expr_node: ast.NodeIndex,
    elements_range: ast.IndexRange,
    span: ast.Span,
    comptime context: grammar.PatternContext,
) Error!void {
    _ = try toArrayPatternImpl(parser, expr_node, elements_range, span, context);
}

fn toArrayPatternImpl(
    parser: *Parser,
    mutate_node: ?ast.NodeIndex,
    elements_range: ast.IndexRange,
    span: ast.Span,
    comptime context: grammar.PatternContext,
) Error!ast.NodeIndex {
    const elements = parser.tree.extra(elements_range);

    var rest: ast.NodeIndex = .null;
    var elements_len = elements_range.len;

    for (elements, 0..) |elem, i| {
        if (elem == .null) continue;

        if (parser.tree.data(elem) == .spread_element) {
            if (i == elements_len - 1 and grammar.isFollowedByComma(parser, elem)) {
                try parser.report(
                    span,
                    "Rest element cannot have a trailing comma in array destructuring.",
                    .{ .help = "Remove the trailing comma after the rest element" },
                );
            }

            if (i != elements_len - 1) {
                try parser.report(
                    parser.tree.span(elem),
                    "Rest element must be the last element",
                    .{ .help = "No elements can follow the rest element in a destructuring" ++
                        " pattern." },
                );
            }

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
        parser.tree.setData(node, pattern_data);
        return node;
    }

    return try parser.tree.addNode(pattern_data, span);
}
