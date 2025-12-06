const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");

const literals = @import("literals.zig");
const expressions = @import("expressions.zig");

pub inline fn parseBindingPattern(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.type.isIdentifierLike()) {
        return parseBindingIdentifier(parser);
    }

    return switch (parser.current_token.type) {
        .left_bracket => parseArrayPattern(parser),
        .left_brace => parseObjectPattern(parser),
        else => {
            try parser.reportFmt(
                parser.current_token.span,
                "Unexpected token '{s}' in binding pattern",
                .{parser.current_token.lexeme},
                .{ .help = "Expected an identifier, array pattern ([a, b]), or object pattern ({a, b})." },
            );
            return null;
        },
    };
}

pub inline fn parseBindingIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    if (!parser.current_token.type.isIdentifierLike()) {
        try parser.reportFmt(
            parser.current_token.span,
            "Expected identifier, found '{s}'",
            .{parser.current_token.lexeme},
            .{ .help = "A variable name must be a valid JavaScript identifier." },
        );
        return null;
    }

    const current = parser.current_token;
    try parser.advance();

    return try parser.addNode(
        .{
            .binding_identifier = .{
                .name_start = current.span.start,
                .name_len = @intCast(current.lexeme.len),
            },
        },
        current.span,
    );
}

fn parseArrayPattern(parser: *Parser) Error!?ast.NodeIndex {
    _ = parser;
}

fn parseObjectPattern(parser: *Parser) Error!?ast.NodeIndex {
    _ = parser;
}

pub fn parseAssignmentPattern(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const start = parser.getSpan(left).start;
    if (parser.current_token.type != .assign) return left;

    try parser.advance();

    const right = try expressions.parseExpression(parser, 0) orelse return null;

    return try parser.addNode(
        .{ .assignment_pattern = .{ .left = left, .right = right } },
        .{ .start = start, .end = parser.getSpan(right).end },
    );
}

pub fn isDestructuringPattern(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.getData(index)) {
        .array_pattern, .object_pattern => true,
        .assignment_pattern => |pattern| isDestructuringPattern(parser, pattern.left),
        else => false,
    };
}
