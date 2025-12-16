const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");

const array = @import("array.zig");
const object = @import("object.zig");
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

    if (!try literals.validateIdentifier(parser, "an identifier", parser.current_token)) {
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
    const cover = try array.parseCover(parser) orelse return null;
    return try array.coverToPattern(parser, cover);
}

fn parseObjectPattern(parser: *Parser) Error!?ast.NodeIndex {
    const cover = try object.parseCover(parser) orelse return null;
    return try object.coverToPattern(parser, cover);
}

pub fn parseAssignmentPattern(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const start = parser.getSpan(left).start;

    if (parser.current_token.type != .assign) return left;

    try parser.advance();

    // right side is AssignmentExpression, not Expression (so 2)
    const right = try expressions.parseExpression(parser, 2, .{}) orelse return null;

    return try parser.addNode(
        .{ .assignment_pattern = .{ .left = left, .right = right } },
        .{ .start = start, .end = parser.getSpan(right).end },
    );
}

pub fn parseBindingRestElement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume ...

    const argument = try parseBindingPattern(parser) orelse return null;
    const end = parser.getSpan(argument).end;

    return try parser.addNode(
        .{ .binding_rest_element = .{ .argument = argument } },
        .{ .start = start, .end = end },
    );
}

pub fn isDestructuringPattern(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.getData(index)) {
        .array_pattern, .object_pattern => true,
        .assignment_pattern => |pattern| isDestructuringPattern(parser, pattern.left),
        else => false,
    };
}
