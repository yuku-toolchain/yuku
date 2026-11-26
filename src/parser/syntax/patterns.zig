const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");

const grammar = @import("../grammar.zig");
const literals = @import("literals.zig");
const ts = @import("ts/types.zig");

pub inline fn parseBindingPattern(parser: *Parser) Error!?ast.NodeIndex {
    try parser.descend();
    defer parser.ascend();
    if (parser.current_token.tag.isIdentifierLike()) {
        return literals.parseBindingIdentifier(parser);
    }
    return parseDestructuringPattern(parser);
}

fn parseDestructuringPattern(parser: *Parser) Error!?ast.NodeIndex {
    switch (parser.current_token.tag) {
        .left_bracket => {
            const literal = try grammar.parseBracketed(parser, .array) orelse return null;
            const span = literal.span();
            const pattern = try grammar.arrayToPattern(parser, literal.elements, span, .binding);
            return try parser.tree.addNode(.{ .array_pattern = pattern }, span);
        },
        .left_brace => {
            const literal = try grammar.parseBracketed(parser, .object) orelse return null;
            const span = literal.span();
            const pattern = try grammar.objectToPattern(parser, literal.elements, span, .binding);
            return try parser.tree.addNode(.{ .object_pattern = pattern }, span);
        },
        else => {
            return parser.failExpected("Expected an identifier or a destructuring pattern", .{});
        },
    }
}

pub fn parseBindingRestElement(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.spread) orelse return null;
    const argument = try parseBindingPattern(parser) orelse return null;
    const type_annotation: ast.NodeIndex = if (parser.tree.isTs() and parser.at(.colon))
        try ts.parseTypeAnnotation(parser) orelse return null
    else
        .null;

    const last = if (type_annotation != .null) type_annotation else argument;
    return try parser.finishAt(start, last, .{
        .binding_rest_element = .{ .argument = argument, .type_annotation = type_annotation },
    });
}

pub fn isDestructuringPattern(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.tree.data(index)) {
        .array_pattern, .object_pattern => true,
        .assignment_pattern => |pattern| isDestructuringPattern(parser, pattern.left),
        else => false,
    };
}
