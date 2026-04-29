const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;

const array = @import("array.zig");
const object = @import("object.zig");
const literals = @import("literals.zig");
const expressions = @import("expressions.zig");
const ts = @import("ts/types.zig");

pub inline fn parseBindingPattern(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag.isIdentifierLike()) {
        return literals.parseBindingIdentifier(parser);
    }

    return switch (parser.current_token.tag) {
        .left_bracket => parseArrayPattern(parser),
        .left_brace => parseObjectPattern(parser),
        else => {
            try parser.report(
                parser.current_token.span,
                try parser.fmt("Unexpected token '{s}' in binding pattern", .{parser.describeToken(parser.current_token)}),
                .{ .help = "Expected an identifier, array pattern ([a, b]), or object pattern ({a, b})." },
            );
            return null;
        },
    };
}

fn parseArrayPattern(parser: *Parser) Error!?ast.NodeIndex {
    const cover = try array.parseCover(parser) orelse return null;
    return try array.coverToPattern(parser, cover, .binding);
}

fn parseObjectPattern(parser: *Parser) Error!?ast.NodeIndex {
    const cover = try object.parseCover(parser) orelse return null;
    return try object.coverToPattern(parser, cover, .binding);
}

pub fn parseAssignmentPattern(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const start = parser.tree.getSpan(left).start;

    if (parser.current_token.tag != .assign) return left;

    try parser.advance() orelse return null;

    const right = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    return try parser.tree.createNode(
        .{ .assignment_pattern = .{ .left = left, .right = right } },
        .{ .start = start, .end = parser.tree.getSpan(right).end },
    );
}

pub fn parseBindingRestElement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume ...

    const argument = try parseBindingPattern(parser) orelse return null;
    var end = parser.tree.getSpan(argument).end;

    // `function f(...rest: Type[]) { ... }`
    var type_annotation: ast.NodeIndex = .null;
    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        type_annotation = try ts.parseTypeAnnotation(parser) orelse return null;
        end = parser.tree.getSpan(type_annotation).end;
    }

    return try parser.tree.createNode(
        .{ .binding_rest_element = .{ .argument = argument, .type_annotation = type_annotation } },
        .{ .start = start, .end = end },
    );
}

pub fn isDestructuringPattern(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.tree.getData(index)) {
        .array_pattern, .object_pattern => true,
        .assignment_pattern => |pattern| isDestructuringPattern(parser, pattern.left),
        else => false,
    };
}
