const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const inRegister = @import("util").Register.inRegister;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;

const array = @import("array.zig");
const object = @import("object.zig");
const literals = @import("literals.zig");
const expressions = @import("expressions.zig");
const ts = @import("ts/types.zig");

pub inline fn parseBindingPattern(parser: *Parser) Error!?ast.NodeIndex {
    try parser.descend();
    defer parser.ascend();
    if (parser.current_token.tag.isIdentifierLike()) {
        return literals.parseBindingIdentifier(parser);
    }

    return switch (parser.current_token.tag) {
        .left_bracket => parseArrayPattern(parser),
        .left_brace => parseObjectPattern(parser),
        else => {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected an identifier or a destructuring pattern",
                .{},
            );
            return null;
        },
    };
}

inline fn parseArrayPattern(parser: *Parser) Error!?ast.NodeIndex {
    return inRegister(parseArrayPatternImpl, .{parser});
}

fn parseArrayPatternImpl(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_bracket);
    const cover = try array.parseCover(parser) orelse return null;
    return try array.coverToPattern(parser, cover, .binding);
}

inline fn parseObjectPattern(parser: *Parser) Error!?ast.NodeIndex {
    return inRegister(parseObjectPatternImpl, .{parser});
}

fn parseObjectPatternImpl(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const cover = try object.parseCover(parser) orelse return null;
    return try object.coverToPattern(parser, cover, .binding);
}

pub inline fn parseAssignmentPattern(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    return inRegister(parseAssignmentPatternImpl, .{ parser, left });
}

fn parseAssignmentPatternImpl(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    std.debug.assert(left != .null);
    if (parser.current_token.tag != .assign) return left;

    try parser.advance() orelse return null;

    const right = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
        return null;

    return try parser.tree.addNode(
        .{ .assignment_pattern = .{ .left = left, .right = right } },
        .{ .start = parser.tree.span(left).start, .end = parser.tree.span(right).end },
    );
}

pub inline fn parseBindingRestElement(parser: *Parser) Error!?ast.NodeIndex {
    return inRegister(parseBindingRestElementImpl, .{parser});
}

fn parseBindingRestElementImpl(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .spread);
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const argument = try parseBindingPattern(parser) orelse return null;
    var end = parser.tree.span(argument).end;

    var type_annotation: ast.NodeIndex = .null;
    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        type_annotation = try ts.parseTypeAnnotation(parser) orelse return null;
        end = parser.tree.span(type_annotation).end;
    }

    return try parser.tree.addNode(
        .{ .binding_rest_element = .{ .argument = argument, .type_annotation = type_annotation } },
        .{ .start = start, .end = end },
    );
}

pub fn isDestructuringPattern(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.tree.data(index)) {
        .array_pattern, .object_pattern => true,
        .assignment_pattern => |pattern| isDestructuringPattern(parser, pattern.left),
        else => false,
    };
}
