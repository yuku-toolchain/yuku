const std = @import("std");
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
        .bitwise_and => parseLazyBindingPattern(parser),
        .left_bracket => parseArrayPattern(parser),
        .left_brace => parseObjectPattern(parser),
        else => {
            try parser.report(
                parser.current_token.span,
                try parser.fmt(
                    "Unexpected token '{s}' in binding pattern",
                    .{parser.describeToken(parser.current_token)},
                ),
                .{ .help = "Expected an identifier, array pattern ([a, b])," ++
                    " or object pattern ({a, b})." },
            );
            return null;
        },
    };
}

pub fn isLazyPatternStart(parser: *Parser) bool {
    if (parser.current_token.tag != .bitwise_and) return false;

    const ampersand = parser.current_token;
    var peek = parser.beginPeek();
    defer peek.end();

    const next = peek.next() orelse return false;
    if (ampersand.span.end != next.span.start) return false;

    return next.tag == .left_brace or next.tag == .left_bracket;
}

pub fn markLazyPattern(parser: *Parser, pattern: ast.NodeIndex, start: u32) void {
    const span = parser.tree.span(pattern);
    std.debug.assert(start <= span.start);

    switch (parser.tree.data(pattern)) {
        .array_pattern => |array_pattern| {
            var lazy_pattern = array_pattern;
            lazy_pattern.lazy = true;
            parser.tree.setData(pattern, .{ .array_pattern = lazy_pattern });
        },
        .object_pattern => |object_pattern| {
            var lazy_pattern = object_pattern;
            lazy_pattern.lazy = true;
            parser.tree.setData(pattern, .{ .object_pattern = lazy_pattern });
        },
        else => unreachable,
    }

    parser.tree.setSpan(pattern, .{ .start = start, .end = span.end });
}

fn parseLazyBindingPattern(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .bitwise_and);
    const ampersand = parser.current_token;

    if (!isLazyPatternStart(parser)) {
        return parseLazyBindingPatternError(parser);
    }

    if (!parser.tree.isTsrx()) {
        try parser.report(
            ampersand.span,
            "Lazy destructuring patterns are only enabled in TSRX files",
            .{ .help = "Use a .tsrx file or remove the '&' lazy-pattern marker." },
        );
        return null;
    }

    try parser.advance() orelse return null;

    const pattern = switch (parser.current_token.tag) {
        .left_bracket => try parseArrayPattern(parser) orelse return null,
        .left_brace => try parseObjectPattern(parser) orelse return null,
        else => unreachable,
    };
    markLazyPattern(parser, pattern, ampersand.span.start);
    return pattern;
}

fn parseLazyBindingPatternError(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.report(
        token.span,
        try parser.fmt(
            "Unexpected token '{s}' in binding pattern",
            .{parser.describeToken(token)},
        ),
        .{ .help = "Expected a contiguous TSRX lazy pattern introducer '&{' or '&['." },
    );
    return null;
}

fn parseArrayPattern(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_bracket);
    const cover = try array.parseCover(parser) orelse return null;
    return try array.coverToPattern(parser, cover, .binding);
}

fn parseObjectPattern(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const cover = try object.parseCover(parser) orelse return null;
    return try object.coverToPattern(parser, cover, .binding);
}

pub fn parseAssignmentPattern(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    std.debug.assert(left != .null);
    const start = parser.tree.span(left).start;

    if (parser.current_token.tag != .assign) return left;

    try parser.advance() orelse return null;

    const right = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
        return null;

    return try parser.tree.addNode(
        .{ .assignment_pattern = .{ .left = left, .right = right } },
        .{ .start = start, .end = parser.tree.span(right).end },
    );
}

pub fn parseBindingRestElement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .spread);
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume ...

    const argument = try parseBindingPattern(parser) orelse return null;
    var end = parser.tree.span(argument).end;

    // `function f(...rest: Type[]) { ... }`
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
