const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const expressions = @import("expressions.zig");

/// Parses a possibly empty run of `@expression` decorators.
pub fn parseDecorators(parser: *Parser) Error!?ast.IndexRange {
    return parseDecoratorsAfter(parser, ast.IndexRange.empty);
}

/// Parses a possibly empty run of decorators continuing `leading`, as one list.
pub fn parseDecoratorsAfter(parser: *Parser, leading: ast.IndexRange) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_decorators.begin();
    defer parser.scratch_decorators.reset(checkpoint);

    for (parser.tree.extra(leading)) |decorator| {
        try parser.scratch_decorators.append(parser.allocator(), decorator);
    }

    while (parser.current_token.tag == .at) {
        const decorator = try parseDecorator(parser) orelse return null;
        try parser.scratch_decorators.append(parser.allocator(), decorator);
    }

    return try parser.flushToExtras(&parser.scratch_decorators, checkpoint);
}

fn parseDecorator(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .at);
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '@'

    const expression = try expressions.parseLeftHandSideExpression(parser, .decorator) orelse
        return null;

    return try parser.tree.addNode(.{
        .decorator = .{ .expression = expression },
    }, .{ .start = start, .end = parser.tree.span(expression).end });
}
