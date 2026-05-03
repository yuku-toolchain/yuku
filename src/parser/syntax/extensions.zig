const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const expressions = @import("expressions.zig");

/// parses a contiguous run of `@expression` decorators (possibly empty).
pub fn parseDecorators(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_decorators.begin();
    defer parser.scratch_decorators.reset(checkpoint);

    while (parser.current_token.tag == .at) {
        const decorator = try parseDecorator(parser) orelse return null;
        try parser.scratch_decorators.append(parser.allocator(), decorator);
    }

    return try parser.addExtraFromScratch(&parser.scratch_decorators, checkpoint);
}

pub fn parseDecorator(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (!try parser.expect(.at, "Expected '@' to start a decorator", null)) return null;

    const expression = try expressions.parseLeftHandSideExpression(parser, .decorator) orelse return null;

    return try parser.tree.addNode(.{
        .decorator = .{ .expression = expression },
    }, .{ .start = start, .end = parser.tree.span(expression).end });
}
