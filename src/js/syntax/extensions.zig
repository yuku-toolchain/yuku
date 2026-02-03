const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const class = @import("class.zig");
const expressions = @import("expressions.zig");

pub fn parseDecorators(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_decorators.begin();
    defer parser.scratch_decorators.reset(checkpoint);

    while (parser.current_token.type == .at) {
        const decorator = try parseDecorator(parser) orelse return null;
        try parser.scratch_decorators.append(parser.allocator(), decorator);
    }

    return try parser.addExtra(try parser.scratch_decorators.take(parser.allocator(), checkpoint));
}

pub fn parseDecorator(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (!try parser.expect(.at, "Expected '@' to start a decorator", null)) return null;

    const expression = try expressions.parseLeftHandSideExpression(parser) orelse return null;
    const end = parser.getSpan(expression).end;

    return try parser.addNode(.{
        .decorator = .{
            .expression = expression,
        },
    }, .{ .start = start, .end = end });
}

pub const ParseDecoratedOpts = packed struct {
    /// whether the decorated construct is parsed in expression position.
    is_expression: bool = false,
    /// whether the decorated construct is a default export.
    /// currently only affects class declarations (allows optional name but produces ClassDeclaration).
    is_default_export: bool = false,
};

pub fn parseDecorated(parser: *Parser, opts: ParseDecoratedOpts) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const decorators = try parseDecorators(parser) orelse return null;

    if (parser.current_token.type != .class) {
        try parser.report(
            parser.current_token.span,
            "Decorators must be followed by a class declaration or expression",
            .{ .help = "Use '@decorator class Foo {}' or remove the decorator." },
        );
        return null;
    }

    return class.parseClassDecorated(parser, .{
        .is_expression = opts.is_expression,
        .is_default_export = opts.is_default_export,
    }, start, decorators);
}
