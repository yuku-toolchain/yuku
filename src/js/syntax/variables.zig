const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const expressions = @import("expressions.zig");
const patterns = @import("patterns.zig");

pub fn parseVariableDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const kind = parseVariableKind(parser) orelse return null;

    const checkpoint = parser.scratch_a.begin();

    const first_declarator = try parseVariableDeclarator(parser, kind) orelse {
        parser.scratch_a.reset(checkpoint);
        return null;
    };

    try parser.scratch_a.append(parser.allocator(), first_declarator);

    var end = parser.getSpan(first_declarator).end;

    // additional declarators: let a, b, c;
    while (parser.current_token.type == .comma) {
        try parser.advance();
        const declarator = try parseVariableDeclarator(parser, kind) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };
        try parser.scratch_a.append(parser.allocator(), declarator);
        end = parser.getSpan(declarator).end;
    }

    return try parser.addNode(
        .{
            .variable_declaration = .{
                .declarators = try parser.addExtra(parser.scratch_a.take(checkpoint)),
                .kind = kind,
            },
        },
        .{ .start = start, .end = try parser.eatSemicolon(end) },
    );
}

inline fn parseVariableKind(parser: *Parser) ?ast.VariableKind {
    const token_type = parser.current_token.type;
    parser.advance() catch return null;

    return switch (token_type) {
        .let => .let,
        .@"const" => .@"const",
        .@"var" => .@"var",
        .using => .using,

        .await => if (parser.current_token.type == .using) blk: {
            parser.advance() catch return null;
            break :blk .await_using;
        } else null,

        else => null,
    };
}

fn parseVariableDeclarator(parser: *Parser, kind: ast.VariableKind) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const id = try patterns.parseBindingPattern(parser) orelse return null;

    var init: ast.NodeIndex = ast.null_node;
    var end = parser.getSpan(id).end;

    // initializer if present
    if (parser.current_token.type == .assign) {
        try parser.advance();
        init = try expressions.parseExpression(parser, 2, .{}) orelse return null;
        end = parser.getSpan(init).end;
    } else if (patterns.isDestructuringPattern(parser, id)) {
        try parser.report(
            parser.getSpan(id),
            "Destructuring declaration must have an initializer",
            .{ .help = "Add '= value' to provide the object or array to destructure from." },
        );
        return null;
    } else if (kind == .@"const") {
        try parser.report(
            parser.getSpan(id),
            "'const' declarations must be initialized",
            .{ .help = "Add '= value' to initialize the constant, or use 'let' if you need to assign it later." },
        );
        return null;
    } else if (kind == .using or kind == .await_using) {
        const keyword = if (kind == .using) "using" else "await using";
        try parser.reportFmt(
            parser.getSpan(id),
            "'{s}' declarations must be initialized",
            .{keyword},
            .{ .help = "Disposable resources require an initial value that implements the dispose protocol." },
        );
        return null;
    }

    return try parser.addNode(
        .{ .variable_declarator = .{ .id = id, .init = init } },
        .{ .start = start, .end = end },
    );
}
