const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Precedence = @import("../token.zig").Precedence;
const expressions = @import("expressions.zig");
const patterns = @import("patterns.zig");
const std = @import("std");

pub fn parseVariableDeclaration(parser: *Parser, await_using: bool, start_from_param: ?u32) Error!?ast.NodeIndex {
    const start = start_from_param orelse parser.current_token.span.start;
    const kind = try parseVariableKind(parser, await_using) orelse return null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    const first_declarator = try parseVariableDeclarator(parser, kind) orelse return null;

    try parser.scratch_a.append(parser.allocator(), first_declarator);

    var end = parser.getSpan(first_declarator).end;

    // additional declarators: let a, b, c;
    while (parser.current_token.type == .comma) {
        try parser.advance() orelse return null;
        const declarator = try parseVariableDeclarator(parser, kind) orelse return null;
        try parser.scratch_a.append(parser.allocator(), declarator);
        end = parser.getSpan(declarator).end;
    }

    const span: ast.Span = .{ .start = start, .end = try parser.eatSemicolon(end) orelse return null };

    // lexical declarations are only allowed inside block statements
    if (parser.context.in_single_statement_context and (kind == .let or kind == .@"const")) {
        @branchHint(.unlikely);

        try parser.report(
            span,
            "Lexical declaration cannot appear in a single-statement context",
            .{ .help = "Wrap this declaration in a block statement" },
        );
    }

    return try parser.addNode(
        .{
            .variable_declaration = .{
                .declarators = try parser.addExtraFromScratch(&parser.scratch_a, checkpoint),
                .kind = kind,
            },
        },
        span,
    );
}

fn parseVariableKind(parser: *Parser, await_using: bool) Error!?ast.VariableKind {
    const token_type = parser.current_token.type;
    try parser.advance() orelse return null;

    return switch (token_type) {
        .let => .let,
        .@"const" => .@"const",
        .@"var" => .@"var",
        .using => blk: {
            if (await_using) {
                break :blk .await_using;
            } else {
                break :blk .using;
            }
        },
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
        try parser.advance() orelse return null;
        init = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
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

/// determines if 'let' should be parsed as an identifier rather than a variable declaration keyword.
pub fn isLetIdentifier(parser: *Parser) Error!?bool {
    std.debug.assert(parser.current_token.type == .let);

    const next = try parser.lookAhead() orelse return null;

    // 'let' followed by a semicolon should be parsed as an identifier, not a declaration.
    if (next.type == .semicolon) {
        return true;
    }

    // in single-statement contexts (eg, if/while bodies), 'let' followed by an implicit semicolon
    // should also be parsed as an identifier, since lexical declarations aren't allowed there.
    if (parser.context.in_single_statement_context and parser.canInsertSemicolon(next)) {
        return true;
    }

    return false;
}
