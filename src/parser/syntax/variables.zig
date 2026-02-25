const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Precedence = @import("../token.zig").Precedence;
const Token = @import("../token.zig").Token;
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
    while (parser.current_token.tag == .comma) {
        try parser.advance() orelse return null;
        const declarator = try parseVariableDeclarator(parser, kind) orelse return null;
        try parser.scratch_a.append(parser.allocator(), declarator);
        end = parser.getSpan(declarator).end;
    }

    const span: ast.Span = .{ .start = start, .end = try parser.eatSemicolon(end) orelse return null };

    // lexical declarations are only allowed inside block statements
    if (
        parser.context.in_single_statement_context and
        (kind == .let or kind == .@"const" or kind == .using or kind == .await_using)
    )
    {
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
    const tag = parser.current_token.tag;
    try parser.advance() orelse return null;

    return switch (tag) {
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
    const id_span = parser.getSpan(id);

    var init: ast.NodeIndex = ast.null_node;
    var end = id_span.end;

    const is_using = kind == .using or kind == .await_using;
    const is_destructuring = patterns.isDestructuringPattern(parser, id);

    if (is_using and is_destructuring) {
        try parser.report(
            id_span,
            "Using declaration cannot have destructuring patterns.",
            .{},
        );
    }

    if (parser.current_token.tag == .assign) {
        try parser.advance() orelse return null;
        init = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        end = parser.getSpan(init).end;
    } else if (is_destructuring) {
        try parser.report(
            id_span,
            "Destructuring declaration must have an initializer",
            .{ .help = "Add '= value' to provide the object or array to destructure from." },
        );
    } else if (kind == .@"const") {
        try parser.report(
            id_span,
            "'const' declarations must be initialized",
            .{ .help = "Add '= value' to initialize the constant, or use 'let' if you need to assign it later." },
        );
    } else if (is_using) {
        try parser.reportFmt(
            id_span,
            "'{s}' declarations must be initialized",
            .{kind.toString()},
            .{ .help = "Disposable resources require an initial value that implements the dispose protocol." },
        );
    }

    return try parser.addNode(
        .{ .variable_declarator = .{ .id = id, .init = init } },
        .{ .start = start, .end = end },
    );
}

/// returns `true` when `token` can begin a lexical binding (identifier, `[` or `{`).
fn canTokenStartLexicalBinding(token: Token) bool {
    return token.tag.isIdentifierLike() or token.tag == .left_bracket or token.tag == .left_brace;
}

/// determines if 'let' should be parsed as an identifier rather than a variable declaration keyword.
pub fn isLetIdentifier(parser: *Parser) Error!?bool {
    std.debug.assert(parser.current_token.tag == .let);

    const next = try parser.lookAhead() orelse return null;

    // 'let' followed by a semicolon should be parsed as an identifier, not a declaration.
    if (next.tag == .semicolon) {
        return true;
    }

    // in single-statement contexts (eg, if/while bodies), parse `let` as an identifier
    // when the next token cannot start a lexical binding.
    if (parser.context.in_single_statement_context and !canTokenStartLexicalBinding(next)) return true;

    return false;
}

/// returns whether the current `using` token is an `IdentifierReference`
/// (expression path) or the contextual keyword for a declaration.
///
/// implements the cover-grammar disambiguation from:
/// - CoverAwaitExpressionAndAwaitUsingDeclarationHead
pub fn isUsingIdentifier(parser: *Parser) Error!?bool {
    std.debug.assert(parser.current_token.tag == .using);

    const next = try parser.lookAhead() orelse return null;

    // if next token starts a BindingList and ASI cannot insert a semicolon,
    // treat `using` as the contextual keyword (declaration form).
    if (next.tag.isIdentifierLike() and !parser.canInsertImplicitSemicolon(next)) {
        return false;
    }

    return true; // `using` is an identifier
}
