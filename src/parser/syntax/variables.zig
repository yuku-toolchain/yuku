const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Precedence = @import("../token.zig").Precedence;
const TokenTag = @import("../token.zig").TokenTag;
const expressions = @import("expressions.zig");
const patterns = @import("patterns.zig");
const ts_types = @import("ts/types.zig");
const std = @import("std");

pub const ParseVariableDeclarationOpts = struct {
    await_using: bool = false,
    /// ts: sets the `declare` flag on the resulting `VariableDeclaration` node
    /// and relaxes ambient-incompatible initializer checks.
    is_declare: bool = false,
};

pub fn parseVariableDeclaration(parser: *Parser, opts: ParseVariableDeclarationOpts, start_from_param: ?u32) Error!?ast.NodeIndex {
    const start = start_from_param orelse parser.current_token.span.start;
    const kind = try parseVariableKind(parser, opts.await_using) orelse return null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    const first_declarator = try parseVariableDeclarator(parser, kind, opts.is_declare) orelse return null;

    try parser.scratch_a.append(parser.allocator(), first_declarator);

    var end = parser.tree.getSpan(first_declarator).end;

    // additional declarators: let a, b, c;
    while (parser.current_token.tag == .comma) {
        try parser.advance() orelse return null;
        const declarator = try parseVariableDeclarator(parser, kind, opts.is_declare) orelse return null;
        try parser.scratch_a.append(parser.allocator(), declarator);
        end = parser.tree.getSpan(declarator).end;
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

    return try parser.tree.createNode(
        .{
            .variable_declaration = .{
                .declarators = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint),
                .kind = kind,
                .declare = opts.is_declare,
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

fn parseVariableDeclarator(parser: *Parser, kind: ast.VariableKind, declare: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const id = try patterns.parseBindingPattern(parser) orelse return null;
    const id_span = parser.tree.getSpan(id);

    // `let x!: T` definite assignment assertion
    var definite = false;
    var end = id_span.end;
    if (parser.tree.isTs() and
        parser.current_token.tag == .logical_not and
        !parser.current_token.hasLineTerminatorBefore())
    {
        definite = true;
        end = parser.current_token.span.end;
        try parser.advance() orelse return null;
    }

    // `let x: Type = ...`
    // annotation attaches to the inner binding pattern (`id`).
    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        const annotation = try ts_types.parseTypeAnnotation(parser) orelse return null;
        ts_types.applyTypeAnnotationToPattern(parser, id, annotation);
        end = parser.tree.getSpan(annotation).end;
    }

    var init: ast.NodeIndex = .null;

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

        end = parser.tree.getSpan(init).end;
    } else if (is_destructuring and !declare) {
        try parser.report(
            id_span,
            "Destructuring declaration must have an initializer",
            .{ .help = "Add '= value' to provide the object or array to destructure from." },
        );
    } else if (kind == .@"const" and !declare) {
        // skipped under `declare` since ambient const declarations omit the
        // initializer.
        try parser.report(
            id_span,
            "'const' declarations must be initialized",
            .{ .help = "Add '= value' to initialize the constant, or use 'let' if you need to assign it later." },
        );
    } else if (is_using) {
        try parser.report(
            id_span,
            try parser.fmt("'{s}' declarations must be initialized", .{kind.toString()}),
            .{ .help = "Disposable resources require an initial value that implements the dispose protocol." },
        );
    }

    return try parser.tree.createNode(
        .{ .variable_declarator = .{ .id = id, .init = init, .definite = definite } },
        .{ .start = start, .end = end },
    );
}

/// returns `true` when `tag` can begin a `BindingIdentifier` or destructuring
/// pattern, i.e. the head of a variable declarator.
pub fn canStartBinding(tag: TokenTag) bool {
    return tag.isIdentifierLike() or tag == .left_bracket or tag == .left_brace;
}

/// determines if 'let' should be parsed as an identifier rather than a variable declaration keyword.
pub fn isLetIdentifier(parser: *Parser) Error!?bool {
    std.debug.assert(parser.current_token.tag == .let);

    const next = try parser.peekAhead() orelse return null;

    // 'let' followed by a semicolon should be parsed as an identifier, not a declaration.
    if (next.tag == .semicolon) {
        return true;
    }

    // in single-statement contexts (eg, if/while bodies), parse `let` as an identifier
    // when the next token cannot start a lexical binding.
    if (parser.context.in_single_statement_context and !canStartBinding(next.tag)) return true;

    return false;
}

/// returns whether the current `using` token is an `IdentifierReference`
/// (expression path) or the contextual keyword for a declaration.
///
/// implements the cover-grammar disambiguation from:
/// - CoverAwaitExpressionAndAwaitUsingDeclarationHead
pub fn isUsingIdentifier(parser: *Parser) Error!?bool {
    std.debug.assert(parser.current_token.tag == .using);

    const next = try parser.peekAhead() orelse return null;

    // if next token starts a BindingList and ASI cannot insert a semicolon,
    // treat `using` as the contextual keyword (declaration form).
    if (next.tag.isIdentifierLike() and !parser.canInsertImplicitSemicolon(next)) {
        return false;
    }

    return true; // `using` is an identifier
}
