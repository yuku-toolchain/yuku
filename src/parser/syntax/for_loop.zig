const std = @import("std");
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;
const TokenTag = @import("../token.zig").TokenTag;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const expressions = @import("expressions.zig");
const variables = @import("variables.zig");
const grammar = @import("../grammar.zig");
const statements = @import("statements.zig");
const static_semantics = @import("spec").static_semantics;

/// Parses `for`, `for-in`, `for-of`, and `for await` statements.
/// https://tc39.es/ecma262/#sec-for-statement
/// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements
pub fn parseForStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"for");
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const is_for_await = parser.current_token.tag == .await;
    if (is_for_await) {
        if (!parser.context.await) {
            try parser.report(
                parser.current_token.span,
                "'for await' is only valid in async functions or modules",
                .{},
            );
        }
        try parser.advance() orelse return null;
    }

    if (!try parser.expect(
        .left_paren,
        if (is_for_await) "Expected '(' after 'for await'" else "Expected '(' after 'for'",
        null,
    )) return null;

    if (parser.current_token.tag == .semicolon) {
        return parseForStatementRest(parser, start, .null, is_for_await);
    }

    const decl_start = parser.current_token.span.start;
    const kind: ast.VariableKind = switch (parser.current_token.tag) {
        .@"const" => .@"const",
        .@"var" => .@"var",
        .let => if (variables.isLetIdentifier(parser))
            return parseForWithExpression(parser, start, is_for_await)
        else
            .let,
        .using => blk: {
            const next = parser.peekAhead();

            // [+Using] using [no LineTerminator here] ForBinding
            if (next.hasLineTerminatorBefore() or
                !variables.canStartBindingIdentifier(next.tag))
            {
                return parseForWithExpression(parser, start, is_for_await);
            }

            // `for (using of x)` is an expression head, `using of = x` a binding
            if (next.tag == .of) {
                var peek = parser.beginPeek();
                _ = peek.next();
                const after_of = peek.next().tag;
                peek.end();
                if (after_of != .assign and after_of != .semicolon and after_of != .colon) {
                    return parseForWithExpression(parser, start, is_for_await);
                }
            }
            break :blk .using;
        },
        .await => blk: {
            if (!variables.isAwaitUsingDeclarationAhead(parser)) {
                return parseForWithExpression(parser, start, is_for_await);
            }
            try parser.advance() orelse return null;
            break :blk .await_using;
        },
        else => return parseForWithExpression(parser, start, is_for_await),
    };
    try parser.advance() orelse return null;
    return parseForWithDeclaration(parser, start, is_for_await, kind, decl_start);
}

fn parseForWithDeclaration(
    parser: *Parser,
    start: u32,
    is_for_await: bool,
    kind: ast.VariableKind,
    decl_start: u32,
) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    var end: u32 = undefined;
    while (true) {
        const declarator = try variables.parseVariableDeclarator(parser, kind, .for_loop) orelse
            return null;
        try parser.scratch_a.append(parser.allocator(), declarator);
        end = parser.tree.span(declarator).end;

        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return null;
    }

    const declarators = try parser.flushToExtras(&parser.scratch_a, checkpoint);
    const decl = try parser.tree.addNode(
        .{ .variable_declaration = .{ .declarators = declarators, .kind = kind } },
        .{ .start = decl_start, .end = end },
    );

    switch (parser.current_token.tag) {
        inline .in, .of => |tag| {
            if (tag == .in and (kind == .using or kind == .await_using)) {
                try parser.report(
                    .{ .start = decl_start, .end = end },
                    try parser.fmt(
                        "The left-hand side of a for...in statement cannot be an {s} declaration.",
                        .{kind.toString()},
                    ),
                    .{ .help = "Did you mean to use a for...of statement?" },
                );
            }
            try validateForInOfDeclarators(parser, declarators, kind, tag);
            return parseForInOfStatementRest(parser, start, decl, is_for_await, tag);
        },
        else => {
            if (!try validateRegularForDeclarators(parser, declarators, kind)) return null;
            return parseForStatementRest(parser, start, decl, is_for_await);
        },
    }
}

fn parseForWithExpression(parser: *Parser, start: u32, is_for_await: bool) Error!?ast.NodeIndex {
    // escaped `let` is exempt from the for-of lookahead restriction
    const head = parser.current_token;
    const head_is_let = head.tag == .let and !head.isEscaped();

    const expr = try expressions.parseExpression(
        parser,
        Precedence.Lowest,
        .{ .allow_in = false },
    ) orelse return null;

    switch (parser.current_token.tag) {
        inline .in, .of => |tag| {
            if (parser.tree.data(expr) == .assignment_expression) {
                try parser.report(
                    parser.tree.span(expr),
                    "The left-hand side of a for-in/of statement cannot be an assignment",
                    .{},
                );
            }
            if (tag == .of) {
                // for ( [lookahead ∉ { let }] LeftHandSideExpression of ... )
                if (head_is_let) {
                    try parser.report(
                        head.span,
                        "The left-hand side of a for-of statement may not start with 'let'",
                        .{ .help = "Rename the variable or wrap it in parentheses:" ++
                            " 'for ((let).x of ...)'" },
                    );
                }
                // for ( [lookahead ∉ { async of }] LeftHandSideExpression of ... )
                if (!is_for_await and isAsyncIdentifier(parser, expr)) {
                    try parser.report(
                        parser.tree.span(expr),
                        "'for (async of ...)' is not allowed, it is ambiguous with 'for await'",
                        .{ .help = "Use a different variable name or add parentheses:" ++
                            " 'for ((async) of ...)'" },
                    );
                }
            }
            try grammar.expressionToPattern(parser, expr, .assignable);
            return parseForInOfStatementRest(parser, start, expr, is_for_await, tag);
        },
        else => return parseForStatementRest(parser, start, expr, is_for_await),
    }
}

fn parseForStatementRest(
    parser: *Parser,
    start: u32,
    init: ast.NodeIndex,
    is_for_await: bool,
) Error!?ast.NodeIndex {
    if (is_for_await) {
        try parser.report(
            parser.current_token.span,
            "'for await' is only valid with for-of loops",
            .{},
        );
        return null;
    }

    if (!try parser.expect(.semicolon, "Expected ';' after for-loop init", null)) return null;

    var test_expr: ast.NodeIndex = .null;
    if (parser.current_token.tag != .semicolon) {
        test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
            return null;
    }

    if (!try parser.expect(.semicolon, "Expected ';' after for-loop condition", null)) return null;

    var update: ast.NodeIndex = .null;
    if (parser.current_token.tag != .right_paren) {
        update = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
            return null;
    }

    if (!try parser.expect(.right_paren, "Expected ')' after for-loop update", null)) return null;

    const body = try statements.parseSingleStatement(parser) orelse return null;

    return try parser.tree.addNode(.{ .for_statement = .{
        .init = init,
        .@"test" = test_expr,
        .update = update,
        .body = body,
    } }, .{ .start = start, .end = parser.tree.span(body).end });
}

fn parseForInOfStatementRest(
    parser: *Parser,
    start: u32,
    left: ast.NodeIndex,
    is_for_await: bool,
    comptime tag: TokenTag,
) Error!?ast.NodeIndex {
    if (tag == .in and is_for_await) {
        try parser.report(parser.current_token.span, "'for await' requires 'of', not 'in'", .{});
        return null;
    }

    try parser.advance() orelse return null;
    const right = try expressions.parseExpression(
        parser,
        if (tag == .of) Precedence.Assignment else Precedence.Lowest,
        .{},
    ) orelse return null;
    if (!try parser.expect(
        .right_paren,
        if (tag == .of)
            "Expected ')' after for-of expression"
        else
            "Expected ')' after for-in expression",
        null,
    )) return null;

    const body = try statements.parseSingleStatement(parser) orelse return null;
    const span: ast.Span = .{ .start = start, .end = parser.tree.span(body).end };
    if (tag == .in) {
        return try parser.tree.addNode(
            .{ .for_in_statement = .{ .left = left, .right = right, .body = body } },
            span,
        );
    }
    return try parser.tree.addNode(.{ .for_of_statement = .{
        .left = left,
        .right = right,
        .body = body,
        .await = is_for_await,
    } }, span);
}

fn isAsyncIdentifier(parser: *Parser, expr: ast.NodeIndex) bool {
    if (parser.tree.data(expr) != .identifier_reference) return false;

    // raw source, escaped `\u0061sync` must not match
    const span = parser.tree.span(expr);
    return std.mem.eql(u8, parser.source[span.start..span.end], "async");
}

fn validateForInOfDeclarators(
    parser: *Parser,
    declarators: ast.IndexRange,
    kind: ast.VariableKind,
    comptime tag: TokenTag,
) Error!void {
    for (parser.tree.extra(declarators)) |declarator| {
        const data = parser.tree.data(declarator).variable_declarator;
        if (data.init == .null) continue;
        if (static_semantics.isAnnexBForInHead(&parser.tree, kind, data.id, tag == .in)) continue;

        try parser.report(
            parser.tree.span(declarator),
            if (tag == .in)
                "for-in loop variable declaration may not have an initializer"
            else
                "for-of loop variable declaration may not have an initializer",
            .{ .help = "Remove the initializer from the loop variable." },
        );
        return;
    }
}

fn validateRegularForDeclarators(
    parser: *Parser,
    declarators: ast.IndexRange,
    kind: ast.VariableKind,
) Error!bool {
    for (parser.tree.extra(declarators)) |declarator| {
        const data = parser.tree.data(declarator).variable_declarator;
        if (data.init != .null) continue;

        const id_span = parser.tree.span(data.id);

        if (parser.tree.data(data.id) != .binding_identifier) {
            try parser.report(
                id_span,
                "Destructuring declaration in for loop initializer must be initialized",
                .{ .help = "Add '= value' to provide the object or array to destructure from." },
            );
            return false;
        }

        if (kind == .@"const") {
            try parser.report(
                id_span,
                "'const' declarations in for loop initializer must be initialized",
                .{ .help = "Add '= value' to initialize the constant in the for loop." },
            );
            return false;
        }

        if (kind == .using or kind == .await_using) {
            try parser.report(
                id_span,
                try parser.fmt("'{s}' declarations must be initialized", .{kind.toString()}),
                .{ .help = "Disposable resources require an initial value that" ++
                    " implements the dispose protocol." },
            );
            return false;
        }
    }

    return true;
}
