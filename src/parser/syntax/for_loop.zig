const std = @import("std");
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const expressions = @import("expressions.zig");
const variables = @import("variables.zig");
const grammar = @import("../grammar.zig");
const statements = @import("statements.zig");

/// https://tc39.es/ecma262/#sec-for-statement
/// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements
pub fn parseForStatement(parser: *Parser, is_for_await: bool) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"for");
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'for'

    if (parser.current_token.tag == .await) {
        if (!parser.context.await) {
            try parser.report(
                parser.current_token.span,
                "'for await' is only valid in async functions or modules",
                .{},
            );
        }

        try parser.advance() orelse return null; // consume 'await'

        if (!try parser.expect(.left_paren, "Expected '(' after 'for await'", null)) return null;

        return parseForHead(parser, start, true);
    }

    if (!try parser.expect(.left_paren, "Expected '(' after 'for'", null)) return null;
    return parseForHead(parser, start, is_for_await);
}

fn parseForHead(parser: *Parser, start: u32, is_for_await: bool) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .semicolon) {
        return parseForStatementRest(parser, start, .null, is_for_await);
    }

    const decl_start = parser.current_token.span.start;

    switch (parser.current_token.tag) {
        .@"const", .@"var" => {
            const kind: ast.VariableKind = switch (parser.current_token.tag) {
                .@"const" => .@"const",
                .@"var" => .@"var",
                else => unreachable,
            };

            try parser.advance() orelse return null;

            return parseForWithDeclaration(parser, start, is_for_await, kind, decl_start);
        },
        .let => {
            const next = parser.peekAhead() orelse return null;

            // `let` heads a declaration only when a binding can follow.
            if (!variables.canStartLetBinding(next.tag)) {
                return parseForWithExpression(parser, start, is_for_await);
            }

            try parser.advance() orelse return null;

            return parseForWithDeclaration(parser, start, is_for_await, .let, decl_start);
        },
        .using => {
            const next = parser.peekAhead() orelse return null;

            // [+Using] using [no LineTerminator here] ForBinding
            if (next.hasLineTerminatorBefore() or
                !variables.canStartBindingIdentifier(next.tag))
            {
                return parseForWithExpression(parser, start, is_for_await);
            }

            // `using of` binds `of` only when `=`, `;`, or `:` follows,
            // else `using` is the expression of a for-of loop
            if (next.tag == .of) {
                const after_of = blk: {
                    var peek = parser.beginPeek();
                    defer peek.end();
                    _ = peek.next();
                    break :blk peek.next() orelse return null;
                };

                const of_is_binding = after_of.tag == .assign or
                    after_of.tag == .semicolon or
                    after_of.tag == .colon;
                if (!of_is_binding) {
                    return parseForWithExpression(parser, start, is_for_await);
                }
            }

            try parser.advance() orelse return null;
            return parseForWithDeclaration(parser, start, is_for_await, .using, decl_start);
        },
        .await => {
            const is_declaration = try variables.isAwaitUsingDeclarationAhead(parser) orelse
                return null;

            if (!is_declaration) {
                return parseForWithExpression(parser, start, is_for_await);
            }

            try parser.advance() orelse return null; // consume 'await'
            try parser.advance() orelse return null; // consume 'using'

            return parseForWithDeclaration(parser, start, is_for_await, .await_using, decl_start);
        },
        else => return parseForWithExpression(parser, start, is_for_await),
    }
}

/// for loop starting with `var`/`let`/`const`/`using`/`await using`
fn parseForWithDeclaration(
    parser: *Parser,
    start: u32,
    is_for_await: bool,
    kind: ast.VariableKind,
    decl_start: u32,
) Error!?ast.NodeIndex {
    const saved_allow_in = parser.context.in;
    parser.context.in = false;
    defer parser.context.in = saved_allow_in;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    const first = try variables.parseVariableDeclarator(parser, kind, .for_loop) orelse
        return null;
    try parser.scratch_a.append(parser.allocator(), first);

    var end = parser.tree.span(first).end;

    while (parser.current_token.tag == .comma) {
        try parser.advance() orelse return null;

        const declarator = try variables.parseVariableDeclarator(parser, kind, .for_loop) orelse
            return null;
        try parser.scratch_a.append(parser.allocator(), declarator);

        end = parser.tree.span(declarator).end;
    }

    const declarators = try parser.flushToExtras(&parser.scratch_a, checkpoint);

    parser.context.in = saved_allow_in;

    const decl = try parser.tree.addNode(.{
        .variable_declaration = .{
            .declarators = declarators,
            .kind = kind,
        },
    }, .{ .start = decl_start, .end = end });

    if (parser.current_token.tag == .in) {
        if (kind == .using or kind == .await_using) {
            try parser.report(
                .{ .start = decl_start, .end = end },
                try parser.fmt(
                    "The left-hand side of a for...in statement cannot be an {s} declaration.",
                    .{kind.toString()},
                ),
                .{ .help = "Did you mean to use a for...of statement?" },
            );
        }
        return parseForInStatementRest(parser, start, decl, is_for_await);
    }

    if (parser.current_token.tag == .of) {
        return parseForOfStatementRest(parser, start, decl, is_for_await);
    }

    if (!try validateRegularForDeclarators(parser, declarators, kind)) return null;

    return parseForStatementRest(parser, start, decl, is_for_await);
}

/// for loop starting with an expression.
fn parseForWithExpression(parser: *Parser, start: u32, is_for_await: bool) Error!?ast.NodeIndex {
    // escaped `let` is a different token sequence, exempt from the
    // for-of lookahead restriction below
    const head = parser.current_token;
    const head_is_let = head.tag == .let and !head.isEscaped();

    const saved_allow_in = parser.context.in;
    parser.context.in = false;

    const expr = try expressions.parseExpression(
        parser,
        Precedence.Lowest,
        .{ .respect_allow_in = true },
    ) orelse {
        parser.context.in = saved_allow_in;
        return null;
    };

    parser.context.in = saved_allow_in;

    if ((parser.current_token.tag == .in or parser.current_token.tag == .of) and
        parser.tree.data(expr) == .assignment_expression)
    {
        try parser.report(
            parser.tree.span(expr),
            "The left-hand side of a for-in/of statement cannot be an assignment",
            .{},
        );
    }

    if (parser.current_token.tag == .in) {
        try grammar.expressionToPattern(parser, expr, .assignable);

        return parseForInStatementRest(parser, start, expr, is_for_await);
    }

    if (parser.current_token.tag == .of) {
        // for ( [lookahead ∉ { let }] LeftHandSideExpression of AssignmentExpression )
        if (head_is_let) {
            try parser.report(
                head.span,
                "The left-hand side of a for-of statement may not start with 'let'",
                .{ .help = "Rename the variable or wrap it in parentheses:" ++
                    " 'for ((let).x of ...)'" },
            );
        }

        // for ( [lookahead ∉ { async of }] LeftHandSideExpression of AssignmentExpression )
        if (!is_for_await and isAsyncIdentifier(parser, expr)) {
            try parser.report(
                parser.tree.span(expr),
                "'for (async of ...)' is not allowed, it is ambiguous with 'for await'",
                .{ .help = "Use a different variable name or add parentheses:" ++
                    " 'for ((async) of ...)'" },
            );
        }

        try grammar.expressionToPattern(parser, expr, .assignable);

        return parseForOfStatementRest(parser, start, expr, is_for_await);
    }

    return parseForStatementRest(parser, start, expr, is_for_await);
}

/// for(init; test; update) body
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

    if (!try parser.expect(.semicolon, "Expected ';' after for-loop condition", null)) {
        return null;
    }

    var update: ast.NodeIndex = .null;
    if (parser.current_token.tag != .right_paren) {
        update = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
            return null;
    }

    if (!try parser.expect(.right_paren, "Expected ')' after for-loop update", null)) {
        return null;
    }

    const body = try statements.parseStatement(
        parser,
        .{ .can_be_single_statement_context = true },
    ) orelse return null;

    return try parser.tree.addNode(.{
        .for_statement = .{
            .init = init,
            .@"test" = test_expr,
            .update = update,
            .body = body,
        },
    }, .{ .start = start, .end = parser.tree.span(body).end });
}

/// for(left in right) body
fn parseForInStatementRest(
    parser: *Parser,
    start: u32,
    left: ast.NodeIndex,
    is_for_await: bool,
) Error!?ast.NodeIndex {
    if (is_for_await) {
        try parser.report(
            parser.current_token.span,
            "'for await' requires 'of', not 'in'",
            .{},
        );
        return null;
    }

    try parser.advance() orelse return null; // consume 'in'
    const right = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
        return null;
    if (!try parser.expect(.right_paren, "Expected ')' after for-in expression", null)) {
        return null;
    }

    const body = try statements.parseStatement(
        parser,
        .{ .can_be_single_statement_context = true },
    ) orelse return null;

    return try parser.tree.addNode(.{
        .for_in_statement = .{
            .left = left,
            .right = right,
            .body = body,
        },
    }, .{ .start = start, .end = parser.tree.span(body).end });
}

/// for(left of right) body
fn parseForOfStatementRest(
    parser: *Parser,
    start: u32,
    left: ast.NodeIndex,
    is_for_await: bool,
) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume 'of'

    const right = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
        return null;

    if (!try parser.expect(.right_paren, "Expected ')' after for-of expression", null)) {
        return null;
    }

    const body = try statements.parseStatement(
        parser,
        .{ .can_be_single_statement_context = true },
    ) orelse return null;

    return try parser.tree.addNode(.{
        .for_of_statement = .{
            .left = left,
            .right = right,
            .body = body,
            .await = is_for_await,
        },
    }, .{ .start = start, .end = parser.tree.span(body).end });
}

fn isAsyncIdentifier(parser: *Parser, expr: ast.NodeIndex) bool {
    if (parser.tree.data(expr) != .identifier_reference) return false;

    // compare raw source text, escaped `\u0061sync` should not match
    const span = parser.tree.span(expr);
    return std.mem.eql(u8, parser.source[span.start..span.end], "async");
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
