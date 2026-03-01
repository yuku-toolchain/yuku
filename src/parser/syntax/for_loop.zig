const std = @import("std");
const ast = @import("../ast.zig");
const TokenTag = @import("../token.zig").TokenTag;
const Precedence = @import("../token.zig").Precedence;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const literals = @import("literals.zig");
const expressions = @import("expressions.zig");
const patterns = @import("patterns.zig");
const grammar = @import("../grammar.zig");
const statements = @import("statements.zig");

/// https://tc39.es/ecma262/#sec-for-statement
/// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements
pub fn parseForStatement(parser: *Parser, is_for_await: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'for'

    if (parser.current_token.tag == .await) {
        if (!parser.context.await_is_keyword) {
            try parser.report(parser.current_token.span, "'for await' is only valid in async functions or modules", .{});
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
        return parseForStatementRest(parser, start, ast.null_node, is_for_await);
    }

    const decl_start = parser.current_token.span.start;

    switch (parser.current_token.tag) {
        .let, .@"const", .@"var" => {
            const kind: ast.VariableKind = switch (parser.current_token.tag) {
                .let => .let,
                .@"const" => .@"const",
                .@"var" => .@"var",
                else => unreachable,
            };

            try parser.advance() orelse return null;

            return parseForWithDeclaration(parser, start, is_for_await, kind, decl_start);
        },
        .using => {
            const next = try parser.lookAhead() orelse return null;

            // [+Using] using [no LineTerminator here] ForBinding
            if (next.hasLineTerminatorBefore()) {
                return parseForWithExpression(parser, start, is_for_await);
            }

            switch (next.tag) {
                // `using.`, `using(`, `using[` are expression forms where `using` is an identifier.
                //
                // `using` doesn't support destructuring, so `for (using {} = ...;;)` is invalid.
                // by returning null (not a declaration), the caller produces a natural "expected ';', found '{'" error.
                .in, .dot, .left_paren, .left_bracket, .left_brace => {
                    return parseForWithExpression(parser, start, is_for_await);
                },
                // `using of` is ambiguous because `of` is a valid identifier:
                //   `for (using of expr)` -> for-of loop, `using` is the expression
                //   `for (using of = 1;;)` -> `using` declaration, `of` is the variable name
                .of => {
                    const using_identifier = try literals.parseIdentifier(parser) orelse return null;

                    const after_of = try parser.lookAhead() orelse return null;

                    if (after_of.tag == .assign or after_of.tag == .semicolon or after_of.tag == .colon) {
                        return parseForWithDeclaration(parser, start, is_for_await, .using, decl_start);
                    }

                    try grammar.expressionToPattern(parser, using_identifier, .assignable);

                    return parseForOfStatementRest(parser, start, using_identifier, is_for_await);
                },
                else => {
                    try parser.advance() orelse return null;
                    return parseForWithDeclaration(parser, start, is_for_await, .using, decl_start);
                },
            }
        },
        .await => {
            const next = try parser.lookAhead() orelse return null;

            if (next.tag != .using or next.hasLineTerminatorBefore()) return parseForWithExpression(parser, start, is_for_await);

            try parser.advance() orelse return null; // consume 'await'
            try parser.advance() orelse return null; // consume 'using'

            return parseForWithDeclaration(parser, start, is_for_await, .await_using, decl_start);
        },
        else => return parseForWithExpression(parser, start, is_for_await),
    }
}

/// for loop starting with a variable declaration (var/let/const/using/await using).
fn parseForWithDeclaration(parser: *Parser, start: u32, is_for_await: bool, kind: ast.VariableKind, decl_start: u32) Error!?ast.NodeIndex {
    const first = try parseForLoopDeclarator(parser) orelse return null;
    const first_end = parser.getSpan(first).end;

    // for-in / for-of: single declarator
    if (parser.current_token.tag == .in) {
        if(kind == .using or kind == .await_using) {
            try parser.reportFmt(
                .{ .start = decl_start, .end = first_end },
                "The left-hand side of a for...in statement cannot be an {s} declaration.",
                .{kind.toString()},
                .{ .help = "Did you mean to use a for...of statement?" });
        }

        const decl = try createSingleDeclaration(parser, kind, first, decl_start, first_end);

        return parseForInStatementRest(parser, start, decl, is_for_await);
    }

    if (parser.current_token.tag == .of) {
        const decl = try createSingleDeclaration(parser, kind, first, decl_start, first_end);
        return parseForOfStatementRest(parser, start, decl, is_for_await);
    }

    if (!try validateRegularForDeclarator(parser, first, kind)) return null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    try parser.scratch_a.append(parser.allocator(), first);

    var end = first_end;

    while (parser.current_token.tag == .comma) {
        try parser.advance() orelse return null;

        const declarator = try parseForLoopDeclarator(parser) orelse return null;

        if (!try validateRegularForDeclarator(parser, declarator, kind)) return null;

        try parser.scratch_a.append(parser.allocator(), declarator);

        end = parser.getSpan(declarator).end;
    }

    const decl = try parser.addNode(.{
        .variable_declaration = .{
            .declarators = try parser.addExtraFromScratch(&parser.scratch_a, checkpoint),
            .kind = kind,
        },
    }, .{ .start = decl_start, .end = end });

    return parseForStatementRest(parser, start, decl, is_for_await);
}

/// for loop starting with an expression.
fn parseForWithExpression(parser: *Parser, start: u32, is_for_await: bool) Error!?ast.NodeIndex {
    const saved_allow_in = parser.context.allow_in;
    parser.context.allow_in = false;

    const expr = try expressions.parseExpression(parser, Precedence.Lowest, .{ .respect_allow_in = true }) orelse {
        parser.context.allow_in = saved_allow_in;
        return null;
    };

    parser.context.allow_in = saved_allow_in;

    if (parser.current_token.tag == .in) {
        try grammar.expressionToPattern(parser, expr, .assignable);

        return parseForInStatementRest(parser, start, expr, is_for_await);
    }

    if (parser.current_token.tag == .of) {
        // for ( [lookahead ∉ { async of }] LeftHandSideExpression of AssignmentExpression )
        if (!is_for_await and isAsyncIdentifier(parser, expr)) {
            try parser.report(parser.getSpan(expr), "'for (async of ...)' is not allowed, it is ambiguous with 'for await'", .{
                .help = "Use a different variable name or add parentheses: 'for ((async) of ...)'",
            });
        }

        try grammar.expressionToPattern(parser, expr, .assignable);

        return parseForOfStatementRest(parser, start, expr, is_for_await);
    }

    return parseForStatementRest(parser, start, expr, is_for_await);
}

/// for(init; test; update) body
fn parseForStatementRest(parser: *Parser, start: u32, init: ast.NodeIndex, is_for_await: bool) Error!?ast.NodeIndex {
    if (is_for_await) {
        try parser.report(parser.current_token.span, "'for await' is only valid with for-of loops", .{});
        return null;
    }

    if (!try parser.expect(.semicolon, "Expected ';' after for-loop init", null)) return null;

    var test_expr: ast.NodeIndex = ast.null_node;
    if (parser.current_token.tag != .semicolon) {
        test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    }

    if (!try parser.expect(.semicolon, "Expected ';' after for-loop condition", null)) return null;

    var update: ast.NodeIndex = ast.null_node;
    if (parser.current_token.tag != .right_paren) {
        update = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    }

    if (!try parser.expect(.right_paren, "Expected ')' after for-loop update", null)) return null;

    const body = try statements.parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .for_statement = .{
            .init = init,
            .@"test" = test_expr,
            .update = update,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// for(left in right) body
fn parseForInStatementRest(parser: *Parser, start: u32, left: ast.NodeIndex, is_for_await: bool) Error!?ast.NodeIndex {
    if (is_for_await) {
        try parser.report(parser.current_token.span, "'for await' requires 'of', not 'in'", .{});
        return null;
    }

    try parser.advance() orelse return null; // consume 'in'
    const right = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    if (!try parser.expect(.right_paren, "Expected ')' after for-in expression", null)) return null;

    const body = try statements.parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .for_in_statement = .{
            .left = left,
            .right = right,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// for(left of right) body
fn parseForOfStatementRest(parser: *Parser, start: u32, left: ast.NodeIndex, is_for_await: bool) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume 'of'

    const right = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after for-of expression", null)) return null;

    const body = try statements.parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .for_of_statement = .{
            .left = left,
            .right = right,
            .body = body,
            .await = is_for_await,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

fn parseForLoopDeclarator(parser: *Parser) Error!?ast.NodeIndex {
    const decl_start = parser.current_token.span.start;
    const id = try patterns.parseBindingPattern(parser) orelse return null;

    var init: ast.NodeIndex = ast.null_node;
    var end = parser.getSpan(id).end;

    if (parser.current_token.tag == .assign) {
        try parser.advance() orelse return null;
        init = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        end = parser.getSpan(init).end;
    }

    return try parser.addNode(.{ .variable_declarator = .{ .id = id, .init = init } }, .{ .start = decl_start, .end = end });
}

fn createSingleDeclaration(parser: *Parser, kind: ast.VariableKind, declarator: ast.NodeIndex, decl_start: u32, decl_end: u32) Error!ast.NodeIndex {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);
    try parser.scratch_a.append(parser.allocator(), declarator);

    return try parser.addNode(.{
        .variable_declaration = .{
            .declarators = try parser.addExtraFromScratch(&parser.scratch_a, checkpoint),
            .kind = kind,
        },
    }, .{ .start = decl_start, .end = decl_end });
}

fn isAsyncIdentifier(parser: *Parser, expr: ast.NodeIndex) bool {
    const data = parser.getData(expr);

    if (data != .identifier_reference) return false;

    const id = data.identifier_reference;

    return id.name_len == 5 and std.mem.eql(u8, parser.getSourceText(id.name_start, id.name_len), "async");
}

/// in a regular for-loop, destructuring patterns and const declarations require an initializer.
fn validateRegularForDeclarator(parser: *Parser, declarator: ast.NodeIndex, kind: ast.VariableKind) Error!bool {
    const data = parser.getData(declarator).variable_declarator;

    if (!ast.isNull(data.init)) return true;

    if (parser.getData(data.id) != .binding_identifier) {
        try parser.report(parser.getSpan(data.id), "Destructuring declaration in for loop initializer must be initialized", .{
            .help = "Add '= value' to provide the object or array to destructure from.",
        });
        return false;
    }

    if (kind == .@"const") {
        try parser.report(parser.getSpan(data.id), "'const' declarations in for loop initializer must be initialized", .{
            .help = "Add '= value' to initialize the constant in the for loop.",
        });
        return false;
    }

    return true;
}
