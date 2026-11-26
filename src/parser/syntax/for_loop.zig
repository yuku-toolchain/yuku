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

/// https://tc39.es/ecma262/#sec-for-statement
/// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements
pub noinline fn parseForStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.@"for") orelse return null;

    const is_for_await = parser.at(.await);
    if (is_for_await) {
        if (!parser.context.await) try parser.report(
            parser.current_token.span,
            "'for await' loops are only allowed within async functions and at the top levels" ++
                " of modules",
            .{},
        );
        try parser.advance() orelse return null;
    }

    if (!try parser.expect(
        .left_paren,
        if (is_for_await) "Expected '(' after 'for await'" else "Expected '(' after 'for'",
        null,
    )) return null;

    if (parser.at(.semicolon)) return parseForStatementRest(parser, start, .null, is_for_await);

    const decl_start = parser.current_token.span.start;
    const declared: ?ast.VariableKind = switch (parser.current_token.tag) {
        .@"const" => .@"const",
        .@"var" => .@"var",
        .let => if (variables.isLetIdentifier(parser)) null else .let,
        .using => blk: {
            const next = parser.peekAhead();
            // [+Using] using [no LineTerminator here] ForBinding
            if (next.hasLineTerminatorBefore() or
                !variables.canStartBindingIdentifier(next.tag)) break :blk null;
            if (next.tag != .of) break :blk .using;

            // `for (using of x)` is an expression head, `using of = x` a binding
            break :blk switch (parser.peekSecond().tag) {
                .assign, .semicolon, .colon => .using,
                else => null,
            };
        },
        .await => blk: {
            if (!variables.isAwaitUsingDeclarationAhead(parser)) break :blk null;
            try parser.advance() orelse return null;
            break :blk .await_using;
        },
        else => null,
    };
    const kind = declared orelse return parseForWithExpression(parser, start, is_for_await);
    try parser.advance() orelse return null;

    const list = parser.list();
    defer list.deinit();
    var end: u32 = undefined;
    while (true) {
        const declarator = try @call(
            .always_inline,
            variables.parseVariableDeclarator,
            .{ parser, kind, .for_loop },
        ) orelse return null;
        try list.append(declarator);
        end = parser.tree.span(declarator).end;

        if (!(try parser.eat(.comma) orelse return null)) break;
    }

    const declarators = try list.finish();
    const decl_span: ast.Span = .{ .start = decl_start, .end = end };
    const decl = try parser.tree.addNode(
        .{ .variable_declaration = .{ .declarators = declarators, .kind = kind } },
        decl_span,
    );

    switch (parser.current_token.tag) {
        inline .in, .of => |tag| {
            if (tag == .in and (kind == .using or kind == .await_using)) try parser.report(
                decl_span,
                try parser.fmt(
                    "The left-hand side of a 'for...in' statement cannot be {s} '{s}'" ++
                        " declaration",
                    .{ if (kind == .using) "a" else "an", kind.toString() },
                ),
                .{ .help = "Use a 'for...of' statement instead" },
            );
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
            const span = parser.tree.span(expr);
            if (parser.tree.data(expr) == .assignment_expression) try parser.report(
                span,
                "The left-hand side of a 'for..." ++ @tagName(tag) ++
                    "' statement must be a variable or a property access",
                .{},
            );
            if (tag == .of) {
                // for ( [lookahead ∉ { let }] LeftHandSideExpression of ... )
                if (head_is_let) try parser.report(
                    head.span,
                    "The left-hand side of a 'for...of' statement may not start with 'let'",
                    .{ .help = "Wrap 'let' in parentheses, such as 'for ((let).x of y)'" },
                );
                // for ( [lookahead ∉ { async of }] LeftHandSideExpression of ... )
                // raw source, escaped `async` must not match
                if (!is_for_await and parser.tree.data(expr) == .identifier_reference and
                    std.mem.eql(u8, parser.spanText(span), "async")) try parser.report(
                    span,
                    "The left-hand side of a 'for...of' statement may not be 'async'",
                    .{ .help = "Wrap 'async' in parentheses, such as 'for ((async) of y)'" },
                );
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
    if (is_for_await) return parser.failExpected(
        "Expected 'of' in the 'for await' loop",
        .{ .help = "'for await' only works with 'for...of' loops" },
    );

    if (!try parser.expect(.semicolon, "Expected ';' after the loop initializer", null)) {
        return null;
    }
    const test_expr: ast.NodeIndex = if (parser.at(.semicolon))
        .null
    else
        try expressions.parseFull(parser) orelse return null;

    if (!try parser.expect(.semicolon, "Expected ';' after the loop condition", null)) return null;
    const update: ast.NodeIndex = if (parser.at(.right_paren))
        .null
    else
        try expressions.parseFull(parser) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after the loop update", null)) return null;

    const body = try statements.parseSingleStatement(parser) orelse return null;
    return try parser.finishAt(start, body, .{ .for_statement = .{
        .init = init,
        .@"test" = test_expr,
        .update = update,
        .body = body,
    } });
}

fn parseForInOfStatementRest(
    parser: *Parser,
    start: u32,
    left: ast.NodeIndex,
    is_for_await: bool,
    comptime tag: TokenTag,
) Error!?ast.NodeIndex {
    if (tag == .in and is_for_await) return parseForStatementRest(parser, start, left, true);

    try parser.advance() orelse return null;
    const right = try expressions.parseExpression(
        parser,
        if (tag == .of) Precedence.Assignment else Precedence.Lowest,
        .{},
    ) orelse return null;
    if (!try parser.expect(
        .right_paren,
        "Expected ')' after the 'for..." ++ @tagName(tag) ++ "' expression",
        null,
    )) return null;

    const body = try statements.parseSingleStatement(parser) orelse return null;
    return try parser.finishAt(start, body, if (tag == .in)
        .{ .for_in_statement = .{ .left = left, .right = right, .body = body } }
    else
        .{ .for_of_statement = .{
            .left = left,
            .right = right,
            .body = body,
            .await = is_for_await,
        } });
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

        return parser.report(
            parser.tree.span(declarator),
            "The variable declaration of a 'for..." ++ @tagName(tag) ++
                "' statement cannot have an initializer",
            .{ .help = "Remove the initializer" },
        );
    }
}

inline fn validateRegularForDeclarators(
    parser: *Parser,
    declarators: ast.IndexRange,
    kind: ast.VariableKind,
) Error!bool {
    for (parser.tree.extra(declarators)) |declarator| {
        const data = parser.tree.data(declarator).variable_declarator;
        if (data.init != .null) continue;

        const message: []const u8, const help: []const u8 =
            if (parser.tree.data(data.id) != .binding_identifier) .{
                "A destructuring declaration in a 'for' loop head must have an initializer",
                "Add an initializer with the value to destructure",
            } else if (kind == .@"const") .{
                "'const' declarations in a 'for' loop head must be initialized",
                "Add an initializer, or use 'let' to assign the value later",
            } else if (kind == .using or kind == .await_using) .{
                try parser.fmt(
                    "'{s}' declarations in a 'for' loop head must be initialized",
                    .{kind.toString()},
                ),
                "Add an initializer with the resource to dispose",
            } else continue;

        try parser.report(parser.tree.span(data.id), message, .{ .help = help });
        return false;
    }
    return true;
}
