const std = @import("std");
const ast = @import("../ast.zig");
const token = @import("../token.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Precedence = @import("../token.zig").Precedence;

const expressions = @import("expressions.zig");
const variables = @import("variables.zig");
const parenthesized = @import("parenthesized.zig");
const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const grammar = @import("../grammar.zig");
const modules = @import("modules.zig");

const ParseStatementOpts = struct {
    can_be_single_statement_context: bool = false,
};

pub fn parseStatement(parser: *Parser, opts: ParseStatementOpts) Error!?ast.NodeIndex {
    parser.context.in_single_statement_context = false;

    if (parser.current_token.type == .left_brace) {
        return parseBlockStatement(parser);
    }

    if (opts.can_be_single_statement_context) {
        parser.context.in_single_statement_context = true;
    }

    if (parser.current_token.type == .await) {
        const next = try parser.lookAhead() orelse return null;

        if (next.type == .using) {
            try parser.advance() orelse return null;
            return variables.parseVariableDeclaration(parser, true);
        }
    }

    if (parser.current_token.type == .import) {
        const next = try parser.lookAhead() orelse return null;

        if (next.type != .left_paren) {
            return modules.parseImportDeclaration(parser);
        }
    }

    if (parser.current_token.type == .async) {
        const next = try parser.lookAhead() orelse return null;

        if (next.type == .function and !next.has_line_terminator_before) {
            const start = parser.current_token.span.start;
            try parser.advance() orelse return null;
            return functions.parseFunction(parser, .{ .is_async = true }, start);
        }
    }

    const statement = switch (parser.current_token.type) {
        .@"var", .@"const", .let, .using => variables.parseVariableDeclaration(parser, false),
        .function => functions.parseFunction(parser, .{}, null),
        .class => class.parseClass(parser, .{}, null),
        .@"export" => modules.parseExportDeclaration(parser),
        .@"if" => parseIfStatement(parser),
        .@"switch" => parseSwitchStatement(parser),
        .@"for" => parseForStatement(parser, false),
        .@"while" => parseWhileStatement(parser),
        .do => parseDoWhileStatement(parser),
        .with => parseWithStatement(parser),
        .@"break" => parseBreakStatement(parser),
        .@"continue" => parseContinueStatement(parser),
        .@"return" => parseReturnStatement(parser),
        .throw => parseThrowStatement(parser),
        .@"try" => parseTryStatement(parser),
        .debugger => parseDebuggerStatement(parser),
        .semicolon => parseEmptyStatement(parser),
        else => parseExpressionStatementOrLabeledOrDirective(parser),
    };

    parser.context.in_single_statement_context = false;

    return statement;
}

fn parseExpressionStatementOrLabeledOrDirective(parser: *Parser) Error!?ast.NodeIndex {
    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    const expression_span = parser.getSpan(expression);
    const expression_data = parser.getData(expression);

    // labeled statement: identifier ':'
    if (expression_data == .identifier_reference and parser.current_token.type == .colon) {
        return parseLabeledStatement(parser, expression);
    }

    const start = expression_span.start;

    if (expression_data == .string_literal and parser.state.in_directive_prologue) {
        const value_start = expression_data.string_literal.raw_start + 1;
        const value_len: u16 = expression_data.string_literal.raw_len - 2;

        if (std.mem.eql(u8, parser.getSourceText(value_start, value_len), "use strict")) {
            parser.strict_mode = true;
            parser.lexer.strict_mode = true;
        }

        return try parser.addNode(.{
            .directive = .{
                .expression = expression,
                .value_start = value_start,
                .value_len = value_len,
            },
        }, .{ .start = start, .end = try parser.eatSemicolon(expression_span.end) orelse return null });
    } else if (parser.state.in_directive_prologue) {
        parser.state.in_directive_prologue = false;
    }

    return try parser.addNode(
        .{ .expression_statement = .{ .expression = expression } },
        .{ .start = start, .end = try parser.eatSemicolon(expression_span.end) orelse return null },
    );
}

/// https://tc39.es/ecma262/#sec-labelled-statements
fn parseLabeledStatement(parser: *Parser, identifier: ast.NodeIndex) Error!?ast.NodeIndex {
    const id_data = parser.getData(identifier);
    const id_span = parser.getSpan(identifier);

    // IdentifierReference to LabelIdentifier
    const label = try parser.addNode(.{
        .label_identifier = .{
            .name_start = id_data.identifier_reference.name_start,
            .name_len = id_data.identifier_reference.name_len,
        },
    }, id_span);

    try parser.advance() orelse return null; // consume ':'

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .labeled_statement = .{ .label = label, .body = body },
    }, .{ .start = id_span.start, .end = parser.getSpan(body).end });
}

/// https://tc39.es/ecma262/#prod-BlockStatement
pub fn parseBlockStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (!try parser.expect(
        .left_brace,
        "Expected '{' to start block statement",
        "Block statements must be enclosed in braces: { ... }",
    )) return null;

    const body = try parser.parseBody(.right_brace);

    const end = parser.current_token.span.end;

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close block statement",
        "Add a closing brace '}' to complete the block statement, or check for unbalanced braces inside.",
    )) return null;

    return try parser.addNode(.{ .block_statement = .{ .body = body } }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-switch-statement
pub fn parseSwitchStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'switch'

    if (!try parser.expect(.left_paren, "Expected '(' after 'switch'", null)) return null;

    const discriminant = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after switch expression", null)) return null;
    if (!try parser.expect(.left_brace, "Expected '{' to start switch body", null)) return null;

    const cases = try parseSwitchCases(parser);

    const end = parser.current_token.span.end;
    if (!try parser.expect(.right_brace, "Expected '}' to close switch body", null)) return null;

    return try parser.addNode(.{
        .switch_statement = .{
            .discriminant = discriminant,
            .cases = cases,
        },
    }, .{ .start = start, .end = end });
}

fn parseSwitchCases(parser: *Parser) Error!ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.type == .case or parser.current_token.type == .default) {
        const case_node = try parseSwitchCase(parser) orelse continue;
        try parser.scratch_a.append(parser.allocator(), case_node);
    }

    return parser.addExtra(parser.scratch_a.take(checkpoint));
}

fn parseSwitchCase(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const is_default = parser.current_token.type == .default;

    try parser.advance() orelse return null; // consume 'case' or 'default'

    var test_expr: ast.NodeIndex = ast.null_node;

    if (!is_default) {
        test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    }

    const colon_end = parser.current_token.span.end;
    if (!try parser.expect(.colon, "Expected ':' after case", null)) return null;

    const consequent = try parseCaseConsequent(parser);
    const end = if (consequent.len > 0)
        parser.getSpan(parser.getExtra(consequent)[consequent.len - 1]).end
    else
        colon_end;

    return try parser.addNode(.{
        .switch_case = .{
            .@"test" = test_expr,
            .consequent = consequent,
        },
    }, .{ .start = start, .end = end });
}

fn parseCaseConsequent(parser: *Parser) Error!ast.IndexRange {
    const checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(checkpoint);

    while (parser.current_token.type != .case and
        parser.current_token.type != .default and
        parser.current_token.type != .right_brace and
        parser.current_token.type != .eof)
    {
        if (try parseStatement(parser, .{})) |stmt| {
            try parser.scratch_b.append(parser.allocator(), stmt);
        } else {
            break;
        }
    }

    return parser.addExtra(parser.scratch_b.take(checkpoint));
}

/// https://tc39.es/ecma262/#sec-if-statement
pub fn parseIfStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'if'

    if (!try parser.expect(.left_paren, "Expected '(' after 'if'", null)) return null;

    const test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after if condition", null)) return null;

    const consequent = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    var end = parser.getSpan(consequent).end;
    var alternate: ast.NodeIndex = ast.null_node;

    if (parser.current_token.type == .@"else") {
        try parser.advance() orelse return null; // consume 'else'
        alternate = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;
        end = parser.getSpan(alternate).end;
    }

    return try parser.addNode(.{
        .if_statement = .{
            .@"test" = test_expr,
            .consequent = consequent,
            .alternate = alternate,
        },
    }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-while-statement
fn parseWhileStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'while'

    if (!try parser.expect(.left_paren, "Expected '(' after 'while'", null)) return null;

    const test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after while condition", null)) return null;

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .while_statement = .{
            .@"test" = test_expr,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// https://tc39.es/ecma262/#sec-do-while-statement
fn parseDoWhileStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'do'

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    if (!try parser.expect(.@"while", "Expected 'while' after do statement body", null)) return null;
    if (!try parser.expect(.left_paren, "Expected '(' after 'while'", null)) return null;

    const test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    const rparen_end = parser.current_token.span.end;
    if (!try parser.expect(.right_paren, "Expected ')' after while condition", null)) return null;

    const end = try parser.eatSemicolonLenient(rparen_end) orelse return null;

    return try parser.addNode(.{
        .do_while_statement = .{
            .body = body,
            .@"test" = test_expr,
        },
    }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-with-statement
fn parseWithStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (parser.strict_mode) {
        try parser.report(parser.current_token.span, "'with' statement is not allowed in strict mode", .{});
        return null;
    }

    try parser.advance() orelse return null; // consume 'with'

    if (!try parser.expect(.left_paren, "Expected '(' after 'with'", null)) return null;

    const object = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after with expression", null)) return null;

    const body = try parseStatement(parser, .{}) orelse return null;

    return try parser.addNode(.{
        .with_statement = .{
            .object = object,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// EmptyStatement: `;`
fn parseEmptyStatement(parser: *Parser) Error!?ast.NodeIndex {
    const span = parser.current_token.span;
    try parser.advance() orelse return null; // consume ';'
    return try parser.addNode(.empty_statement, span);
}

/// https://tc39.es/ecma262/#sec-break-statement
fn parseBreakStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume 'break'

    var label: ast.NodeIndex = ast.null_node;

    // break [no LineTerminator here] LabelIdentifier;
    if (!parser.canInsertSemicolon() and parser.current_token.type != .semicolon) {
        const label_node = try literals.parseLabelIdentifier(parser) orelse return null;
        label = label_node;
        end = parser.getSpan(label_node).end;
    }

    end = try parser.eatSemicolon(end) orelse return null;

    return try parser.addNode(.{ .break_statement = .{ .label = label } }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-continue-statement
fn parseContinueStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume 'continue'

    var label: ast.NodeIndex = ast.null_node;

    // continue [no LineTerminator here] LabelIdentifier;
    if (!parser.canInsertSemicolon() and parser.current_token.type != .semicolon) {
        const label_node = try literals.parseLabelIdentifier(parser) orelse return null;
        label = label_node;
        end = parser.getSpan(label_node).end;
    }

    end = try parser.eatSemicolon(end) orelse return null;

    return try parser.addNode(.{ .continue_statement = .{ .label = label } }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-for-statement
/// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements
fn parseForStatement(parser: *Parser, is_await: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'for'

    // check for `for await (...)`
    if (parser.current_token.type == .await) {
        if (!parser.context.in_async and !parser.isModule()) {
            try parser.report(parser.current_token.span, "'for await' is only valid in async functions or modules", .{});
            return null;
        }
        try parser.advance() orelse return null; // consume 'await'

        // continue parsing with is_await = true

        if (!try parser.expect(.left_paren, "Expected '(' after 'for await'", null)) return null;

        return parseForHead(parser, start, true);
    }

    if (!try parser.expect(.left_paren, "Expected '(' after 'for'", null)) return null;

    // first part and determine which kind of for statement this is
    return parseForHead(parser, start, is_await);
}

/// parse the head of a for statement to determine its type
fn parseForHead(parser: *Parser, start: u32, is_await: bool) Error!?ast.NodeIndex {
    const token_type = parser.current_token.type;

    // empty init: for (;;)
    if (token_type == .semicolon) {
        return parseForStatementRest(parser, start, ast.null_node);
    }

    // variable declaration: for (var/let/const ... )
    if (token_type == .@"var" or token_type == .let or token_type == .@"const") {
        return parseForWithDeclaration(parser, start, is_await);
    }

    // expression or assignment target: for (expr in/of ...) or for (expr; ...)
    return parseForWithExpression(parser, start, is_await);
}

/// for loop starting with variable declaration
fn parseForWithDeclaration(parser: *Parser, start: u32, is_await: bool) Error!?ast.NodeIndex {
    const decl_start = parser.current_token.span.start;
    const kind = try parseVariableKindForLoop(parser) orelse return null;

    // first declarator
    const first_declarator = try parseForLoopDeclarator(parser) orelse return null;
    const first_end = parser.getSpan(first_declarator).end;

    // check for for-in/for-of
    if (parser.current_token.type == .in) {
        // for (var/let/const x in ...)
        if (is_await) {
            try parser.report(parser.current_token.span, "'for await' requires 'of', not 'in'", .{});
            return null;
        }

        const decl = try createSingleDeclaration(parser, kind, first_declarator, decl_start, first_end);
        return parseForInStatementRest(parser, start, decl);
    }

    if (parser.current_token.type == .of) {
        // for (var/let/const x of ...)
        const decl = try createSingleDeclaration(parser, kind, first_declarator, decl_start, first_end);
        return parseForOfStatementRest(parser, start, decl, is_await);
    }

    // regular for statement, might have more declarators
    var end = first_end;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    try parser.scratch_a.append(parser.allocator(), first_declarator);

    // additional declarators: for (let a = 1, b = 2; ...)
    while (parser.current_token.type == .comma) {
        try parser.advance() orelse return null;
        const declarator = try parseForLoopDeclarator(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), declarator);
        end = parser.getSpan(declarator).end;
    }

    const declarators = parser.scratch_a.take(checkpoint);

    const declarators_range = try parser.addExtra(declarators);

    // init is required for non idenitifer id's in regular loop
    // for example, this is an error:
    // for (let { a: b = let };;) {}
    for (declarators) |decl| {
        const data = parser.getData(decl).variable_declarator;
        const id_data = parser.getData(data.id);
        if (ast.isNull(data.init) and id_data != .binding_identifier) {
            try parser.report(parser.getSpan(data.id), "Missing initializer in destructuring declaration", .{ .help = "Add an initializer (e.g. ` = undefined`) here" });
            return null;
        }
    }

    const decl = try parser.addNode(.{
        .variable_declaration = .{
            .declarators = declarators_range,
            .kind = kind,
        },
    }, .{ .start = decl_start, .end = end });

    return parseForStatementRest(parser, start, decl);
}

/// for loop starting with expression
fn parseForWithExpression(parser: *Parser, start: u32, is_await: bool) Error!?ast.NodeIndex {
    // disable 'in' as binary operator while parsing for-loop initializer
    const saved_allow_in = parser.context.allow_in;
    parser.context.allow_in = false;
    const expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse {
        parser.context.allow_in = saved_allow_in;
        return null;
    };
    parser.context.allow_in = saved_allow_in;

    // check for for-in/for-of
    if (parser.current_token.type == .in) {
        // for (expr in ...)
        if (is_await) {
            try parser.report(parser.current_token.span, "'for await' requires 'of', not 'in'", .{});
            return null;
        }

        try grammar.expressionToPattern(parser, expr, .assignable) orelse return null;

        // expr is now pattern

        return parseForInStatementRest(parser, start, expr);
    }

    if (parser.current_token.type == .of) {
        // for (expr of ...)
        try grammar.expressionToPattern(parser, expr, .assignable) orelse return null;

        // expr is now pattern

        return parseForOfStatementRest(parser, start, expr, is_await);
    }

    // regular for statement
    return parseForStatementRest(parser, start, expr);
}

/// parse rest of regular for statement after init
fn parseForStatementRest(parser: *Parser, start: u32, init: ast.NodeIndex) Error!?ast.NodeIndex {
    if (!try parser.expect(.semicolon, "Expected ';' after for-loop init", null)) return null;

    var test_expr: ast.NodeIndex = ast.null_node;

    if (parser.current_token.type != .semicolon) {
        test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    }

    if (!try parser.expect(.semicolon, "Expected ';' after for-loop condition", null)) return null;

    var update: ast.NodeIndex = ast.null_node;

    if (parser.current_token.type != .right_paren) {
        update = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    }

    if (!try parser.expect(.right_paren, "Expected ')' after for-loop update", null)) return null;

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .for_statement = .{
            .init = init,
            .@"test" = test_expr,
            .update = update,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// rest of for-in statement after left
fn parseForInStatementRest(parser: *Parser, start: u32, left: ast.NodeIndex) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume 'in'

    const right = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after for-in expression", null)) return null;

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .for_in_statement = .{
            .left = left,
            .right = right,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// rest of for-of statement after left
fn parseForOfStatementRest(parser: *Parser, start: u32, left: ast.NodeIndex, is_await: bool) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume 'of'

    // for-of right side is AssignmentExpression, not Expression (no comma)
    const right = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after for-of expression", null)) return null;

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .for_of_statement = .{
            .left = left,
            .right = right,
            .body = body,
            .await = is_await,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// https://tc39.es/ecma262/#sec-return-statement
fn parseReturnStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;

    if (!parser.context.in_function) {
        try parser.report(
            .{ .start = start, .end = end },
            "'return' statement is only valid inside a function",
            .{ .help = "Remove the 'return' statement or wrap the code in a function." },
        );
        return null;
    }

    try parser.advance() orelse return null; // consume 'return'

    var argument: ast.NodeIndex = ast.null_node;

    // return [no LineTerminator here] Expression?
    if (!parser.canInsertSemicolon() and parser.current_token.type != .semicolon) {
        argument = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
        end = parser.getSpan(argument).end;
    }

    end = try parser.eatSemicolon(end) orelse return null;

    return try parser.addNode(.{ .return_statement = .{ .argument = argument } }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-throw-statement
fn parseThrowStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'throw'

    // throw [no LineTerminator here] Expression
    if (parser.current_token.has_line_terminator_before) {
        try parser.report(parser.current_token.span, "Illegal newline after throw", .{
            .help = "The thrown expression must be on the same line as 'throw'",
        });
        return null;
    }

    const argument = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    const end = try parser.eatSemicolon(parser.getSpan(argument).end) orelse return null;

    return try parser.addNode(.{ .throw_statement = .{ .argument = argument } }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-try-statement
fn parseTryStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'try'

    const block = try parseBlockStatement(parser) orelse return null;

    var handler: ast.NodeIndex = ast.null_node;
    var finalizer: ast.NodeIndex = ast.null_node;
    var end = parser.getSpan(block).end;

    if (parser.current_token.type == .@"catch") {
        handler = try parseCatchClause(parser) orelse return null;
        end = parser.getSpan(handler).end;
    }

    if (parser.current_token.type == .finally) {
        try parser.advance() orelse return null; // consume 'finally'
        finalizer = try parseBlockStatement(parser) orelse return null;
        end = parser.getSpan(finalizer).end;
    }

    if (ast.isNull(handler) and ast.isNull(finalizer)) {
        try parser.report(parser.current_token.span, "Try statement requires catch or finally clause", .{});
        return null;
    }

    return try parser.addNode(.{
        .try_statement = .{
            .block = block,
            .handler = handler,
            .finalizer = finalizer,
        },
    }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#prod-Catch
fn parseCatchClause(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'catch'

    var param: ast.NodeIndex = ast.null_node;

    // optional catch binding: catch (param) or catch
    if (parser.current_token.type == .left_paren) {
        try parser.advance() orelse return null; // consume '('
        param = try patterns.parseBindingPattern(parser) orelse return null;
        if (!try parser.expect(.right_paren, "Expected ')' after catch parameter", null)) return null;
    }

    const body = try parseBlockStatement(parser) orelse return null;

    return try parser.addNode(.{
        .catch_clause = .{
            .param = param,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// https://tc39.es/ecma262/#sec-debugger-statement
fn parseDebuggerStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume 'debugger'
    end = try parser.eatSemicolon(end) orelse return null;
    return try parser.addNode(.debugger_statement, .{ .start = start, .end = end });
}

/// variable kind for for loops
fn parseVariableKindForLoop(parser: *Parser) Error!?ast.VariableKind {
    const token_type = parser.current_token.type;
    try parser.advance() orelse return null;

    return switch (token_type) {
        .let => .let,
        .@"const" => .@"const",
        .@"var" => .@"var",
        else => null,
    };
}

/// a single variable declarator for for loops
fn parseForLoopDeclarator(parser: *Parser) Error!?ast.NodeIndex {
    const decl_start = parser.current_token.span.start;
    const id = try patterns.parseBindingPattern(parser) orelse return null;

    var init: ast.NodeIndex = ast.null_node;
    var end = parser.getSpan(id).end;

    if (parser.current_token.type == .assign) {
        try parser.advance() orelse return null;
        init = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        end = parser.getSpan(init).end;
    }

    return try parser.addNode(.{ .variable_declarator = .{ .id = id, .init = init } }, .{ .start = decl_start, .end = end });
}

/// create a variable declaration with a single declarator
fn createSingleDeclaration(parser: *Parser, kind: ast.VariableKind, declarator: ast.NodeIndex, decl_start: u32, decl_end: u32) Error!ast.NodeIndex {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);
    try parser.scratch_a.append(parser.allocator(), declarator);

    return try parser.addNode(.{
        .variable_declaration = .{
            .declarators = try parser.addExtra(parser.scratch_a.take(checkpoint)),
            .kind = kind,
        },
    }, .{ .start = decl_start, .end = decl_end });
}
