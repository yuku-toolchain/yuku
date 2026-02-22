const std = @import("std");
const ast = @import("../ast.zig");
const token = @import("../token.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Precedence = @import("../token.zig").Precedence;

const expressions = @import("expressions.zig");
const variables = @import("variables.zig");
const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const extensions = @import("extensions.zig");
const grammar = @import("../grammar.zig");
const for_loop = @import("for_loop.zig");
const modules = @import("modules.zig");

const ParseStatementOpts = struct {
    /// true when parsing the body of `if`, `while`, `do`, `for`, `with`, or labeled statements,
    /// where lexical declarations (`let`, `const`) are not allowed without a block.
    can_be_single_statement_context: bool = false,
};

pub fn parseStatement(parser: *Parser, opts: ParseStatementOpts) Error!?ast.NodeIndex {
    parser.context.in_single_statement_context = opts.can_be_single_statement_context;
    defer parser.context.in_single_statement_context = false;

    parser.context.in_directive_prologue = parser.context.in_directive_prologue and parser.current_token.tag == .string_literal;

    return switch (parser.current_token.tag) {
        .at => extensions.parseDecorated(parser, .{}),
        .await => parseAwaitUsingOrExpression(parser),
        .import => parseImportDeclarationOrExpression(parser),
        .async => parseAsyncFunctionOrExpression(parser),
        .@"var", .@"const" => variables.parseVariableDeclaration(parser, false, null),
        .let => parseLet(parser),
        .using => parseUsingOrExpression(parser),
        .function => functions.parseFunction(parser, .{}, null),
        .class => class.parseClass(parser, .{}, null),
        .@"export" => modules.parseExportDeclaration(parser),
        .@"if" => parseIfStatement(parser),
        .@"switch" => parseSwitchStatement(parser),
        .@"for" => for_loop.parseForStatement(parser, false),
        .@"while" => parseWhileStatement(parser),
        .do => parseDoWhileStatement(parser),
        .with => parseWithStatement(parser),
        .@"break" => parseBreakStatement(parser),
        .@"continue" => parseContinueStatement(parser),
        .@"return" => parseReturnStatement(parser),
        .throw => parseThrowStatement(parser),
        .@"try" => parseTryStatement(parser),
        .left_brace => parseBlockStatement(parser),
        .debugger => parseDebuggerStatement(parser),
        .semicolon => parseEmptyStatement(parser),
        else => parseExpressionOrLabeledStatementOrDirective(parser),
    };
}

fn parseExpressionOrLabeledStatementOrDirective(parser: *Parser) Error!?ast.NodeIndex {
    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    const expression_data = parser.getData(expression);

    if (parser.context.in_directive_prologue and expression_data == .string_literal) {
        return parseDirective(parser, expression, expression_data);
    }

    // labeled statement: identifier ':'
    if (expression_data == .identifier_reference and parser.current_token.tag == .colon) {
        return parseLabeledStatement(parser, expression);
    }

    return parseExpressionStatementWithExpression(parser, expression);
}

fn parseExpressionStatement(parser: *Parser) Error!?ast.NodeIndex {
    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    return parseExpressionStatementWithExpression(parser, expression);
}

fn parseExpressionStatementWithExpression(
    parser: *Parser,
    expression: ast.NodeIndex,
) Error!?ast.NodeIndex {
    const span = parser.getSpan(expression);

    return try parser.addNode(
        .{ .expression_statement = .{ .expression = expression } },
        .{ .start = span.start, .end = try parser.eatSemicolon(span.end) orelse return null },
    );
}

fn parseDirective(parser: *Parser, expression: ast.NodeIndex, expression_data: ast.NodeData) Error!?ast.NodeIndex {
    const string_literal_span = parser.getSpan(expression);

    const value_start = expression_data.string_literal.raw_start + 1;
    const value_len: u16 = expression_data.string_literal.raw_len - 2;

    // "use strict" directive enables strict mode for the current scope
    if (std.mem.eql(u8, parser.getSourceText(value_start, value_len), "use strict")) {
        _ = parser.enterStrictMode();
    }

    return try parser.addNode(.{
        .directive = .{
            .expression = expression,
            .value_start = value_start,
            .value_len = value_len,
        },
    }, .{
        .start = string_literal_span.start,
        .end = try parser.eatSemicolon(string_literal_span.end) orelse return null,
    });
}

/// 'let' can be either a keyword or an identifier depending on context.
/// check if it should be parsed as an identifier (eg, `let;`) before treating it as a declaration.
fn parseLet(parser: *Parser) Error!?ast.NodeIndex {
    const is_identifier = try variables.isLetIdentifier(parser) orelse return null;

    if (!is_identifier) {
        // parse as variable declaration: let x = 5;
        return variables.parseVariableDeclaration(parser, false, null);
    }

    // otherwise, fall through to parse 'let' as an identifier in an expression statement.
    return parseExpressionStatement(parser);
}

/// `using` declaration, or fall through to expression statement.
fn parseUsingOrExpression(parser: *Parser) Error!?ast.NodeIndex {
    // determine if 'using' is an identifier or a keyword
    const is_using_identifier = try variables.isUsingIdentifier(parser) orelse return null;

    if (!is_using_identifier) {
        return variables.parseVariableDeclaration(parser, false, null);
    }

    return parseExpressionStatement(parser);
}

/// `await using` declaration, or fall through to expression statement.
fn parseAwaitUsingOrExpression(parser: *Parser) Error!?ast.NodeIndex {
    const next = try parser.lookAhead() orelse return null;

    return switch (next.tag) {
        .using => {
            const start = parser.current_token.span.start;

            try parser.advance() orelse return null; // consume 'await'

            // determine if 'using' is an identifier or a keyword
            const is_using_identifier = try variables.isUsingIdentifier(parser) orelse return null;

            if (!is_using_identifier) {
                return variables.parseVariableDeclaration(parser, true, start);
            }

            return parseAwaitExpressionStatement(parser, start);
        },
        else => parseExpressionOrLabeledStatementOrDirective(parser),
    };
}

fn parseAwaitExpressionStatement(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    const await_expr = try expressions.parseAwaitExpression(parser, start) orelse return null;

    return parseExpressionStatementWithExpression(parser, await_expr);
}

/// import declaration, or fall through to import expression statement (`import(` / `import.`).
fn parseImportDeclarationOrExpression(parser: *Parser) Error!?ast.NodeIndex {
    const next = try parser.lookAhead() orelse return null;

    return switch (next.tag) {
        // `import(` and `import.` are expression forms (dynamic import / import.meta / phase imports)
        .left_paren, .dot => parseExpressionStatement(parser),
        else => modules.parseImportDeclaration(parser),
    };
}

/// `async function` declaration, or fall through to expression statement.
fn parseAsyncFunctionOrExpression(parser: *Parser) Error!?ast.NodeIndex {
    const next = try parser.lookAhead() orelse return null;

    if (next.tag == .function and !next.hasLineTerminatorBefore()) {
        const start = parser.current_token.span.start;
        try parser.advance() orelse return null; // consume 'async'
        return functions.parseFunction(parser, .{ .is_async = true }, start);
    }

    return parseExpressionOrLabeledStatementOrDirective(parser);
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

    while (parser.current_token.tag == .case or parser.current_token.tag == .default) {
        const case_node = try parseSwitchCase(parser) orelse continue;
        try parser.scratch_a.append(parser.allocator(), case_node);
    }

    return parser.addExtraFromScratch(&parser.scratch_a, checkpoint);
}

fn parseSwitchCase(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const is_default = parser.current_token.tag == .default;

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

    while (parser.current_token.tag != .case and
        parser.current_token.tag != .default and
        parser.current_token.tag != .right_brace and
        parser.current_token.tag != .eof)
    {
        if (try parseStatement(parser, .{})) |stmt| {
            try parser.scratch_b.append(parser.allocator(), stmt);
        } else {
            break;
        }
    }

    return parser.addExtraFromScratch(&parser.scratch_b, checkpoint);
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

    if (parser.current_token.tag == .@"else") {
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

    if (parser.isStrictMode()) {
        try parser.report(parser.current_token.span, "'with' statements are not allowed in strict mode", .{});
    }

    try parser.advance() orelse return null; // consume 'with'

    if (!try parser.expect(.left_paren, "Expected '(' after 'with'", null)) return null;

    const object = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after with expression", null)) return null;

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

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
    if (!parser.canInsertImplicitSemicolon(parser.current_token) and parser.current_token.tag != .semicolon) {
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
    if (!parser.canInsertImplicitSemicolon(parser.current_token) and parser.current_token.tag != .semicolon) {
        const label_node = try literals.parseLabelIdentifier(parser) orelse return null;
        label = label_node;
        end = parser.getSpan(label_node).end;
    }

    end = try parser.eatSemicolon(end) orelse return null;

    return try parser.addNode(.{ .continue_statement = .{ .label = label } }, .{ .start = start, .end = end });
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
    }

    try parser.advance() orelse return null; // consume 'return'

    var argument: ast.NodeIndex = ast.null_node;

    // return [no LineTerminator here] Expression?
    if (!parser.canInsertImplicitSemicolon(parser.current_token) and parser.current_token.tag != .semicolon) {
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
    if (parser.current_token.hasLineTerminatorBefore()) {
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

    if (parser.current_token.tag == .@"catch") {
        handler = try parseCatchClause(parser) orelse return null;
        end = parser.getSpan(handler).end;
    }

    if (parser.current_token.tag == .finally) {
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
    if (parser.current_token.tag == .left_paren) {
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
