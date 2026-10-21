const std = @import("std");
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;
const TokenTag = @import("../token.zig").TokenTag;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const expressions = @import("expressions.zig");
const variables = @import("variables.zig");
const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const decorator = @import("decorators.zig");
const for_loop = @import("for_loop.zig");
const modules = @import("modules.zig");
const ts_types = @import("ts/types.zig");
const ts_decl = @import("ts/statements.zig");
const dialect = @import("../dialect/root.zig");

const ParseStatementOptions = struct {
    can_be_single_statement_context: bool = false,
    body: Parser.BodyKind = .other,
};

pub fn parseStatement(parser: *Parser, options: ParseStatementOptions) Error!?ast.NodeIndex {
    parser.context.single_statement = options.can_be_single_statement_context;
    defer parser.context.single_statement = false;

    parser.context.directive_prologue = parser.context.directive_prologue and
        parser.current_token.tag == .string_literal;

    if (comptime dialect.has(.statement)) {
        if (try dialect.at(.statement, parser, .{})) |statement| return statement;
    }

    if (parser.current_token.tag == .identifier) {
        return parseExpressionOrLabeledStatementOrDirective(parser);
    }

    return switch (parser.current_token.tag) {
        .at => checkImportExportPosition(parser, try parseDecoratedStatement(parser), options.body),
        .await => parseAwaitUsingOrExpression(parser),
        .import => checkImportExportPosition(
            parser,
            try parseImportDeclarationOrExpression(parser),
            options.body,
        ),
        .async => parseAsyncFunctionOrExpression(parser),
        .@"var" => variables.parseVariableDeclaration(parser, .{}, null),
        .@"const" => parseConstOrConstEnum(parser),
        .let => parseLet(parser),
        .using => parseUsingOrExpression(parser),
        .function => functions.parseFunction(parser, .{}, null),
        .class => class.parseClass(parser, .{}, null),
        .type,
        .interface,
        .@"enum",
        .namespace,
        .module,
        .global,
        .declare,
        .abstract,
        => parseTsDeclarationOrExpression(parser),
        .@"export" => checkImportExportPosition(
            parser,
            try modules.parseExportDeclaration(parser),
            options.body,
        ),
        .@"if" => parseIfStatement(parser),
        .@"switch" => parseSwitchStatement(parser),
        .@"for" => for_loop.parseForStatement(parser),
        .@"while" => parseWhileStatement(parser),
        .do => parseDoWhileStatement(parser),
        .with => parseWithStatement(parser),
        .@"break" => parseBreakOrContinueStatement(parser, .@"break"),
        .@"continue" => parseBreakOrContinueStatement(parser, .@"continue"),
        .@"return" => parseReturnStatement(parser),
        .throw => parseThrowStatement(parser),
        .@"try" => parseTryStatement(parser),
        .left_brace => parseBlockStatement(parser),
        .debugger => parseDebuggerStatement(parser),
        .semicolon => parseEmptyStatement(parser),
        else => parseExpressionOrLabeledStatementOrDirective(parser),
    };
}

pub inline fn parseSingleStatement(parser: *Parser) Error!?ast.NodeIndex {
    return parseStatement(parser, .{ .can_be_single_statement_context = true });
}

fn parseParenthesizedHead(
    parser: *Parser,
    comptime open_message: []const u8,
    comptime close_message: []const u8,
) Error!?ast.NodeIndex {
    if (!try parser.expect(.left_paren, open_message, null)) return null;
    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
        return null;
    if (!try parser.expect(.right_paren, close_message, null)) return null;
    return expression;
}

fn checkImportExportPosition(
    parser: *Parser,
    statement: ?ast.NodeIndex,
    body: Parser.BodyKind,
) Error!?ast.NodeIndex {
    const node = statement orelse return null;
    const messages = importExportMessages(parser.tree.data(node)) orelse return node;
    const span = parser.tree.span(node);
    std.debug.assert(span.start < span.end);
    if (body == .module_block) std.debug.assert(parser.tree.isTs());

    switch (body) {
        .program => if (!parser.tree.isModule()) {
            try parser.report(span, messages.outside_module, .{});
        },
        .module_block => {},
        .function, .other => try parser.report(span, messages.nested, .{}),
    }
    return node;
}

const ImportExportMessages = struct {
    outside_module: []const u8,
    nested: []const u8,
};

fn importExportMessages(data: ast.NodeData) ?ImportExportMessages {
    return switch (data) {
        .import_declaration => .{
            .outside_module = "Cannot use import statement outside a module",
            .nested = "'import' declaration may only appear at the top level",
        },
        .export_named_declaration => .{
            .outside_module = "Cannot use 'export' declaration outside a module",
            .nested = "'export' declaration may only appear at the top level",
        },
        .export_default_declaration => .{
            .outside_module = "Cannot use 'export default' declaration outside a module",
            .nested = "'export default' declaration may only appear at the top level",
        },
        .export_all_declaration => .{
            .outside_module = "Cannot use 'export *' declaration outside a module",
            .nested = "'export *' declaration may only appear at the top level",
        },
        else => null,
    };
}

fn parseDecoratedStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .at);
    const start = parser.current_token.span.start;
    const decorators = try decorator.parseDecorators(parser) orelse return null;

    if (parser.current_token.tag == .@"export") {
        return modules.parseExportDecorated(parser, decorators);
    }

    return class.parseClassDecorated(parser, .{}, start, decorators);
}

fn parseExpressionOrLabeledStatementOrDirective(parser: *Parser) Error!?ast.NodeIndex {
    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
        return null;
    const expression_data = parser.tree.data(expression);

    if (parser.context.directive_prologue and expression_data == .string_literal) {
        return parseDirective(parser, expression);
    }

    if (expression_data == .identifier_reference and parser.current_token.tag == .colon) {
        return parseLabeledStatement(parser, expression);
    }

    return parseExpressionStatementWithExpression(parser, expression);
}

fn parseExpressionStatement(parser: *Parser) Error!?ast.NodeIndex {
    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
        return null;
    return parseExpressionStatementWithExpression(parser, expression);
}

fn parseExpressionStatementWithExpression(
    parser: *Parser,
    expression: ast.NodeIndex,
) Error!?ast.NodeIndex {
    const span = parser.tree.span(expression);

    return try parser.tree.addNode(
        .{ .expression_statement = .{ .expression = expression } },
        .{ .start = span.start, .end = try parser.eatSemicolon(span.end) orelse return null },
    );
}

fn parseDirective(parser: *Parser, expression: ast.NodeIndex) Error!?ast.NodeIndex {
    const span = parser.tree.span(expression);

    return try parser.tree.addNode(.{
        .directive = .{
            .expression = expression,
            // "use strict" recognition matches exact code units, not the decoded value
            .value = parser.tree.sourceSlice(span.start + 1, span.end - 1),
        },
    }, .{ .start = span.start, .end = try parser.eatSemicolon(span.end) orelse return null });
}

fn parseLet(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .let);
    if (variables.isLetIdentifier(parser)) return parseExpressionStatement(parser);
    return variables.parseVariableDeclaration(parser, .{}, null);
}

fn parseUsingOrExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .using);
    if (variables.isUsingIdentifier(parser)) return parseExpressionStatement(parser);
    return variables.parseVariableDeclaration(parser, .{}, null);
}

fn parseAwaitUsingOrExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .await);

    if (variables.isAwaitUsingDeclarationAhead(parser)) {
        const start = parser.current_token.span.start;
        try parser.advance() orelse return null;
        return variables.parseVariableDeclaration(parser, .{ .await_using = true }, start);
    }

    return parseExpressionOrLabeledStatementOrDirective(parser);
}

fn parseImportDeclarationOrExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .import);
    return switch (parser.peekAhead().tag) {
        .left_paren, .dot => parseExpressionStatement(parser),
        else => modules.parseImportDeclaration(parser, null),
    };
}

fn parseTsDeclarationOrExpression(parser: *Parser) Error!?ast.NodeIndex {
    if (ts_decl.isStartOfTsDeclaration(parser)) return ts_decl.parseTsDeclaration(parser);
    return parseExpressionOrLabeledStatementOrDirective(parser);
}

fn parseConstOrConstEnum(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"const");
    if (parser.tree.isTs() and ts_decl.isConstEnumHead(parser.peekAhead())) {
        return ts_decl.parseTsDeclaration(parser);
    }
    return variables.parseVariableDeclaration(parser, .{}, null);
}

fn parseAsyncFunctionOrExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .async);
    const next = parser.peekAhead();

    if (next.tag == .function and !next.hasLineTerminatorBefore()) {
        const start = parser.current_token.span.start;
        try parser.advance() orelse return null;
        return functions.parseFunction(parser, .{ .is_async = true }, start);
    }

    return parseExpressionOrLabeledStatementOrDirective(parser);
}

// https://tc39.es/ecma262/#sec-labelled-statements
fn parseLabeledStatement(parser: *Parser, identifier: ast.NodeIndex) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .colon);
    std.debug.assert(parser.tree.data(identifier) == .identifier_reference);
    const id_span = parser.tree.span(identifier);
    const name = parser.tree.data(identifier).identifier_reference.name;
    const label = try parser.tree.addNode(.{ .label_identifier = .{ .name = name } }, id_span);

    try parser.advance() orelse return null;
    const body = try parseSingleStatement(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .labeled_statement = .{ .label = label, .body = body } },
        .{ .start = id_span.start, .end = parser.tree.span(body).end },
    );
}

/// Parses a braced block statement.
/// https://tc39.es/ecma262/#prod-BlockStatement
fn parseBlockStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (!try parser.expect(
        .left_brace,
        "Expected '{' to start block statement",
        "Block statements must be enclosed in braces: { ... }",
    )) return null;

    const body = try parser.parseBody(.right_brace, .other);
    const end = parser.current_token.span.end;
    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close block statement",
        "Add a closing brace '}' to complete the block statement, " ++
            "or check for unbalanced braces inside.",
    )) return null;

    return try parser.tree.addNode(
        .{ .block_statement = .{ .body = body } },
        .{ .start = start, .end = end },
    );
}

/// Parses a `switch` statement.
/// https://tc39.es/ecma262/#sec-switch-statement
fn parseSwitchStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"switch");
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const discriminant = try parseParenthesizedHead(
        parser,
        "Expected '(' after 'switch'",
        "Expected ')' after switch expression",
    ) orelse return null;
    if (!try parser.expect(.left_brace, "Expected '{' to start switch body", null)) return null;

    const cases = try parseSwitchCases(parser);

    const end = parser.current_token.span.end;
    if (!try parser.expect(.right_brace, "Expected '}' to close switch body", null)) return null;

    return try parser.tree.addNode(
        .{ .switch_statement = .{ .discriminant = discriminant, .cases = cases } },
        .{ .start = start, .end = end },
    );
}

fn parseSwitchCases(parser: *Parser) Error!ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag == .case or parser.current_token.tag == .default) {
        if (try parseSwitchCase(parser)) |case_node| {
            try parser.scratch_a.append(parser.allocator(), case_node);
        } else {
            // a failed case parks the cursor on a lexical error, resync so the loop cannot spin
            try parser.recover(.right_brace);
        }
    }

    return parser.flushToExtras(&parser.scratch_a, checkpoint);
}

fn parseSwitchCase(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .case or parser.current_token.tag == .default);
    const start = parser.current_token.span.start;
    const is_default = parser.current_token.tag == .default;

    try parser.advance() orelse return null;
    const test_expr: ast.NodeIndex = if (is_default)
        .null
    else
        try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    const colon_end = parser.current_token.span.end;
    if (!try parser.expect(.colon, "Expected ':' after case", null)) return null;

    const consequent = try parseCaseConsequent(parser);
    const end = if (consequent.len > 0)
        parser.tree.span(parser.tree.extra(consequent)[consequent.len - 1]).end
    else
        colon_end;

    return try parser.tree.addNode(
        .{ .switch_case = .{ .@"test" = test_expr, .consequent = consequent } },
        .{ .start = start, .end = end },
    );
}

fn parseCaseConsequent(parser: *Parser) Error!ast.IndexRange {
    const checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(checkpoint);

    while (parser.current_token.tag != .case and
        parser.current_token.tag != .default and
        parser.current_token.tag != .right_brace and
        parser.current_token.tag != .eof)
    {
        const statement = try parseStatement(parser, .{}) orelse break;

        const data = parser.tree.data(statement);
        if (data == .variable_declaration) {
            const kind = data.variable_declaration.kind;
            if (kind == .using or kind == .await_using) {
                try parser.report(
                    parser.tree.span(statement),
                    "Using declaration cannot appear in the bare case statement.",
                    .{ .help = "Wrap this declaration in a block statement" },
                );
            }
        }

        try parser.scratch_b.append(parser.allocator(), statement);
    }

    return parser.flushToExtras(&parser.scratch_b, checkpoint);
}

/// Parses an `if` statement.
/// https://tc39.es/ecma262/#sec-if-statement
fn parseIfStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"if");
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const test_expr = try parseParenthesizedHead(
        parser,
        "Expected '(' after 'if'",
        "Expected ')' after if condition",
    ) orelse return null;
    const consequent = try parseSingleStatement(parser) orelse return null;

    var end = parser.tree.span(consequent).end;
    var alternate: ast.NodeIndex = .null;

    if (parser.current_token.tag == .@"else") {
        try parser.advance() orelse return null;
        alternate = try parseSingleStatement(parser) orelse return null;
        end = parser.tree.span(alternate).end;
    }

    return try parser.tree.addNode(.{ .if_statement = .{
        .@"test" = test_expr,
        .consequent = consequent,
        .alternate = alternate,
    } }, .{ .start = start, .end = end });
}

// https://tc39.es/ecma262/#sec-while-statement
fn parseWhileStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"while");
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const test_expr = try parseParenthesizedHead(
        parser,
        "Expected '(' after 'while'",
        "Expected ')' after while condition",
    ) orelse return null;
    const body = try parseSingleStatement(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .while_statement = .{ .@"test" = test_expr, .body = body } },
        .{ .start = start, .end = parser.tree.span(body).end },
    );
}

// https://tc39.es/ecma262/#sec-do-while-statement
fn parseDoWhileStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .do);
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const body = try parseSingleStatement(parser) orelse return null;

    if (!try parser.expect(.@"while", "Expected 'while' after do statement body", null)) {
        return null;
    }

    const test_expr = try parseParenthesizedHead(
        parser,
        "Expected '(' after 'while'",
        "Expected ')' after while condition",
    ) orelse return null;

    // ASI always inserts a semicolon after the `)` of do-while
    var end = parser.prev_token_end;
    if (parser.current_token.tag == .semicolon) {
        end = parser.current_token.span.end;
        try parser.advance() orelse return null;
    }

    return try parser.tree.addNode(
        .{ .do_while_statement = .{ .body = body, .@"test" = test_expr } },
        .{ .start = start, .end = end },
    );
}

// https://tc39.es/ecma262/#sec-with-statement
fn parseWithStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .with);
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const object = try parseParenthesizedHead(
        parser,
        "Expected '(' after 'with'",
        "Expected ')' after with expression",
    ) orelse return null;
    const body = try parseSingleStatement(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .with_statement = .{ .object = object, .body = body } },
        .{ .start = start, .end = parser.tree.span(body).end },
    );
}

fn parseEmptyStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .semicolon);
    const span = parser.current_token.span;
    try parser.advance() orelse return null;
    return try parser.tree.addNode(.{ .empty_statement = .{} }, span);
}

// https://tc39.es/ecma262/#sec-break-statement
// https://tc39.es/ecma262/#sec-continue-statement
fn parseBreakOrContinueStatement(
    parser: *Parser,
    comptime keyword: TokenTag,
) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == keyword);
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance() orelse return null;

    var label: ast.NodeIndex = .null;
    // break [no LineTerminator here] LabelIdentifier
    if (!parser.canInsertImplicitSemicolon(parser.current_token) and
        parser.current_token.tag != .semicolon)
    {
        label = try literals.parseLabelIdentifier(parser) orelse return null;
        end = parser.tree.span(label).end;
    }

    end = try parser.eatSemicolon(end) orelse return null;

    const data: ast.NodeData = switch (keyword) {
        .@"break" => .{ .break_statement = .{ .label = label } },
        .@"continue" => .{ .continue_statement = .{ .label = label } },
        else => @compileError("expected 'break' or 'continue'"),
    };
    return try parser.tree.addNode(data, .{ .start = start, .end = end });
}

// https://tc39.es/ecma262/#sec-return-statement
fn parseReturnStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"return");
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;

    if (!parser.context.@"return") {
        try parser.report(
            .{ .start = start, .end = end },
            "'return' statement is only valid inside a function",
            .{ .help = "Remove the 'return' statement or wrap the code in a function." },
        );
    }

    try parser.advance() orelse return null;

    var argument: ast.NodeIndex = .null;
    // return [no LineTerminator here] Expression?
    if (!parser.canInsertImplicitSemicolon(parser.current_token) and
        parser.current_token.tag != .semicolon)
    {
        argument = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
            return null;
        end = parser.tree.span(argument).end;
    }

    end = try parser.eatSemicolon(end) orelse return null;

    return try parser.tree.addNode(
        .{ .return_statement = .{ .argument = argument } },
        .{ .start = start, .end = end },
    );
}

// https://tc39.es/ecma262/#sec-throw-statement
fn parseThrowStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .throw);
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    // throw [no LineTerminator here] Expression
    if (parser.current_token.hasLineTerminatorBefore()) {
        try parser.report(parser.current_token.span, "Illegal newline after throw", .{
            .help = "The thrown expression must be on the same line as 'throw'",
        });
        return null;
    }

    const argument = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
        return null;
    const end = try parser.eatSemicolon(parser.tree.span(argument).end) orelse return null;

    return try parser.tree.addNode(
        .{ .throw_statement = .{ .argument = argument } },
        .{ .start = start, .end = end },
    );
}

// https://tc39.es/ecma262/#sec-try-statement
fn parseTryStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"try");
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const block = try parseBlockStatement(parser) orelse return null;

    var handler: ast.NodeIndex = .null;
    var finalizer: ast.NodeIndex = .null;
    var end = parser.tree.span(block).end;

    if (parser.current_token.tag == .@"catch") {
        handler = try parseCatchClause(parser) orelse return null;
        end = parser.tree.span(handler).end;
    }

    if (parser.current_token.tag == .finally) {
        try parser.advance() orelse return null;
        finalizer = try parseBlockStatement(parser) orelse return null;
        end = parser.tree.span(finalizer).end;
    }

    if (handler == .null and finalizer == .null) {
        try parser.report(
            parser.current_token.span,
            "Try statement requires catch or finally clause",
            .{},
        );
        return null;
    }

    return try parser.tree.addNode(.{ .try_statement = .{
        .block = block,
        .handler = handler,
        .finalizer = finalizer,
    } }, .{ .start = start, .end = end });
}

// https://tc39.es/ecma262/#prod-Catch
fn parseCatchClause(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"catch");
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    var param: ast.NodeIndex = .null;
    if (parser.current_token.tag == .left_paren) {
        try parser.advance() orelse return null;
        param = try patterns.parseBindingPattern(parser) orelse return null;

        if (parser.tree.isTs() and parser.current_token.tag == .colon) {
            param = try ts_types.parsePatternTypeAnnotation(parser, param) orelse return null;
        }

        if (!try parser.expect(.right_paren, "Expected ')' after catch parameter", null)) {
            return null;
        }
    }

    const body = try parseBlockStatement(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .catch_clause = .{ .param = param, .body = body } },
        .{ .start = start, .end = parser.tree.span(body).end },
    );
}

// https://tc39.es/ecma262/#sec-debugger-statement
fn parseDebuggerStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .debugger);
    const span = parser.current_token.span;
    try parser.advance() orelse return null;
    const end = try parser.eatSemicolon(span.end) orelse return null;
    return try parser.tree.addNode(
        .{ .debugger_statement = .{} },
        .{ .start = span.start, .end = end },
    );
}
