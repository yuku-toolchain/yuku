const std = @import("std");
const ast = @import("../ast.zig");
const TokenTag = @import("../token.zig").TokenTag;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const expressions = @import("expressions.zig");
const variables = @import("variables.zig");
const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const for_loop = @import("for_loop.zig");
const modules = @import("modules.zig");
const ts_types = @import("ts/types.zig");
const ts_decl = @import("ts/statements.zig");

pub const StatementPosition = struct {
    single: bool = false,
    body: Parser.BodyKind = .other,
};

pub fn parseStatement(parser: *Parser, position: StatementPosition) Error!?ast.NodeIndex {
    try parser.descend();
    defer parser.ascend();
    const body = position.body;
    parser.context.single_statement = position.single;
    defer parser.context.single_statement = false;

    parser.context.directive_prologue = parser.context.directive_prologue and
        parser.at(.string_literal);

    if (parser.at(.identifier)) return parseExpressionOrLabeledStatementOrDirective(parser);

    return switch (parser.current_token.tag) {
        .at => checkImportExportPosition(parser, try parseDecoratedStatement(parser), body),
        .await => parseAwaitUsingOrExpression(parser),
        .import => checkImportExportPosition(parser, switch (parser.peekAhead().tag) {
            .left_paren, .dot => try parseExpressionStatement(parser),
            else => try modules.parseImportDeclaration(parser, null),
        }, body),
        .async => parseAsyncFunctionOrExpression(parser),
        .@"var" => variables.parseVariableDeclaration(parser, .{}, null),
        .@"const" => if (parser.tree.isTs() and ts_decl.isConstEnumHead(parser.peekAhead()))
            ts_decl.parseTsDeclaration(parser)
        else
            variables.parseVariableDeclaration(parser, .{}, null),
        .let => if (variables.isLetIdentifier(parser))
            parseExpressionStatement(parser)
        else
            variables.parseVariableDeclaration(parser, .{}, null),
        .using => if (variables.isUsingIdentifier(parser))
            parseExpressionStatement(parser)
        else
            variables.parseVariableDeclaration(parser, .{}, null),
        .function => functions.parseFunction(parser, .{}, null),
        .class => class.parseClass(parser, .{}, null),
        .type, .interface, .@"enum", .namespace, .module, .global, .declare, .abstract => {
            if (ts_decl.isStartOfTsDeclaration(parser)) return ts_decl.parseTsDeclaration(parser);
            return parseExpressionOrLabeledStatementOrDirective(parser);
        },
        .@"export" => checkImportExportPosition(
            parser,
            try modules.parseExportDeclaration(parser),
            body,
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
        .semicolon => parser.leaf(.{ .empty_statement = .{} }),
        else => parseExpressionOrLabeledStatementOrDirective(parser),
    };
}

pub inline fn parseSingleStatement(parser: *Parser) Error!?ast.NodeIndex {
    return parseStatement(parser, .{ .single = true });
}

fn parseParenthesizedHead(
    parser: *Parser,
    comptime open_message: []const u8,
    comptime close_message: []const u8,
) Error!?ast.NodeIndex {
    if (!try parser.expect(.left_paren, open_message, null)) return null;
    const expression = try expressions.parseFull(parser) orelse return null;
    if (!try parser.expect(.right_paren, close_message, null)) return null;
    return expression;
}

fn checkImportExportPosition(
    parser: *Parser,
    statement: ?ast.NodeIndex,
    body: Parser.BodyKind,
) Error!?ast.NodeIndex {
    const node = statement orelse return null;
    const outside_module: []const u8, const nested: []const u8 = switch (parser.tree.data(node)) {
        .import_declaration => .{
            "Cannot use an import declaration outside a module",
            "An import declaration can only be used at the top level of a module",
        },
        .export_named_declaration => .{
            "Cannot use an export declaration outside a module",
            "An export declaration can only be used at the top level of a module",
        },
        .export_default_declaration => .{
            "Cannot use 'export default' outside a module",
            "A default export must be at the top level of a file or module declaration",
        },
        .export_all_declaration => .{
            "Cannot use 'export *' outside a module",
            "An 'export *' declaration can only be used at the top level of a module",
        },
        else => return node,
    };
    const span = parser.tree.span(node);
    std.debug.assert(span.start < span.end);
    if (body == .module_block) std.debug.assert(parser.tree.isTs());

    switch (body) {
        .program => if (!parser.tree.isModule()) try parser.report(span, outside_module, .{}),
        .module_block => {},
        .function, .other => try parser.report(span, nested, .{}),
    }
    return node;
}

noinline fn parseDecoratedStatement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.at(.at));
    const start = parser.current_token.span.start;
    const decorators = try class.parseDecorators(parser, .empty) orelse return null;
    if (parser.at(.@"export")) return modules.parseExportDecorated(parser, decorators);
    return class.parseClass(parser, .{ .decorators = decorators }, start);
}

fn parseExpressionOrLabeledStatementOrDirective(parser: *Parser) Error!?ast.NodeIndex {
    const expression = try expressions.parseFull(parser) orelse return null;
    const data = parser.tree.data(expression);

    if (parser.context.directive_prologue and data == .string_literal) {
        const span = parser.tree.span(expression);
        const end = try parser.eatSemicolon(span.end) orelse return null;
        return try parser.tree.addNode(.{
            .directive = .{
                .expression = expression,
                // "use strict" recognition matches exact code units, not the decoded value
                .value = parser.tree.sourceSlice(span.start + 1, span.end - 1),
            },
        }, .{ .start = span.start, .end = end });
    }
    if (data == .identifier_reference and parser.at(.colon)) {
        return parseLabeledStatement(parser, expression);
    }
    return @call(.always_inline, finishExpressionStatement, .{ parser, expression });
}

fn parseExpressionStatement(parser: *Parser) Error!?ast.NodeIndex {
    const expression = try expressions.parseFull(parser) orelse return null;
    return finishExpressionStatement(parser, expression);
}

fn finishExpressionStatement(parser: *Parser, expression: ast.NodeIndex) Error!?ast.NodeIndex {
    const span = parser.tree.span(expression);
    const end = try parser.eatSemicolon(span.end) orelse return null;
    return try parser.tree.addNode(
        .{ .expression_statement = .{ .expression = expression } },
        .{ .start = span.start, .end = end },
    );
}

noinline fn parseAwaitUsingOrExpression(parser: *Parser) Error!?ast.NodeIndex {
    if (!variables.isAwaitUsingDeclarationAhead(parser)) {
        return parseExpressionOrLabeledStatementOrDirective(parser);
    }
    const start = try parser.take(.await) orelse return null;
    return variables.parseVariableDeclaration(parser, .{ .await_using = true }, start);
}

noinline fn parseAsyncFunctionOrExpression(parser: *Parser) Error!?ast.NodeIndex {
    const next = parser.peekAhead();
    if (next.tag != .function or next.hasLineTerminatorBefore()) {
        return parseExpressionOrLabeledStatementOrDirective(parser);
    }
    const start = try parser.take(.async) orelse return null;
    return functions.parseFunction(parser, .{ .is_async = true }, start);
}

// https://tc39.es/ecma262/#sec-labelled-statements
fn parseLabeledStatement(parser: *Parser, identifier: ast.NodeIndex) Error!?ast.NodeIndex {
    const span = parser.tree.span(identifier);
    const name = parser.tree.data(identifier).identifier_reference.name;
    const label = try parser.tree.addNode(.{ .label_identifier = .{ .name = name } }, span);

    _ = try parser.take(.colon) orelse return null;
    const body = try parseSingleStatement(parser) orelse return null;
    return try parser.finishAt(span.start, body, .{
        .labeled_statement = .{ .label = label, .body = body },
    });
}

// https://tc39.es/ecma262/#prod-BlockStatement
fn parseBlockStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (!try parser.expect(.left_brace, "Expected '{' to start the block", null)) return null;

    const body = try @call(.always_inline, Parser.parseBody, .{ parser, .right_brace, .other });
    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close the block",
        "Check that every '{' in the block has a matching '}'",
    )) return null;
    return try parser.finish(start, .{ .block_statement = .{ .body = body } });
}

// https://tc39.es/ecma262/#sec-switch-statement
noinline fn parseSwitchStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.@"switch") orelse return null;
    const discriminant = try parseParenthesizedHead(
        parser,
        "Expected '(' after 'switch'",
        "Expected ')' after the 'switch' expression",
    ) orelse return null;
    if (!try parser.expect(.left_brace, "Expected '{' to start the 'switch' body", null)) {
        return null;
    }

    const cases = parser.list();
    defer cases.deinit();
    while (parser.at(.case) or parser.at(.default)) {
        if (try parseSwitchCase(parser)) |case| {
            try cases.append(case);
        } else {
            // a failed case parks the cursor on a lexical error, resync so the loop cannot spin
            try parser.recover(.right_brace);
        }
    }
    const case_nodes = try cases.finish();

    if (!try parser.expect(.right_brace, "Expected '}' to close the 'switch' body", null)) {
        return null;
    }
    return try parser.finish(start, .{
        .switch_statement = .{ .discriminant = discriminant, .cases = case_nodes },
    });
}

fn parseSwitchCase(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const is_default = parser.at(.default);
    try parser.advance() orelse return null;
    const test_expr: ast.NodeIndex = if (is_default)
        .null
    else
        try expressions.parseFull(parser) orelse return null;

    var end = parser.current_token.span.end;
    const colon_message = if (is_default)
        "Expected ':' after 'default'"
    else
        "Expected ':' after the 'case' expression";
    if (!try parser.expect(.colon, colon_message, null)) return null;

    const consequent = parser.list();
    defer consequent.deinit();
    while (!parser.at(.case) and !parser.at(.default) and !parser.at(.right_brace) and
        !parser.at(.eof))
    {
        const statement = try parseStatement(parser, .{}) orelse break;
        end = parser.tree.span(statement).end;

        const data = parser.tree.data(statement);
        if (data == .variable_declaration) {
            const kind = data.variable_declaration.kind;
            if (kind == .using or kind == .await_using) try parser.report(
                parser.tree.span(statement),
                try parser.fmt(
                    "'{s}' declarations are not allowed in 'case' or 'default' clauses" ++
                        " unless contained within a block",
                    .{kind.toString()},
                ),
                .{ .help = "Wrap the clause body in a block" },
            );
        }
        try consequent.append(statement);
    }

    return try parser.tree.addNode(
        .{ .switch_case = .{ .@"test" = test_expr, .consequent = try consequent.finish() } },
        .{ .start = start, .end = end },
    );
}

// https://tc39.es/ecma262/#sec-if-statement
fn parseIfStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.@"if") orelse return null;
    const test_expr = try parseParenthesizedHead(
        parser,
        "Expected '(' after 'if'",
        "Expected ')' after the 'if' condition",
    ) orelse return null;
    const consequent = try parseSingleStatement(parser) orelse return null;
    const alternate: ast.NodeIndex = if (try parser.eat(.@"else") orelse return null)
        try parseSingleStatement(parser) orelse return null
    else
        .null;

    return try parser.finishAt(start, if (alternate != .null) alternate else consequent, .{
        .if_statement = .{ .@"test" = test_expr, .consequent = consequent, .alternate = alternate },
    });
}

// https://tc39.es/ecma262/#sec-while-statement
fn parseWhileStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.@"while") orelse return null;
    const test_expr = try parseParenthesizedHead(
        parser,
        "Expected '(' after 'while'",
        "Expected ')' after the 'while' condition",
    ) orelse return null;
    const body = try parseSingleStatement(parser) orelse return null;
    return try parser.finishAt(start, body, .{
        .while_statement = .{ .@"test" = test_expr, .body = body },
    });
}

// https://tc39.es/ecma262/#sec-do-while-statement
noinline fn parseDoWhileStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.do) orelse return null;
    const body = try parseSingleStatement(parser) orelse return null;
    if (!try parser.expect(.@"while", "Expected 'while' after the 'do' body", null)) return null;
    const test_expr = try parseParenthesizedHead(
        parser,
        "Expected '(' after 'while'",
        "Expected ')' after the 'while' condition",
    ) orelse return null;

    // ASI always inserts a semicolon after the `)` of do-while
    _ = try parser.eat(.semicolon) orelse return null;
    return try parser.finish(start, .{
        .do_while_statement = .{ .body = body, .@"test" = test_expr },
    });
}

// https://tc39.es/ecma262/#sec-with-statement
noinline fn parseWithStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.with) orelse return null;
    const object = try parseParenthesizedHead(
        parser,
        "Expected '(' after 'with'",
        "Expected ')' after the 'with' object",
    ) orelse return null;
    const body = try parseSingleStatement(parser) orelse return null;
    return try parser.finishAt(start, body, .{
        .with_statement = .{ .object = object, .body = body },
    });
}

// https://tc39.es/ecma262/#sec-break-statement
// https://tc39.es/ecma262/#sec-continue-statement
fn parseBreakOrContinueStatement(
    parser: *Parser,
    comptime keyword: TokenTag,
) Error!?ast.NodeIndex {
    const start = try parser.take(keyword) orelse return null;
    // break [no LineTerminator here] LabelIdentifier
    const label: ast.NodeIndex = if (parser.canInsertSemicolon() or parser.at(.semicolon))
        .null
    else
        try literals.parseLabelIdentifier(parser) orelse return null;

    return parser.finishStatement(start, switch (keyword) {
        .@"break" => .{ .break_statement = .{ .label = label } },
        .@"continue" => .{ .continue_statement = .{ .label = label } },
        else => comptime unreachable,
    });
}

// https://tc39.es/ecma262/#sec-return-statement
fn parseReturnStatement(parser: *Parser) Error!?ast.NodeIndex {
    const keyword = parser.current_token.span;
    if (!parser.context.@"return") try parser.report(
        keyword,
        "A 'return' statement can only be used within a function body",
        .{ .help = "Remove the 'return', or move it into a function" },
    );
    try parser.advance() orelse return null;

    // return [no LineTerminator here] Expression?
    const argument: ast.NodeIndex = if (parser.canInsertSemicolon() or parser.at(.semicolon))
        .null
    else
        try expressions.parseFull(parser) orelse return null;
    const end = try parser.eatSemicolon(
        if (argument != .null) parser.tree.span(argument).end else keyword.end,
    ) orelse return null;

    return try parser.tree.addNode(
        .{ .return_statement = .{ .argument = argument } },
        .{ .start = keyword.start, .end = end },
    );
}

// https://tc39.es/ecma262/#sec-throw-statement
fn parseThrowStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.throw) orelse return null;
    // throw [no LineTerminator here] Expression
    if (parser.current_token.hasLineTerminatorBefore()) return parser.fail(
        parser.current_token.span,
        "Line break not permitted after 'throw'",
        .{ .help = "Put the thrown expression on the same line as 'throw'" },
    );

    const argument = try expressions.parseFull(parser) orelse return null;
    const end = try parser.eatSemicolon(parser.tree.span(argument).end) orelse return null;
    return try parser.tree.addNode(
        .{ .throw_statement = .{ .argument = argument } },
        .{ .start = start, .end = end },
    );
}

// https://tc39.es/ecma262/#sec-try-statement
noinline fn parseTryStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.@"try") orelse return null;
    const block = try parseBlockStatement(parser) orelse return null;
    const handler: ast.NodeIndex = if (parser.at(.@"catch"))
        try parseCatchClause(parser) orelse return null
    else
        .null;
    const finalizer: ast.NodeIndex = if (try parser.eat(.finally) orelse return null)
        try parseBlockStatement(parser) orelse return null
    else
        .null;

    if (handler == .null and finalizer == .null) {
        return parser.failExpected("Expected 'catch' or 'finally' after the 'try' block", .{});
    }
    return try parser.finish(start, .{
        .try_statement = .{ .block = block, .handler = handler, .finalizer = finalizer },
    });
}

// https://tc39.es/ecma262/#prod-Catch
fn parseCatchClause(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.@"catch") orelse return null;

    var param: ast.NodeIndex = .null;
    if (try parser.eat(.left_paren) orelse return null) {
        param = try patterns.parseBindingPattern(parser) orelse return null;
        if (parser.tree.isTs() and parser.at(.colon)) {
            param = try ts_types.parsePatternTypeAnnotation(parser, param) orelse return null;
        }
        if (!try parser.expect(.right_paren, "Expected ')' after the 'catch' parameter", null)) {
            return null;
        }
    }

    const body = try parseBlockStatement(parser) orelse return null;
    return try parser.finish(start, .{ .catch_clause = .{ .param = param, .body = body } });
}

// https://tc39.es/ecma262/#sec-debugger-statement
fn parseDebuggerStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.debugger) orelse return null;
    return parser.finishStatement(start, .{ .debugger_statement = .{} });
}
