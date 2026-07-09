const std = @import("std");
const ast = @import("../../ast.zig");
const Precedence = @import("../../token.zig").Precedence;
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;

const expressions = @import("../expressions.zig");
const for_loop = @import("../for_loop.zig");
const patterns = @import("../patterns.zig");
const ts = @import("../ts/types.zig");
const tsrx = @import("root.zig");

const TsrxNodeWalkDepthMax: u32 = 64;

pub fn parseCodeBlock(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(parser.current_token.tag == .at);
    std.debug.assert(tsrx.isCodeBlockStart(parser));

    const start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '@'

    if (!try parser.expect(
        .left_brace,
        "Expected '{' after TSRX code block marker",
        "TSRX code blocks are written '@{ ... }' with no whitespace after '@'",
    )) return null;

    const allow_return_statement = parser.context.@"return";
    const parsed_body = blk: {
        const saved_return = parser.context.@"return";
        parser.context.@"return" = true;
        defer parser.context.@"return" = saved_return;

        break :blk try parser.parseBody(.right_brace, .other);
    };
    const code_block = try parseJsxCodeBlockBody(parser, parsed_body, .{
        .allow_return_statement = allow_return_statement,
    });

    if (parser.current_token.tag != .right_brace) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '}' to close TSRX code block",
            .{ .help = "Add '}' to close the '@{' block" },
        );
        return null;
    }

    const end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume '}'

    return try parser.tree.addNode(
        .{ .jsx_code_block = code_block },
        .{ .start = start, .end = end },
    );
}

pub fn parseControlFlowExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(parser.current_token.tag == .at);
    std.debug.assert(tsrx.isControlFlowDirectiveStart(parser));

    if (tsrx.isIfDirectiveStart(parser)) return parseTsrxIfExpression(parser);
    if (tsrx.isForDirectiveStart(parser)) return parseTsrxForExpression(parser);
    if (tsrx.isSwitchDirectiveStart(parser)) return parseTsrxSwitchExpression(parser);
    if (tsrx.isTryDirectiveStart(parser)) return parseTsrxTryExpression(parser);

    try parser.reportExpected(
        parser.current_token.span,
        "Expected supported TSRX control-flow directive",
        .{ .help = "Supported TSRX directives are '@if', '@for', '@switch', and '@try'." },
    );
    return null;
}

fn parseTsrxIfExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(tsrx.isIfDirectiveStart(parser));

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '@'

    if (!try parser.expect(
        .@"if",
        "Expected 'if' after '@'",
        "TSRX if directives are written '@if (...) { ... }'",
    )) return null;

    if (!try parser.expect(.left_paren, "Expected '(' after '@if'", null)) return null;

    const condition = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
        return null;

    if (!try parser.expect(.right_paren, "Expected ')' after '@if' condition", null)) return null;

    const consequent = try parseTsrxTemplateBlock(parser) orelse return null;

    const alternate = if (tsrx.isElseDirectiveStart(parser)) blk: {
        try parser.advance() orelse return null; // consume '@'
        if (!try parser.expect(
            .@"else",
            "Expected 'else' after '@'",
            "TSRX else clauses are written '@else { ... }'",
        )) return null;

        if (tsrx.isIfDirectiveStart(parser)) {
            break :blk try parseTsrxIfExpression(parser) orelse return null;
        }

        break :blk try parseTsrxTemplateBlock(parser) orelse return null;
    } else .null;

    const end = if (alternate != .null)
        parser.tree.span(alternate).end
    else
        parser.tree.span(consequent).end;

    return try parser.tree.addNode(
        .{ .jsx_if_expression = .{
            .@"test" = condition,
            .consequent = consequent,
            .alternate = alternate,
        } },
        .{ .start = start, .end = end },
    );
}

fn parseTsrxForExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(tsrx.isForDirectiveStart(parser));

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '@'

    const statement = try for_loop.parseForStatementWithBody(
        parser,
        false,
        parseTsrxTemplateBlock,
    ) orelse return null;

    const empty = if (parser.current_token.tag == .at and tsrx.isEmptyDirectiveStart(parser)) blk: {
        try parser.advance() orelse return null; // consume '@'
        if (!try parser.expect(
            .identifier,
            "Expected 'empty' after '@'",
            "TSRX empty clauses are written '@empty { ... }'",
        )) return null;

        break :blk try parseTsrxTemplateBlock(parser) orelse return null;
    } else .null;

    const end = if (empty != .null)
        parser.tree.span(empty).end
    else
        parser.tree.span(statement).end;

    return try parser.tree.addNode(
        .{ .jsx_for_expression = .{
            .statement = statement,
            .empty = empty,
        } },
        .{ .start = start, .end = end },
    );
}

fn parseTsrxTryExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(tsrx.isTryDirectiveStart(parser));

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '@'

    const try_start = parser.current_token.span.start;
    if (!try parser.expect(
        .@"try",
        "Expected 'try' after '@'",
        "TSRX try directives are written '@try { ... }'.",
    )) return null;

    const block = try parseTsrxTemplateBlock(parser) orelse return null;

    const pending = if (parser.current_token.tag == .at and
        tsrx.isPendingDirectiveStart(parser))
    blk: {
        try parser.advance() orelse return null; // consume '@'
        if (!try parser.expect(
            .identifier,
            "Expected 'pending' after '@'",
            "TSRX pending clauses are written '@pending { ... }'.",
        )) return null;

        break :blk try parseTsrxTemplateBlock(parser) orelse return null;
    } else .null;

    const handler = if (parser.current_token.tag == .at and
        tsrx.isCatchDirectiveStart(parser))
    blk: {
        break :blk try parseTsrxCatchClause(parser) orelse return null;
    } else .null;

    if (pending == .null and handler == .null) {
        try parser.report(
            .{ .start = start, .end = parser.tree.span(block).end },
            "TSRX try directive requires '@pending' or '@catch'",
            .{},
        );
        return null;
    }

    const end = if (handler != .null)
        parser.tree.span(handler).end
    else if (pending != .null)
        parser.tree.span(pending).end
    else
        parser.tree.span(block).end;

    const statement = try parser.tree.addNode(
        .{ .try_statement = .{
            .block = block,
            .handler = handler,
            .finalizer = .null,
        } },
        .{ .start = try_start, .end = end },
    );

    return try parser.tree.addNode(
        .{ .jsx_try_expression = .{
            .statement = statement,
            .pending = pending,
        } },
        .{ .start = start, .end = end },
    );
}

fn parseTsrxCatchClause(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(tsrx.isCatchDirectiveStart(parser));

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '@'

    if (!try parser.expect(
        .@"catch",
        "Expected 'catch' after '@'",
        "TSRX catch clauses are written '@catch { ... }' or '@catch (error) { ... }'.",
    )) return null;

    var param: ast.NodeIndex = .null;
    var reset_param: ast.NodeIndex = .null;
    if (parser.current_token.tag == .left_paren) {
        try parser.advance() orelse return null; // consume '('
        param = try patterns.parseBindingPattern(parser) orelse return null;

        if (parser.tree.isTs() and parser.current_token.tag == .colon) {
            const annotation = try ts.parseTypeAnnotation(parser) orelse return null;
            ts.applyTypeAnnotationToPattern(parser, param, annotation);
        }

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null; // consume ','
            reset_param = try patterns.parseBindingPattern(parser) orelse return null;

            if (parser.tree.isTs() and parser.current_token.tag == .colon) {
                const annotation = try ts.parseTypeAnnotation(parser) orelse return null;
                ts.applyTypeAnnotationToPattern(parser, reset_param, annotation);
            }
        }

        if (!try parser.expect(.right_paren, "Expected ')' after catch parameter", null))
            return null;
    }

    const body = try parseTsrxTemplateBlock(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .catch_clause = .{
            .param = param,
            .reset_param = reset_param,
            .body = body,
        } },
        .{ .start = start, .end = parser.tree.span(body).end },
    );
}

fn parseTsrxSwitchExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(tsrx.isSwitchDirectiveStart(parser));

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '@'

    const switch_start = parser.current_token.span.start;
    if (!try parser.expect(
        .@"switch",
        "Expected 'switch' after '@'",
        "TSRX switch directives are written '@switch (...) { ... }'",
    )) return null;

    if (!try parser.expect(.left_paren, "Expected '(' after '@switch'", null)) return null;

    const discriminant = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
        return null;

    if (!try parser.expect(
        .right_paren,
        "Expected ')' after '@switch' expression",
        null,
    )) return null;

    if (!try parser.expect(
        .left_brace,
        "Expected '{' to start TSRX switch body",
        "TSRX switch bodies contain '@case' and '@default' clauses.",
    )) return null;

    const cases = try parseTsrxSwitchCases(parser);

    const switch_end = parser.current_token.span.end;
    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close TSRX switch body",
        "Add '}' after the last TSRX switch clause.",
    )) return null;

    const statement = try parser.tree.addNode(
        .{ .switch_statement = .{
            .discriminant = discriminant,
            .cases = cases,
        } },
        .{ .start = switch_start, .end = switch_end },
    );

    return try parser.tree.addNode(
        .{ .jsx_switch_expression = .{ .statement = statement } },
        .{ .start = start, .end = switch_end },
    );
}

fn parseTsrxSwitchCases(parser: *Parser) Error!ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    var saw_default = false;
    while (parser.current_token.tag != .right_brace and parser.current_token.tag != .eof) {
        const case_node = try parseTsrxSwitchCase(parser, &saw_default) orelse return try parser.flushToExtras(&parser.scratch_a, checkpoint);
        try parser.scratch_a.append(parser.allocator(), case_node);
    }

    return parser.flushToExtras(&parser.scratch_a, checkpoint);
}

fn parseTsrxSwitchCase(parser: *Parser, saw_default: *bool) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .at) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected TSRX switch clause",
            .{ .help = "Switch clauses are written '@case expr: { ... }' or '@default: { ... }'." },
        );
        return null;
    }

    const start = parser.current_token.span.start;
    const is_default = tsrx.isDefaultDirectiveStart(parser);
    if (!is_default and !tsrx.isCaseDirectiveStart(parser)) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected TSRX switch clause",
            .{ .help = "Switch clauses are written '@case expr: { ... }' or '@default: { ... }'." },
        );
        return null;
    }

    try parser.advance() orelse return null; // consume '@'

    var test_expr: ast.NodeIndex = .null;
    if (is_default) {
        if (saw_default.*) {
            try parser.report(parser.current_token.span, "Multiple default clauses", .{});
        }
        saw_default.* = true;
        if (!try parser.expect(.default, "Expected 'default' after '@'", null)) return null;
    } else {
        if (!try parser.expect(.case, "Expected 'case' after '@'", null)) return null;
        test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
            return null;
    }

    if (!try parser.expect(.colon, "Expected ':' after TSRX switch clause", null)) return null;

    const body = try parseTsrxTemplateBlockWithOptions(parser, .{
        .allow_return_statement = true,
    }) orelse return null;
    const block = parser.tree.data(body).block_statement;
    try validateTsrxSwitchCaseStatements(parser, block.body);

    return try parser.tree.addNode(
        .{ .switch_case = .{
            .@"test" = test_expr,
            .consequent = block.body,
        } },
        .{ .start = start, .end = parser.tree.span(body).end },
    );
}

const ParseTsrxTemplateBlockOptions = struct {
    allow_return_statement: bool = false,
};

fn parseTsrxTemplateBlock(parser: *Parser) Error!?ast.NodeIndex {
    return parseTsrxTemplateBlockWithOptions(parser, .{});
}

fn parseTsrxTemplateBlockWithOptions(
    parser: *Parser,
    opts: ParseTsrxTemplateBlockOptions,
) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (!try parser.expect(
        .left_brace,
        "Expected '{' after TSRX control-flow directive",
        "TSRX control-flow bodies are written with braces.",
    )) return null;

    const parsed_body = blk: {
        const saved_return = parser.context.@"return";
        parser.context.@"return" = true;
        defer parser.context.@"return" = saved_return;

        break :blk try parser.parseBody(.right_brace, .other);
    };
    const body = try parseTsrxTemplateBlockBody(parser, parsed_body, .{
        .allow_return_statement = opts.allow_return_statement,
    });

    const end = parser.current_token.span.end;
    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close TSRX control-flow block",
        "Add '}' to close the control-flow block.",
    )) return null;

    return try parser.tree.addNode(
        .{ .block_statement = .{ .body = body } },
        .{ .start = start, .end = end },
    );
}

const ParseJsxCodeBlockBodyOptions = struct {
    allow_return_statement: bool = false,
};

fn parseTsrxTemplateBlockBody(
    parser: *Parser,
    parsed_body: ast.IndexRange,
    opts: ParseJsxCodeBlockBodyOptions,
) Error!ast.IndexRange {
    const code_block = try parseJsxCodeBlockBody(parser, parsed_body, opts);

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    for (parser.tree.extra(code_block.body)) |statement| {
        try parser.scratch_a.append(parser.allocator(), statement);
    }

    if (code_block.render != .null) {
        try parser.scratch_a.append(parser.allocator(), code_block.render);
    }

    return try parser.flushToExtras(&parser.scratch_a, checkpoint);
}

fn parseJsxCodeBlockBody(
    parser: *Parser,
    parsed_body: ast.IndexRange,
    opts: ParseJsxCodeBlockBodyOptions,
) Error!ast.JSXCodeBlock {
    std.debug.assert(parser.tree.isTsrx());

    const parsed_statements = parser.tree.extra(parsed_body);
    std.debug.assert(parsed_statements.len == parsed_body.len);

    var significant_count: u32 = parsed_body.len;

    while (significant_count > 0) {
        const statement = parsed_statements[significant_count - 1];
        if (parser.tree.data(statement) != .empty_statement) break;
        significant_count -= 1;
    }

    var render_seen = false;
    var index: u32 = 0;
    while (index < significant_count) : (index += 1) {
        const statement = parsed_statements[index];
        if (parseJsxCodeBlockRender(parser, statement) != .null) {
            if (render_seen) {
                try parser.report(
                    parser.tree.span(statement),
                    "A code block renders a single node",
                    .{ .help = "Wrap multiple nodes or text in a fragment '<>...</>'." },
                );
            }
            render_seen = true;
        } else {
            if (render_seen) {
                try parser.report(
                    parser.tree.span(statement),
                    "Code must be at the top of '@{ }'",
                    .{ .help = "Statements cannot follow the rendered output." },
                );
            }
        }
    }

    var body = parsed_body;
    body.len = significant_count;

    if (significant_count == 0) {
        return .{ .body = body, .render = .null };
    }

    const last_statement = parsed_statements[significant_count - 1];
    const render = parseJsxCodeBlockRender(parser, last_statement);
    if (render == .null) {
        if (!opts.allow_return_statement) {
            try validateTsrxTemplateReturnStatements(parser, body);
        }
        return .{ .body = body, .render = .null };
    }

    body.len -= 1;
    if (!opts.allow_return_statement) {
        try validateTsrxTemplateReturnStatements(parser, body);
    }
    return .{ .body = body, .render = render };
}

fn parseJsxCodeBlockRender(parser: *const Parser, statement: ast.NodeIndex) ast.NodeIndex {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(statement != .null);

    switch (parser.tree.data(statement)) {
        .expression_statement => |expr_stmt| {
            if (parseJsxCodeBlockIsRenderOutput(parser, expr_stmt.expression)) {
                return expr_stmt.expression;
            }
        },
        else => {},
    }

    return .null;
}

fn parseJsxCodeBlockIsRenderOutput(parser: *const Parser, node: ast.NodeIndex) bool {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(node != .null);

    return switch (parser.tree.data(node)) {
        .jsx_element,
        .jsx_fragment,
        .jsx_style_element,
        .jsx_code_block,
        .jsx_if_expression,
        .jsx_for_expression,
        .jsx_switch_expression,
        .jsx_try_expression,
        => true,
        else => false,
    };
}

fn validateTsrxTemplateReturnStatements(
    parser: *Parser,
    statements: ast.IndexRange,
) Error!void {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(statements.start + statements.len <= parser.tree.extras.items.len);

    for (parser.tree.extra(statements)) |statement| {
        try validateTsrxTemplateReturnStatement(parser, statement, false, 0);
    }
}

fn validateTsrxTemplateReturnStatement(
    parser: *Parser,
    statement: ast.NodeIndex,
    inside_loop: bool,
    depth: u32,
) Error!void {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(depth <= TsrxNodeWalkDepthMax);

    if (statement == .null) return;
    if (depth == TsrxNodeWalkDepthMax) {
        try parser.report(
            parser.tree.span(statement),
            "TSRX template block is too deeply nested to validate",
            .{},
        );
        return;
    }

    const next_depth = depth + 1;
    switch (parser.tree.data(statement)) {
        .return_statement => {
            if (inside_loop) return;
            try parser.report(
                parser.tree.span(statement),
                "`return` is invalid inside TSRX template blocks",
                .{ .help = "Use rendered output as the final expression instead." },
            );
        },
        .function,
        .arrow_function_expression,
        => return,
        .block_statement => |block| try validateTsrxTemplateReturnRange(
            parser,
            block.body,
            inside_loop,
            next_depth,
        ),
        .if_statement => |stmt| {
            try validateTsrxTemplateReturnStatement(
                parser,
                stmt.consequent,
                inside_loop,
                next_depth,
            );
            if (stmt.alternate != .null) {
                try validateTsrxTemplateReturnStatement(
                    parser,
                    stmt.alternate,
                    inside_loop,
                    next_depth,
                );
            }
        },
        .switch_statement => |stmt| {
            for (parser.tree.extra(stmt.cases)) |case_node| {
                const case_data = parser.tree.data(case_node).switch_case;
                try validateTsrxTemplateReturnRange(
                    parser,
                    case_data.consequent,
                    inside_loop,
                    next_depth,
                );
            }
        },
        .for_statement => |stmt| try validateTsrxTemplateReturnStatement(
            parser,
            stmt.body,
            true,
            next_depth,
        ),
        .for_in_statement => |stmt| try validateTsrxTemplateReturnStatement(
            parser,
            stmt.body,
            true,
            next_depth,
        ),
        .for_of_statement => |stmt| try validateTsrxTemplateReturnStatement(
            parser,
            stmt.body,
            true,
            next_depth,
        ),
        .while_statement => |stmt| try validateTsrxTemplateReturnStatement(
            parser,
            stmt.body,
            true,
            next_depth,
        ),
        .do_while_statement => |stmt| try validateTsrxTemplateReturnStatement(
            parser,
            stmt.body,
            true,
            next_depth,
        ),
        .labeled_statement => |stmt| try validateTsrxTemplateReturnStatement(
            parser,
            stmt.body,
            inside_loop,
            next_depth,
        ),
        .with_statement => |stmt| try validateTsrxTemplateReturnStatement(
            parser,
            stmt.body,
            inside_loop,
            next_depth,
        ),
        .try_statement => |stmt| {
            try validateTsrxTemplateReturnStatement(
                parser,
                stmt.block,
                inside_loop,
                next_depth,
            );
            if (stmt.handler != .null) {
                try validateTsrxTemplateReturnStatement(
                    parser,
                    stmt.handler,
                    inside_loop,
                    next_depth,
                );
            }
            if (stmt.finalizer != .null) {
                try validateTsrxTemplateReturnStatement(
                    parser,
                    stmt.finalizer,
                    inside_loop,
                    next_depth,
                );
            }
        },
        .catch_clause => |clause| try validateTsrxTemplateReturnStatement(
            parser,
            clause.body,
            inside_loop,
            next_depth,
        ),
        else => {},
    }
}

fn validateTsrxTemplateReturnRange(
    parser: *Parser,
    statements: ast.IndexRange,
    inside_loop: bool,
    depth: u32,
) Error!void {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(depth <= TsrxNodeWalkDepthMax);
    std.debug.assert(statements.start + statements.len <= parser.tree.extras.items.len);

    for (parser.tree.extra(statements)) |statement| {
        try validateTsrxTemplateReturnStatement(parser, statement, inside_loop, depth);
    }
}

fn validateTsrxSwitchCaseStatements(
    parser: *Parser,
    statements: ast.IndexRange,
) Error!void {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(statements.start + statements.len <= parser.tree.extras.items.len);

    try validateTsrxSwitchCaseRange(parser, statements, true, 0);
}

fn validateTsrxSwitchCaseRange(
    parser: *Parser,
    statements: ast.IndexRange,
    report_break: bool,
    depth: u32,
) Error!void {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(depth <= TsrxNodeWalkDepthMax);
    std.debug.assert(statements.start + statements.len <= parser.tree.extras.items.len);

    for (parser.tree.extra(statements)) |statement| {
        try validateTsrxSwitchCaseStatement(parser, statement, report_break, depth);
    }
}

fn validateTsrxSwitchCaseStatement(
    parser: *Parser,
    statement: ast.NodeIndex,
    report_break: bool,
    depth: u32,
) Error!void {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(depth <= TsrxNodeWalkDepthMax);

    if (statement == .null) return;
    if (depth == TsrxNodeWalkDepthMax) {
        try parser.report(
            parser.tree.span(statement),
            "TSRX switch case is too deeply nested to validate",
            .{},
        );
        return;
    }

    const next_depth = depth + 1;
    switch (parser.tree.data(statement)) {
        .break_statement => {
            if (!report_break) return;
            try parser.report(
                parser.tree.span(statement),
                "`break` is invalid inside `@switch` cases.",
                .{},
            );
        },
        .return_statement => try parser.report(
            parser.tree.span(statement),
            "`return` is invalid inside `@switch` cases.",
            .{},
        ),
        .function,
        .arrow_function_expression,
        .for_statement,
        .for_in_statement,
        .for_of_statement,
        .while_statement,
        .do_while_statement,
        => return,
        .block_statement => |block| try validateTsrxSwitchCaseRange(
            parser,
            block.body,
            report_break,
            next_depth,
        ),
        .if_statement => |stmt| {
            try validateTsrxSwitchCaseStatement(
                parser,
                stmt.consequent,
                report_break,
                next_depth,
            );
            if (stmt.alternate != .null) {
                try validateTsrxSwitchCaseStatement(
                    parser,
                    stmt.alternate,
                    report_break,
                    next_depth,
                );
            }
        },
        .switch_statement => |stmt| {
            for (parser.tree.extra(stmt.cases)) |case_node| {
                const case_data = parser.tree.data(case_node).switch_case;
                try validateTsrxSwitchCaseRange(
                    parser,
                    case_data.consequent,
                    false,
                    next_depth,
                );
            }
        },
        .labeled_statement => |stmt| try validateTsrxSwitchCaseStatement(
            parser,
            stmt.body,
            report_break,
            next_depth,
        ),
        .with_statement => |stmt| try validateTsrxSwitchCaseStatement(
            parser,
            stmt.body,
            report_break,
            next_depth,
        ),
        .try_statement => |stmt| {
            try validateTsrxSwitchCaseStatement(
                parser,
                stmt.block,
                report_break,
                next_depth,
            );
            if (stmt.handler != .null) {
                try validateTsrxSwitchCaseStatement(
                    parser,
                    stmt.handler,
                    report_break,
                    next_depth,
                );
            }
            if (stmt.finalizer != .null) {
                try validateTsrxSwitchCaseStatement(
                    parser,
                    stmt.finalizer,
                    report_break,
                    next_depth,
                );
            }
        },
        .catch_clause => |clause| try validateTsrxSwitchCaseStatement(
            parser,
            clause.body,
            report_break,
            next_depth,
        ),
        else => {},
    }
}
