const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const array = @import("array.zig");
const object = @import("object.zig");
const literals = @import("literals.zig");
const functions = @import("functions.zig");
const parenthesized = @import("parenthesized.zig");

pub fn parseExpression(parser: *Parser, precedence: u5) Error!?ast.NodeIndex {
    var left = try parsePrefix(parser) orelse return null;

    while (true) {
        const current_type = parser.current_token.type;
        if (current_type == .eof) break;

        const left_binding_power = parser.current_token.leftBindingPower();
        if (precedence > left_binding_power or left_binding_power == 0) break;

        left = try parseInfix(parser, left_binding_power, left) orelse return null;
    }

    return left;
}

fn parseInfix(parser: *Parser, precedence: u5, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const current = parser.current_token;

    if (current.type == .increment or current.type == .decrement) {
        return parseUpdateExpression(parser, false, left);
    }

    if (current.type.isBinaryOperator()) {
        return parseBinaryExpression(parser, precedence, left);
    }

    if (current.type.isLogicalOperator()) {
        return parseLogicalExpression(parser, precedence, left);
    }

    if (current.type.isAssignmentOperator()) {
        return parseAssignmentExpression(parser, precedence, left);
    }

    try parser.reportFmt(
        current.span,
        "Unexpected token '{s}' in expression",
        .{current.lexeme},
        .{ .help = "This token cannot be used here. Expected an operator, semicolon, or end of expression." },
    );
    return null;
}

fn parsePrefix(parser: *Parser) Error!?ast.NodeIndex {
    const token_type = parser.current_token.type;

    if (token_type == .increment or token_type == .decrement) {
        return parseUpdateExpression(parser, true, ast.null_node);
    }

    if (token_type.isUnaryOperator()) {
        return parseUnaryExpression(parser);
    }

    if (token_type == .left_paren) {
        return parseParenthesizedOrArrowFunction(parser, false);
    }

    return parsePrimaryExpression(parser);
}

inline fn parsePrimaryExpression(parser: *Parser) Error!?ast.NodeIndex {
    return switch (parser.current_token.type) {
        .identifier => parseIdentifierOrArrowFunction(parser),
        .private_identifier => literals.parsePrivateIdentifier(parser),
        .string_literal => literals.parseStringLiteral(parser),
        .true, .false => literals.parseBooleanLiteral(parser),
        .null_literal => literals.parseNullLiteral(parser),
        .numeric_literal, .hex_literal, .octal_literal, .binary_literal => literals.parseNumericLiteral(parser),
        .bigint_literal => literals.parseBigIntLiteral(parser),
        .slash => literals.parseRegExpLiteral(parser),
        .template_head => literals.parseTemplateLiteral(parser),
        .no_substitution_template => literals.parseNoSubstitutionTemplate(parser),
        .left_bracket => parseArrayExpression(parser),
        .left_brace => parseObjectExpression(parser),
        .function => functions.parseFunction(parser, .{ .is_expression = true }, null),
        .async => parseAsyncFunctionOrArrow(parser),
        else => {
            const tok = parser.current_token;
            if (tok.type == .eof) {
                try parser.report(
                    tok.span,
                    "Unexpected end of input while parsing expression",
                    .{ .help = "The parser reached the end of the file but expected an expression. Check for missing values or unclosed brackets." },
                );
            } else {
                try parser.reportFmt(
                    tok.span,
                    "Unexpected token '{s}'",
                    .{tok.lexeme},
                    .{ .help = "Expected an expression (identifier, literal, array, object, or parenthesized expression)." },
                );
            }
            return null;
        },
    };
}

/// parenthesized expression or arrow function: (a) or (a, b) => ...
fn parseParenthesizedOrArrowFunction(parser: *Parser, is_async: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const cover = try parenthesized.parseCover(parser) orelse return null;

    //  [no LineTerminator here] => ConciseBody
    if (parser.current_token.type == .arrow and !parser.current_token.has_line_terminator_before) {
        return parenthesized.coverToArrowFunction(parser, cover, is_async, start);
    }

    // not an arrow function - convert to parenthesized expression
    return parenthesized.coverToExpression(parser, cover);
}

/// identifier, checking for arrow function: x => ...
fn parseIdentifierOrArrowFunction(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const id = try literals.parseIdentifier(parser) orelse return null;

    //  [no LineTerminator here] => ConciseBody
    if (parser.current_token.type == .arrow and !parser.current_token.has_line_terminator_before) {
        return parenthesized.identifierToArrowFunction(parser, id, false, start);
    }

    return id;
}

/// async function or async arrow function
fn parseAsyncFunctionOrArrow(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const async_id = try literals.parseIdentifier(parser); // save as id and consume 'async'

    // async function ...
    if (parser.current_token.type == .function) {
        return functions.parseFunction(parser, .{ .is_expression = true, .is_async = true }, start);
    }

    // async (params) => ...
    if (parser.current_token.type == .left_paren) {
        return parseParenthesizedOrArrowFunction(parser, true);
    }

    //  [no LineTerminator here] => ConciseBody
    if (parser.current_token.type == .identifier and !parser.current_token.has_line_terminator_before) {
        const id = try literals.parseIdentifier(parser) orelse return null;

        if (parser.current_token.type == .arrow and !parser.current_token.has_line_terminator_before) {
            return parenthesized.identifierToArrowFunction(parser, id, true, start);
        }

        try parser.report(
            parser.current_token.span,
            "Expected '=>' after async arrow function parameter",
            .{ .help = "Use 'async x => ...' or 'async (x) => ...' for async arrow functions." },
        );
        return null;
    }

    return async_id;
}

fn parseUnaryExpression(parser: *Parser) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    try parser.advance();

    const argument = try parseExpression(parser, 14) orelse return null;

    return try parser.addNode(
        .{
            .unary_expression = .{
                .argument = argument,
                .operator = ast.UnaryOperator.fromToken(operator_token.type),
            },
        },
        .{ .start = operator_token.span.start, .end = parser.getSpan(argument).end },
    );
}

fn parseUpdateExpression(parser: *Parser, prefix: bool, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.UpdateOperator.fromToken(operator_token.type);
    try parser.advance();

    if (prefix) {
        const argument = try parseExpression(parser, 14) orelse return null;
        const span = parser.getSpan(argument);

        if (!isSimpleAssignmentTarget(parser, argument)) {
            try parser.report(
                span,
                "Invalid operand for increment/decrement operator",
                .{ .help = "The '++' and '--' operators require a variable or property reference, not a literal or complex expression." },
            );
            return null;
        }

        return try parser.addNode(
            .{ .update_expression = .{ .argument = argument, .operator = operator, .prefix = true } },
            .{ .start = operator_token.span.start, .end = span.end },
        );
    }

    if (!isSimpleAssignmentTarget(parser, left)) {
        const span = parser.getSpan(left);
        try parser.report(
            span,
            "Invalid operand for increment/decrement operator",
            .{ .help = "The '++' and '--' operators require a variable or property reference, not a literal or complex expression." },
        );
        return null;
    }

    return try parser.addNode(
        .{ .update_expression = .{ .argument = left, .operator = operator, .prefix = false } },
        .{ .start = parser.getSpan(left).start, .end = operator_token.span.end },
    );
}

fn parseBinaryExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.BinaryOperator.fromToken(operator_token.type);
    try parser.advance();

    const next_precedence = if (operator == .exponent) precedence else precedence + 1;
    const right = try parseExpression(parser, next_precedence) orelse return null;

    return try parser.addNode(
        .{ .binary_expression = .{ .left = left, .right = right, .operator = operator } },
        .{ .start = parser.getSpan(left).start, .end = parser.getSpan(right).end },
    );
}

fn parseLogicalExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    try parser.advance();

    const right = try parseExpression(parser, precedence + 1) orelse return null;
    const current_operator = ast.LogicalOperator.fromToken(operator_token.type);

    // check for operator mixing: can't mix ?? with && or ||
    const left_data = parser.getData(left);
    const right_data = parser.getData(right);

    if (left_data == .logical_expression or right_data == .logical_expression) {
        const operator_to_check = if (left_data == .logical_expression) left_data.logical_expression.operator else right_data.logical_expression.operator;

        if ((current_operator == .nullish_coalescing) != (operator_to_check == .nullish_coalescing)) {
            const left_span = parser.getSpan(left);
            try parser.report(
                .{ .start = left_span.start, .end = parser.getSpan(right).end },
                "Logical expressions and nullish coalescing cannot be mixed",
                .{ .help = "Wrap either expression in parentheses" },
            );
            return null;
        }
    }

    return try parser.addNode(
        .{
            .logical_expression = .{
                .left = left,
                .right = right,
                .operator = current_operator,
            },
        },
        .{ .start = parser.getSpan(left).start, .end = parser.getSpan(right).end },
    );
}

fn parseAssignmentExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.AssignmentOperator.fromToken(operator_token.type);
    const left_span = parser.getSpan(left);

    // validate that left side can be assigned to
    if (!isValidAssignmentTarget(parser, left)) {
        try parser.report(
            left_span,
            "Invalid left-hand side in assignment",
            .{ .help = "The left side of an assignment must be a variable, property access, or destructuring pattern." },
        );
        return null;
    }

    // logical assignments (&&=, ||=, ??=) require simple targets
    const is_logical = operator == .logical_and_assign or operator == .logical_or_assign or operator == .nullish_assign;
    if (is_logical and !isSimpleAssignmentTarget(parser, left)) {
        try parser.report(
            left_span,
            "Invalid left-hand side in logical assignment",
            .{ .help = "Logical assignment operators (&&=, ||=, ??=) require a simple reference like a variable or property, not a destructuring pattern." },
        );
        return null;
    }

    try parser.advance();

    const right = try parseExpression(parser, precedence) orelse return null;

    return try parser.addNode(
        .{ .assignment_expression = .{ .left = left, .right = right, .operator = operator } },
        .{ .start = left_span.start, .end = parser.getSpan(right).end },
    );
}

/// AssignmentTarget: can be simple (identifier/member) or pattern (destructuring)
pub fn isValidAssignmentTarget(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.getData(index)) {
        // SimpleAssignmentTarget
        .identifier_reference => true,
        // TODO: add member expressions when implemented
        // .member_expression, .computed_member_expression => true,

        // AssignmentPattern (destructuring)
        .array_pattern, .object_pattern => true,

        else => false,
    };
}

/// SimpleAssignmentTarget: only identifier and member expressions (no destructuring)
pub fn isSimpleAssignmentTarget(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.getData(index)) {
        .identifier_reference => true,
        // TODO: add member expressions when implemented
        // .member_expression, .computed_member_expression => true,
        else => false,
    };
}

fn parseArrayExpression(parser: *Parser) Error!?ast.NodeIndex {
    const saved_flag = parser.state.cover_has_init_name;
    parser.state.cover_has_init_name = false;

    const cover = try array.parseCover(parser) orelse return null;
    const needs_validation = parser.state.cover_has_init_name;
    parser.state.cover_has_init_name = saved_flag or needs_validation;

    if (parser.current_token.type == .assign) {
        return try array.coverToPattern(parser, cover);
    }

    return array.coverToExpression(parser, cover, needs_validation);
}

fn parseObjectExpression(parser: *Parser) Error!?ast.NodeIndex {
    const saved_flag = parser.state.cover_has_init_name;
    parser.state.cover_has_init_name = false;

    const cover = try object.parseCover(parser) orelse return null;
    const needs_validation = parser.state.cover_has_init_name;
    parser.state.cover_has_init_name = saved_flag or needs_validation;

    if (parser.current_token.type == .assign) {
        return try object.coverToPattern(parser, cover);
    }

    return object.coverToExpression(parser, cover, needs_validation);
}
