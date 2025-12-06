const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const grammar = @import("../grammar.zig");
const literals = @import("literals.zig");
const functions = @import("functions.zig");

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

    // if (token_type == .left_paren) {
    //     return parseParenthesizedExpressionOrArrowExpression(parser);
    // }

    return parsePrimaryExpression(parser);
}

inline fn parsePrimaryExpression(parser: *Parser) Error!?ast.NodeIndex {
    return switch (parser.current_token.type) {
        .identifier => literals.parseIdentifier(parser),
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
        .function => functions.parseFunction(parser, .{ .is_expression = true }),
        .async => functions.parseFunction(parser, .{ .is_expression = true, .is_async = true }),
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

fn parseParenthesizedOrArrowFunctionExpression(parser: *Parser) Error!?ast.NodeIndex {
    parser.advance(); // (
    // const cover_list = try grammar.coverParenthesizedExpressionAndArrowParameterList(parser) orelse return null;
    // parser.advance(); // )

    // const checkpoint = parser.scratch_a.begin();

    if (parser.current_token.type == .arrow) {
        // it's a arrow function
        // for (cover_list) |elem| {
        //     const data = parser.getData(elem);
        // }
    }
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
    const cover = try grammar.parseArrayCover(parser) orelse return null;

    // check for destructuring assignment: [a, b] = expr
    if (parser.current_token.type == .assign) {
        // it's a destructuring assignment pattern, so return as array pattern
        return try grammar.arrayCoverToPattern(parser, cover) orelse return null;
    }

    // regular array expression
    return grammar.arrayCoverToExpression(parser, cover);
}

fn parseObjectExpression(parser: *Parser) Error!?ast.NodeIndex {
    const cover = try grammar.parseObjectCover(parser) orelse return null;

    // check for destructuring assignment: {a, b} = expr
    if (parser.current_token.type == .assign) {
        // it's a destructuring assignment pattern, so return as pattern
        return try grammar.objectCoverToPattern(parser, cover) orelse return null;
    }

    // Regular object expression
    return grammar.objectCoverToExpression(parser, cover);
}
