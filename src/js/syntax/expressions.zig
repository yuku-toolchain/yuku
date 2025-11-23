const std = @import("std");
const token = @import("../token.zig");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;

const literals = @import("literals.zig");

pub fn parseExpression(parser: *Parser, prec: u5) ?*ast.Expression {
    var left: *ast.Expression = parseExpressionPrefix(parser) orelse return null;

    while (parser.current_token.type != .EOF) {
        const lbp = parser.current_token.type.leftBindingPower();

           if (prec > lbp or lbp == 0) break;

           left = parseExpressionInfix(parser, lbp, left) orelse return null;
    }

    return left;
}

fn parseExpressionInfix(parser: *Parser, prec: u5, left: *ast.Expression) ?*ast.Expression {
    const current_token = parser.current_token;

    // (x++, x--)
    if (current_token.type == .Increment or current_token.type == .Decrement) {
        return parseUpdateExpression(parser, false, left);
    }

    if (current_token.type.isBinaryOperator()) {
        return parseBinaryExpression(parser, prec, left);
    }

    if (current_token.type.isLogicalOperator()) {
        return parseLogicalExpression(parser, prec, left);
    }

    if (current_token.type.isAssignmentOperator()) {
        return parseAssignmentExpression(parser, prec, left);
    }

    // TODO: haha we need to remove this after we implement all expressions
    parser.err(
        current_token.span.start,
        current_token.span.end,
        parser.formatMessage("Unexpected token '{s}' in expression", .{current_token.lexeme}),
        "This operator or syntax is not yet supported by the parser",
    );

    return null;
}

fn parseExpressionPrefix(parser: *Parser) ?*ast.Expression {
    // (++x, --x)
    if (parser.current_token.type == .Increment or parser.current_token.type == .Decrement) {
        return parseUpdateExpression(parser, true, undefined);
    }

    if (parser.current_token.type.isUnaryOperator()) {
        return parseUnaryExpression(parser);
    }

    return parsePrimaryExpression(parser);
}

fn parsePrimaryExpression(parser: *Parser) ?*ast.Expression {
    return switch (parser.current_token.type) {
        .Identifier => literals.parseIdentifierReference(parser),
        .PrivateIdentifier => literals.parsePrivateIdentifier(parser),
        .StringLiteral => literals.parseStringLiteral(parser),
        .True, .False => literals.parseBooleanLiteral(parser),
        .NullLiteral => literals.parseNullLiteral(parser),
        .NumericLiteral, .HexLiteral, .OctalLiteral, .BinaryLiteral => literals.parseNumericLiteral(parser),
        .BigIntLiteral => literals.parseBigIntLiteral(parser),
        .Slash => literals.parseRegExpLiteral(parser),
        .TemplateHead => literals.parseTemplateLiteral(parser),
        .NoSubstitutionTemplate => literals.parseNoSubstitutionTemplateLiteral(parser),
        else => {
            const bad_token = parser.current_token;
            parser.err(
                bad_token.span.start,
                bad_token.span.end,
                "Unexpected token in expression position",
                "Expected an expression like a variable name, number, string, or other literal value",
            );
            return null;
        },
    };
}

fn parseUnaryExpression(parser: *Parser) ?*ast.Expression {
    const operator_token = parser.current_token;
    const operator = ast.UnaryOperator.fromToken(operator_token.type);
    const start = operator_token.span.start;

    parser.advance();

    const argument = parseExpression(parser, 14) orelse return null;

    const unary_expression = ast.UnaryExpression{
        .span = .{
            .start = start,
            .end = argument.getSpan().end,
        },
        .operator = operator,
        .argument = argument,
    };

    return parser.createNode(ast.Expression, .{ .unary_expression = unary_expression });
}

fn parseUpdateExpression(parser: *Parser, is_prefix: bool, left: ?*ast.Expression) ?*ast.Expression {
    const operator_token = parser.current_token;

    if (!is_prefix and operator_token.has_line_terminator_before) {
        parser.err(
            operator_token.span.start - 1,
            operator_token.span.end,
            "Line terminator not allowed before postfix operator",
            parser.formatMessage("Remove the line break before '{s}'", .{operator_token.lexeme}),
        );
        return null;
    }

    const operator = ast.UpdateOperator.fromToken(operator_token.type);

    const start = if (is_prefix) operator_token.span.start else left.?.getSpan().start;

    parser.advance();

    var argument: *ast.Expression = undefined;
    var end: u32 = undefined;

    if (is_prefix) {
        // ++x, --x
        argument = parseExpression(parser, 14) orelse return null;
        const arg_span = argument.getSpan();
        end = arg_span.end;

        if (!isValidAssignmentTarget(parser, argument)) {
            parser.err(
                arg_span.start,
                arg_span.end,
                "Invalid left-hand side expression in prefix operation",
                "Prefix increment/decrement requires a variable or property, not an expression result",
            );
            return null;
        }
    } else {
        // x++, x--
        argument = left orelse unreachable;

        end = operator_token.span.end;

        if (!isValidAssignmentTarget(parser, argument)) {
            const arg_span = argument.getSpan();
            parser.err(
                arg_span.start,
                arg_span.end,
                "Invalid left-hand side expression in postfix operation",
                "Postfix increment/decrement requires a variable or property, not an expression result",
            );
            return null;
        }
    }

    const update_expression = ast.UpdateExpression{
        .span = .{
            .start = start,
            .end = end,
        },
        .operator = operator,
        .prefix = is_prefix,
        .argument = argument,
    };

    return parser.createNode(ast.Expression, .{ .update_expression = update_expression });
}

fn parseBinaryExpression(parser: *Parser, prec: u5, left: *ast.Expression) ?*ast.Expression {
    const operator_token = parser.current_token;
    const operator = ast.BinaryOperator.fromToken(operator_token.type);

    parser.advance();

    // ** is right assosiative
    const next_prec = if (operator == .Exponent) prec else prec + 1;

    const right = parseExpression(parser, next_prec) orelse return null;

    const binary_expression = ast.BinaryExpression{
        .span = .{
            .start = left.getSpan().start,
            .end = right.getSpan().end,
        },
        .operator = operator,
        .left = left,
        .right = right,
    };

    return parser.createNode(ast.Expression, .{ .binary_expression = binary_expression });
}

fn parseLogicalExpression(parser: *Parser, prec: u5, left: *ast.Expression) ?*ast.Expression {
    const operator_token = parser.current_token;

    const operator = ast.LogicalOperator.fromToken(operator_token.type);

    parser.advance();

    const right = parseExpression(parser, prec + 1) orelse return null;

    const logical_expression = ast.LogicalExpression{
        .span = .{
            .start = left.getSpan().start,
            .end = right.getSpan().end,
        },
        .operator = operator,
        .left = left,
        .right = right,
    };

    return parser.createNode(ast.Expression, .{ .logical_expression = logical_expression });
}

fn parseAssignmentExpression(parser: *Parser, prec: u5, left: *ast.Expression) ?*ast.Expression {
    const operator_token = parser.current_token;
    const operator = ast.AssignmentOperator.fromToken(operator_token.type);

    if (!isValidAssignmentTarget(parser, left)) {
        const left_span = left.getSpan();
        parser.err(
            left_span.start,
            left_span.end,
            "Invalid left-hand side in assignment",
            "The left side of an assignment must be a variable or property access",
        );
        return null;
    }

    // for logical assignment operators (&&=, ||=, ??=), check for simple assignment target
    const is_logical_assign = operator == .LogicalAndAssign or operator == .LogicalOrAssign or operator == .NullishAssign;

    if (is_logical_assign and !isSimpleAssignmentTarget(parser, left)) {
        const left_span = left.getSpan();
        parser.err(
            left_span.start,
            left_span.end,
            "Invalid left-hand side in logical assignment",
            "Logical assignment operators (&&=, ||=, ??=) require a simple reference (variable or property access)",
        );
        return null;
    }

    parser.advance();

    // assignment is right-associative, so parse with same precedence
    const right = parseExpression(parser, prec) orelse return null;

    const target = parseAssignmentTarget(parser, left) orelse return null;

    const assignment_expression = ast.AssignmentExpression{
        .span = .{
            .start = left.getSpan().start,
            .end = right.getSpan().end,
        },
        .operator = operator,
        .left = target,
        .right = right,
    };

    return parser.createNode(ast.Expression, .{ .assignment_expression = assignment_expression });
}

fn parseAssignmentTarget(parser: *Parser, expr: *ast.Expression) ?*ast.AssignmentTarget {
    if (!isValidAssignmentTarget(parser, expr)) {
        return null;
    }

    const target = ast.AssignmentTarget{ .simple_assignment_target = expr };
    return parser.createNode(ast.AssignmentTarget, target);
}

// validators

pub inline fn isValidAssignmentTarget(parser: *Parser, expr: *ast.Expression) bool {
    _ = parser;
    return switch (expr.*) {
        .identifier_reference => true,
        // TODO: uncomment when add member_expression
        // .member_expression => true,

        else => false,
    };
}

pub inline fn isSimpleAssignmentTarget(parser: *Parser, expr: *ast.Expression) bool {
    _ = parser;
    return switch (expr.*) {
        .identifier_reference => true,
        // TODO: uncomment when add member_expression
        // .member_expression => true,

        else => false,
    };
}
