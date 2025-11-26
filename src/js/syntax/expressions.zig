const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const literals = @import("literals.zig");

pub fn parseExpression(parser: *Parser, precedence: u5) ?ast.NodeIndex {
    var left = parsePrefix(parser) orelse return null;
    while (parser.current_token.type != .EOF) {
        const left_binding_power = parser.current_token.leftBindingPower();
        if (precedence > left_binding_power or left_binding_power == 0) break;
        left = parseInfix(parser, left_binding_power, left) orelse return null;
    }
    return left;
}

fn parseInfix(parser: *Parser, precedence: u5, left: ast.NodeIndex) ?ast.NodeIndex {
    const current = parser.current_token;
    if (current.type == .Increment or current.type == .Decrement) {
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
    parser.err(current.span.start, current.span.end, parser.formatMessage("Unexpected token '{s}'", .{current.lexeme}), null);
    return null;
}

fn parsePrefix(parser: *Parser) ?ast.NodeIndex {
    if (parser.current_token.type == .Increment or parser.current_token.type == .Decrement) {
        return parseUpdateExpression(parser, true, ast.null_node);
    }
    if (parser.current_token.type.isUnaryOperator()) {
        return parseUnaryExpression(parser);
    }
    return parsePrimaryExpression(parser);
}

fn parsePrimaryExpression(parser: *Parser) ?ast.NodeIndex {
    return switch (parser.current_token.type) {
        .Identifier => literals.parseIdentifier(parser),
        .PrivateIdentifier => literals.parsePrivateIdentifier(parser),
        .StringLiteral => literals.parseStringLiteral(parser),
        .True, .False => literals.parseBooleanLiteral(parser),
        .NullLiteral => literals.parseNullLiteral(parser),
        .NumericLiteral, .HexLiteral, .OctalLiteral, .BinaryLiteral => literals.parseNumericLiteral(parser),
        .BigIntLiteral => literals.parseBigIntLiteral(parser),
        .Slash => literals.parseRegExpLiteral(parser),
        .TemplateHead => literals.parseTemplateLiteral(parser),
        .NoSubstitutionTemplate => literals.parseNoSubstitutionTemplate(parser),
        .LeftBracket => parseArrayExpression(parser),
        .LeftBrace => parseObjectExpression(parser),
        else => {
            parser.err(parser.current_token.span.start, parser.current_token.span.end, "Unexpected token", null);
            return null;
        },
    };
}

fn parseUnaryExpression(parser: *Parser) ?ast.NodeIndex {
    const operator_token = parser.current_token;
    parser.advance();
    const argument = parseExpression(parser, 14) orelse return null;
    return parser.addNode(.{
        .unary_expression = .{
            .argument = argument,
            .operator = ast.unaryOperatorFromToken(operator_token.type),
        },
    }, .{ .start = operator_token.span.start, .end = parser.getSpan(argument).end });
}

fn parseUpdateExpression(parser: *Parser, prefix: bool, left: ast.NodeIndex) ?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.updateOperatorFromToken(operator_token.type);
    parser.advance();

    if (prefix) {
        const argument = parseExpression(parser, 14) orelse return null;
        const span = parser.getSpan(argument);
        if (!isValidAssignmentTarget(parser, argument)) {
            parser.err(span.start, span.end, "Invalid operand", null);
            return null;
        }
        return parser.addNode(.{
            .update_expression = .{ .argument = argument, .operator = operator, .prefix = true },
        }, .{ .start = operator_token.span.start, .end = span.end });
    }

    if (!isValidAssignmentTarget(parser, left)) {
        const span = parser.getSpan(left);
        parser.err(span.start, span.end, "Invalid operand", null);
        return null;
    }
    return parser.addNode(.{
        .update_expression = .{ .argument = left, .operator = operator, .prefix = false },
    }, .{ .start = parser.getSpan(left).start, .end = operator_token.span.end });
}

fn parseBinaryExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) ?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.binaryOperatorFromToken(operator_token.type);
    parser.advance();
    const next_precedence = if (operator == .Exponent) precedence else precedence + 1;
    const right = parseExpression(parser, next_precedence) orelse return null;
    return parser.addNode(.{
        .binary_expression = .{ .left = left, .right = right, .operator = operator },
    }, .{ .start = parser.getSpan(left).start, .end = parser.getSpan(right).end });
}

fn parseLogicalExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) ?ast.NodeIndex {
    const operator_token = parser.current_token;
    parser.advance();
    const right = parseExpression(parser, precedence + 1) orelse return null;
    return parser.addNode(.{
        .logical_expression = .{
            .left = left,
            .right = right,
            .operator = ast.logicalOperatorFromToken(operator_token.type),
        },
    }, .{ .start = parser.getSpan(left).start, .end = parser.getSpan(right).end });
}

fn parseAssignmentExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) ?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.assignmentOperatorFromToken(operator_token.type);
    const left_span = parser.getSpan(left);

    if (!isValidAssignmentTarget(parser, left)) {
        parser.err(left_span.start, left_span.end, "Invalid assignment target", null);
        return null;
    }

    const is_logical = operator == .LogicalAndAssign or operator == .LogicalOrAssign or operator == .NullishAssign;
    if (is_logical and !isSimpleAssignmentTarget(parser, left)) {
        parser.err(left_span.start, left_span.end, "Invalid logical assignment target", null);
        return null;
    }

    parser.advance();
    const right = parseExpression(parser, precedence) orelse return null;
    const target = parser.addNode(.{ .simple_assignment_target = left }, left_span);
    return parser.addNode(.{
        .assignment_expression = .{ .left = target, .right = right, .operator = operator },
    }, .{ .start = left_span.start, .end = parser.getSpan(right).end });
}

pub fn isValidAssignmentTarget(parser: *Parser, index: ast.NodeIndex) bool {
    return parser.getData(index) == .identifier;
}

pub fn isSimpleAssignmentTarget(parser: *Parser, index: ast.NodeIndex) bool {
    return parser.getData(index) == .identifier;
}

fn parseArrayExpression(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;
    parser.advance();

    var elements: [256]ast.NodeIndex = undefined;
    var length: usize = 0;

    while (parser.current_token.type != .RightBracket and parser.current_token.type != .EOF) {
        if (parser.current_token.type == .Comma) {
            elements[length] = ast.null_node;
            length += 1;
            parser.advance();
            continue;
        }
        elements[length] = parseArrayElement(parser) orelse return null;
        length += 1;
        if (parser.current_token.type == .Comma) parser.advance() else break;
    }

    if (parser.current_token.type != .RightBracket) {
        parser.err(start, parser.current_token.span.end, "Expected ']'", null);
        return null;
    }
    const end = parser.current_token.span.end;
    parser.advance();

    return parser.addNode(.{
        .array_expression = .{ .elements = parser.addExtra(elements[0..length]) },
    }, .{ .start = start, .end = end });
}

fn parseArrayElement(parser: *Parser) ?ast.NodeIndex {
    if (parser.current_token.type == .Spread) return parseSpreadElement(parser);
    return parseExpression(parser, 0);
}

pub fn parseSpreadElement(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;
    parser.advance();
    const argument = parseExpression(parser, 0) orelse return null;
    return parser.addNode(.{
        .spread_element = .{ .argument = argument },
    }, .{ .start = start, .end = parser.getSpan(argument).end });
}

fn parseObjectExpression(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;
    parser.advance();

    var properties: [256]ast.NodeIndex = undefined;
    var length: usize = 0;

    while (parser.current_token.type != .RightBrace and parser.current_token.type != .EOF) {
        properties[length] = parseObjectProperty(parser) orelse return null;
        length += 1;
        if (parser.current_token.type == .Comma) parser.advance() else break;
    }

    if (parser.current_token.type != .RightBrace) {
        parser.err(start, parser.current_token.span.end, "Expected '}'", null);
        return null;
    }
    const end = parser.current_token.span.end;
    parser.advance();

    return parser.addNode(.{
        .object_expression = .{ .properties = parser.addExtra(properties[0..length]) },
    }, .{ .start = start, .end = end });
}

fn parseObjectProperty(parser: *Parser) ?ast.NodeIndex {
    if (parser.current_token.type == .Spread) return parseSpreadElement(parser);

    const start = parser.current_token.span.start;
    var computed = false;
    var key: ast.NodeIndex = undefined;
    var shorthand_token: ?@import("../token.zig").Token = null;

    if (parser.current_token.type == .LeftBracket) {
        computed = true;
        parser.advance();
        key = parseExpression(parser, 0) orelse return null;
        if (parser.current_token.type != .RightBracket) {
            parser.err(start, parser.current_token.span.end, "Expected ']'", null);
            return null;
        }
        parser.advance();
    } else if (parser.current_token.type.isIdentifierLike()) {
        shorthand_token = parser.current_token;
        key = parser.addNode(.{
            .identifier_name = .{
                .name_start = parser.current_token.span.start,
                .name_len = @intCast(parser.current_token.lexeme.len),
            },
        }, parser.current_token.span);
        parser.advance();
    } else if (parser.current_token.type.isNumericLiteral()) {
        key = literals.parseNumericLiteral(parser) orelse return null;
    } else if (parser.current_token.type == .StringLiteral) {
        key = literals.parseStringLiteral(parser) orelse return null;
    } else {
        parser.err(parser.current_token.span.start, parser.current_token.span.end, "Expected property key", null);
        return null;
    }

    const is_shorthand = !computed and shorthand_token != null and
        (parser.current_token.type == .Comma or parser.current_token.type == .RightBrace);

    var value: ast.NodeIndex = undefined;
    if (is_shorthand) {
        const token = shorthand_token.?;
        value = parser.addNode(.{
            .identifier = .{
                .name_start = token.span.start,
                .name_len = @intCast(token.lexeme.len),
            },
        }, token.span);
    } else {
        if (parser.current_token.type != .Colon) {
            parser.err(parser.getSpan(key).start, parser.current_token.span.end, "Expected ':'", null);
            return null;
        }
        parser.advance();
        value = parseExpression(parser, 0) orelse return null;
    }

    return parser.addNode(.{
        .object_property = .{
            .key = key,
            .value = value,
            .kind = .Init,
            .shorthand = is_shorthand,
            .computed = computed,
        },
    }, .{ .start = start, .end = parser.getSpan(value).end });
}
