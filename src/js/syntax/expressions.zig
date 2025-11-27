const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const literals = @import("literals.zig");

pub fn parseExpression(parser: *Parser, precedence: u5) ?ast.NodeIndex {
    var left = parsePrefix(parser) orelse return null;

    while (true) {
        const current_type = parser.current_token.type;
        if (current_type == .EOF) break;

        const left_binding_power = parser.current_token.leftBindingPower();
        if (precedence > left_binding_power or left_binding_power == 0) break;

        left = parseInfix(parser, left_binding_power, left) orelse return null;
    }

    return left;
}

inline fn parseInfix(parser: *Parser, precedence: u5, left: ast.NodeIndex) ?ast.NodeIndex {
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

    parser.err(
        current.span.start,
        current.span.end,
        parser.formatMessage("Unexpected token '{s}' in expression", .{current.lexeme}),
        "This token cannot be used here. Expected an operator, semicolon, or end of expression.",
    );
    return null;
}

inline fn parsePrefix(parser: *Parser) ?ast.NodeIndex {
    const token_type = parser.current_token.type;

    if (token_type == .Increment or token_type == .Decrement) {
        return parseUpdateExpression(parser, true, ast.null_node);
    }

    if (token_type.isUnaryOperator()) {
        return parseUnaryExpression(parser);
    }

    return parsePrimaryExpression(parser);
}

inline fn parsePrimaryExpression(parser: *Parser) ?ast.NodeIndex {
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
            const tok = parser.current_token;
            if (tok.type == .EOF) {
                parser.err(
                    tok.span.start,
                    tok.span.end,
                    "Unexpected end of input while parsing expression",
                    "The parser reached the end of the file but expected an expression. Check for missing values or unclosed brackets.",
                );
            } else {
                parser.err(
                    tok.span.start,
                    tok.span.end,
                    parser.formatMessage("Unexpected token '{s}'", .{tok.lexeme}),
                    "Expected an expression (identifier, literal, array, object, or parenthesized expression).",
                );
            }
            return null;
        },
    };
}

fn parseUnaryExpression(parser: *Parser) ?ast.NodeIndex {
    const operator_token = parser.current_token;
    parser.advance();

    const argument = parseExpression(parser, 14) orelse return null;

    return parser.addNode(
        .{
            .unary_expression = .{
                .argument = argument,
                .operator = ast.unaryOperatorFromToken(operator_token.type),
            },
        },
        .{ .start = operator_token.span.start, .end = parser.getSpan(argument).end },
    );
}

fn parseUpdateExpression(parser: *Parser, prefix: bool, left: ast.NodeIndex) ?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.updateOperatorFromToken(operator_token.type);
    parser.advance();

    if (prefix) {
        const argument = parseExpression(parser, 14) orelse return null;
        const span = parser.getSpan(argument);

        if (!isValidAssignmentTarget(parser, argument)) {
            parser.err(
                span.start,
                span.end,
                "Invalid operand for increment/decrement operator",
                "The '++' and '--' operators require a variable or property reference, not a literal or complex expression.",
            );
            return null;
        }

        return parser.addNode(
            .{ .update_expression = .{ .argument = argument, .operator = operator, .prefix = true } },
            .{ .start = operator_token.span.start, .end = span.end },
        );
    }

    if (!isValidAssignmentTarget(parser, left)) {
        const span = parser.getSpan(left);
        parser.err(
            span.start,
            span.end,
            "Invalid operand for increment/decrement operator",
            "The '++' and '--' operators require a variable or property reference, not a literal or complex expression.",
        );
        return null;
    }

    return parser.addNode(
        .{ .update_expression = .{ .argument = left, .operator = operator, .prefix = false } },
        .{ .start = parser.getSpan(left).start, .end = operator_token.span.end },
    );
}

inline fn parseBinaryExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) ?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.binaryOperatorFromToken(operator_token.type);
    parser.advance();

    const next_precedence = if (operator == .Exponent) precedence else precedence + 1;
    const right = parseExpression(parser, next_precedence) orelse return null;

    return parser.addNode(
        .{ .binary_expression = .{ .left = left, .right = right, .operator = operator } },
        .{ .start = parser.getSpan(left).start, .end = parser.getSpan(right).end },
    );
}

inline fn parseLogicalExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) ?ast.NodeIndex {
    const operator_token = parser.current_token;
    parser.advance();

    const right = parseExpression(parser, precedence + 1) orelse return null;

    return parser.addNode(
        .{
            .logical_expression = .{
                .left = left,
                .right = right,
                .operator = ast.logicalOperatorFromToken(operator_token.type),
            },
        },
        .{ .start = parser.getSpan(left).start, .end = parser.getSpan(right).end },
    );
}

fn parseAssignmentExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) ?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.assignmentOperatorFromToken(operator_token.type);
    const left_span = parser.getSpan(left);

    // validate that left side can be assigned to
    if (!isValidAssignmentTarget(parser, left)) {
        parser.err(
            left_span.start,
            left_span.end,
            "Invalid left-hand side in assignment",
            "The left side of an assignment must be a variable, property access, or destructuring pattern.",
        );
        return null;
    }

    // logical assignments (&&=, ||=, ??=) require simple targets
    const is_logical = operator == .LogicalAndAssign or operator == .LogicalOrAssign or operator == .NullishAssign;
    if (is_logical and !isSimpleAssignmentTarget(parser, left)) {
        parser.err(
            left_span.start,
            left_span.end,
            "Invalid left-hand side in logical assignment",
            "Logical assignment operators (&&=, ||=, ??=) require a simple reference like a variable or property, not a destructuring pattern.",
        );
        return null;
    }

    parser.advance();
    const right = parseExpression(parser, precedence) orelse return null;

    return parser.addNode(
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

fn parseArrayExpression(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;
    parser.advance();

    const checkpoint = parser.scratch_a.begin();

    while (true) {
        const token_type = parser.current_token.type;
        if (token_type == .RightBracket or token_type == .EOF) break;

        // elision (holes in array): [1, , 3]
        if (token_type == .Comma) {
            parser.scratch_a.append(ast.null_node);
            parser.advance();
            continue;
        }

        const element = parseArrayElement(parser) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };
        parser.scratch_a.append(element);

        if (parser.current_token.type == .Comma) parser.advance() else break;
    }

    if (parser.current_token.type != .RightBracket) {
        parser.err(
            start,
            parser.current_token.span.end,
            "Unclosed array literal",
            "Add a closing bracket ']' to complete the array, or check for missing commas between elements.",
        );
        parser.scratch_a.reset(checkpoint);
        return null;
    }

    const end = parser.current_token.span.end;
    parser.advance();

    return parser.addNode(
        .{ .array_expression = .{ .elements = parser.addExtra(parser.scratch_a.take(checkpoint)) } },
        .{ .start = start, .end = end },
    );
}

inline fn parseArrayElement(parser: *Parser) ?ast.NodeIndex {
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

    const checkpoint = parser.scratch_a.begin();

    while (true) {
        const token_type = parser.current_token.type;
        if (token_type == .RightBrace or token_type == .EOF) break;

        const property = parseObjectProperty(parser) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };
        parser.scratch_a.append(property);
        if (parser.current_token.type == .Comma) parser.advance() else break;
    }

    if (parser.current_token.type != .RightBrace) {
        parser.err(
            start,
            parser.current_token.span.end,
            "Unclosed object literal",
            "Add a closing brace '}' to complete the object, or check for missing commas between properties.",
        );
        parser.scratch_a.reset(checkpoint);
        return null;
    }
    const end = parser.current_token.span.end;
    parser.advance();

    return parser.addNode(.{
        .object_expression = .{ .properties = parser.addExtra(parser.scratch_a.take(checkpoint)) },
    }, .{ .start = start, .end = end });
}

fn parseObjectProperty(parser: *Parser) ?ast.NodeIndex {
    if (parser.current_token.type == .Spread) return parseSpreadElement(parser);

    const start = parser.current_token.span.start;
    var computed = false;
    var key: ast.NodeIndex = undefined;
    var shorthand_token: ?@import("../token.zig").Token = null;

    // computed property names: [expr]
    if (parser.current_token.type == .LeftBracket) {
        computed = true;
        parser.advance();
        key = parseExpression(parser, 0) orelse return null;

        if (parser.current_token.type != .RightBracket) {
            parser.err(
                start,
                parser.current_token.span.end,
                "Unclosed computed property name",
                "Add a closing bracket ']' after the expression used as the property name.",
            );
            return null;
        }
        parser.advance();
    } else if (parser.current_token.type.isIdentifierLike()) {
        shorthand_token = parser.current_token;
        key = parser.addNode(
            .{
                .identifier_name = .{
                    .name_start = parser.current_token.span.start,
                    .name_len = @intCast(parser.current_token.lexeme.len),
                },
            },
            parser.current_token.span,
        );
        parser.advance();
    } else if (parser.current_token.type.isNumericLiteral()) {
        key = literals.parseNumericLiteral(parser) orelse return null;
    } else if (parser.current_token.type == .StringLiteral) {
        key = literals.parseStringLiteral(parser) orelse return null;
    } else {
        parser.err(
            parser.current_token.span.start,
            parser.current_token.span.end,
            parser.formatMessage("Unexpected token '{s}' in object literal", .{parser.current_token.lexeme}),
            "Object properties must start with an identifier, string, number, or computed property name ([expr]).",
        );
        return null;
    }

    // check for shorthand property: { x } instead of { x: x }
    const is_shorthand = !computed and shorthand_token != null and
        (parser.current_token.type == .Comma or parser.current_token.type == .RightBrace);

    var value: ast.NodeIndex = undefined;
    if (is_shorthand) {
        const token = shorthand_token.?;

        value = parser.addNode(
            .{
                .identifier_reference = .{
                    .name_start = token.span.start,
                    .name_len = @intCast(token.lexeme.len),
                },
            },
            token.span,
        );
    } else {
        if (parser.current_token.type != .Colon) {
            parser.err(
                parser.getSpan(key).start,
                parser.current_token.span.end,
                "Missing colon after property name in object literal",
                "Use 'key: value' syntax for object properties, or just 'key' for shorthand when the variable has the same name.",
            );
            return null;
        }
        parser.advance();
        value = parseExpression(parser, 0) orelse return null;
    }

    return parser.addNode(
        .{
            .object_property = .{
                .key = key,
                .value = value,
                .kind = .Init,
                .shorthand = is_shorthand,
                .computed = computed,
            },
        },
        .{ .start = start, .end = parser.getSpan(value).end },
    );
}
