const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const literals = @import("literals.zig");
const functions = @import("functions.zig");

pub fn parseExpression(parser: *Parser, precedence: u5) Error!?ast.NodeIndex {
    var left = try parsePrefix(parser) orelse return null;

    while (true) {
        const current_type = parser.current_token.type;
        if (current_type == .EOF) break;

        const left_binding_power = parser.current_token.leftBindingPower();
        if (precedence > left_binding_power or left_binding_power == 0) break;

        left = try parseInfix(parser, left_binding_power, left) orelse return null;
    }

    return left;
}

fn parseInfix(parser: *Parser, precedence: u5, left: ast.NodeIndex) Error!?ast.NodeIndex {
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

    if (token_type == .Function) {
        return functions.parseFunction(parser, .{ .is_expression = true });
    }

    if (token_type == .Async) {
        return functions.parseFunction(parser, .{ .is_expression = true, .is_async = true });
    }

    if (token_type == .Increment or token_type == .Decrement) {
        return parseUpdateExpression(parser, true, ast.null_node);
    }

    if (token_type.isUnaryOperator()) {
        return parseUnaryExpression(parser);
    }

    return parsePrimaryExpression(parser);
}

inline fn parsePrimaryExpression(parser: *Parser) Error!?ast.NodeIndex {
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

        if (!isValidAssignmentTarget(parser, argument)) {
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

    if (!isValidAssignmentTarget(parser, left)) {
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

    const next_precedence = if (operator == .Exponent) precedence else precedence + 1;
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

        if ((current_operator == .NullishCoalescing) != (operator_to_check == .NullishCoalescing)) {
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
    const is_logical = operator == .LogicalAndAssign or operator == .LogicalOrAssign or operator == .NullishAssign;
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
    const start = parser.current_token.span.start;
    try parser.advance();

    const checkpoint = parser.scratch_a.begin();

    while (true) {
        const token_type = parser.current_token.type;
        if (token_type == .RightBracket or token_type == .EOF) break;

        // elision (holes in array): [1, , 3]
        if (token_type == .Comma) {
            try parser.scratch_a.append(parser.allocator(), ast.null_node);
            try parser.advance();
            continue;
        }

        const element = try parseArrayElement(parser) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };
        try parser.scratch_a.append(parser.allocator(), element);

        if (parser.current_token.type == .Comma) try parser.advance() else break;
    }

    if (parser.current_token.type != .RightBracket) {
        try parser.report(
            parser.current_token.span,
            "Unclosed array literal",
            .{
                .help = "Add a closing bracket ']' to complete the array, or check for missing commas between elements.",
                .labels = try parser.makeLabels(&.{
                    parser.label(.{ .start = start, .end = start + 1 }, "opened here"),
                }),
            },
        );
        parser.scratch_a.reset(checkpoint);
        return null;
    }

    const end = parser.current_token.span.end;
    try parser.advance();

    return try parser.addNode(
        .{ .array_expression = .{ .elements = try parser.addExtra(parser.scratch_a.take(checkpoint)) } },
        .{ .start = start, .end = end },
    );
}

inline fn parseArrayElement(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.type == .Spread) return parseSpreadElement(parser);
    return parseExpression(parser, 0);
}

pub fn parseSpreadElement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance();
    const argument = try parseExpression(parser, 0) orelse return null;
    return try parser.addNode(.{
        .spread_element = .{ .argument = argument },
    }, .{ .start = start, .end = parser.getSpan(argument).end });
}

fn parseObjectExpression(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance();

    const checkpoint = parser.scratch_a.begin();

    while (true) {
        const token_type = parser.current_token.type;
        if (token_type == .RightBrace or token_type == .EOF) break;

        const property = try parseObjectProperty(parser) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };
        try parser.scratch_a.append(parser.allocator(), property);
        if (parser.current_token.type == .Comma) try parser.advance() else break;
    }

    if (parser.current_token.type != .RightBrace) {
        try parser.report(
            parser.current_token.span,
            "Unclosed object literal",
            .{
                .help = "Add a closing brace '}' to complete the object, or check for missing commas between properties.",
                .labels = try parser.makeLabels(&.{
                    parser.label(.{ .start = start, .end = start + 1 }, "opened here"),
                }),
            },
        );
        parser.scratch_a.reset(checkpoint);
        return null;
    }
    const end = parser.current_token.span.end;
    try parser.advance();

    return try parser.addNode(.{
        .object_expression = .{ .properties = try parser.addExtra(parser.scratch_a.take(checkpoint)) },
    }, .{ .start = start, .end = end });
}

fn parseObjectProperty(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.type == .Spread) return parseSpreadElement(parser);

    const start = parser.current_token.span.start;
    var computed = false;
    var key: ast.NodeIndex = undefined;
    var shorthand_token: ?@import("../token.zig").Token = null;

    // computed property names: [expr]
    if (parser.current_token.type == .LeftBracket) {
        computed = true;
        try parser.advance();
        key = try parseExpression(parser, 0) orelse return null;

        if (parser.current_token.type != .RightBracket) {
            try parser.report(
                parser.current_token.span,
                "Unclosed computed property name",
                .{
                    .help = "Add a closing bracket ']' after the expression used as the property name.",
                    .labels = try parser.makeLabels(&.{
                        parser.label(.{ .start = start, .end = start + 1 }, "opened here"),
                    }),
                },
            );
            return null;
        }
        try parser.advance();
    } else if (parser.current_token.type.isIdentifierLike()) {
        shorthand_token = parser.current_token;
        key = try parser.addNode(
            .{
                .identifier_name = .{
                    .name_start = parser.current_token.span.start,
                    .name_len = @intCast(parser.current_token.lexeme.len),
                },
            },
            parser.current_token.span,
        );
        try parser.advance();
    } else if (parser.current_token.type.isNumericLiteral()) {
        key = try literals.parseNumericLiteral(parser) orelse return null;
    } else if (parser.current_token.type == .StringLiteral) {
        key = try literals.parseStringLiteral(parser) orelse return null;
    } else {
        try parser.reportFmt(
            parser.current_token.span,
            "Unexpected token '{s}' in object literal",
            .{parser.current_token.lexeme},
            .{ .help = "Object properties must start with an identifier, string, number, or computed property name ([expr])." },
        );
        return null;
    }

    // check for shorthand property: { x } instead of { x: x }
    const is_shorthand = !computed and shorthand_token != null and
        (parser.current_token.type == .Comma or parser.current_token.type == .RightBrace);

    var value: ast.NodeIndex = undefined;
    if (is_shorthand) {
        const token = shorthand_token.?;

        value = try parser.addNode(
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
            try parser.report(
                .{ .start = parser.getSpan(key).start, .end = parser.current_token.span.end },
                "Missing colon after property name in object literal",
                .{ .help = "Use 'key: value' syntax for object properties, or just 'key' for shorthand when the variable has the same name." },
            );
            return null;
        }
        try parser.advance();
        value = try parseExpression(parser, 0) orelse return null;
    }

    return try parser.addNode(
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

    // TODO: handle methods, getter, setter.
    // first need to implement the function/arrow function expressions.
}
