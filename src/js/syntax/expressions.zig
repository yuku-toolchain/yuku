const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const token = @import("../token.zig");
const std = @import("std");

const array = @import("array.zig");
const object = @import("object.zig");
const literals = @import("literals.zig");
const functions = @import("functions.zig");
const parenthesized = @import("parenthesized.zig");
const patterns = @import("patterns.zig");

const ParseExpressionOpts = packed struct {
    /// whether to enable "expression -> pattern" validations, for example ObjectExpression -> ObjectPattern
    /// disable this when parsing expressions in cover contexts, where we don't need validations, where we do validations on top level
    enable_validation: bool = true,
};

pub fn parseExpression(parser: *Parser, precedence: u5, opts: ParseExpressionOpts) Error!?ast.NodeIndex {
    var left = try parsePrefix(parser, opts.enable_validation) orelse return null;

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

    if (current.type == .question) {
        return parseConditionalExpression(parser, precedence, left);
    }

    // member access, call, and tagged template expressions
    switch (current.type) {
        .dot => return parseStaticMemberExpression(parser, left, false),
        .left_bracket => return parseComputedMemberExpression(parser, left, false),
        .left_paren => return parseCallExpression(parser, left, false),
        .template_head, .no_substitution_template => return parseTaggedTemplateExpression(parser, left),
        .optional_chaining => return parseOptionalChain(parser, left),
        else => {},
    }

    try parser.reportFmt(
        current.span,
        "Unexpected token '{s}' in expression",
        .{current.lexeme},
        .{ .help = "This token cannot be used here. Expected an operator, semicolon, or end of expression." },
    );
    return null;
}

fn parsePrefix(parser: *Parser, enable_validation: bool) Error!?ast.NodeIndex {
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

    if (token_type == .await and (parser.context.in_async or parser.isModule())) {
        return parseAwaitExpression(parser);
    }

    if (token_type == .yield and parser.context.in_generator) {
        return parseYieldExpression(parser);
    }

    if (token_type == .new) {
        return parseNewExpression(parser);
    }

    return parsePrimaryExpression(parser, enable_validation);
}

inline fn parsePrimaryExpression(parser: *Parser, enable_validation: bool) Error!?ast.NodeIndex {
    return switch (parser.current_token.type) {
        // .yield and .await will be checked for reserved word
        .identifier, .yield, .await => parseIdentifierOrArrowFunction(parser),
        .private_identifier => literals.parsePrivateIdentifier(parser),
        .string_literal => literals.parseStringLiteral(parser),
        .true, .false => literals.parseBooleanLiteral(parser),
        .null_literal => literals.parseNullLiteral(parser),
        .this => parseThisExpression(parser),
        .numeric_literal, .hex_literal, .octal_literal, .binary_literal => literals.parseNumericLiteral(parser),
        .bigint_literal => literals.parseBigIntLiteral(parser),
        .slash => literals.parseRegExpLiteral(parser),
        .template_head => literals.parseTemplateLiteral(parser),
        .no_substitution_template => literals.parseNoSubstitutionTemplate(parser),
        .left_bracket => parseArrayExpression(parser, enable_validation),
        .left_brace => parseObjectExpression(parser, enable_validation),
        .function => functions.parseFunction(parser, .{ .is_expression = true }, null),
        .async => parseAsyncFunctionOrArrow(parser),
        else => {
            const tok = parser.current_token;
            try parser.reportFmt(
                tok.span,
                "Unexpected token '{s}'",
                .{tok.lexeme},
                .{ .help = "Expected an expression (identifier, literal, array, object, or parenthesized expression)." },
            );
            return null;
        },
    };
}

// parse only (a), not arrow, this function is used in the 'new' expression parsing
// where we only need parenthesized
fn parseParenthesizedExpression(parser: *Parser) Error!?ast.NodeIndex {
    const cover = try parenthesized.parseCover(parser) orelse return null;

    return parenthesized.coverToExpression(parser, cover);
}

/// (a) or (a, b) => ...
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

    const argument = try parseExpression(parser, 14, .{}) orelse return null;

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

/// `await expression`
/// https://tc39.es/ecma262/#sec-await
fn parseAwaitExpression(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'await'

    const argument = try parseExpression(parser, 14, .{}) orelse return null;

    return try parser.addNode(
        .{ .await_expression = .{ .argument = argument } },
        .{ .start = start, .end = parser.getSpan(argument).end },
    );
}

/// `yield`, `yield expression`, or `yield* expression`
/// https://tc39.es/ecma262/#sec-generator-function-definitions-runtime-semantics-evaluation
fn parseYieldExpression(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance(); // consume 'yield'

    // check for delegate: yield*
    var delegate = false;
    if (parser.current_token.type == .star and !parser.current_token.has_line_terminator_before) {
        delegate = true;
        end = parser.current_token.span.end;
        try parser.advance(); // consume '*'
    }

    var argument: ast.NodeIndex = ast.null_node;

    if (!parser.current_token.has_line_terminator_before) {
        argument = try parseExpression(parser, 2, .{}) orelse return null;
        end = parser.getSpan(argument).end;
    }

    return try parser.addNode(
        .{ .yield_expression = .{ .argument = argument, .delegate = delegate } },
        .{ .start = start, .end = end },
    );
}

/// `this`
/// https://tc39.es/ecma262/#sec-this-keyword
fn parseThisExpression(parser: *Parser) Error!?ast.NodeIndex {
    const this_token = parser.current_token;
    try parser.advance(); // consume 'this'
    return try parser.addNode(.this_expression, this_token.span);
}

/// `new Callee` or `new Callee(args)`
/// https://tc39.es/ecma262/#sec-new-operator
fn parseNewExpression(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'new'

    var callee: ast.NodeIndex = blk: {
        // parenthesized, allows any expression inside
        if (parser.current_token.type == .left_paren) {
            break :blk try parseParenthesizedExpression(parser) orelse return null;
        }

        // `new new Foo()`
        if (parser.current_token.type == .new) {
            break :blk try parseNewExpression(parser) orelse return null;
        }

        // otherwise, start with a primary expression
        break :blk try parsePrimaryExpression(parser, true) orelse return null;
    };

    // member expression chain (. [] and tagged templates)
    while (true) {
        callee = switch (parser.current_token.type) {
            .dot => try parseStaticMemberExpression(parser, callee, false) orelse return null,
            .left_bracket => try parseComputedMemberExpression(parser, callee, false) orelse return null,
            .template_head, .no_substitution_template => try parseTaggedTemplateExpression(parser, callee) orelse return null,
            .optional_chaining => {
                try parser.report(
                    parser.current_token.span,
                    "Optional chaining is not allowed in new expression",
                    .{ .help = "Remove the '?.' operator or use regular member access." },
                );
                return null;
            },
            else => break,
        };
    }

    // optional arguments
    var arguments = ast.IndexRange.empty;

    const end = if (parser.current_token.type == .left_paren) blk: {
        try parser.advance();
        arguments = try parseArguments(parser) orelse return null;
        const arguments_end = parser.current_token.span.end;

        if (!try parser.expect(.right_paren, "Expected ')' after constructor arguments", "Constructor calls must end with ')'.")) {
            return null;
        }

        break :blk arguments_end;
    } else parser.getSpan(callee).end;

    return try parser.addNode(
        .{ .new_expression = .{ .callee = callee, .arguments = arguments } },
        .{ .start = start, .end = end },
    );
}

fn parseUpdateExpression(parser: *Parser, prefix: bool, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.UpdateOperator.fromToken(operator_token.type);
    try parser.advance();

    if (prefix) {
        const argument = try parseExpression(parser, 14, .{}) orelse return null;
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
    const right = try parseExpression(parser, next_precedence, .{}) orelse return null;

    return try parser.addNode(
        .{ .binary_expression = .{ .left = left, .right = right, .operator = operator } },
        .{ .start = parser.getSpan(left).start, .end = parser.getSpan(right).end },
    );
}

fn parseLogicalExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    try parser.advance();

    const right = try parseExpression(parser, precedence + 1, .{}) orelse return null;
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

    const right = try parseExpression(parser, precedence, .{}) orelse return null;

    return try parser.addNode(
        .{ .assignment_expression = .{ .left = left, .right = right, .operator = operator } },
        .{ .start = left_span.start, .end = parser.getSpan(right).end },
    );
}

/// `test ? consequent : alternate`
/// https://tc39.es/ecma262/#sec-conditional-operator
fn parseConditionalExpression(parser: *Parser, precedence: u5, @"test": ast.NodeIndex) Error!?ast.NodeIndex {
    const test_span = parser.getSpan(@"test");

    try parser.advance(); // consume '?'

    // consequent
    // right-associative, so same prec, not precedence + 1
    const consequent = try parseExpression(parser, precedence, .{}) orelse return null;

    if (!try parser.expect(.colon, "Expected ':' after conditional expression consequent", "The ternary operator requires a colon (:) to separate the consequent and alternate expressions.")) {
        return null;
    }

    // alternate
    // right-associative, so same prec, not precedence + 1
    const alternate = try parseExpression(parser, precedence, .{}) orelse return null;

    return try parser.addNode(
        .{
            .conditional_expression = .{
                .@"test" = @"test",
                .consequent = consequent,
                .alternate = alternate,
            },
        },
        .{ .start = test_span.start, .end = parser.getSpan(alternate).end },
    );
}

/// AssignmentTarget: can be simple (identifier/member) or pattern (destructuring)
pub fn isValidAssignmentTarget(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.getData(index)) {
        // SimpleAssignmentTarget
        .identifier_reference => true,
        .member_expression => |m| !m.optional, // optional chaining is not a valid assignment target

        // AssignmentPattern (destructuring)
        .array_pattern, .object_pattern => true,

        else => false,
    };
}

/// SimpleAssignmentTarget: only identifier and member expressions (no destructuring)
pub fn isSimpleAssignmentTarget(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.getData(index)) {
        .identifier_reference => true,
        .member_expression => |m| !m.optional, // optional chaining is not a valid assignment target
        else => false,
    };
}

pub fn parseArrayExpression(parser: *Parser, enable_validation: bool) Error!?ast.NodeIndex {
    const saved_flag = parser.state.cover_has_init_name;
    parser.state.cover_has_init_name = false;

    const cover = try array.parseCover(parser) orelse return null;
    const needs_validation = enable_validation and parser.state.cover_has_init_name;
    parser.state.cover_has_init_name = saved_flag or needs_validation;

    if (parser.current_token.type == .assign) {
        return try array.coverToPattern(parser, cover);
    }

    return array.coverToExpression(parser, cover, needs_validation);
}

pub fn parseObjectExpression(parser: *Parser, enable_validation: bool) Error!?ast.NodeIndex {
    const saved_flag = parser.state.cover_has_init_name;
    parser.state.cover_has_init_name = false;

    const cover = try object.parseCover(parser) orelse return null;
    const needs_validation = enable_validation and parser.state.cover_has_init_name;
    parser.state.cover_has_init_name = saved_flag or needs_validation;

    if (parser.current_token.type == .assign) {
        return try object.coverToPattern(parser, cover);
    }

    return object.coverToExpression(parser, cover, needs_validation);
}

/// obj.prop or obj.#priv
fn parseStaticMemberExpression(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    try parser.advance(); // consume '.'
    return parseMemberProperty(parser, object_node, optional);
}

/// property after '.' or '?.'
fn parseMemberProperty(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    const tok_type = parser.current_token.type;

    const property = if (tok_type.isIdentifierLike())
        try parseIdentifierName(parser)
    else if (tok_type == .private_identifier)
        try literals.parsePrivateIdentifier(parser)
    else {
        try parser.report(
            parser.current_token.span,
            "Expected property name after '.'",
            .{ .help = "Use an identifier or private identifier (#name) for member access." },
        );
        return null;
    };

    const prop = property orelse return null;

    return try parser.addNode(.{
        .member_expression = .{
            .object = object_node,
            .property = prop,
            .computed = false,
            .optional = optional,
        },
    }, .{ .start = parser.getSpan(object_node).start, .end = parser.getSpan(prop).end });
}

fn parseIdentifierName(parser: *Parser) Error!ast.NodeIndex {
    const tok = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .identifier_name = .{
            .name_start = tok.span.start,
            .name_len = @intCast(tok.lexeme.len),
        },
    }, tok.span);
}

/// obj[expr]
fn parseComputedMemberExpression(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    try parser.advance(); // consume '['

    const property = try parseExpression(parser, 0, .{}) orelse return null;

    const end = parser.current_token.span.end; // ']' position
    if (!try parser.expect(.right_bracket, "Expected ']' after computed property", "Computed member access must end with ']'.")) {
        return null;
    }

    return try parser.addNode(.{
        .member_expression = .{
            .object = object_node,
            .property = property,
            .computed = true,
            .optional = optional,
        },
    }, .{ .start = parser.getSpan(object_node).start, .end = end });
}

/// func(args)
fn parseCallExpression(parser: *Parser, callee_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    const start = parser.getSpan(callee_node).start;
    try parser.advance(); // consume '('

    const args = try parseArguments(parser) orelse return null;

    const end = parser.current_token.span.end; // ')' position
    if (!try parser.expect(.right_paren, "Expected ')' after function arguments", "Function calls must end with ')'. Check for missing commas or unclosed parentheses.")) {
        return null;
    }

    return try parser.addNode(.{
        .call_expression = .{
            .callee = callee_node,
            .arguments = args,
            .optional = optional,
        },
    }, .{ .start = start, .end = end });
}

/// function call arguments
fn parseArguments(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();

    while (parser.current_token.type != .right_paren and parser.current_token.type != .eof) {
        const arg = if (parser.current_token.type == .spread) blk: {
            const spread_start = parser.current_token.span.start;
            try parser.advance(); // consume '...'
            const argument = try parseExpression(parser, 2, .{}) orelse return null;
            const arg_span = parser.getSpan(argument);
            break :blk try parser.addNode(.{
                .spread_element = .{ .argument = argument },
            }, .{ .start = spread_start, .end = arg_span.end });
        } else try parseExpression(parser, 2, .{}) orelse return null;

        try parser.scratch_a.append(parser.allocator(), arg);

        if (parser.current_token.type == .comma) {
            try parser.advance();
        } else {
            break;
        }
    }

    return try parser.addExtra(parser.scratch_a.take(checkpoint));
}

/// tag`template`
fn parseTaggedTemplateExpression(parser: *Parser, tag_node: ast.NodeIndex) Error!?ast.NodeIndex {
    const start = parser.getSpan(tag_node).start;

    const quasi = if (parser.current_token.type == .no_substitution_template)
        try literals.parseNoSubstitutionTemplate(parser)
    else
        try literals.parseTemplateLiteral(parser);

    if (quasi == null) return null;

    const quasi_span = parser.getSpan(quasi.?);

    return try parser.addNode(.{
        .tagged_template_expression = .{
            .tag = tag_node,
            .quasi = quasi.?,
        },
    }, .{ .start = start, .end = quasi_span.end });
}

/// optional chain: a?.b, a?.[b], a?.()
fn parseOptionalChain(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const chain_start = parser.getSpan(left).start;
    try parser.advance(); // consume '?.'

    // first optional operation
    var expr = try parseOptionalChainElement(parser, left, true) orelse return null;

    // Continue parsing the chain
    while (true) {
        switch (parser.current_token.type) {
            .dot => expr = try parseStaticMemberExpression(parser, expr, false) orelse return null,
            .left_bracket => expr = try parseComputedMemberExpression(parser, expr, false) orelse return null,
            .left_paren => expr = try parseCallExpression(parser, expr, false) orelse return null,
            .optional_chaining => {
                try parser.advance();
                expr = try parseOptionalChainElement(parser, expr, true) orelse return null;
            },
            .template_head, .no_substitution_template => {
                // tagged template in optional chain, not allowed (unless line terminator separates)
                if (!parser.current_token.has_line_terminator_before) {
                    try parser.report(
                        parser.current_token.span,
                        "Tagged template expressions are not permitted in an optional chain",
                        .{ .help = "Remove the optional chaining operator '?.' before the template literal or add parentheses." },
                    );
                    return null;
                }
                break;
            },
            else => break,
        }
    }

    return try parser.addNode(.{
        .chain_expression = .{ .expression = expr },
    }, .{ .start = chain_start, .end = parser.getSpan(expr).end });
}

/// parse element after ?. (property access, computed, or call), '?.' already consumed
fn parseOptionalChainElement(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    const tok_type = parser.current_token.type;

    // identifier-like tokens become property access (a?.b)
    if (tok_type.isIdentifierLike() or tok_type == .private_identifier) {
        return parseMemberProperty(parser, object_node, optional);
    }

    return switch (tok_type) {
        .left_bracket => parseComputedMemberExpression(parser, object_node, optional),
        .left_paren => parseCallExpression(parser, object_node, optional),
        .template_head, .no_substitution_template => {
            try parser.report(
                parser.current_token.span,
                "Tagged template expressions are not permitted in an optional chain",
                .{ .help = "Remove the optional chaining operator '?.' before the template literal." },
            );
            return null;
        },
        else => {
            try parser.report(
                parser.current_token.span,
                "Expected property name, '[', or '(' after '?.'",
                .{ .help = "Optional chaining must be followed by property access (.x), computed access ([x]), or a call (())." },
            );
            return null;
        },
    };
}
