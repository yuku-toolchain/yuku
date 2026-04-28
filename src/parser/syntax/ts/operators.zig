const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const TokenTag = @import("../../token.zig").TokenTag;
const Token = @import("../../token.zig").Token;
const Precedence = @import("../../token.zig").Precedence;

const expressions = @import("../expressions.zig");
const types = @import("types.zig");

/// `<Type>expr`. operand binds at unary precedence.
pub fn parseTypeAssertion(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .less_than);

    const start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '<'

    const type_node = try types.parseType(parser) orelse return null;

    if (!try parser.expect(
        .greater_than,
        "Expected '>' to close a type assertion",
        "A type assertion is written '<Type>expression'",
    )) return null;

    const expr = try expressions.parseExpression(parser, Precedence.Unary, .{}) orelse return null;

    const end = parser.tree.getSpan(expr).end;

    return try parser.tree.createNode(
        .{ .ts_type_assertion = .{ .type_annotation = type_node, .expression = expr } },
        .{ .start = start, .end = end },
    );
}

/// expr as Type    expr satisfies Type
pub fn parseAsOrSatisfiesExpression(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const keyword_tag = parser.current_token.tag;
    std.debug.assert(keyword_tag == .as or keyword_tag == .satisfies);

    try parser.advance() orelse return null; // consume 'as' or 'satisfies'

    const type_node = try types.parseType(parser) orelse return null;

    const start = parser.tree.getSpan(left).start;
    const end = parser.tree.getSpan(type_node).end;

    const data: ast.NodeData = if (keyword_tag == .as)
        .{ .ts_as_expression = .{ .expression = left, .type_annotation = type_node } }
    else
        .{ .ts_satisfies_expression = .{ .expression = left, .type_annotation = type_node } };

    return try parser.tree.createNode(data, .{ .start = start, .end = end });
}

/// `expr!` outside an optional chain. inside a chain
/// `parseOptionalChain` owns the `!`.
pub fn parseNonNullExpression(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .logical_not);

    const bang_end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume '!'

    return try parser.tree.createNode(
        .{ .ts_non_null_expression = .{ .expression = left } },
        .{ .start = parser.tree.getSpan(left).start, .end = bang_end },
    );
}

/// `<...>` after a callee. folds into a call or tagged template if one
/// follows, otherwise builds `TSInstantiationExpression`. returns null
/// when `<` stays relational.
pub fn parseTypeArgumentedCallOrInstantiation(parser: *Parser, callee: ast.NodeIndex) Error!?ast.NodeIndex {
    const type_arguments = try tryParseTypeArgumentsInExpression(parser);
    if (type_arguments == .null) return null;

    switch (parser.current_token.tag) {
        .left_paren => return try expressions.parseCallExpression(parser, callee, false, type_arguments),
        .no_substitution_template, .template_head => return try expressions.parseTaggedTemplateExpression(parser, callee, type_arguments),
        else => {
            const callee_span = parser.tree.getSpan(callee);
            const type_args_end = parser.tree.getSpan(type_arguments).end;
            return try parser.tree.createNode(
                .{ .ts_instantiation_expression = .{ .expression = callee, .type_arguments = type_arguments } },
                .{ .start = callee_span.start, .end = type_args_end },
            );
        },
    }
}

/// speculatively parses `<...>`. rewinds on failure so `<` stays
/// relational
pub fn tryParseTypeArgumentsInExpression(parser: *Parser) Error!ast.NodeIndex {
    if (!parser.tree.isTs()) return .null;
    if (!types.isAngleOpen(parser.current_token.tag)) return .null;

    const cp = parser.checkpoint();

    const args = try types.parseTypeArguments(parser);
    if (args == .null or !canFollowTypeArgumentsInExpression(parser.current_token)) {
        parser.rewind(cp);
        return .null;
    }

    return args;
}

/// post-commit filter for `<...>` in expression position.
fn canFollowTypeArgumentsInExpression(token: Token) bool {
    return switch (token.tag) {
        .left_paren, .no_substitution_template, .template_head => true,

        // bias toward the relational reading for `a < b > -c`.
        .less_than, .greater_than, .plus, .minus => false,

        else => token.hasLineTerminatorBefore() or
            isBinaryOperatorLike(token.tag) or
            !isStartOfExpression(token.tag),
    };
}

fn isBinaryOperatorLike(tag: TokenTag) bool {
    return tag.isBinaryOperator() or tag.isLogicalOperator() or
        tag.isAssignmentOperator() or tag == .comma or tag == .question or
        tag == .as or tag == .satisfies;
}

fn isStartOfExpression(tag: TokenTag) bool {
    return switch (tag) {
        .identifier,
        .private_identifier,
        .string_literal,
        .no_substitution_template,
        .template_head,
        .regex_literal,
        .left_paren,
        .left_bracket,
        .left_brace,
        .plus,
        .minus,
        .logical_not,
        .bitwise_not,
        .increment,
        .decrement,
        .spread,
        .at,
        .slash,
        .slash_assign,
        => true,
        else => tag.isIdentifierLike() or tag.isNumericLiteral(),
    };
}
