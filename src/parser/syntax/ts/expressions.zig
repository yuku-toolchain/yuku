const std = @import("std");
const ast = @import("../../ast.zig");
const parser_module = @import("../../parser.zig");
const Parser = parser_module.Parser;
const Error = parser_module.Error;
const Token = @import("../../token.zig").Token;
const TokenTag = @import("../../token.zig").TokenTag;
const Precedence = @import("../../token.zig").Precedence;

const expressions = @import("../expressions.zig");
const functions = @import("../functions.zig");
const parenthesized = @import("../parenthesized.zig");
const types = @import("types.zig");

const ParseExpressionOptions = expressions.ParseExpressionOptions;

pub const Speculation = packed struct {
    rule: Rule,
    position: u32,
    context: parser_module.Context,
    ts_context: parser_module.TsContext,
    options: ParseExpressionOptions,

    pub const Rule = enum(u2) { arrow, type_arguments, call_type_arguments, infer_constraint };

    pub fn at(parser: *const Parser, rule: Rule, options: ParseExpressionOptions) Speculation {
        return .{
            .rule = rule,
            .position = parser.current_token.span.start,
            .context = parser.context,
            .ts_context = parser.ts_context,
            .options = options,
        };
    }

    pub fn isRejected(self: Speculation, parser: *const Parser) bool {
        return parser.ts_rejected_speculations.contains(self);
    }

    pub fn reject(self: Speculation, parser: *Parser) Error!void {
        try parser.ts_rejected_speculations.put(parser.allocator(), self, {});
    }
};

pub fn parseTypeAssertion(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.less_than) orelse return null;
    const type_annotation = try types.parseType(parser) orelse return null;
    if (!try parser.expect(
        .greater_than,
        "Expected '>' to close the type assertion",
        "Write a type assertion as '<Type>value'",
    )) return null;

    const expression = try expressions.parseExpression(parser, Precedence.Unary, .{}) orelse
        return null;
    return try parser.finishAt(start, expression, .{
        .ts_type_assertion = .{ .type_annotation = type_annotation, .expression = expression },
    });
}

pub fn parseAsOrSatisfiesExpression(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const keyword = parser.current_token.tag;
    std.debug.assert(keyword == .as or keyword == .satisfies);
    try parser.advance() orelse return null;

    const annotation = try types.parseType(parser) orelse return null;
    const start = parser.tree.span(left).start;
    if (keyword == .as) return try parser.finishAt(start, annotation, .{
        .ts_as_expression = .{ .expression = left, .type_annotation = annotation },
    });
    return try parser.finishAt(start, annotation, .{
        .ts_satisfies_expression = .{ .expression = left, .type_annotation = annotation },
    });
}

pub fn parseNonNullExpression(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    _ = try parser.take(.logical_not) orelse return null;
    return try parser.finish(parser.tree.span(left).start, .{
        .ts_non_null_expression = .{ .expression = left },
    });
}

pub fn parseTypeArgumentedCallOrInstantiation(
    parser: *Parser,
    callee: ast.NodeIndex,
) Error!?ast.NodeIndex {
    const type_arguments = try tryParseTypeArgumentsInExpression(parser, false);
    if (type_arguments == .null) return null;

    switch (parser.current_token.tag) {
        .left_paren => {
            return expressions.parseCallExpression(parser, callee, false, type_arguments);
        },
        .no_substitution_template, .template_head => {
            return expressions.parseTaggedTemplateExpression(parser, callee, type_arguments);
        },
        else => {},
    }

    const node = try parser.finishAt(parser.tree.span(callee).start, type_arguments, .{
        .ts_instantiation_expression = .{
            .expression = callee,
            .type_arguments = type_arguments,
        },
    });
    if (parser.at(.dot)) try parser.report(
        parser.tree.span(node),
        "An instantiation expression cannot be followed by a property access",
        .{ .help = "Wrap the instantiation expression in parentheses" },
    );
    return node;
}

// rewind so `<` can stay a relational compare
pub fn tryParseTypeArgumentsInExpression(parser: *Parser, call_only: bool) Error!ast.NodeIndex {
    if (!types.isAngleOpen(parser.current_token.tag)) return .null;

    const after = parser.peekAhead();
    if (!types.isStartOfType(after.tag) and after.tag != .greater_than) return .null;

    const rule: Speculation.Rule = if (call_only) .call_type_arguments else .type_arguments;
    const speculation: Speculation = .at(parser, rule, .{});
    if (speculation.isRejected(parser)) return .null;

    const cp = parser.checkpoint();
    const arguments = try types.parseTypeArgumentsInExpression(parser);
    const follows = if (call_only)
        parser.at(.left_paren)
    else
        canFollowTypeArgumentsInExpression(parser.current_token);
    if (arguments != .null and follows) return arguments;

    parser.rewind(cp);
    try speculation.reject(parser);
    return .null;
}

fn canFollowTypeArgumentsInExpression(token: Token) bool {
    const tag = token.tag;
    return switch (tag) {
        .left_paren, .no_substitution_template, .template_head => true,
        // favor relational parse for `a < b > -c`
        .less_than, .greater_than, .plus, .minus => false,
        .comma, .question, .as, .satisfies => true,
        else => token.hasLineTerminatorBefore() or
            tag.isBinaryOperator() or tag.isLogicalOperator() or tag.isAssignmentOperator() or
            !isStartOfExpression(tag),
    };
}

fn isStartOfExpression(tag: TokenTag) bool {
    return switch (tag) {
        .private_identifier,
        .string_literal,
        .no_substitution_template,
        .template_head,
        .regex_literal,
        .left_paren,
        .left_bracket,
        .left_brace,
        .increment,
        .decrement,
        .spread,
        .at,
        .slash,
        .slash_assign,
        .this,
        .super,
        .null_literal,
        .true,
        .false,
        .function,
        .class,
        .new,
        .import,
        => true,
        else => tag.isUnaryOperator() or tag.isNumericLiteral() or
            (tag.isIdentifierLike() and !tag.isUnconditionallyReserved()),
    };
}

pub const ArrowHead = enum { no, yes, maybe };

pub fn classifyArrowHead(parser: *Parser) ArrowHead {
    if (!parser.at(.left_paren)) return .no;

    var peek = parser.beginPeek();
    defer peek.end();

    const first = peek.next().tag;
    return switch (first) {
        .spread => .yes,
        .left_bracket, .left_brace => .maybe,
        .right_paren => switch (peek.next().tag) {
            .arrow, .colon => .yes,
            else => .no,
        },
        .this => if (peek.next().tag == .colon) .yes else .no,
        else => if (!first.isIdentifierLike()) .no else switch (peek.next().tag) {
            .colon => .yes,
            .question => switch (peek.next().tag) {
                .colon, .comma, .assign, .right_paren => .yes,
                else => .no,
            },
            .comma, .assign, .right_paren => .maybe,
            else => .no,
        },
    };
}

pub fn parseArrow(
    parser: *Parser,
    is_async: bool,
    arrow_start: u32,
    options: ParseExpressionOptions,
    speculative: bool,
) Error!?ast.NodeIndex {
    const diagnostics_len = parser.diagnostics.items.len;
    const saved_await = parser.context.await;
    if (is_async) parser.context.await = true;
    defer parser.context.await = saved_await;

    const has_angle = types.isAngleOpen(parser.current_token.tag);
    const type_parameters = try types.parseTypeParameters(parser);
    if (has_angle and type_parameters == .null) return null;
    if (!parser.at(.left_paren)) return null;

    const params = try functions.parseFormalParameters(
        parser,
        .arrow_formal_parameters,
        false,
    ) orelse return null;
    const return_type: ast.NodeIndex = if (parser.tree.isTs() and parser.at(.colon))
        try types.parseReturnTypeAnnotation(parser) orelse return null
    else
        .null;
    if (speculative and return_type != .null and
        parser.diagnostics.items.len != diagnostics_len) return null;

    if (!parser.at(.arrow)) {
        return parser.failExpected("Expected '=>' after the arrow function parameters", .{});
    }
    if (parser.current_token.hasLineTerminatorBefore()) return parser.fail(
        parser.current_token.span,
        "Line terminator not permitted before arrow",
        .{ .help = "Move '=>' to the line of the parameters" },
    );
    return parenthesized.buildArrowFunction(
        parser,
        params,
        is_async,
        arrow_start,
        type_parameters,
        return_type,
        options,
    );
}

// rewind on failure so a parenthesized expression, call, jsx, or `<T>` assertion can win
pub fn tryParseArrow(
    parser: *Parser,
    is_async: bool,
    arrow_start: u32,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const speculation: Speculation = .at(parser, .arrow, options);
    if (speculation.isRejected(parser)) return null;

    const cp = parser.checkpoint();
    if (try parseArrow(parser, is_async, arrow_start, options, true)) |arrow| {
        // the `:` after an annotated arrow may belong to an enclosing conditional
        const return_type = parser.tree.data(arrow).arrow_function_expression.return_type;
        if (options.allow_arrow_return_type or
            return_type == .null or
            (parser.at(.colon) and parser.diagnostics.items.len == cp.diagnostics_len))
        {
            return arrow;
        }
    }

    parser.rewind(cp);
    try speculation.reject(parser);
    return null;
}

// skip the checkpoint when jsx or a type assertion is likelier
pub fn tryParseGenericArrow(
    parser: *Parser,
    is_async: bool,
    arrow_start: u32,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    std.debug.assert(parser.at(.less_than));
    if (!parser.peekAhead().tag.isIdentifierLike()) return null;
    return tryParseArrow(parser, is_async, arrow_start, options);
}
