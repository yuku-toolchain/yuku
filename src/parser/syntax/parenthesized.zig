const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;

const grammar = @import("../grammar.zig");
const functions = @import("functions.zig");
const expressions = @import("expressions.zig");

const ParseExpressionOptions = expressions.ParseExpressionOptions;

pub fn coverToParenthesizedExpression(
    parser: *Parser,
    cover: grammar.Bracketed,
) Error!?ast.NodeIndex {
    const span = cover.span();
    const elements = parser.tree.extra(cover.elements);

    if (elements.len == 0) return parser.fail(
        span,
        "Empty parentheses are only allowed as arrow function parameters",
        .{ .help = "Add '=>' after ')' to make this an arrow function" },
    );
    if (cover.has_trailing_comma) return parser.fail(
        span,
        "A parenthesized expression cannot have a trailing comma",
        .{ .help = "Remove the trailing comma, or add '=>' after ')' to make this an arrow" ++
            " function" },
    );
    for (elements) |element| {
        if (parser.tree.data(element) == .spread_element) return parser.fail(
            parser.tree.span(element),
            "A rest parameter is only allowed in arrow function parameters",
            .{ .help = "Add '=>' after ')' to make this an arrow function" },
        );
    }

    const expression = if (elements.len == 1) elements[0] else try parser.tree.addNode(
        .{ .sequence_expression = .{ .expressions = cover.elements } },
        .{
            .start = parser.tree.span(elements[0]).start,
            .end = parser.tree.span(elements[elements.len - 1]).end,
        },
    );
    return try parser.tree.addNode(
        .{ .parenthesized_expression = .{ .expression = expression } },
        span,
    );
}

// async and TypeScript arrows go through `parseArrow` instead
pub fn coverToArrowFunction(
    parser: *Parser,
    cover: grammar.Bracketed,
    arrow_start: u32,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const items = parser.list();
    defer items.deinit();

    var rest: ast.NodeIndex = .null;
    for (parser.tree.extra(cover.elements)) |element| {
        if (rest != .null) return parser.fail(
            parser.tree.span(rest),
            "A rest parameter must be last in a parameter list",
            .{ .help = "Move the rest parameter to the end" },
        );

        if (parser.tree.data(element) != .spread_element) {
            try items.append(try toFormalParameter(parser, element));
            continue;
        }

        try grammar.expressionToPattern(parser, element, .binding);
        rest = element;
        if (cover.has_trailing_comma) return parser.fail(
            parser.tree.span(element),
            "A rest parameter cannot have a trailing comma",
            .{ .help = "Remove the trailing comma" },
        );
    }

    const params = try parser.tree.addNode(.{ .formal_parameters = .{
        .items = try items.finish(),
        .rest = rest,
        .kind = .arrow_formal_parameters,
    } }, cover.span());
    return buildArrowFunction(parser, params, false, arrow_start, .null, .null, options);
}

pub fn identifierToArrowFunction(
    parser: *Parser,
    id: ast.NodeIndex,
    is_async: bool,
    start: u32,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const param = try toFormalParameter(parser, id);
    const params = try parser.tree.addNode(.{ .formal_parameters = .{
        .items = try parser.tree.addExtra(&.{param}),
        .rest = .null,
        .kind = .arrow_formal_parameters,
    } }, parser.tree.span(id));
    return buildArrowFunction(parser, params, is_async, start, .null, .null, options);
}

pub fn buildArrowFunction(
    parser: *Parser,
    params: ast.NodeIndex,
    is_async: bool,
    arrow_start: u32,
    type_parameters: ast.NodeIndex,
    return_type: ast.NodeIndex,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    try parser.advance() orelse return null;

    const outer = parser.enterFunction(is_async, false);
    defer parser.leaveFunction(outer);

    var body = try functions.parseFunctionBody(parser, .optional) orelse return null;
    const is_expression = body == .null;
    if (is_expression) {
        body = try expressions.parseExpression(parser, Precedence.Assignment, options) orelse
            return null;
    }

    return try parser.finishAt(arrow_start, body, .{ .arrow_function_expression = .{
        .expression = is_expression,
        .async = is_async,
        .params = params,
        .body = body,
        .type_parameters = type_parameters,
        .return_type = return_type,
    } });
}

fn toFormalParameter(parser: *Parser, expr: ast.NodeIndex) Error!ast.NodeIndex {
    try grammar.expressionToPattern(parser, expr, .binding);
    const span = parser.tree.span(expr);
    return parser.tree.addNode(.{ .formal_parameter = .{ .pattern = expr } }, span);
}
