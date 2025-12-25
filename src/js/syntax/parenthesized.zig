const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const token = @import("../token.zig");
const Precedence = @import("../token.zig").Precedence;

const grammar = @import("../grammar.zig");
const functions = @import("functions.zig");
const expressions = @import("expressions.zig");
const array = @import("array.zig");
const object = @import("object.zig");

/// cover grammar result for parenthesized expressions and arrow parameters.
/// https://tc39.es/ecma262/#prod-CoverParenthesizedExpressionAndArrowParameterList
pub const ParenthesizedCover = struct {
    /// parsed elements (expressions + spread elements)
    elements: []const ast.NodeIndex,
    start: u32,
    end: u32,
    /// trailing comma present (valid for arrow params, not for parenthesized expr)
    has_trailing_comma: bool,
};

/// parse CoverParenthesizedExpressionAndArrowParameterList.
/// returns the cover which can be converted to either parenthesized expression or arrow params.
pub fn parseCover(parser: *Parser) Error!?ParenthesizedCover {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume (

    const checkpoint = parser.scratch_cover.begin();
    errdefer parser.scratch_cover.reset(checkpoint);

    var end = start + 1;
    var has_trailing_comma = false;

    // empty parens: ()
    if (parser.current_token.type == .right_paren) {
        end = parser.current_token.span.end;
        try parser.advance();
        return .{
            .elements = parser.scratch_cover.take(checkpoint),
            .start = start,
            .end = end,
            .has_trailing_comma = false,
        };
    }

    while (parser.current_token.type != .right_paren and parser.current_token.type != .eof) {
        // rest element: (...x)
        if (parser.current_token.type == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance();

            const argument = try grammar.parseCoverExpression(parser, Precedence.Assignment) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };
            const spread_end = parser.getSpan(argument).end;

            // for now, store as spread_element; will convert to rest param for arrow functions
            const rest = try parser.addNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );

            try parser.scratch_cover.append(parser.allocator(), rest);

            end = spread_end;

            if (parser.current_token.type == .comma) {
                try parser.advance();
                has_trailing_comma = true;
            }

            continue;
        }

        // regular element
        const element = try grammar.parseCoverExpression(parser, Precedence.Assignment) orelse {
            parser.scratch_cover.reset(checkpoint);
            return null;
        };

        try parser.scratch_cover.append(parser.allocator(), element);

        end = parser.getSpan(element).end;

        // comma or end
        if (parser.current_token.type == .comma) {
            try parser.advance();
            has_trailing_comma = parser.current_token.type == .right_paren;
        } else if (parser.current_token.type != .right_paren) {
            try parser.report(
                parser.current_token.span,
                "Expected ',' or ')' in parenthesized expression",
                .{ .help = "Add a comma between elements or close with ')'." },
            );
            parser.scratch_cover.reset(checkpoint);
            return null;
        }
    }

    if (parser.current_token.type != .right_paren) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated parenthesized expression",
            .{
                .help = "Add a closing ')' to complete the expression.",
                .labels = try parser.makeLabels(&.{parser.label(.{ .start = start, .end = start + 1 }, "Opened here")}),
            },
        );
        parser.scratch_cover.reset(checkpoint);
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance(); // consume )

    return .{
        .elements = parser.scratch_cover.take(checkpoint),
        .start = start,
        .end = end,
        .has_trailing_comma = has_trailing_comma,
    };
}

/// convert cover to CallExpression.
pub fn coverToCallExpression(parser: *Parser, cover: ParenthesizedCover, callee: ast.NodeIndex) Error!?ast.NodeIndex {
    // validate no CoverInitializedName in nested objects
    for (cover.elements) |elem| {
        if (!try grammar.validateNoInvalidCoverSyntax(parser, elem)) {
            return null;
        }
    }

    return try parser.addNode(
        .{ .call_expression = .{ .callee = callee, .arguments = try parser.addExtra(cover.elements), .optional = false } },
        .{ .start = parser.getSpan(callee).start, .end = cover.end },
    );
}

/// convert cover to ParenthesizedExpression.
pub fn coverToParenthesizedExpression(parser: *Parser, cover: ParenthesizedCover) Error!?ast.NodeIndex {
    // empty parens () without arrow is invalid
    if (cover.elements.len == 0) {
        try parser.report(
            .{ .start = cover.start, .end = cover.end },
            "Empty parentheses are only valid as arrow function parameters",
            .{},
        );
        return null;
    }

    if (cover.has_trailing_comma) {
        try parser.report(
            .{ .start = cover.start, .end = cover.end },
            "Trailing comma is not allowed in parenthesized expression",
            .{ .help = "Remove the trailing comma or use as arrow function parameters." },
        );
        return null;
    }

    // validate no CoverInitializedName in nested objects
    for (cover.elements) |elem| {
        if (parser.getData(elem) == .spread_element) {
            try parser.report(
                parser.getSpan(elem),
                "Rest element is not allowed in parenthesized expression",
                .{ .help = "Spread in parentheses is only valid for arrow function parameters." },
            );

            return null;
        }

        if (!try grammar.validateNoInvalidCoverSyntax(parser, elem)) {
            return null;
        }
    }

    if (cover.elements.len == 1) {
        return try parser.addNode(
            .{ .parenthesized_expression = .{ .expression = cover.elements[0] } },
            .{ .start = cover.start, .end = cover.end },
        );
    }

    const first_span = parser.getSpan(cover.elements[0]);
    const last_span = parser.getSpan(cover.elements[cover.elements.len - 1]);

    const seq_expr = try parser.addNode(
        .{ .sequence_expression = .{ .expressions = try parser.addExtra(cover.elements) } },
        .{ .start = first_span.start, .end = last_span.end },
    );

    return try parser.addNode(
        .{ .parenthesized_expression = .{ .expression = seq_expr } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// convert cover to ArrowFunctionExpression parameters and body.
pub fn coverToArrowFunction(parser: *Parser, cover: ParenthesizedCover, is_async: bool, arrow_start: u32) Error!?ast.NodeIndex {
    try parser.advance(); // consume =>

    // convert elements to formal parameters
    const params = try convertToFormalParameters(parser, cover) orelse return null;

    // arrow body (expression or block)
    const body_result = try parseArrowBody(parser, is_async) orelse return null;

    return try parser.addNode(
        .{ .arrow_function_expression = .{
            .expression = body_result.is_expression,
            .async = is_async,
            .params = params,
            .body = body_result.body,
        } },
        .{ .start = arrow_start, .end = parser.getSpan(body_result.body).end },
    );
}

/// convert a single identifier to arrow function (x => body case).
pub fn identifierToArrowFunction(parser: *Parser, id: ast.NodeIndex, is_async: bool, start: u32) Error!?ast.NodeIndex {
    try parser.advance(); // consume =>

    // convert identifier_reference to binding_identifier
    const id_data = parser.getData(id).identifier_reference;

    parser.setData(id, .{ .binding_identifier = .{
        .name_start = id_data.name_start,
        .name_len = id_data.name_len,
    } });

    const param = try parser.addNode(
        .{ .formal_parameter = .{ .pattern = id } },
        parser.getSpan(id),
    );

    // create formal_parameters with single param
    const params_range = try parser.addExtra(&[_]ast.NodeIndex{param});

    const params = try parser.addNode(
        .{ .formal_parameters = .{ .items = params_range, .rest = ast.null_node, .kind = .arrow_formal_parameters } },
        parser.getSpan(id),
    );

    // parse arrow body
    const body_result = try parseArrowBody(parser, is_async) orelse return null;

    return try parser.addNode(
        .{ .arrow_function_expression = .{
            .expression = body_result.is_expression,
            .async = is_async,
            .params = params,
            .body = body_result.body,
        } },
        .{ .start = start, .end = parser.getSpan(body_result.body).end },
    );
}

const ArrowBodyResult = struct {
    body: ast.NodeIndex,
    is_expression: bool,
};

fn parseArrowBody(parser: *Parser, is_async: bool) Error!?ArrowBodyResult {
    const saved_async = parser.context.in_async;
    const saved_generator = parser.context.in_generator;
    const saved_in_function = parser.context.in_function;

    parser.context.in_async = is_async;
    parser.context.in_generator = false;
    parser.context.in_function = true;

    defer {
        parser.context.in_generator = saved_generator;
        parser.context.in_async = saved_async;
        parser.context.in_function = saved_in_function;
    }

    if (parser.current_token.type == .left_brace) {
        // block body: () => { ... }
        const body = try functions.parseFunctionBody(parser) orelse return null;
        return .{ .body = body, .is_expression = false };
    }

    // expression body: () => expr
    // arrow body is parsed at assignment precedence
    const expr = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
    return .{ .body = expr, .is_expression = true };
}

fn convertToFormalParameters(parser: *Parser, cover: ParenthesizedCover) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_cover.begin();
    errdefer parser.scratch_cover.reset(checkpoint);

    var rest: ast.NodeIndex = ast.null_node;

    for (cover.elements) |elem| {
        if (!ast.isNull(rest)) {
            try parser.report(
                parser.getSpan(rest),
                "Rest parameter must be last formal parameter",
                .{ .help = "Move the rest parameter to the end of the parameter list" },
            );

            return null;
        }

        if (parser.getData(elem) == .spread_element) {
            const spread_data = parser.getData(elem).spread_element;

            const pattern = try grammar.expressionToPattern(parser, spread_data.argument, .binding) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };

            parser.setData(elem, .{ .binding_rest_element = .{ .argument = pattern } });

            rest = elem;

            continue;
        }

        const param = try convertToFormalParameter(parser, elem) orelse {
            parser.scratch_cover.reset(checkpoint);
            return null;
        };

        try parser.scratch_cover.append(parser.allocator(), param);
    }

    const items = try parser.addExtra(parser.scratch_cover.take(checkpoint));

    return try parser.addNode(
        .{ .formal_parameters = .{ .items = items, .rest = rest, .kind = .arrow_formal_parameters } },
        .{ .start = cover.start, .end = cover.end },
    );
}

fn convertToFormalParameter(parser: *Parser, expr: ast.NodeIndex) Error!?ast.NodeIndex {
    // convert expression to binding pattern
    const pattern = try grammar.expressionToPattern(parser, expr, .binding) orelse return null;

    return try parser.addNode(
        .{ .formal_parameter = .{ .pattern = pattern } },
        parser.getSpan(expr),
    );
}
pub fn unwrapParenthesized(parser: *Parser, node: ast.NodeIndex) ast.NodeIndex {
    const data = parser.getData(node);

    if (data == .parenthesized_expression) {
        return unwrapParenthesized(parser, data.parenthesized_expression.expression);
    }

    return node;
}
