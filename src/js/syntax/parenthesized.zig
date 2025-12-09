const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const token = @import("../token.zig");

const grammar = @import("../grammar.zig");
const functions = @import("functions.zig");
const expressions = @import("expressions.zig");
const array = @import("array.zig");
const object = @import("object.zig");

/// cover grammar result for parenthesized expressions and arrow parameters.
/// https://tc39.es/ecma262/#prod-CoverParenthesizedExpressionAndArrowParameterList
pub const ParenthesizedCover = struct {
    /// parsed elements (expressions that may become parameters)
    elements: []const ast.NodeIndex,
    /// rest element if any (for arrow function rest param)
    rest: ast.NodeIndex,
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

    const checkpoint = parser.scratch_a.begin();
    errdefer parser.scratch_a.reset(checkpoint);

    var rest: ast.NodeIndex = ast.null_node;
    var end = start + 1;
    var has_trailing_comma = false;

    // empty parens: ()
    if (parser.current_token.type == .right_paren) {
        end = parser.current_token.span.end;
        try parser.advance();
        return .{
            .elements = parser.scratch_a.take(checkpoint),
            .rest = rest,
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

            const argument = try grammar.parseCoverElement(parser) orelse {
                parser.scratch_a.reset(checkpoint);
                return null;
            };
            const spread_end = parser.getSpan(argument).end;

            // for now, store as spread_element; will convert to rest param for arrow functions
            rest = try parser.addNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );
            end = spread_end;

            // rest must be last (can have trailing comma in some cases)
            if (parser.current_token.type == .comma) {
                try parser.advance();
                has_trailing_comma = true;
            }
            break;
        }

        // regular element
        const element = try grammar.parseCoverElement(parser) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };
        try parser.scratch_a.append(parser.allocator(), element);
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
            parser.scratch_a.reset(checkpoint);
            return null;
        }
    }

    if (parser.current_token.type != .right_paren) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated parenthesized expression",
            .{ .help = "Add a closing ')' to complete the expression." },
        );
        parser.scratch_a.reset(checkpoint);
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance(); // consume )

    return .{
        .elements = parser.scratch_a.take(checkpoint),
        .rest = rest,
        .start = start,
        .end = end,
        .has_trailing_comma = has_trailing_comma,
    };
}

/// convert cover to ParenthesizedExpression.
pub fn coverToExpression(parser: *Parser, cover: ParenthesizedCover) Error!?ast.NodeIndex {
    // empty parens () without arrow is invalid
    if (cover.elements.len == 0 and ast.isNull(cover.rest)) {
        try parser.report(
            .{ .start = cover.start, .end = cover.end },
            "Empty parentheses are only valid as arrow function parameters",
            .{ .help = "Use '() => ...' for arrow functions or add an expression inside." },
        );
        return null;
    }

    // rest element without arrow is a spread, which is invalid in parenthesized expr
    if (!ast.isNull(cover.rest)) {
        try parser.report(
            parser.getSpan(cover.rest),
            "Rest element is not allowed in parenthesized expression",
            .{ .help = "Spread in parentheses is only valid for arrow function parameters." },
        );
        return null;
    }

    // trailing comma is invalid in parenthesized expressions
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
    const body_result = try parseArrowBody(parser) orelse return null;

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
    const body_result = try parseArrowBody(parser) orelse return null;

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

fn parseArrowBody(parser: *Parser) Error!?ArrowBodyResult {
    if (parser.current_token.type == .left_brace) {
        // block body: () => { ... }
        const body = try functions.parseFunctionBody(parser) orelse return null;
        return .{ .body = body, .is_expression = false };
    }

    // expression body: () => expr
    // arrow body is parsed at assignment precedence (not comma)
    const expr = try expressions.parseExpression(parser, 2) orelse return null;
    return .{ .body = expr, .is_expression = true };
}

fn convertToFormalParameters(parser: *Parser, cover: ParenthesizedCover) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_b.begin();
    errdefer parser.scratch_b.reset(checkpoint);

    for (cover.elements) |elem| {
        const param = try convertToFormalParameter(parser, elem) orelse {
            parser.scratch_b.reset(checkpoint);
            return null;
        };
        try parser.scratch_b.append(parser.allocator(), param);
    }

    // handle rest parameter
    var rest: ast.NodeIndex = ast.null_node;

    if (!ast.isNull(cover.rest)) {
        const spread_data = parser.getData(cover.rest).spread_element;
        const pattern = try grammar.expressionToPattern(parser, spread_data.argument) orelse return null;
        parser.setData(cover.rest, .{ .binding_rest_element = .{ .argument = pattern } });
        rest = cover.rest;
    }

    const items = try parser.addExtra(parser.scratch_b.take(checkpoint));

    return try parser.addNode(
        .{ .formal_parameters = .{ .items = items, .rest = rest, .kind = .arrow_formal_parameters } },
        .{ .start = cover.start, .end = cover.end },
    );
}

fn convertToFormalParameter(parser: *Parser, expr: ast.NodeIndex) Error!?ast.NodeIndex {
    // convert expression to binding pattern
    const pattern = try grammar.expressionToPattern(parser, expr) orelse return null;

    return try parser.addNode(
        .{ .formal_parameter = .{ .pattern = pattern } },
        parser.getSpan(expr),
    );
}
