const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const TokenTag = @import("../token.zig").TokenTag;
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
    elements: ast.IndexRange,
    start: u32,
    end: u32,
    /// trailing comma present (valid for arrow params, not for parenthesized expr)
    has_trailing_comma: bool,
};

/// parse CoverParenthesizedExpressionAndArrowParameterList.
/// returns the cover which can be converted to either parenthesized expression or arrow params.
pub fn parseCover(parser: *Parser) Error!?ParenthesizedCover {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume (

    const checkpoint = parser.scratch_cover.begin();
    defer parser.scratch_cover.reset(checkpoint);

    var end = start + 1;
    var has_trailing_comma = false;

    // empty parens: ()
    if (parser.current_token.tag == .right_paren) {
        end = parser.current_token.span.end;
        try parser.advance() orelse return null;
        const elements = try parser.createExtraFromScratch(&parser.scratch_cover, checkpoint);
        return .{
            .elements = elements,
            .start = start,
            .end = end,
            .has_trailing_comma = false,
        };
    }

    while (parser.current_token.tag != .right_paren and parser.current_token.tag != .eof) {
        // rest element: (...x)
        if (parser.current_token.tag == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance() orelse return null;

            const argument = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse return null;

            const spread_end = parser.tree.getSpan(argument).end;

            // for now, store as spread_element; will convert to rest param for arrow functions
            const rest = try parser.tree.createNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );

            try parser.scratch_cover.append(parser.allocator(), rest);

            end = spread_end;

            if (parser.current_token.tag == .comma) {
                try parser.advance() orelse return null;
                has_trailing_comma = true;
            }

            continue;
        }

        // regular element
        const element = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse return null;

        try parser.scratch_cover.append(parser.allocator(), element);

        end = parser.tree.getSpan(element).end;

        // comma or end
        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
            has_trailing_comma = parser.current_token.tag == .right_paren;
        } else if (parser.current_token.tag != .right_paren) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected ',' or ')' in parenthesized expression",
                .{ .help = "Add a comma between elements or close with ')'." },
            );
            return null;
        }
    }

    if (parser.current_token.tag != .right_paren) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated parenthesized expression",
            .{
                .help = "Add a closing ')' to complete the expression.",
                .labels = try parser.labels(&.{parser.label(.{ .start = start, .end = start + 1 }, "Opened here")}),
            },
        );
        return null;
    }

    end = parser.current_token.span.end;

    try parser.advance() orelse return null; // consume )

    const elements = try parser.createExtraFromScratch(&parser.scratch_cover, checkpoint);

    return .{
        .elements = elements,
        .start = start,
        .end = end,
        .has_trailing_comma = has_trailing_comma,
    };
}

/// convert cover to CallExpression.
pub fn coverToCallExpression(parser: *Parser, cover: ParenthesizedCover, callee: ast.NodeIndex) Error!?ast.NodeIndex {
    const elements = parser.tree.getExtra(cover.elements);
    // validate no CoverInitializedName in nested objects
    for (elements) |elem| {
        try grammar.validateNoCoverInitializedSyntax(parser, elem);
    }

    return try parser.tree.createNode(
        .{ .call_expression = .{ .callee = callee, .arguments = cover.elements, .optional = false } },
        .{ .start = parser.tree.getSpan(callee).start, .end = cover.end },
    );
}

/// convert cover to ParenthesizedExpression.
pub fn coverToParenthesizedExpression(parser: *Parser, cover: ParenthesizedCover) Error!?ast.NodeIndex {
    const elements = parser.tree.getExtra(cover.elements);
    // empty parens () without arrow is invalid
    if (elements.len == 0) {
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
    for (elements) |elem| {
        if (parser.tree.getData(elem) == .spread_element) {
            try parser.report(
                parser.tree.getSpan(elem),
                "Rest element is not allowed in parenthesized expression",
                .{ .help = "Spread in parentheses is only valid for arrow function parameters." },
            );

            return null;
        }

        try grammar.validateNoCoverInitializedSyntax(parser, elem);
    }

    if (elements.len == 1) {
        return try parser.tree.createNode(
            .{ .parenthesized_expression = .{ .expression = elements[0] } },
            .{ .start = cover.start, .end = cover.end },
        );
    }

    const first_span = parser.tree.getSpan(elements[0]);
    const last_span = parser.tree.getSpan(elements[elements.len - 1]);

    const seq_expr = try parser.tree.createNode(
        .{ .sequence_expression = .{ .expressions = cover.elements } },
        .{ .start = first_span.start, .end = last_span.end },
    );

    return try parser.tree.createNode(
        .{ .parenthesized_expression = .{ .expression = seq_expr } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// convert cover to ArrowFunctionExpression parameters and body.
pub fn coverToArrowFunction(parser: *Parser, cover: ParenthesizedCover, is_async: bool, arrow_start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume =>

    // convert elements to formal parameters
    const params = try convertToFormalParameters(parser, cover) orelse return null;

    // arrow body (expression or block)
    const body_result = try parseArrowBody(parser) orelse return null;

    return try parser.tree.createNode(
        .{ .arrow_function_expression = .{
            .expression = body_result.is_expression,
            .async = is_async,
            .params = params,
            .body = body_result.body,
        } },
        .{ .start = arrow_start, .end = parser.tree.getSpan(body_result.body).end },
    );
}

/// convert a single identifier to arrow function (x => body case).
pub fn identifierToArrowFunction(parser: *Parser, id: ast.NodeIndex, is_async: bool, start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume =>

    const saved_await_is_keyword = parser.context.await_is_keyword;

    parser.context.await_is_keyword = is_async;

    defer parser.context.await_is_keyword = saved_await_is_keyword;


    // convert identifier_reference to binding_identifier
    try grammar.expressionToPattern(parser, id, .binding);

    const param = try parser.tree.createNode(
        .{ .formal_parameter = .{ .pattern = id } },
        parser.tree.getSpan(id),
    );

    // create formal_parameters with single param
    const params_range = try parser.tree.createExtra(&[_]ast.NodeIndex{param});

    const params = try parser.tree.createNode(
        .{ .formal_parameters = .{ .items = params_range, .rest = .null, .kind = .arrow_formal_parameters } },
        parser.tree.getSpan(id),
    );

    // parse arrow body
    const body_result = try parseArrowBody(parser) orelse return null;

    return try parser.tree.createNode(
        .{ .arrow_function_expression = .{
            .expression = body_result.is_expression,
            .async = is_async,
            .params = params,
            .body = body_result.body,
        } },
        .{ .start = start, .end = parser.tree.getSpan(body_result.body).end },
    );
}

const ArrowBodyResult = struct {
    body: ast.NodeIndex,
    is_expression: bool,
};

fn parseArrowBody(parser: *Parser) Error!?ArrowBodyResult {
    if (parser.current_token.tag == .left_brace) {
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
    defer parser.scratch_cover.reset(checkpoint);

    var rest: ast.NodeIndex = .null;

    const elements = parser.tree.getExtra(cover.elements);
    for (elements) |elem| {
        if (rest != .null) {
            try parser.report(
                parser.tree.getSpan(rest),
                "Rest parameter must be last formal parameter",
                .{ .help = "Move the rest parameter to the end of the parameter list" },
            );

            return null;
        }

        if (parser.tree.getData(elem) == .spread_element) {
            // spread_element to binding_rest_element
            try grammar.expressionToPattern(parser, elem, .binding);
            rest = elem;

            if (cover.has_trailing_comma) {
                try parser.report(
                    parser.tree.getSpan(elem),
                    "Rest parameter must be last formal parameter",
                    .{ .help = "Remove the trailing comma after the rest parameter" },
                );
                return null;
            }

            continue;
        }

        const param = try convertToFormalParameter(parser, elem) orelse return null;

        try parser.scratch_cover.append(parser.allocator(), param);
    }

    const items = try parser.createExtraFromScratch(&parser.scratch_cover, checkpoint);

    return try parser.tree.createNode(
        .{ .formal_parameters = .{ .items = items, .rest = rest, .kind = .arrow_formal_parameters } },
        .{ .start = cover.start, .end = cover.end },
    );
}

fn convertToFormalParameter(parser: *Parser, expr: ast.NodeIndex) Error!?ast.NodeIndex {
    // convert expression to binding pattern
    try grammar.expressionToPattern(parser, expr, .binding);

    // expr is now pattern

    return try parser.tree.createNode(
        .{ .formal_parameter = .{ .pattern = expr } },
        parser.tree.getSpan(expr),
    );
}

pub fn unwrapParens(parser: *Parser, node: ast.NodeIndex) ast.NodeIndex {
    const data = parser.tree.getData(node);

    if (data == .parenthesized_expression) {
        return unwrapParens(parser, data.parenthesized_expression.expression);
    }

    return node;
}
