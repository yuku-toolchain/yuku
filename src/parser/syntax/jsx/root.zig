const std = @import("std");
const ast = @import("../../ast.zig");
const Precedence = @import("../../token.zig").Precedence;
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;

const literals = @import("../literals.zig");
const expressions = @import("../expressions.zig");
const ts = @import("../ts/types.zig");
const extension = @import("../../extension.zig");

const JsxElementContext = enum {
    top_level,
    child,
    attribute,
};

inline fn enterJsxTag(parser: *Parser) void {
    parser.setLexerMode(.jsx_tag);
}

inline fn exitJsxTag(parser: *Parser) void {
    parser.setLexerMode(.normal);
}

// https://facebook.github.io/jsx/#prod-JSXElement
pub fn parseJsxExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .less_than);
    return parseJsxElement(parser, .top_level);
}

fn parseJsxElement(parser: *Parser, comptime context: JsxElementContext) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    // peek in jsx_tag mode so a fragment's `>` is not glued to what follows, as in `<>=</>`
    enterJsxTag(parser);
    const next = parser.peekAhead();
    exitJsxTag(parser);

    if (next.tag == .greater_than) {
        return parseJsxFragment(parser);
    }

    const opening = try parseJsxOpeningElement(parser, context) orelse return null;
    const opening_data = parser.tree.data(opening).jsx_opening_element;
    const opening_end = parser.tree.span(opening).end;
    if (try extension.at(.jsx_element_tail, .{ parser, opening, context })) |outcome| {
        return outcome.node;
    }
    try extension.at(.jsx_element_name_check, .{ parser, opening_data.name });

    if (opening_data.self_closing) {
        return try parser.tree.addNode(.{
            .jsx_element = .{
                .opening_element = opening,
                .children = ast.IndexRange.empty,
                .closing_element = .null,
            },
        }, .{ .start = start, .end = opening_end });
    }

    const children = try parseJsxChildren(parser, opening_end) orelse return null;

    const closing = try parseJsxClosingElement(
        parser,
        opening_data.name,
        context,
    ) orelse return null;

    return try parser.tree.addNode(.{
        .jsx_element = .{
            .opening_element = opening,
            .children = children,
            .closing_element = closing,
        },
    }, .{ .start = start, .end = parser.tree.span(closing).end });
}

// https://facebook.github.io/jsx/#prod-JSXFragment
fn parseJsxFragment(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    enterJsxTag(parser);
    try parser.advance() orelse return null; // consume '<'
    if (parser.current_token.tag != .greater_than) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '>' to close JSX opening fragment",
            .{ .help = "Add '>' to complete the fragment opening tag" },
        );
        return null;
    }
    const opening_end = parser.current_token.span.end;
    const opening = try parser.tree.addNode(
        .{ .jsx_opening_fragment = .{} },
        .{ .start = start, .end = opening_end },
    );
    if (try extension.at(.jsx_fragment_tail, .{ parser, opening })) |outcome| return outcome.node;

    // parseJsxChildren rescans from the `>`
    const children = try parseJsxChildren(parser, opening_end) orelse return null;

    const closing_start = parser.current_token.span.start;

    enterJsxTag(parser);

    try parser.advance() orelse return null; // consume '<'

    if (!try parser.expect(
        .slash,
        "Expected '/' in JSX closing fragment",
        "Add '/' to close the fragment",
    )) return null;

    const closing_end = parser.current_token.span.end;

    // leave jsx_tag before `>` so the token after the fragment is plain javascript
    exitJsxTag(parser);

    if (!try parser.expect(
        .greater_than,
        "Expected '>' to close JSX closing fragment",
        "Add '>' to complete the fragment closing tag",
    )) return null;

    const closing = try parser.tree.addNode(
        .{ .jsx_closing_fragment = .{} },
        .{ .start = closing_start, .end = closing_end },
    );

    return try parser.tree.addNode(.{
        .jsx_fragment = .{
            .opening_fragment = opening,
            .children = children,
            .closing_fragment = closing,
        },
    }, .{ .start = start, .end = closing_end });
}

// https://facebook.github.io/jsx/#prod-JSXSelfClosingElement
// https://facebook.github.io/jsx/#prod-JSXOpeningElement
fn parseJsxOpeningElement(
    parser: *Parser,
    comptime context: JsxElementContext,
) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .less_than);
    const start = parser.current_token.span.start;

    enterJsxTag(parser);
    try parser.advance() orelse return null; // consume '<'

    const name = try parseJsxElementName(parser) orelse return null;

    const is_ts_generic = parser.tree.isTs() and ts.isAngleOpen(parser.current_token.tag);
    const type_arguments = if (is_ts_generic) blk: {
        exitJsxTag(parser);
        const args = try ts.parseTypeArguments(parser);
        enterJsxTag(parser);
        try parser.reScanCurrent() orelse return null;
        break :blk args;
    } else .null;

    const attributes = try parseJsxAttributes(parser) orelse return null;

    const self_closing = parser.current_token.tag == .slash;
    if (self_closing) {
        try parser.advance() orelse return null; // consume '/'
    }

    if (parser.current_token.tag != .greater_than) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '>' to close JSX opening element",
            .{ .help = "Add '>' to close the JSX tag" },
        );
        return null;
    }
    const end = parser.current_token.span.end;

    // a self-closing child leaves `>` for parseJsxChildren to rescan, an attribute value
    // resumes jsx_tag mode, top level returns to javascript
    if (self_closing) {
        if (context == .attribute) {
            enterJsxTag(parser);
            try parser.advance() orelse return null;
        } else {
            exitJsxTag(parser);
            if (context == .top_level) {
                try parser.advance() orelse return null;
            }
        }
    }

    return try parser.tree.addNode(.{
        .jsx_opening_element = .{
            .name = name,
            .attributes = attributes,
            .self_closing = self_closing,
            .type_arguments = type_arguments,
        },
    }, .{ .start = start, .end = end });
}

// https://facebook.github.io/jsx/#prod-JSXClosingElement
fn parseJsxClosingElement(
    parser: *Parser,
    opening_name: ast.NodeIndex,
    comptime context: JsxElementContext,
) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .less_than) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '</' to close the JSX element",
            .{ .help = "Add a closing tag to match the opening element" },
        );
        return null;
    }
    const start = parser.current_token.span.start;

    enterJsxTag(parser);

    try parser.advance() orelse return null; // consume '<'

    const slash_ok = try parser.expect(
        .slash,
        "Expected '/' in JSX closing element",
        "Add '/' after '<' to close the element",
    );
    if (!slash_ok) return null;

    const name = try parseJsxElementName(parser) orelse return null;

    if (parser.current_token.tag != .greater_than) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '>' to close JSX closing element",
            .{ .help = "Add '>' to complete the closing tag" },
        );
        return null;
    }
    const end = parser.current_token.span.end;

    // a child closing tag leaves `>` so parseJsxChildren can rescan the following text
    switch (context) {
        .child => exitJsxTag(parser),
        .top_level => {
            exitJsxTag(parser);
            try parser.advance() orelse return null;
        },
        .attribute => {
            enterJsxTag(parser);
            try parser.advance() orelse return null;
        },
    }

    if (!jsxNamesMatch(parser, opening_name, name)) {
        const opening_span = parser.tree.span(opening_name);
        const closing_span = parser.tree.span(name);

        try parser.report(closing_span, try parser.fmt(
            "Expected closing tag for '<{s}>' but found '</{s}>'",
            .{ parser.spanText(opening_span), parser.spanText(closing_span) },
        ), .{
            .help = "JSX opening and closing tags must have matching names",
            .labels = try parser.labels(&.{parser.label(opening_span, "opening tag")}),
        });

        return null;
    }

    return try parser.tree.addNode(
        .{ .jsx_closing_element = .{ .name = name } },
        .{ .start = start, .end = end },
    );
}

fn jsxNamesMatch(parser: *const Parser, a: ast.NodeIndex, b: ast.NodeIndex) bool {
    if (extension.at(.jsx_names_match, .{ parser, a, b })) |matches| return matches;
    const span_a = parser.tree.span(a);
    const span_b = parser.tree.span(b);

    const len_a = span_a.end - span_a.start;
    const len_b = span_b.end - span_b.start;

    if (len_a != len_b) return false;

    const text_a = parser.spanText(span_a);
    const text_b = parser.spanText(span_b);

    return std.mem.eql(u8, text_a, text_b);
}

// https://facebook.github.io/jsx/#prod-JSXChildren
fn parseJsxChildren(parser: *Parser, gt_end: u32) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(checkpoint);

    exitJsxTag(parser);

    var scan_from = gt_end;

    while (true) {
        const text_token = parser.lexer.reScanJsxText(scan_from);

        if (text_token.len() > 0) {
            var text_value = parser.tree.sourceSlice(text_token.span.start, text_token.span.end);
            if (try extension.at(.jsx_text_value, .{ parser, text_token.span })) |value| {
                text_value = value;
            }
            const text_node = try parser.tree.addNode(.{
                .jsx_text = .{
                    .value = text_value,
                },
            }, text_token.span);

            try parser.scratch_b.append(parser.allocator(), text_node);
        }

        try parser.advanceWithRescannedToken(text_token) orelse return null;

        switch (parser.current_token.tag) {
            .less_than => {
                const next = parser.peekAhead();
                if (next.tag == .slash) break;

                const child = try parseJsxElement(parser, .child) orelse return null;
                scan_from = parser.tree.span(child).end;
                try parser.scratch_b.append(parser.allocator(), child);
            },
            .left_brace => {
                const child = try parseJsxChildFromLeftBrace(parser) orelse return null;
                scan_from = parser.tree.span(child).end;
                try parser.scratch_b.append(parser.allocator(), child);
            },
            .greater_than, .right_brace => {
                try parser.report(
                    parser.current_token.span,
                    if (parser.current_token.tag == .greater_than)
                        "Unexpected '>' in JSX text"
                    else
                        "Unexpected '}' in JSX text",
                    .{ .help = "Escape it with an HTML entity or wrap it in an" ++
                        " expression container like {'>'}." },
                );
                scan_from = parser.current_token.span.end;
            },
            else => break,
        }
    }

    return try parser.flushToExtras(&parser.scratch_b, checkpoint);
}

fn parseJsxChildFromLeftBrace(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '{'

    if (try extension.at(.jsx_child, .{parser})) |outcome| return outcome.node;

    if (parser.current_token.tag == .spread) {
        try parser.advance() orelse return null; // consume '...'

        const expression = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
            return null;
        const end = try expectJsxChildRightBrace(parser, "JSX spread") orelse return null;

        return try parser.tree.addNode(
            .{ .jsx_spread_child = .{ .expression = expression } },
            .{ .start = start, .end = end },
        );
    }

    if (parser.current_token.tag == .right_brace) {
        const end = parser.current_token.span.end;
        const empty = try parser.tree.addNode(
            .{ .jsx_empty_expression = .{} },
            .{ .start = start + 1, .end = end - 1 },
        );
        return try parser.tree.addNode(
            .{ .jsx_expression_container = .{ .expression = empty } },
            .{ .start = start, .end = end },
        );
    }

    const expression = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
        return null;
    const end = try expectJsxChildRightBrace(parser, "JSX expression") orelse return null;

    return try parser.tree.addNode(
        .{ .jsx_expression_container = .{ .expression = expression } },
        .{ .start = start, .end = end },
    );
}

fn expectJsxChildRightBrace(parser: *Parser, comptime what: []const u8) Error!?u32 {
    if (parser.current_token.tag != .right_brace) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '}' to close " ++ what,
            .{ .help = "Add '}' to close the expression" },
        );
        return null;
    }
    return parser.current_token.span.end;
}

// https://facebook.github.io/jsx/#prod-JSXAttributes
fn parseJsxAttributes(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag == .jsx_identifier or parser.current_token.tag == .left_brace) {
        const attr = try parseJsxAttribute(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), attr);
    }

    return try parser.flushToExtras(&parser.scratch_a, checkpoint);
}

// https://facebook.github.io/jsx/#prod-JSXAttribute
fn parseJsxAttribute(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .left_brace) {
        return parseJsxSpreadAttribute(parser);
    }

    const name = try parseJsxAttributeName(parser) orelse return null;
    const name_start = parser.tree.span(name).start;

    if (parser.current_token.tag != .assign) {
        return try parser.tree.addNode(.{
            .jsx_attribute = .{ .name = name, .value = .null },
        }, .{ .start = name_start, .end = parser.tree.span(name).end });
    }

    try parser.advance() orelse return null; // consume '='
    const value = try parseJsxAttributeValue(parser) orelse return null;

    return try parser.tree.addNode(.{
        .jsx_attribute = .{ .name = name, .value = value },
    }, .{ .start = name_start, .end = parser.tree.span(value).end });
}

// https://facebook.github.io/jsx/#prod-JSXAttributeName
fn parseJsxAttributeName(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var name = try parser.tree.addNode(.{
        .jsx_identifier = .{
            .name = try parser.identifierName(parser.current_token),
        },
    }, parser.current_token.span);

    try parser.advance() orelse return null;

    if (parser.current_token.tag == .colon) {
        try parser.advance() orelse return null; // consume ':'

        if (parser.current_token.tag != .jsx_identifier) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected identifier after ':' in namespaced attribute",
                .{ .help = "Namespaced attributes must have the form 'namespace:name'" },
            );
            return null;
        }

        const local = try parser.tree.addNode(.{
            .jsx_identifier = .{
                .name = try parser.identifierName(parser.current_token),
            },
        }, parser.current_token.span);
        const end = parser.current_token.span.end;

        try parser.advance() orelse return null;

        name = try parser.tree.addNode(.{
            .jsx_namespaced_name = .{ .namespace = name, .name = local },
        }, .{ .start = start, .end = end });
    }

    return name;
}

// https://facebook.github.io/jsx/#prod-JSXAttributeValue
fn parseJsxAttributeValue(parser: *Parser) Error!?ast.NodeIndex {
    switch (parser.current_token.tag) {
        .string_literal => return literals.parseStringLiteral(parser),

        .left_brace => {
            const container = try parseJsxExpressionContainer(parser) orelse return null;

            const expr = parser.tree.data(container).jsx_expression_container.expression;
            if (parser.tree.data(expr) == .jsx_empty_expression) {
                try parser.report(
                    parser.tree.span(container),
                    "JSX attribute value cannot be an empty expression",
                    .{ .help = "Replace {} with a valid expression or remove the braces" ++
                        " to use a string literal" },
                );
                return null;
            }

            return container;
        },

        .less_than => return parseJsxElement(parser, .attribute),

        else => {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected string literal or JSX expression for attribute value",
                .{ .help = "JSX attribute values must be either a string literal" ++
                    " (e.g. \"value\") or an expression in braces (e.g. {expression})" },
            );
            return null;
        },
    }
}

fn parseJsxExpressionContainer(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const start = parser.current_token.span.start;

    exitJsxTag(parser);

    try parser.advance() orelse return null; // consume '{'

    if (parser.current_token.tag == .right_brace) {
        const end = parser.current_token.span.end;
        enterJsxTag(parser);
        try parser.advance() orelse return null;

        const empty = try parser.tree.addNode(
            .{ .jsx_empty_expression = .{} },
            .{ .start = start + 1, .end = end - 1 },
        );
        return try parser.tree.addNode(
            .{ .jsx_expression_container = .{ .expression = empty } },
            .{ .start = start, .end = end },
        );
    }

    const expression = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
        return null;
    const end = parser.current_token.span.end;

    // restore jsx_tag before `}` so the next attribute is scanned in tag mode
    enterJsxTag(parser);

    const brace_ok = try parser.expect(
        .right_brace,
        "Expected '}' to close JSX expression",
        "Add '}' to close the expression",
    );
    if (!brace_ok) return null;

    return try parser.tree.addNode(
        .{ .jsx_expression_container = .{ .expression = expression } },
        .{ .start = start, .end = end },
    );
}

fn parseJsxSpreadAttribute(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const start = parser.current_token.span.start;

    exitJsxTag(parser);

    try parser.advance() orelse return null; // consume '{'

    const spread_ok = try parser.expect(
        .spread,
        "Expected '...' after '{' in JSX spread",
        "Add '...' to spread the expression",
    );
    if (!spread_ok) return null;

    const expression = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
        return null;
    const end = parser.current_token.span.end;

    enterJsxTag(parser);

    const brace_ok = try parser.expect(
        .right_brace,
        "Expected '}' to close JSX spread",
        "Add '}' to close the spread expression",
    );
    if (!brace_ok) return null;

    return try parser.tree.addNode(
        .{ .jsx_spread_attribute = .{ .argument = expression } },
        .{ .start = start, .end = end },
    );
}

// https://facebook.github.io/jsx/#prod-JSXElementName
fn parseJsxElementName(parser: *Parser) Error!?ast.NodeIndex {
    if (try extension.at(.jsx_element_name, .{parser})) |outcome| return outcome.node;
    if (parser.current_token.tag != .jsx_identifier) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected JSX element name",
            .{ .help = "JSX element names must start with a valid identifier" },
        );
        return null;
    }

    const start = parser.current_token.span.start;
    var name = try parser.tree.addNode(.{
        .jsx_identifier = .{
            .name = try parser.identifierName(parser.current_token),
        },
    }, parser.current_token.span);

    try parser.advance() orelse return null;

    var is_member = false;
    while (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null; // consume '.'

        if (parser.current_token.tag != .jsx_identifier) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected identifier after '.' in JSX member expression",
                .{ .help = "Member expressions in JSX must have the form 'object.property'" },
            );
            return null;
        }

        is_member = true;
        const property = try parser.tree.addNode(.{
            .jsx_identifier = .{
                .name = try parser.identifierName(parser.current_token),
            },
        }, parser.current_token.span);
        const end = parser.current_token.span.end;

        try parser.advance() orelse return null;

        name = try parser.tree.addNode(.{
            .jsx_member_expression = .{ .object = name, .property = property },
        }, .{ .start = start, .end = end });
    }

    if (parser.current_token.tag == .colon and !is_member) {
        try parser.advance() orelse return null; // consume ':'

        if (parser.current_token.tag != .jsx_identifier) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected identifier after ':' in namespaced element name",
                .{ .help = "Namespaced element names must have the form 'namespace:name'" },
            );
            return null;
        }

        const local = try parser.tree.addNode(.{
            .jsx_identifier = .{
                .name = try parser.identifierName(parser.current_token),
            },
        }, parser.current_token.span);
        const end = parser.current_token.span.end;

        try parser.advance() orelse return null;

        name = try parser.tree.addNode(.{
            .jsx_namespaced_name = .{ .namespace = name, .name = local },
        }, .{ .start = start, .end = end });
    }

    return name;
}
