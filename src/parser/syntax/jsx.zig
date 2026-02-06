const std = @import("std");
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const literals = @import("literals.zig");
const expressions = @import("expressions.zig");

/// context for JSX element parsing, determines post-parse behavior
const JsxElementContext = enum {
    /// top-level JSX expression, needs to advance past final '>'
    top_level,
    /// child of another JSX element, parent's parseJsxChildren handles continuation
    child,
    /// attribute value, restores jsx_tag mode
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
    return parseJsxElement(parser, .top_level);
}

fn parseJsxElement(parser: *Parser, comptime context: JsxElementContext) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const next = try parser.lookAhead() orelse return null;

    // fragment: <>...</>
    if (next.type == .greater_than) {
        return parseJsxFragment(parser);
    }

    const opening = try parseJsxOpeningElement(parser, context) orelse return null;
    const opening_data = parser.getData(opening).jsx_opening_element;
    const opening_end = parser.getSpan(opening).end;

    // self-closing element: <elem />
    if (opening_data.self_closing) {
        return try parser.addNode(.{
            .jsx_element = .{
                .opening_element = opening,
                .children = ast.IndexRange.empty,
                .closing_element = ast.null_node,
            },
        }, .{ .start = start, .end = opening_end });
    }

    // element with children: <elem>...</elem>
    const children = try parseJsxChildren(parser, opening_end) orelse return null;

    const closing = try parseJsxClosingElement(parser, opening_data.name) orelse return null;

    return try parser.addNode(.{
        .jsx_element = .{
            .opening_element = opening,
            .children = children,
            .closing_element = closing,
        },
    }, .{ .start = start, .end = parser.getSpan(closing).end });
}

// https://facebook.github.io/jsx/#prod-JSXFragment
fn parseJsxFragment(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    // parse <>
    try parser.advance() orelse return null; // consume '<'
    if (parser.current_token.type != .greater_than) {
        try parser.report(parser.current_token.span, "Expected '>' to close JSX opening fragment", .{ .help = "Add '>' to complete the fragment opening tag" });
        return null;
    }
    const opening_end = parser.current_token.span.end;
    const opening = try parser.addNode(.{ .jsx_opening_fragment = .{} }, .{ .start = start, .end = opening_end });

    // parse children (don't advance past '>', parseJsxChildren scans from there)
    const children = try parseJsxChildren(parser, opening_end) orelse return null;

    // parse </>
    const closing_start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '<'

    if (!try parser.expect(.slash, "Expected '/' in JSX closing fragment", "Add '/' to close the fragment")) return null;

    const closing_end = parser.current_token.span.end;

    if (!try parser.expect(.greater_than, "Expected '>' to close JSX closing fragment", "Add '>' to complete the fragment closing tag")) return null;

    const closing = try parser.addNode(.{ .jsx_closing_fragment = .{} }, .{ .start = closing_start, .end = closing_end });

    return try parser.addNode(.{
        .jsx_fragment = .{
            .opening_fragment = opening,
            .children = children,
            .closing_fragment = closing,
        },
    }, .{ .start = start, .end = closing_end });
}

// https://facebook.github.io/jsx/#prod-JSXSelfClosingElement
// https://facebook.github.io/jsx/#prod-JSXOpeningElement
fn parseJsxOpeningElement(parser: *Parser, comptime context: JsxElementContext) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    enterJsxTag(parser);
    try parser.advance() orelse return null; // consume '<'

    const name = try parseJsxElementName(parser) orelse return null;
    const attributes = try parseJsxAttributes(parser) orelse return null;

    const self_closing = parser.current_token.type == .slash;
    if (self_closing) {
        try parser.advance() orelse return null; // consume '/'
    }

    if (parser.current_token.type != .greater_than) {
        try parser.report(parser.current_token.span, "Expected '>' to close JSX opening element", .{ .help = "Add '>' to close the JSX tag" });
        return null;
    }
    const end = parser.current_token.span.end;

    // mode and advance handling depends on context and self-closing status:
    // - self-closing attribute: switch to jsx_tag (resume attribute parsing), advance past '>'
    // - self-closing top-level: switch to normal (expression complete), advance past '>'
    // - self-closing child: switch to normal (resume children parsing), don't advance (parseJsxChildren continues)
    // - non-self-closing: stay in current mode (will switch in parseJsxChildren), don't advance
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

    return try parser.addNode(.{
        .jsx_opening_element = .{
            .name = name,
            .attributes = attributes,
            .self_closing = self_closing,
        },
    }, .{ .start = start, .end = end });
}

// https://facebook.github.io/jsx/#prod-JSXClosingElement
fn parseJsxClosingElement(parser: *Parser, opening_name: ast.NodeIndex) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    enterJsxTag(parser);

    try parser.advance() orelse return null; // consume '<'

    if (!try parser.expect(.slash, "Expected '/' in JSX closing element", "Add '/' after '<' to close the element")) return null;

    const name = try parseJsxElementName(parser) orelse return null;
    const end = parser.current_token.span.end;

    exitJsxTag(parser);

    if (!try parser.expect(.greater_than, "Expected '>' to close JSX closing element", "Add '>' to complete the closing tag")) return null;

    if (!jsxNamesMatch(parser, opening_name, name)) {
        const opening_span = parser.getSpan(opening_name);
        const closing_span = parser.getSpan(name);

        try parser.report(closing_span, try parser.formatMessage(
            "Expected closing tag for '<{s}>' but found '</{s}>'",
            .{ parser.source[opening_span.start..opening_span.end], parser.source[closing_span.start..closing_span.end] },
        ), .{
            .help = "JSX opening and closing tags must have matching names",
            .labels = try parser.makeLabels(&.{parser.label(opening_span, "opening tag")}),
        });

        return null;
    }

    return try parser.addNode(.{ .jsx_closing_element = .{ .name = name } }, .{ .start = start, .end = end });
}

fn jsxNamesMatch(parser: *const Parser, a: ast.NodeIndex, b: ast.NodeIndex) bool {
    const span_a = parser.getSpan(a);
    const span_b = parser.getSpan(b);

    const len_a = span_a.end - span_a.start;
    const len_b = span_b.end - span_b.start;

    if (len_a != len_b) return false;

    const text_a = parser.source[span_a.start..span_a.end];
    const text_b = parser.source[span_b.start..span_b.end];

    return std.mem.eql(u8, text_a, text_b);
}

// https://facebook.github.io/jsx/#prod-JSXChildren
fn parseJsxChildren(parser: *Parser, gt_end: u32) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(checkpoint);

    // switch to normal mode for children
    exitJsxTag(parser);

    var scan_from = gt_end;

    while (true) {
        // scan text content until '<' or '{'
        const text_token = parser.lexer.reScanJsxText(scan_from);

        if (text_token.lexeme.len > 0) {
            const text_node = try parser.addNode(.{
                .jsx_text = .{
                    .raw_start = text_token.span.start,
                    .raw_len = @intCast(text_token.lexeme.len),
                },
            }, text_token.span);

            try parser.scratch_b.append(parser.allocator(), text_node);
        }

        // advance past jsx_text to get the delimiter token ('<' or '{')
        try parser.advanceWithRescannedToken(text_token) orelse return null;

        switch (parser.current_token.type) {
            .less_than => {
                // check if it's a closing tag
                const next = try parser.lookAhead() orelse return null;
                if (next.type == .slash) break;

                // nested element
                const child = try parseJsxElement(parser, .child) orelse return null;
                scan_from = parser.getSpan(child).end;
                try parser.scratch_b.append(parser.allocator(), child);
            },
            .left_brace => {
                const next = try parser.lookAhead() orelse return null;
                const child = if (next.type == .spread)
                    try parseJsxSpreadChild(parser) orelse return null
                else
                    try parseJsxExpressionContainer(parser, .child) orelse return null;
                scan_from = parser.getSpan(child).end;
                try parser.scratch_b.append(parser.allocator(), child);
            },
            else => break,
        }
    }

    return try parser.addExtraFromScratch(&parser.scratch_b, checkpoint);
}

// https://facebook.github.io/jsx/#prod-JSXAttributes
fn parseJsxAttributes(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.type == .jsx_identifier or parser.current_token.type == .left_brace) {
        const attr = try parseJsxAttribute(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), attr);
    }

    return try parser.addExtraFromScratch(&parser.scratch_a, checkpoint);
}

// https://facebook.github.io/jsx/#prod-JSXAttribute
fn parseJsxAttribute(parser: *Parser) Error!?ast.NodeIndex {
    // spread attribute: {...expr}
    if (parser.current_token.type == .left_brace) {
        return parseJsxSpreadAttribute(parser);
    }

    // regular attribute: name or name=value
    const name = try parseJsxAttributeName(parser) orelse return null;
    const name_start = parser.getSpan(name).start;

    if (parser.current_token.type != .assign) {
        // boolean attribute: <elem disabled />
        return try parser.addNode(.{
            .jsx_attribute = .{ .name = name, .value = ast.null_node },
        }, .{ .start = name_start, .end = parser.getSpan(name).end });
    }

    try parser.advance() orelse return null; // consume '='
    const value = try parseJsxAttributeValue(parser) orelse return null;

    return try parser.addNode(.{
        .jsx_attribute = .{ .name = name, .value = value },
    }, .{ .start = name_start, .end = parser.getSpan(value).end });
}

// https://facebook.github.io/jsx/#prod-JSXAttributeName
fn parseJsxAttributeName(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var name = try parser.addNode(.{
        .jsx_identifier = .{
            .name_start = start,
            .name_len = @intCast(parser.current_token.lexeme.len),
        },
    }, parser.current_token.span);

    try parser.advance() orelse return null;

    // check for namespaced name: ns:name
    if (parser.current_token.type == .colon) {
        try parser.advance() orelse return null; // consume ':'

        if (parser.current_token.type != .jsx_identifier) {
            try parser.reportFmt(
                parser.current_token.span,
                "Expected identifier after ':' in namespaced attribute, but found '{s}'",
                .{parser.describeToken(parser.current_token)},
                .{ .help = "Namespaced attributes must have the form 'namespace:name'" },
            );
            return null;
        }

        const local = try parser.addNode(.{
            .jsx_identifier = .{
                .name_start = parser.current_token.span.start,
                .name_len = @intCast(parser.current_token.lexeme.len),
            },
        }, parser.current_token.span);
        const end = parser.current_token.span.end;

        try parser.advance() orelse return null;

        name = try parser.addNode(.{
            .jsx_namespaced_name = .{ .namespace = name, .name = local },
        }, .{ .start = start, .end = end });
    }

    return name;
}

// https://facebook.github.io/jsx/#prod-JSXAttributeValue
fn parseJsxAttributeValue(parser: *Parser) Error!?ast.NodeIndex {
    switch (parser.current_token.type) {
        // string literal: "value" or 'value'
        .string_literal => return literals.parseStringLiteral(parser),

        // expression: {expr}
        .left_brace => {
            const container = try parseJsxExpressionContainer(parser, .tag) orelse return null;

            // validate non-empty
            const expr = parser.getData(container).jsx_expression_container.expression;
            if (parser.getData(expr) == .jsx_empty_expression) {
                try parser.report(
                    parser.getSpan(container),
                    "JSX attribute value cannot be an empty expression",
                    .{ .help = "Replace {} with a valid expression or remove the braces to use a string literal" },
                );
                return null;
            }

            return container;
        },

        // nested JSX element: <elem />
        .less_than => return parseJsxElement(parser, .attribute),

        else => {
            try parser.reportFmt(
                parser.current_token.span,
                "Expected string literal or JSX expression for attribute value, but found '{s}'",
                .{parser.describeToken(parser.current_token)},
                .{ .help = "JSX attribute values must be either a string literal (e.g. \"value\") or an expression in braces (e.g. {expression})" },
            );
            return null;
        },
    }
}

const JsxExprContext = enum {
    /// inside a tag (attribute value), restore jsx_tag after '}'
    tag,
    /// inside children area, stay in normal mode after '}'
    child,
};

// parses {expr} in children context
fn parseJsxExpressionContainer(parser: *Parser, comptime context: JsxExprContext) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    // switch to normal mode for JS expression parsing
    // (only matters when called from tag context, already normal in child context)
    if (context == .tag) {
        exitJsxTag(parser);
    }

    try parser.advance() orelse return null; // consume '{'

    // empty expression: {}
    if (parser.current_token.type == .right_brace) {
        const end = parser.current_token.span.end;
        if (context == .tag) {
            enterJsxTag(parser);
        }
        try parser.advance() orelse return null;

        const empty = try parser.addNode(.{ .jsx_empty_expression = .{} }, .{ .start = start + 1, .end = end - 1 });
        return try parser.addNode(.{ .jsx_expression_container = .{ .expression = empty } }, .{ .start = start, .end = end });
    }

    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    const end = parser.current_token.span.end;

    // restore mode before consuming '}'
    if (context == .tag) {
        enterJsxTag(parser);
    }

    if (!try parser.expect(.right_brace, "Expected '}' to close JSX expression", "Add '}' to close the expression")) return null;

    return try parser.addNode(.{ .jsx_expression_container = .{ .expression = expression } }, .{ .start = start, .end = end });
}

// parses {...expr} as spread attribute
fn parseJsxSpreadAttribute(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    exitJsxTag(parser);

    try parser.advance() orelse return null; // consume '{'

    if (!try parser.expect(.spread, "Expected '...' after '{' in JSX spread", "Add '...' to spread the expression")) return null;

    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    const end = parser.current_token.span.end;

    enterJsxTag(parser);

    if (!try parser.expect(.right_brace, "Expected '}' to close JSX spread", "Add '}' to close the spread expression")) return null;

    return try parser.addNode(.{ .jsx_spread_attribute = .{ .argument = expression } }, .{ .start = start, .end = end });
}

// parses {...expr} as spread child
fn parseJsxSpreadChild(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    // already in normal mode from parseJsxChildren
    try parser.advance() orelse return null; // consume '{'

    if (!try parser.expect(.spread, "Expected '...' after '{' in JSX spread", "Add '...' to spread the expression")) return null;

    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    const end = parser.current_token.span.end;

    // stay in normal mode for children
    if (!try parser.expect(.right_brace, "Expected '}' to close JSX spread", "Add '}' to close the spread expression")) return null;

    return try parser.addNode(.{ .jsx_spread_child = .{ .expression = expression } }, .{ .start = start, .end = end });
}

// https://facebook.github.io/jsx/#prod-JSXElementName
fn parseJsxElementName(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.type != .jsx_identifier) {
        try parser.reportFmt(
            parser.current_token.span,
            "Expected JSX element name, but found '{s}'",
            .{parser.describeToken(parser.current_token)},
            .{ .help = "JSX element names must start with a valid identifier" },
        );
        return null;
    }

    const start = parser.current_token.span.start;
    var name = try parser.addNode(.{
        .jsx_identifier = .{
            .name_start = start,
            .name_len = @intCast(parser.current_token.lexeme.len),
        },
    }, parser.current_token.span);

    try parser.advance() orelse return null;

    // member expression: Foo.Bar.Baz
    var is_member = false;
    while (parser.current_token.type == .dot) {
        try parser.advance() orelse return null; // consume '.'

        if (parser.current_token.type != .jsx_identifier) {
            try parser.reportFmt(
                parser.current_token.span,
                "Expected identifier after '.' in JSX member expression, but found '{s}'",
                .{parser.describeToken(parser.current_token)},
                .{ .help = "Member expressions in JSX must have the form 'object.property'" },
            );
            return null;
        }

        is_member = true;
        const property = try parser.addNode(.{
            .jsx_identifier = .{
                .name_start = parser.current_token.span.start,
                .name_len = @intCast(parser.current_token.lexeme.len),
            },
        }, parser.current_token.span);
        const end = parser.current_token.span.end;

        try parser.advance() orelse return null;

        name = try parser.addNode(.{
            .jsx_member_expression = .{ .object = name, .property = property },
        }, .{ .start = start, .end = end });
    }

    // namespaced name: ns:name (not allowed after member expression)
    if (parser.current_token.type == .colon and !is_member) {
        try parser.advance() orelse return null; // consume ':'

        if (parser.current_token.type != .jsx_identifier) {
            try parser.reportFmt(
                parser.current_token.span,
                "Expected identifier after ':' in namespaced element name, but found '{s}'",
                .{parser.describeToken(parser.current_token)},
                .{ .help = "Namespaced element names must have the form 'namespace:name'" },
            );
            return null;
        }

        const local = try parser.addNode(.{
            .jsx_identifier = .{
                .name_start = parser.current_token.span.start,
                .name_len = @intCast(parser.current_token.lexeme.len),
            },
        }, parser.current_token.span);
        const end = parser.current_token.span.end;

        try parser.advance() orelse return null;

        name = try parser.addNode(.{
            .jsx_namespaced_name = .{ .namespace = name, .name = local },
        }, .{ .start = start, .end = end });
    }

    return name;
}
