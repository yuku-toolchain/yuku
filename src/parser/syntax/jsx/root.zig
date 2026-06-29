const std = @import("std");
const ast = @import("../../ast.zig");
const Precedence = @import("../../token.zig").Precedence;
const Token = @import("../../token.zig").Token;
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;

const literals = @import("../literals.zig");
const expressions = @import("../expressions.zig");
const ts = @import("../ts/types.zig");
const tsrx = @import("../tsrx/root.zig");
const tsrx_dynamic_tag = @import("../tsrx/dynamic_tag.zig");
const tsrx_template = @import("../tsrx/template.zig");

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
    std.debug.assert(parser.current_token.tag == .less_than);
    return parseJsxElement(parser, .top_level);
}

fn parseJsxElement(parser: *Parser, comptime context: JsxElementContext) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const next = parser.peekAhead() orelse return null;

    // fragment: <>...</>
    if (next.tag == .greater_than) {
        return parseJsxFragment(parser);
    }

    const opening = try parseJsxOpeningElement(parser, context) orelse return null;
    const opening_data = parser.tree.data(opening).jsx_opening_element;
    const opening_end = parser.tree.span(opening).end;

    if (isTsrxStyleElementName(parser, opening_data.name)) {
        return try parseTsrxStyleElement(
            parser,
            opening,
            opening_data,
            start,
            opening_end,
            context,
        );
    }

    // self-closing element: <elem />
    if (opening_data.self_closing) {
        return try parser.tree.addNode(.{
            .jsx_element = .{
                .opening_element = opening,
                .children = ast.IndexRange.empty,
                .closing_element = .null,
            },
        }, .{ .start = start, .end = opening_end });
    }

    // element with children: <elem>...</elem>
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

fn parseTsrxStyleElement(
    parser: *Parser,
    opening: ast.NodeIndex,
    opening_data: ast.JSXOpeningElement,
    start: u32,
    opening_end: u32,
    comptime context: JsxElementContext,
) Error!?ast.NodeIndex {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(isTsrxStyleElementName(parser, opening_data.name));
    std.debug.assert(start <= opening_end);

    if (opening_data.self_closing) {
        return try parser.tree.addNode(.{
            .jsx_style_element = .{
                .opening_element = opening,
                .children = ast.IndexRange.empty,
                .closing_element = .null,
                .css = .empty,
            },
        }, .{ .start = start, .end = opening_end });
    }

    const close_text = "</style>";
    const close_index = std.mem.indexOfPos(u8, parser.source, opening_end, close_text) orelse {
        try parser.report(
            .{ .start = opening_end, .end = opening_end },
            "Unclosed TSRX style element",
            .{ .help = "Add '</style>' before the end of the template." },
        );
        return null;
    };

    const close_start: u32 = @intCast(close_index);
    const close_end = close_start + @as(u32, @intCast(close_text.len));

    const css = parser.tree.sourceSlice(opening_end, close_start);
    const style_sheet = try parser.tree.addNode(
        .{ .style_sheet = .{ .source = css } },
        .{ .start = opening_end, .end = close_start },
    );
    const children = try parser.tree.addExtra(&.{style_sheet});

    const name_start = close_start + 2;
    const name_end = name_start + @as(u32, @intCast("style".len));
    const name = try parser.tree.addNode(.{
        .jsx_identifier = .{ .name = parser.tree.sourceSlice(name_start, name_end) },
    }, .{ .start = name_start, .end = name_end });
    const closing = try parser.tree.addNode(
        .{ .jsx_closing_element = .{ .name = name } },
        .{ .start = close_start, .end = close_end },
    );

    try finishTsrxStyleElement(parser, close_end, context) orelse return null;

    return try parser.tree.addNode(.{
        .jsx_style_element = .{
            .opening_element = opening,
            .children = children,
            .closing_element = closing,
            .css = css,
        },
    }, .{ .start = start, .end = close_end });
}

fn finishTsrxStyleElement(
    parser: *Parser,
    close_end: u32,
    comptime context: JsxElementContext,
) Error!?void {
    std.debug.assert(close_end <= parser.source.len);

    const closing_gt: Token = .{
        .tag = .greater_than,
        .span = .{ .start = close_end - 1, .end = close_end },
    };

    parser.lexer.rewindTo(close_end);
    switch (context) {
        .child => {
            exitJsxTag(parser);
            parser.current_token = closing_gt;
            parser.prev_token_end = close_end;
        },
        .top_level => {
            exitJsxTag(parser);
            try parser.advanceWithRescannedToken(closing_gt) orelse return null;
        },
        .attribute => {
            enterJsxTag(parser);
            try parser.advanceWithRescannedToken(closing_gt) orelse return null;
        },
    }
}

// https://facebook.github.io/jsx/#prod-JSXFragment
fn parseJsxFragment(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    // parse <>
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

    // parse children (don't advance past '>', parseJsxChildren scans from there)
    const children = try parseJsxChildren(parser, opening_end) orelse return null;

    // parse </>
    const closing_start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '<'

    if (!try parser.expect(
        .slash,
        "Expected '/' in JSX closing fragment",
        "Add '/' to close the fragment",
    )) return null;

    const closing_end = parser.current_token.span.end;

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

    // mode and advance handling depends on context and self-closing status:
    // - self-closing attribute: switch to jsx_tag (resume attribute parsing), advance past '>'
    // - self-closing top-level: switch to normal (expression complete), advance past '>'
    // - self-closing child: switch to normal (resume children parsing), don't advance
    //   (parseJsxChildren continues)
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

    // a .child closing tag leaves the `>` in place so the parent `parseJsxChildren` loop
    // can rescan the following jsx text without a stray identifier scan.
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
    switch (parser.tree.data(a)) {
        .jsx_expression_container => |a_container| switch (parser.tree.data(b)) {
            .jsx_expression_container => |b_container| {
                const expression_a = parser.spanText(parser.tree.span(a_container.expression));
                const expression_b = parser.spanText(parser.tree.span(b_container.expression));
                const trimmed_a = std.mem.trim(u8, expression_a, " \t\r\n");
                const trimmed_b = std.mem.trim(u8, expression_b, " \t\r\n");
                return std.mem.eql(u8, trimmed_a, trimmed_b);
            },
            else => {},
        },
        else => {},
    }

    const span_a = parser.tree.span(a);
    const span_b = parser.tree.span(b);

    const len_a = span_a.end - span_a.start;
    const len_b = span_b.end - span_b.start;

    if (len_a != len_b) return false;

    const text_a = parser.spanText(span_a);
    const text_b = parser.spanText(span_b);

    return std.mem.eql(u8, text_a, text_b);
}

fn isTsrxStyleElementName(parser: *const Parser, name: ast.NodeIndex) bool {
    if (!parser.tree.isTsrx()) return false;

    return switch (parser.tree.data(name)) {
        .jsx_identifier => |id| std.mem.eql(u8, parser.tree.string(id.name), "style"),
        else => false,
    };
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
        const text_token = parser.lexer.reScanJsxText(scan_from, parser.tree.isTsrx());

        if (text_token.len() > 0) {
            const text_node = try parser.tree.addNode(.{
                .jsx_text = .{
                    .value = try parseJsxTextValue(parser, text_token.span),
                },
            }, text_token.span);

            try parser.scratch_b.append(parser.allocator(), text_node);
        }

        // advance past jsx_text to get the delimiter token ('<' or '{')
        try parser.advanceWithRescannedToken(text_token) orelse return null;

        switch (parser.current_token.tag) {
            .less_than => {
                // check if it's a closing tag
                const next = parser.peekAhead() orelse return null;
                if (next.tag == .slash) break;

                // nested element
                const child = try parseJsxElement(parser, .child) orelse return null;
                scan_from = parser.tree.span(child).end;
                try parser.scratch_b.append(parser.allocator(), child);
            },
            .left_brace => {
                const child = try parseJsxChildFromLeftBrace(parser) orelse return null;
                scan_from = parser.tree.span(child).end;
                try parser.scratch_b.append(parser.allocator(), child);
            },
            .at => {
                const child = if (tsrx.isCodeBlockStart(parser))
                    try tsrx_template.parseCodeBlock(parser) orelse return null
                else if (tsrx.isControlFlowDirectiveStart(parser))
                    try tsrx_template.parseControlFlowExpression(parser) orelse return null
                else {
                    try parser.reportExpected(
                        parser.current_token.span,
                        "Expected TSRX template directive",
                        .{ .help = "TSRX template directives are '@{...}', '@if', '@for'," ++
                            " '@switch', or '@try'." },
                    );
                    return null;
                };
                scan_from = parser.tree.span(child).end;
                try parser.scratch_b.append(parser.allocator(), child);
            },
            else => break,
        }
    }

    return try parser.flushToExtras(&parser.scratch_b, checkpoint);
}

fn parseJsxTextValue(parser: *Parser, span: ast.Span) Error!ast.String {
    std.debug.assert(span.start <= span.end);
    std.debug.assert(span.end <= parser.source.len);

    const text = parser.spanText(span);
    if (std.mem.indexOfScalar(u8, text, '&') == null) {
        return parser.tree.sourceSlice(span.start, span.end);
    }

    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(parser.allocator());
    try out.ensureTotalCapacity(parser.allocator(), text.len);

    var index: u32 = 0;
    while (index < text.len) {
        if (text[index] != '&') {
            try out.append(parser.allocator(), text[index]);
            index += 1;
            continue;
        }

        const entity = parseJsxTextEntity(text[index..]) orelse {
            try out.append(parser.allocator(), text[index]);
            index += 1;
            continue;
        };

        if (entity.codepoint) |codepoint| {
            var bytes: [4]u8 = undefined;
            const len = std.unicode.utf8Encode(codepoint, &bytes) catch unreachable;
            try out.appendSlice(parser.allocator(), bytes[0..len]);
        } else {
            try out.appendSlice(parser.allocator(), entity.value);
        }
        index += entity.len;
    }

    return try parser.tree.addString(out.items);
}

const JsxTextEntity = struct {
    value: []const u8 = "",
    codepoint: ?u21 = null,
    len: u32,
};

fn parseJsxTextEntity(text: []const u8) ?JsxTextEntity {
    std.debug.assert(text.len > 0);
    std.debug.assert(text[0] == '&');

    if (text.len < 4) return null;

    if (std.mem.startsWith(u8, text, "&amp;")) {
        return .{ .value = "&", .len = 5 };
    }
    if (std.mem.startsWith(u8, text, "&lt;")) {
        return .{ .value = "<", .len = 4 };
    }
    if (std.mem.startsWith(u8, text, "&gt;")) {
        return .{ .value = ">", .len = 4 };
    }
    if (std.mem.startsWith(u8, text, "&quot;")) {
        return .{ .value = "\"", .len = 6 };
    }
    if (std.mem.startsWith(u8, text, "&apos;")) {
        return .{ .value = "'", .len = 6 };
    }
    if (std.mem.startsWith(u8, text, "&nbsp;")) {
        return .{ .value = "\u{00a0}", .len = 6 };
    }
    if (text[1] == '#') {
        return parseJsxNumericTextEntity(text);
    }

    return null;
}

fn parseJsxNumericTextEntity(text: []const u8) ?JsxTextEntity {
    std.debug.assert(text.len >= 2);
    std.debug.assert(text[0] == '&');
    std.debug.assert(text[1] == '#');

    var base: u8 = 10;
    var index: u32 = 2;
    if (index < text.len and (text[index] == 'x' or text[index] == 'X')) {
        base = 16;
        index += 1;
    }

    const digits_start = index;
    var codepoint: u32 = 0;
    while (index < text.len) : (index += 1) {
        const c = text[index];
        if (c == ';') break;
        const digit = std.fmt.charToDigit(c, base) catch return null;
        if (codepoint > (@as(u32, 0x10ffff) - digit) / base) return null;
        codepoint = codepoint * base + digit;
    }

    if (index == text.len) return null;
    if (text[index] != ';') return null;
    if (index == digits_start) return null;
    if (codepoint > 0x10ffff) return null;
    if (codepoint >= 0xd800 and codepoint <= 0xdfff) return null;

    return .{ .codepoint = @intCast(codepoint), .len = index + 1 };
}

fn parseJsxChildFromLeftBrace(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const start = parser.current_token.span.start;

    // already in normal mode from parseJsxChildren
    try parser.advance() orelse return null; // consume '{'

    if (parser.current_token.tag == .spread) {
        try parser.advance() orelse return null; // consume '...'

        const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
            return null;
        const end = try expectJsxChildRightBrace(parser, "JSX spread") orelse return null;

        return try parser.tree.addNode(
            .{ .jsx_spread_child = .{ .expression = expression } },
            .{ .start = start, .end = end },
        );
    }

    // empty expression: {}
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

    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
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
    // spread attribute: {...expr}
    if (parser.current_token.tag == .left_brace) {
        return parseJsxSpreadAttribute(parser);
    }

    // regular attribute: name or name=value
    const name = try parseJsxAttributeName(parser) orelse return null;
    const name_start = parser.tree.span(name).start;

    if (parser.current_token.tag != .assign) {
        // boolean attribute: <elem disabled />
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

    // check for namespaced name: ns:name
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
        // string literal: "value" or 'value'
        .string_literal => return literals.parseStringLiteral(parser),

        // expression: {expr}
        .left_brace => {
            const container = try parseJsxExpressionContainer(parser, .tag) orelse return null;

            // validate non-empty
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

        // nested JSX element: <elem />
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

const JsxExprContext = enum {
    /// inside a tag (attribute value), restore jsx_tag after '}'
    tag,
    /// inside children area, stay in normal mode after '}'
    child,
};

// parses {expr} in children context
fn parseJsxExpressionContainer(
    parser: *Parser,
    comptime context: JsxExprContext,
) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const start = parser.current_token.span.start;

    // switch to normal mode for JS expression parsing
    // (only matters when called from tag context, already normal in child context)
    if (context == .tag) {
        exitJsxTag(parser);
    }

    try parser.advance() orelse return null; // consume '{'

    // empty expression: {}
    if (parser.current_token.tag == .right_brace) {
        const end = parser.current_token.span.end;
        if (context == .tag) {
            enterJsxTag(parser);
        }
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

    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
        return null;
    const end = parser.current_token.span.end;

    // restore mode before consuming '}'
    if (context == .tag) {
        enterJsxTag(parser);
    }

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

// parses {...expr} as spread attribute
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

    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse
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
    if (parser.current_token.tag == .left_brace) {
        if (parser.tree.isTsrx()) {
            const name = try parseJsxExpressionContainer(parser, .tag) orelse return null;
            try tsrx_dynamic_tag.validateExpression(parser, name);
            return name;
        }

        try parser.reportExpected(
            parser.current_token.span,
            "Expected JSX element name",
            .{ .help = "Dynamic JSX tag names are only enabled in TSRX files" },
        );
        return null;
    }

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

    // member expression: Foo.Bar.Baz
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

    // namespaced name: ns:name (not allowed after member expression)
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
