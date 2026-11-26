const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;

const expressions = @import("../expressions.zig");
const ts = @import("../ts/types.zig");

const ElementContext = enum { top_level, child, attribute };

// https://react.github.io/jsx/#prod-JSXElement
pub fn parseJsxExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.at(.less_than));
    return parseJsxElement(parser, .top_level);
}

fn parseJsxElement(parser: *Parser, comptime context: ElementContext) Error!?ast.NodeIndex {
    try parser.descend();
    defer parser.ascend();
    const start = parser.current_token.span.start;

    // peek in jsx_tag mode so a fragment's `>` is not glued to what follows, as in `<>=</>`
    parser.lexer.setMode(.jsx_tag);
    const next = parser.peekAhead();
    parser.lexer.setMode(.normal);
    if (next.tag == .greater_than) return parseJsxFragment(parser, context);

    const opening = try parseJsxOpeningElement(parser, context) orelse return null;
    const opening_data = parser.tree.data(opening).jsx_opening_element;
    var children: ast.IndexRange = .empty;
    var closing: ast.NodeIndex = .null;
    if (!opening_data.self_closing) {
        children = try parseJsxChildren(parser, parser.tree.span(opening).end) orelse return null;
        closing = try parseJsxClosingElement(parser, opening_data.name, context) orelse
            return null;
    }
    return try parser.finishAt(start, if (closing != .null) closing else opening, .{
        .jsx_element = .{
            .opening_element = opening,
            .children = children,
            .closing_element = closing,
        },
    });
}

// https://react.github.io/jsx/#prod-JSXFragment
fn parseJsxFragment(parser: *Parser, comptime context: ElementContext) Error!?ast.NodeIndex {
    parser.lexer.setMode(.jsx_tag);
    const start = try parser.take(.less_than) orelse return null;
    // parseJsxElement saw the `>`, and parseJsxChildren rescans from it
    std.debug.assert(parser.at(.greater_than));
    const opening_end = parser.current_token.span.end;
    const opening = try parser.tree.addNode(
        .{ .jsx_opening_fragment = .{} },
        .{ .start = start, .end = opening_end },
    );
    const children = try parseJsxChildren(parser, opening_end) orelse return null;

    const closing_start = parser.current_token.span.start;
    parser.lexer.setMode(.jsx_tag);
    try parser.advance() orelse return null;
    const message = "Expected corresponding closing tag for JSX fragment";
    const help = "Close the fragment with '</>'";
    if (!try parser.expect(.slash, message, help)) return null;
    const closing_end = try endJsxTag(parser, message, help) orelse return null;
    try exitJsxElement(parser, context) orelse return null;

    const closing = try parser.tree.addNode(
        .{ .jsx_closing_fragment = .{} },
        .{ .start = closing_start, .end = closing_end },
    );
    return try parser.finishAt(start, closing, .{ .jsx_fragment = .{
        .opening_fragment = opening,
        .children = children,
        .closing_fragment = closing,
    } });
}

// https://react.github.io/jsx/#prod-JSXSelfClosingElement
// https://react.github.io/jsx/#prod-JSXOpeningElement
fn parseJsxOpeningElement(
    parser: *Parser,
    comptime context: ElementContext,
) Error!?ast.NodeIndex {
    parser.lexer.setMode(.jsx_tag);
    const start = try parser.take(.less_than) orelse return null;
    const name = try parseJsxName(parser, .element) orelse return null;

    var type_arguments: ast.NodeIndex = .null;
    if (parser.tree.isTs() and ts.isAngleOpen(parser.current_token.tag)) {
        parser.lexer.setMode(.normal);
        type_arguments = try ts.parseTypeArguments(parser);
        parser.lexer.setMode(.jsx_tag);
        try parser.reScanCurrent() orelse return null;
    }

    const attributes = try parseJsxAttributes(parser) orelse return null;
    const self_closing = try parser.eat(.slash) orelse return null;
    const end = try endJsxTag(parser, "Expected '>' to end the JSX opening tag", null) orelse
        return null;
    if (self_closing) try exitJsxElement(parser, context) orelse return null;

    return try parser.tree.addNode(.{ .jsx_opening_element = .{
        .name = name,
        .attributes = attributes,
        .self_closing = self_closing,
        .type_arguments = type_arguments,
    } }, .{ .start = start, .end = end });
}

// https://react.github.io/jsx/#prod-JSXClosingElement
fn parseJsxClosingElement(
    parser: *Parser,
    opening_name: ast.NodeIndex,
    comptime context: ElementContext,
) Error!?ast.NodeIndex {
    if (!parser.at(.less_than)) {
        const opening_span = parser.tree.span(opening_name);
        const opening_text = parser.spanText(opening_span);
        return parser.fail(
            parser.current_token.span,
            try parser.fmt("JSX element '{s}' has no corresponding closing tag", .{opening_text}),
            .{
                .help = try parser.fmt("Add '</{s}>' to close the element", .{opening_text}),
                .label = .{ .span = opening_span, .message = "opening tag" },
            },
        );
    }

    parser.lexer.setMode(.jsx_tag);
    const start = try parser.take(.less_than) orelse return null;
    if (!try parser.expect(.slash, "Expected '/' in the JSX closing tag", null)) return null;
    const name = try parseJsxName(parser, .element) orelse return null;
    const end = try endJsxTag(parser, "Expected '>' to end the JSX closing tag", null) orelse
        return null;
    try exitJsxElement(parser, context) orelse return null;

    if (!jsxNamesMatch(&parser.tree, opening_name, name)) {
        const opening_span = parser.tree.span(opening_name);
        const opening_text = parser.spanText(opening_span);
        const closing_span = parser.tree.span(name);
        try parser.report(closing_span, try parser.fmt(
            "Expected corresponding JSX closing tag for '<{s}>', but found '</{s}>'",
            .{ opening_text, parser.spanText(closing_span) },
        ), .{
            .help = try parser.fmt("Rename the closing tag to '</{s}>'", .{opening_text}),
            .label = .{ .span = opening_span, .message = "opening tag" },
        });
    }
    return try parser.tree.addNode(
        .{ .jsx_closing_element = .{ .name = name } },
        .{ .start = start, .end = end },
    );
}

fn endJsxTag(parser: *Parser, message: []const u8, help: ?[]const u8) Error!?u32 {
    if (!parser.at(.greater_than)) {
        try parser.reportExpected(message, .{ .help = help });
        return null;
    }
    return parser.current_token.span.end;
}

// a child leaves the final `>` for parseJsxChildren to rescan
fn exitJsxElement(parser: *Parser, comptime context: ElementContext) Error!?void {
    std.debug.assert(parser.at(.greater_than));
    parser.lexer.setMode(if (context == .attribute) .jsx_tag else .normal);
    if (context != .child) try parser.advance() orelse return null;
}

fn jsxNamesMatch(tree: *const ast.Tree, opening: ast.NodeIndex, closing: ast.NodeIndex) bool {
    var a = opening;
    var b = closing;
    while (true) {
        const x = tree.data(a);
        const y = tree.data(b);
        if (std.meta.activeTag(x) != std.meta.activeTag(y)) return false;
        switch (x) {
            .jsx_identifier => |identifier| return std.mem.eql(
                u8,
                tree.string(identifier.name),
                tree.string(y.jsx_identifier.name),
            ),
            .jsx_namespaced_name => |name| {
                return jsxNamesMatch(tree, name.namespace, y.jsx_namespaced_name.namespace) and
                    jsxNamesMatch(tree, name.name, y.jsx_namespaced_name.name);
            },
            .jsx_member_expression => |member| {
                const other = y.jsx_member_expression;
                if (!jsxNamesMatch(tree, member.property, other.property)) return false;
                a = member.object;
                b = other.object;
            },
            else => unreachable,
        }
    }
}

// https://react.github.io/jsx/#prod-JSXChildren
fn parseJsxChildren(parser: *Parser, opening_end: u32) Error!?ast.IndexRange {
    const children = parser.list();
    defer children.deinit();

    parser.lexer.setMode(.normal);
    var scan_from = opening_end;
    while (true) {
        const text = parser.lexer.reScanJsxText(scan_from);
        if (text.len() > 0) try children.append(try parser.tree.addNode(.{ .jsx_text = .{
            .value = try parser.jsxValue(text.span.start, text.span.end),
            .raw = parser.tree.sourceSlice(text.span.start, text.span.end),
        } }, text.span));
        try parser.advanceWithRescannedToken(text) orelse return null;

        const child = switch (parser.current_token.tag) {
            .less_than => if (parser.peekAhead().tag == .slash)
                break
            else
                try parseJsxElement(parser, .child) orelse return null,
            .left_brace => try parseJsxBraces(parser, .child) orelse return null,
            inline .greater_than, .right_brace => |tag| {
                const char, const entity = if (tag == .greater_than)
                    .{ ">", "&gt;" }
                else
                    .{ "}", "&rbrace;" };
                try parser.report(
                    parser.current_token.span,
                    "Unexpected token '" ++ char ++ "' in JSX text",
                    .{ .help = "Write {'" ++ char ++ "'} or '" ++ entity ++ "' instead" },
                );
                scan_from = parser.current_token.span.end;
                continue;
            },
            else => break,
        };
        scan_from = parser.tree.span(child).end;
        try children.append(child);
    }

    return try children.finish();
}

fn parseJsxBraces(
    parser: *Parser,
    comptime place: enum { child, attribute, attribute_value },
) Error!?ast.NodeIndex {
    const in_tag = place != .child;
    if (in_tag) parser.lexer.setMode(.normal);
    const start = try parser.take(.left_brace) orelse return null;

    if (place == .attribute and !try parser.expect(
        .spread,
        "Expected '...' after '{' in the JSX attributes",
        "Write a spread attribute as '{...props}'",
    )) return null;
    const is_spread = place == .attribute or
        (place == .child and (try parser.eat(.spread) orelse return null));

    const expression: ast.NodeIndex = if (!is_spread and parser.at(.right_brace))
        .null
    else
        try expressions.parseAssignment(parser) orelse return null;

    const span: ast.Span = .{ .start = start, .end = parser.current_token.span.end };
    if (in_tag) parser.lexer.setMode(.jsx_tag);
    if (!parser.at(.right_brace)) return parser.failExpected(
        if (!is_spread)
            "Expected '}' to close the JSX expression"
        else if (place == .child)
            "Expected '}' to close the JSX spread child"
        else
            "Expected '}' to close the JSX spread attribute",
        .{},
    );
    if (in_tag) try parser.advance() orelse return null;

    if (is_spread) return try parser.tree.addNode(if (place == .child)
        .{ .jsx_spread_child = .{ .expression = expression } }
    else
        .{ .jsx_spread_attribute = .{ .argument = expression } }, span);
    return try parser.tree.addNode(.{ .jsx_expression_container = .{
        .expression = if (expression != .null) expression else try parser.tree.addNode(
            .{ .jsx_empty_expression = .{} },
            .{ .start = span.start + 1, .end = span.end - 1 },
        ),
    } }, span);
}

// https://react.github.io/jsx/#prod-JSXAttributes
fn parseJsxAttributes(parser: *Parser) Error!?ast.IndexRange {
    const attributes = parser.list();
    defer attributes.deinit();
    while (parser.at(.jsx_identifier) or parser.at(.left_brace)) {
        try attributes.append(try parseJsxAttribute(parser) orelse return null);
    }
    return try attributes.finish();
}

// https://react.github.io/jsx/#prod-JSXAttribute
fn parseJsxAttribute(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.at(.left_brace)) return parseJsxBraces(parser, .attribute);

    const name = try parseJsxName(parser, .attribute) orelse return null;
    const value: ast.NodeIndex = if (try parser.eat(.assign) orelse return null)
        try parseJsxAttributeValue(parser) orelse return null
    else
        .null;
    return try parser.finishAt(
        parser.tree.span(name).start,
        if (value != .null) value else name,
        .{ .jsx_attribute = .{ .name = name, .value = value } },
    );
}

// https://react.github.io/jsx/#prod-JSXAttributeValue
fn parseJsxAttributeValue(parser: *Parser) Error!?ast.NodeIndex {
    switch (parser.current_token.tag) {
        .string_literal => {
            const span = parser.current_token.span;
            try parser.advance() orelse return null;
            return try parser.tree.addNode(.{ .string_literal = .{
                .value = try parser.jsxValue(span.start + 1, span.end - 1),
                .raw = parser.tree.sourceSlice(span.start, span.end),
            } }, span);
        },
        .left_brace => {
            const container = try parseJsxBraces(parser, .attribute_value) orelse return null;
            const expression = parser.tree.data(container).jsx_expression_container.expression;
            if (parser.tree.data(expression) == .jsx_empty_expression) return parser.fail(
                parser.tree.span(container),
                "A JSX attribute value cannot be an empty expression",
                .{ .help = "Add an expression inside the braces, or use a string" },
            );
            return container;
        },
        .less_than => return parseJsxElement(parser, .attribute),
        else => return parser.failExpected(
            "Expected an attribute value",
            .{ .help = "Use a string, an expression in braces, or a JSX element" },
        ),
    }
}

// https://react.github.io/jsx/#prod-JSXElementName
// https://react.github.io/jsx/#prod-JSXAttributeName
fn parseJsxName(
    parser: *Parser,
    comptime owner: enum { element, attribute },
) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var name = try parseJsxIdentifier(
        parser,
        "Expected a JSX element name",
        "Start the tag name with a letter, '_', or '$'",
    ) orelse return null;

    if (parser.at(.colon)) {
        try parser.advance() orelse return null;
        const local = try parseJsxIdentifier(
            parser,
            "Expected an identifier after ':' in the " ++ @tagName(owner) ++ " name",
            "Write a namespaced name as 'namespace:name'",
        ) orelse return null;
        return try parser.finishAt(start, local, .{
            .jsx_namespaced_name = .{ .namespace = name, .name = local },
        });
    }
    if (owner == .element) while (parser.at(.dot)) {
        try parser.advance() orelse return null;
        const property = try parseJsxIdentifier(
            parser,
            "Expected an identifier after '.' in the element name",
            "Write a member name as 'object.property'",
        ) orelse return null;
        name = try parser.finishAt(start, property, .{
            .jsx_member_expression = .{ .object = name, .property = property },
        });
    };
    return name;
}

fn parseJsxIdentifier(
    parser: *Parser,
    comptime message: []const u8,
    comptime help: []const u8,
) Error!?ast.NodeIndex {
    const token = parser.current_token;
    if (token.tag != .jsx_identifier) return parser.failExpected(message, .{ .help = help });

    const identifier = try parser.tree.addNode(
        .{ .jsx_identifier = .{ .name = try parser.identifierName(token) } },
        token.span,
    );
    try parser.advance() orelse return null;
    return identifier;
}
