const std = @import("std");
const ast = @import("../../ast.zig");
const Precedence = @import("../../token.zig").Precedence;
const TokenTag = @import("../../token.zig").TokenTag;
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;

const expressions = @import("../expressions.zig");
const ts = @import("../ts/types.zig");

pub const JsxElementContext = enum { top_level, child, attribute };

// a child leaves the final `>` for parseJsxChildren to rescan
fn exitJsxElement(parser: *Parser, comptime context: JsxElementContext) Error!?void {
    std.debug.assert(parser.current_token.tag == .greater_than);
    parser.setLexerMode(if (context == .attribute) .jsx_tag else .normal);
    if (context != .child) try parser.advance() orelse return null;
}

// https://react.github.io/jsx/#prod-JSXElement
pub fn parseJsxExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .less_than);
    return parseJsxElement(parser, .top_level);
}

fn parseJsxElement(parser: *Parser, comptime context: JsxElementContext) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    // peek in jsx_tag mode so a fragment's `>` is not glued to what follows, as in `<>=</>`
    parser.setLexerMode(.jsx_tag);
    const next = parser.peekAhead();
    parser.setLexerMode(.normal);

    if (next.tag == .greater_than) return parseJsxFragment(parser, context);

    const opening = try parseJsxOpeningElement(parser, context) orelse return null;
    const opening_data = parser.tree.data(opening).jsx_opening_element;
    var end = parser.tree.span(opening).end;
    var children: ast.IndexRange = .empty;
    var closing: ast.NodeIndex = .null;
    if (!opening_data.self_closing) {
        children = try parseJsxChildren(parser, end) orelse return null;
        closing = try parseJsxClosingElement(parser, opening_data.name, context) orelse
            return null;
        end = parser.tree.span(closing).end;
    }

    return try parser.tree.addNode(.{ .jsx_element = .{
        .opening_element = opening,
        .children = children,
        .closing_element = closing,
    } }, .{ .start = start, .end = end });
}

// https://react.github.io/jsx/#prod-JSXFragment
fn parseJsxFragment(parser: *Parser, comptime context: JsxElementContext) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    parser.setLexerMode(.jsx_tag);
    try parser.advance() orelse return null;
    const opening_end = try expectJsxToken(
        parser,
        .greater_than,
        "Expected '>' to end the opening fragment tag",
        null,
    ) orelse return null;
    const opening = try parser.tree.addNode(
        .{ .jsx_opening_fragment = .{} },
        .{ .start = start, .end = opening_end },
    );
    // parseJsxChildren rescans from the `>`
    const children = try parseJsxChildren(parser, opening_end) orelse return null;

    const closing_start = parser.current_token.span.start;
    parser.setLexerMode(.jsx_tag);
    try parser.advance() orelse return null;

    if (!try parser.expect(
        .slash,
        "Expected corresponding closing tag for JSX fragment",
        "Close the fragment with '</>'",
    )) return null;

    const closing_end = try expectJsxToken(
        parser,
        .greater_than,
        "Expected corresponding closing tag for JSX fragment",
        "Close the fragment with '</>'",
    ) orelse return null;
    try exitJsxElement(parser, context) orelse return null;

    const closing = try parser.tree.addNode(
        .{ .jsx_closing_fragment = .{} },
        .{ .start = closing_start, .end = closing_end },
    );

    return try parser.tree.addNode(.{ .jsx_fragment = .{
        .opening_fragment = opening,
        .children = children,
        .closing_fragment = closing,
    } }, .{ .start = start, .end = closing_end });
}

// https://react.github.io/jsx/#prod-JSXSelfClosingElement
// https://react.github.io/jsx/#prod-JSXOpeningElement
fn parseJsxOpeningElement(
    parser: *Parser,
    comptime context: JsxElementContext,
) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .less_than);
    const start = parser.current_token.span.start;

    parser.setLexerMode(.jsx_tag);
    try parser.advance() orelse return null;

    const name = try parseJsxElementName(parser) orelse return null;

    var type_arguments: ast.NodeIndex = .null;
    if (parser.tree.isTs() and ts.isAngleOpen(parser.current_token.tag)) {
        parser.setLexerMode(.normal);
        type_arguments = try ts.parseTypeArguments(parser);
        parser.setLexerMode(.jsx_tag);
        try parser.reScanCurrent() orelse return null;
    }

    const attributes = try parseJsxAttributes(parser) orelse return null;

    const self_closing = parser.current_token.tag == .slash;
    if (self_closing) try parser.advance() orelse return null;

    const end = try expectJsxToken(
        parser,
        .greater_than,
        "Expected '>' to end the JSX opening tag",
        null,
    ) orelse return null;
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
    comptime context: JsxElementContext,
) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (parser.current_token.tag != .less_than) {
        try parser.report(
            parser.current_token.span,
            try parser.fmt(
                "JSX element '{s}' has no corresponding closing tag",
                .{parser.spanText(parser.tree.span(opening_name))},
            ),
            .{
                .help = try parser.fmt("Add '</{s}>' to close the element", .{
                    parser.spanText(parser.tree.span(opening_name)),
                }),
                .labels = try parser.labels(&.{
                    parser.label(parser.tree.span(opening_name), "opening tag"),
                }),
            },
        );
        return null;
    }
    parser.setLexerMode(.jsx_tag);
    try parser.advance() orelse return null;

    if (!try parser.expect(
        .slash,
        "Expected '/' in the JSX closing tag",
        null,
    )) return null;

    const name = try parseJsxElementName(parser) orelse return null;

    const end = try expectJsxToken(
        parser,
        .greater_than,
        "Expected '>' to end the JSX closing tag",
        null,
    ) orelse return null;
    try exitJsxElement(parser, context) orelse return null;

    if (!jsxNamesMatch(parser, opening_name, name)) {
        const opening_span = parser.tree.span(opening_name);
        const closing_span = parser.tree.span(name);

        try parser.report(closing_span, try parser.fmt(
            "Expected corresponding JSX closing tag for '<{s}>', but found '</{s}>'",
            .{ parser.spanText(opening_span), parser.spanText(closing_span) },
        ), .{
            .help = try parser.fmt("Rename the closing tag to '</{s}>'", .{
                parser.spanText(opening_span),
            }),
            .labels = try parser.labels(&.{parser.label(opening_span, "opening tag")}),
        });
    }

    return try parser.tree.addNode(
        .{ .jsx_closing_element = .{ .name = name } },
        .{ .start = start, .end = end },
    );
}

fn jsxNamesMatch(parser: *const Parser, opening: ast.NodeIndex, closing: ast.NodeIndex) bool {
    const tree = &parser.tree;
    var a = opening;
    var b = closing;
    while (true) switch (tree.data(a)) {
        .jsx_identifier => return jsxIdentifiersMatch(tree, a, b),
        .jsx_namespaced_name => |x| {
            if (tree.data(b) != .jsx_namespaced_name) return false;
            const y = tree.data(b).jsx_namespaced_name;
            return jsxIdentifiersMatch(tree, x.namespace, y.namespace) and
                jsxIdentifiersMatch(tree, x.name, y.name);
        },
        .jsx_member_expression => |x| {
            if (tree.data(b) != .jsx_member_expression) return false;
            const y = tree.data(b).jsx_member_expression;
            if (!jsxIdentifiersMatch(tree, x.property, y.property)) return false;
            a = x.object;
            b = y.object;
        },
        else => return std.mem.eql(
            u8,
            parser.spanText(tree.span(a)),
            parser.spanText(tree.span(b)),
        ),
    };
}

fn jsxIdentifiersMatch(tree: *const ast.Tree, a: ast.NodeIndex, b: ast.NodeIndex) bool {
    if (tree.data(a) != .jsx_identifier or tree.data(b) != .jsx_identifier) return false;
    const name_a = tree.string(tree.data(a).jsx_identifier.name);
    const name_b = tree.string(tree.data(b).jsx_identifier.name);
    return std.mem.eql(u8, name_a, name_b);
}

// https://react.github.io/jsx/#prod-JSXChildren
fn parseJsxChildren(parser: *Parser, opening_end: u32) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(checkpoint);

    parser.setLexerMode(.normal);
    var scan_from = opening_end;

    while (true) {
        const text_token = parser.lexer.reScanJsxText(scan_from);

        if (text_token.len() > 0) {
            const span = text_token.span;
            const text_node = try parser.tree.addNode(.{ .jsx_text = .{
                .value = try parser.jsxValue(span.start, span.end),
                .raw = parser.tree.sourceSlice(span.start, span.end),
            } }, span);
            try parser.scratch_b.append(parser.allocator(), text_node);
        }

        try parser.advanceWithRescannedToken(text_token) orelse return null;

        const child = switch (parser.current_token.tag) {
            .less_than => if (parser.peekAhead().tag == .slash)
                break
            else
                try parseJsxElement(parser, .child) orelse return null,
            .left_brace => try parseJsxChildFromLeftBrace(parser) orelse return null,
            .greater_than, .right_brace => {
                try parser.report(
                    parser.current_token.span,
                    if (parser.current_token.tag == .greater_than)
                        "Unexpected token '>' in JSX text"
                    else
                        "Unexpected token '}' in JSX text",
                    .{ .help = if (parser.current_token.tag == .greater_than)
                        "Write {'>'} or '&gt;' instead"
                    else
                        "Write {'}'} or '&rbrace;' instead" },
                );
                scan_from = parser.current_token.span.end;
                continue;
            },
            else => break,
        };
        scan_from = parser.tree.span(child).end;
        try parser.scratch_b.append(parser.allocator(), child);
    }

    return try parser.flushToExtras(&parser.scratch_b, checkpoint);
}

fn parseJsxChildFromLeftBrace(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    if (parser.current_token.tag == .spread) {
        try parser.advance() orelse return null;
        const expression = try expressions.parseExpression(
            parser,
            Precedence.Assignment,
            .{},
        ) orelse return null;
        const end = try expectJsxToken(
            parser,
            .right_brace,
            "Expected '}' to close the JSX spread child",
            null,
        ) orelse return null;

        return try parser.tree.addNode(
            .{ .jsx_spread_child = .{ .expression = expression } },
            .{ .start = start, .end = end },
        );
    }

    if (parser.current_token.tag == .right_brace) {
        return try addEmptyExpressionContainer(parser, start, parser.current_token.span.end);
    }

    const expression = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
        return null;
    const end = try expectJsxToken(
        parser,
        .right_brace,
        "Expected '}' to close the JSX expression",
        null,
    ) orelse return null;

    return try parser.tree.addNode(
        .{ .jsx_expression_container = .{ .expression = expression } },
        .{ .start = start, .end = end },
    );
}

// checks the current token without consuming it and returns its end
fn expectJsxToken(
    parser: *Parser,
    comptime tag: TokenTag,
    message: []const u8,
    help: ?[]const u8,
) Error!?u32 {
    if (parser.current_token.tag != tag) {
        try parser.reportExpected(parser.current_token.span, message, .{ .help = help });
        return null;
    }
    return parser.current_token.span.end;
}

fn addEmptyExpressionContainer(parser: *Parser, start: u32, end: u32) Error!ast.NodeIndex {
    const empty = try parser.tree.addNode(
        .{ .jsx_empty_expression = .{} },
        .{ .start = start + 1, .end = end - 1 },
    );
    return parser.tree.addNode(
        .{ .jsx_expression_container = .{ .expression = empty } },
        .{ .start = start, .end = end },
    );
}

// https://react.github.io/jsx/#prod-JSXAttributes
fn parseJsxAttributes(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag == .jsx_identifier or parser.current_token.tag == .left_brace) {
        const attribute = try parseJsxAttribute(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), attribute);
    }

    return try parser.flushToExtras(&parser.scratch_a, checkpoint);
}

// https://react.github.io/jsx/#prod-JSXAttribute
fn parseJsxAttribute(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .left_brace) {
        return parseJsxSpreadAttribute(parser);
    }

    const name = try parseJsxAttributeName(parser) orelse return null;
    var value: ast.NodeIndex = .null;
    if (parser.current_token.tag == .assign) {
        try parser.advance() orelse return null;
        value = try parseJsxAttributeValue(parser) orelse return null;
    }

    return try parser.tree.addNode(.{ .jsx_attribute = .{ .name = name, .value = value } }, .{
        .start = parser.tree.span(name).start,
        .end = parser.tree.span(if (value == .null) name else value).end,
    });
}

// https://react.github.io/jsx/#prod-JSXAttributeName
fn parseJsxAttributeName(parser: *Parser) Error!?ast.NodeIndex {
    const name = try parseJsxIdentifier(parser) orelse return null;
    if (parser.current_token.tag != .colon) return name;

    return parseJsxNamespacedName(
        parser,
        name,
        "Expected an identifier after ':' in the attribute name",
        "Write a namespaced name as 'namespace:name'",
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
            const container = try parseJsxExpressionContainer(parser) orelse return null;
            const expr = parser.tree.data(container).jsx_expression_container.expression;
            if (parser.tree.data(expr) == .jsx_empty_expression) {
                try parser.report(
                    parser.tree.span(container),
                    "A JSX attribute value cannot be an empty expression",
                    .{ .help = "Add an expression inside the braces, or use a string" },
                );
                return null;
            }

            return container;
        },
        .less_than => return parseJsxElement(parser, .attribute),
        else => {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected an attribute value",
                .{ .help = "Use a string, an expression in braces, or a JSX element" },
            );
            return null;
        },
    }
}

fn parseJsxExpressionContainer(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const start = parser.current_token.span.start;
    parser.setLexerMode(.normal);
    try parser.advance() orelse return null;

    if (parser.current_token.tag == .right_brace) {
        const end = parser.current_token.span.end;
        parser.setLexerMode(.jsx_tag);
        try parser.advance() orelse return null;
        return try addEmptyExpressionContainer(parser, start, end);
    }

    const expression = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
        return null;
    const end = parser.current_token.span.end;

    // restore jsx_tag before `}` so the next attribute is scanned in tag mode
    parser.setLexerMode(.jsx_tag);

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close the JSX expression",
        null,
    )) return null;

    return try parser.tree.addNode(
        .{ .jsx_expression_container = .{ .expression = expression } },
        .{ .start = start, .end = end },
    );
}

fn parseJsxSpreadAttribute(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const start = parser.current_token.span.start;
    parser.setLexerMode(.normal);
    try parser.advance() orelse return null;

    if (!try parser.expect(
        .spread,
        "Expected '...' after '{' in the JSX attributes",
        "Write a spread attribute as '{...props}'",
    )) return null;

    const expression = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
        return null;
    const end = parser.current_token.span.end;
    parser.setLexerMode(.jsx_tag);

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close the JSX spread attribute",
        null,
    )) return null;

    return try parser.tree.addNode(
        .{ .jsx_spread_attribute = .{ .argument = expression } },
        .{ .start = start, .end = end },
    );
}

// https://react.github.io/jsx/#prod-JSXElementName
fn parseJsxElementName(parser: *Parser) Error!?ast.NodeIndex {
    _ = try expectJsxToken(
        parser,
        .jsx_identifier,
        "Expected a JSX element name",
        "Start the tag name with a letter, '_', or '$'",
    ) orelse return null;

    const start = parser.current_token.span.start;
    var name = try parseJsxIdentifier(parser) orelse return null;

    if (parser.current_token.tag == .colon) {
        return parseJsxNamespacedName(
            parser,
            name,
            "Expected an identifier after ':' in the element name",
            "Write a namespaced name as 'namespace:name'",
        );
    }

    while (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null;
        _ = try expectJsxToken(
            parser,
            .jsx_identifier,
            "Expected an identifier after '.' in the element name",
            "Write a member name as 'object.property'",
        ) orelse return null;
        const property = try parseJsxIdentifier(parser) orelse return null;
        name = try parser.tree.addNode(.{
            .jsx_member_expression = .{ .object = name, .property = property },
        }, .{ .start = start, .end = parser.tree.span(property).end });
    }

    return name;
}

fn parseJsxNamespacedName(
    parser: *Parser,
    namespace: ast.NodeIndex,
    comptime message: []const u8,
    comptime help: []const u8,
) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .colon);
    try parser.advance() orelse return null;
    _ = try expectJsxToken(parser, .jsx_identifier, message, help) orelse return null;
    const name = try parseJsxIdentifier(parser) orelse return null;
    return try parser.tree.addNode(.{
        .jsx_namespaced_name = .{ .namespace = namespace, .name = name },
    }, .{ .start = parser.tree.span(namespace).start, .end = parser.tree.span(name).end });
}

fn parseJsxIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .jsx_identifier);
    const token = parser.current_token;
    const identifier = try parser.tree.addNode(
        .{ .jsx_identifier = .{ .name = try parser.identifierName(token) } },
        token.span,
    );
    try parser.advance() orelse return null;
    return identifier;
}
