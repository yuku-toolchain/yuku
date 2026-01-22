const std = @import("std");
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const literals = @import("literals.zig");
const expressions = @import("expressions.zig");

// https://facebook.github.io/jsx/#prod-JSXElement
pub fn parseJsxExpression(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const next = try parser.lookAhead() orelse return null;

    if (next.type == .greater_than) {
        // it's a fragment
        return parseJsxFragment(parser);
    }

    // otherwise it's a regular element
    const opening_element = try parseJsxOpeningElement(parser) orelse return null;

    const opening_element_end = parser.getSpan(opening_element).end;

    var children = ast.IndexRange.empty;
    var closing_element = ast.null_node;

    if (!parser.getData(opening_element).jsx_opening_element.self_closing) {
        children = try parseJsxChildren(parser, opening_element_end) orelse return null;
        closing_element = try parseJsxClosingElement(parser) orelse return null;
    }

    const end = if (!ast.isNull(closing_element)) parser.getSpan(closing_element).end else opening_element_end;

    return try parser.addNode(.{ .jsx_element = .{ .opening_element = opening_element, .children = children, .closing_element = closing_element } }, .{ .start = start, .end = end });
}

// https://facebook.github.io/jsx/#prod-JSXFragment
pub fn parseJsxFragment(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const opening_fragment = try parseJsxOpeningFragment(parser) orelse return null;

    const opening_fragment_end = parser.getSpan(opening_fragment).end;

    const children = try parseJsxChildren(parser, opening_fragment_end) orelse return null;

    const closing_fragment = try parseJsxClosingFragment(parser) orelse return null;

    const end = parser.getSpan(closing_fragment).end;

    return try parser.addNode(.{ .jsx_fragment = .{ .opening_fragment = opening_fragment, .children = children, .closing_fragment = closing_fragment } }, .{ .start = start, .end = end });
}

// https://facebook.github.io/jsx/#prod-JSXOpeningFragment
pub fn parseJsxOpeningFragment(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '<'

    const end = parser.current_token.span.end;

    if (!try advanceIntoJsxContent(parser, "Expected '>' to close JSX opening fragment", "Add '>' to complete the fragment opening tag")) return null;

    return try parser.addNode(.{ .jsx_opening_fragment = .{} }, .{ .start = start, .end = end });
}

// https://facebook.github.io/jsx/#prod-JSXClosingFragment
pub fn parseJsxClosingFragment(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '<'

    if (!try parser.expect(.slash, "Expected '/' in JSX closing fragment", "Add '/' to close the fragment")) return null;

    const end = parser.current_token.span.end;

    if (!try parser.expect(.greater_than, "Expected '>' to close JSX closing fragment", "Add '>' to complete the fragment closing tag")) return null;

    return try parser.addNode(.{ .jsx_closing_fragment = .{} }, .{ .start = start, .end = end });
}

// https://facebook.github.io/jsx/#prod-JSXSelfClosingElement
// https://facebook.github.io/jsx/#prod-JSXOpeningElement
pub fn parseJsxOpeningElement(parser: *Parser) Error!?ast.NodeIndex {
    parser.setLexerMode(.jsx_tag);

    const start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '<'

    var self_closing = false;

    const name = try parseJsxElementName(parser) orelse return null;

    const attributes = try parseJsxAttributes(parser) orelse return null;

    if (parser.current_token.type == .slash) {
        self_closing = true;
        try parser.advance() orelse return null;
    }

    const end = parser.current_token.span.end;

    if (!try advanceIntoJsxContent(parser, "Expected '>' to close JSX opening element", "Add '>' to close the JSX tag")) return null;

    return try parser.addNode(.{
        .jsx_opening_element = .{
            .name = name,
            .attributes = attributes,
            .self_closing = self_closing,
        },
    }, .{ .start = start, .end = end });
}

/// advances past '>' and into jsx content, ensuring the next token is lexed as text
fn advanceIntoJsxContent(parser: *Parser, err_msg: []const u8, help: []const u8) Error!bool {
    parser.setLexerMode(.jsx_text);
    defer parser.setLexerMode(.normal);
    return parser.expect(.greater_than, err_msg, help);
}

// https://facebook.github.io/jsx/#prod-JSXClosingElement
pub fn parseJsxClosingElement(parser: *Parser) Error!?ast.NodeIndex {
    parser.setLexerMode(.jsx_tag);

    const start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '<'

    if (!try parser.expect(.slash, "Expected '/' in JSX closing element", "Add '/' after '<' to close the element")) return null;

    const name = try parseJsxElementName(parser) orelse return null;

    const end = parser.current_token.span.end;

    parser.setLexerMode(.normal);

    if (!try parser.expect(.greater_than, "Expected '>' to close JSX closing element", "Add '>' to complete the closing tag")) return null;

    return try parser.addNode(.{
        .jsx_closing_element = .{
            .name = name,
        },
    }, .{ .start = start, .end = end });
}

// https://facebook.github.io/jsx/#prod-JSXChildren
pub fn parseJsxChildren(
    parser: *Parser,
    start: u32, // byte position right after '>' (no whitespace skipped)
) Error!?ast.IndexRange {
    const children_checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(children_checkpoint);

    // tracks where to start next text scan. updated after each child to preserve all text/whitespace.
    // this prevents missing text between children since normal tokenization skips whitespace.
    var scanJsxTextFrom = start;

    while (parser.current_token.type != .eof) {
        // manually scan jsx text from tracked position (not from current token position)
        const jsx_text_token = parser.lexer.scanJsxText(scanJsxTextFrom);

        if (jsx_text_token.lexeme.len > 0) {
            const jsx_text = try parser.addNode(.{ .jsx_text = .{ .raw_start = jsx_text_token.span.start, .raw_len = @intCast(jsx_text_token.lexeme.len) } }, jsx_text_token.span);

            try parser.scratch_b.append(parser.allocator(), jsx_text);
        }

        try parser.replaceTokenAndAdvance(jsx_text_token) orelse return null;

        switch (parser.current_token.type) {
            .less_than => {
                const next = try parser.lookAhead() orelse return null;

                if (next.type == .slash) {
                    break; // closing element
                }

                const jsx_expression = try parseJsxExpression(parser) orelse return null;

                scanJsxTextFrom = parser.getSpan(jsx_expression).end; //  scan from end of element

                try parser.scratch_b.append(parser.allocator(), jsx_expression);
            },
            .left_brace => {
                const next = try parser.lookAhead() orelse return null;

                if (next.type == .spread) {
                    const jsx_spread_child = try parseJsxSpread(parser, .child) orelse return null;

                    scanJsxTextFrom = parser.getSpan(jsx_spread_child).end; // scan from end of spread

                    try parser.scratch_b.append(parser.allocator(), jsx_spread_child);

                    continue;
                }

                const expression_container = try parseJsxExpressionContainer(parser, .child) orelse return null;

                scanJsxTextFrom = parser.getSpan(expression_container).end; // scan from end of expression

                try parser.scratch_b.append(parser.allocator(), expression_container);
            },
            else => break,
        }
    }

    return try parser.addExtra(parser.scratch_b.take(children_checkpoint));
}

const SpreadContext = enum { tag, child };

pub fn parseJsxSpread(parser: *Parser, context: SpreadContext) Error!?ast.NodeIndex {
    parser.setLexerMode(.normal);

    const start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '{'

    if (!try parser.expect(.spread, "Expected '...' after '{' in JSX spread", "Add '...' to spread the expression")) return null;

    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    const end = parser.current_token.span.end;

    parser.setLexerMode(if (context == .tag) .jsx_tag else .normal);

    if (!try parser.expect(.right_brace, "Expected '}' to close JSX spread", "Add '}' to close the spread expression")) return null;

    const span = ast.Span{ .start = start, .end = end };

    if (context == .child) {
        return try parser.addNode(.{ .jsx_spread_child = .{ .expression = expression } }, span);
    }

    return try parser.addNode(.{ .jsx_spread_attribute = .{ .argument = expression } }, span);
}

// https://facebook.github.io/jsx/#prod-JSXAttributes
pub fn parseJsxAttributes(parser: *Parser) Error!?ast.IndexRange {
    const attributes_checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(attributes_checkpoint);

    while (parser.current_token.type == .jsx_identifier or parser.current_token.type == .left_brace) {
        const attribute = try parseJsxAttribute(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), attribute);
    }

    return try parser.addExtra(parser.scratch_a.take(attributes_checkpoint));
}

// https://facebook.github.io/jsx/#prod-JSXAttribute
pub fn parseJsxAttribute(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.type == .left_brace) {
        return try parseJsxSpread(parser, .tag);
    }

    const name = try parseJsxAttributeName(parser) orelse return null;

    // null for boolean-like attributes
    var value = ast.null_node;

    if (parser.current_token.type == .assign) {
        try parser.advance() orelse return null;

        value = try parseJsxAttributeValue(parser) orelse return null;
    }

    const end = if (!ast.isNull(value)) parser.getSpan(value).end else parser.getSpan(name).end;

    return try parser.addNode(.{ .jsx_attribute = .{ .name = name, .value = value } }, .{ .start = parser.getSpan(name).start, .end = end });
}

// https://facebook.github.io/jsx/#prod-JSXAttributeName
pub fn parseJsxAttributeName(parser: *Parser) Error!?ast.NodeIndex {
    var name = try parser.addNode(.{ .jsx_identifier = .{ .name_len = @intCast(parser.current_token.lexeme.len), .name_start = parser.current_token.span.start } }, parser.current_token.span);

    try parser.advance() orelse return null;

    // namespaced attribute name
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

        const namespace_name = try parser.addNode(.{ .jsx_identifier = .{ .name_len = @intCast(parser.current_token.lexeme.len), .name_start = parser.current_token.span.start } }, parser.current_token.span);

        try parser.advance() orelse return null;

        name = try parser.addNode(.{ .jsx_namespaced_name = .{ .namespace = name, .name = namespace_name } }, .{ .start = parser.getSpan(name).start, .end = parser.getSpan(namespace_name).end });
    }

    return name;
}

// https://facebook.github.io/jsx/#prod-JSXAttributeValue
pub fn parseJsxAttributeValue(parser: *Parser) Error!?ast.NodeIndex {
    return switch (parser.current_token.type) {
        .string_literal => literals.parseStringLiteral(parser),
        .left_brace => {
            const expression_container = try parseJsxExpressionContainer(parser, .tag) orelse return null;

            if (parser.getData(parser.getData(expression_container).jsx_expression_container.expression) == .jsx_empty_expression) {
                try parser.report(
                    parser.getSpan(expression_container),
                    "JSX attribute value cannot be an empty expression",
                    .{ .help = "Replace {} with a valid expression or remove the braces to use a string literal" },
                );
                return null;
            }

            return expression_container;
        },
        .less_than => {
            const jsx_expression = try parseJsxExpression(parser) orelse return null;

            parser.setLexerMode(.jsx_tag);

            return jsx_expression;
        },
        else => {
            try parser.reportFmt(
                parser.current_token.span,
                "Expected string literal or JSX expression for attribute value, but found '{s}'",
                .{parser.describeToken(parser.current_token)},
                .{ .help = "JSX attribute values must be either a string literal (e.g. \"value\") or an expression in braces (e.g. {expression})" },
            );
            return null;
        },
    };
}

const ExpressionContainerContext = enum { tag, child };

pub fn parseJsxExpressionContainer(parser: *Parser, context: ExpressionContainerContext) Error!?ast.NodeIndex {
    parser.setLexerMode(.normal);

    const start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '{'

    // empty expression
    if (parser.current_token.type == .right_brace) {
        const end = parser.current_token.span.end;

        try parser.advance() orelse return null;

        const empty_expr = try parser.addNode(.{ .jsx_empty_expression = .{} }, .{ .start = start + 1, .end = end - 1 });

        return try parser.addNode(.{ .jsx_expression_container = .{ .expression = empty_expr } }, .{ .start = start, .end = end });
    }

    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    const end = parser.current_token.span.end;

    parser.setLexerMode(if (context == .tag) .jsx_tag else .normal);

    if (!try parser.expect(.right_brace, "Expected '}' to close JSX expression", "Add '}' to close the expression")) return null;

    return try parser.addNode(.{ .jsx_expression_container = .{ .expression = expression } }, .{ .start = start, .end = end });
}

// https://facebook.github.io/jsx/#prod-JSXElementName
pub fn parseJsxElementName(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.type != .jsx_identifier) {
        try parser.reportFmt(
            parser.current_token.span,
            "Expected JSX element name, but found '{s}'",
            .{parser.describeToken(parser.current_token)},
            .{ .help = "JSX element names must start with a valid identifier" },
        );
        return null;
    }

    var was_member_expression = false;

    var name = try parser.addNode(.{ .jsx_identifier = .{ .name_len = @intCast(parser.current_token.lexeme.len), .name_start = parser.current_token.span.start } }, parser.current_token.span);

    try parser.advance() orelse return null;

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

        was_member_expression = true;

        const property = try parser.addNode(.{ .jsx_identifier = .{ .name_len = @intCast(parser.current_token.lexeme.len), .name_start = parser.current_token.span.start } }, parser.current_token.span);

        try parser.advance() orelse return null;

        name = try parser.addNode(.{ .jsx_member_expression = .{ .object = name, .property = property } }, .{ .start = parser.getSpan(name).start, .end = parser.getSpan(property).end });
    }

    // namespace only if it was not a member expression
    // which means, these '<one.two:three>' or '<one:two.three>' are not allowed
    if (parser.current_token.type == .colon and !was_member_expression) {
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

        const namespace_name = try parser.addNode(.{ .jsx_identifier = .{ .name_len = @intCast(parser.current_token.lexeme.len), .name_start = parser.current_token.span.start } }, parser.current_token.span);

        try parser.advance() orelse return null;

        name = try parser.addNode(.{ .jsx_namespaced_name = .{ .namespace = name, .name = namespace_name } }, .{ .start = parser.getSpan(name).start, .end = parser.getSpan(namespace_name).end });
    }

    return name;
}
