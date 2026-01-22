// WIP WIP WIP

const std = @import("std");
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const literals = @import("literals.zig");
const expressions = @import("expressions.zig");

pub fn parseJsxExpression(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const opening_element = try parseJsxOpeningElement(parser) orelse return null;

    var children = ast.IndexRange.empty;

    if(!parser.getData(opening_element).jsx_opening_element.self_closing) {
        children = try parseJsxChildren(parser) orelse return null;
    }

    const end = parser.current_token.span.end;

    return try parser.addNode(.{ .jsx_element = .{ .opening_element = opening_element, .children = children, .closing_element = ast.null_node } }, .{ .start = start, .end = end });
}

pub fn parseJsxChildren(parser: *Parser) Error!?ast.IndexRange {
    const children_checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(children_checkpoint);

    while (parser.current_token.type != .eof) {
        switch (parser.current_token.type) {
            .jsx_text => {
                const jsx_text = try parser.addNode(.{ .jsx_text = .{ .raw_start = parser.current_token.span.start, .raw_len = @intCast(parser.current_token.lexeme.len) } }, parser.current_token.span);

                try parser.scratch_b.append(parser.allocator(), jsx_text);

                parser.setLexerMode(.normal);

                try parser.advance() orelse return null;
            },
            .less_than => {
                // parser.setLexerMode(.jsx_identifier);

                //
                //
                return null;
            },
            .left_brace => {
                parser.setLexerMode(.normal);

                const next = try parser.lookAhead() orelse return null;

                if(next.type == .spread) {
                    try parser.advance() orelse return null; // consume '{'

                    const jsx_spread_child = try parseJsxSpread(parser, .child) orelse return null;

                    parser.setLexerMode(.jsx_text);

                    if (!try parser.expect(.right_brace, "", "")) return null;

                    try parser.scratch_b.append(parser.allocator(), jsx_spread_child);

                    continue;
                }

                const expression_container = try parseJsxExpressionContainer(parser) orelse return null;

                try parser.scratch_b.append(parser.allocator(), expression_container);

                parser.setLexerMode(.jsx_text);
            },
            else => break,
        }
    }

    return try parser.addExtra(parser.scratch_b.take(children_checkpoint));
}

// https://facebook.github.io/jsx/#prod-JSXOpeningElement
pub fn parseJsxOpeningElement(parser: *Parser) Error!?ast.NodeIndex {
    parser.setLexerMode(.jsx_identifier);

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

    // if the element is self closed, which means, we are done with the jsx element and back to normal javascript
    // otherwise, it just a opening of a jsx element, and the next is jsx children (so to start, set lexer mode to jsx_text)
    parser.setLexerMode(if (self_closing) .normal else .jsx_text);

    if (!try parser.expect(.greater_than, "Expected '>' to close JSX opening element", "Add '>' to close the JSX tag")) return null;

    return try parser.addNode(.{
        .jsx_opening_element = .{
            .name = name,
            .attributes = attributes,
            .self_closing = self_closing,
        },
    }, .{ .start = start, .end = end });
}

const SpreadContext = enum {
    attribute,
    child
};

pub fn parseJsxSpread(parser: *Parser, context: SpreadContext) Error!?ast.NodeIndex {
    parser.setLexerMode(.normal);
    defer parser.setLexerMode(if (context == .attribute) .jsx_identifier else .jsx_text);

    if (!try parser.expect(.spread, "", "")) return null;

    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if(context == .child) {
        return try parser.addNode(.{ .jsx_spread_child = .{ .expression = expression } }, parser.getSpan(expression));
    }

    return try parser.addNode(.{ .jsx_spread_attribute = .{ .argument = expression } }, parser.getSpan(expression));
}

pub fn parseJsxAttributes(parser: *Parser) Error!?ast.IndexRange {
    const attributes_checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(attributes_checkpoint);

    while (parser.current_token.type == .jsx_identifier or parser.current_token.type == .left_brace) {
        const attribute = try parseJsxAttribute(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), attribute);
    }

    return try parser.addExtra(parser.scratch_a.take(attributes_checkpoint));
}

pub fn parseJsxAttribute(parser: *Parser) Error!?ast.NodeIndex {
    if(parser.current_token.type == .left_brace) {
        try parser.advance() orelse return null; // consume '{'

        const jsx_spread_attribute = try parseJsxSpread(parser, .attribute) orelse return null;

        if (!try parser.expect(.right_brace, "", "")) return null;

        return jsx_spread_attribute;
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

pub fn parseJsxAttributeValue(parser: *Parser) Error!?ast.NodeIndex {
    return switch (parser.current_token.type) {
        .string_literal => literals.parseStringLiteral(parser),
        .left_brace => {
            parser.setLexerMode(.normal);

            const expression_container = try parseJsxExpressionContainer(parser) orelse return null;

            if (parser.getData(expression_container) == .jsx_empty_expression) {
                try parser.report(
                    parser.getSpan(expression_container),
                    "JSX attribute value cannot be an empty expression",
                    .{ .help = "Replace {} with a valid expression or remove the braces to use a string literal" },
                );
                return null;
            }

            parser.setLexerMode(.jsx_identifier);

            return expression_container;
        },
        .less_than => parseJsxExpression(parser),
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

pub fn parseJsxExpressionContainer(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '{'

    // empty expression
    if (parser.current_token.type == .right_brace) {
        const end = parser.current_token.span.end;

        try parser.advance() orelse return null;

        return try parser.addNode(.{ .jsx_empty_expression = .{} }, .{ .start = start, .end = end });
    }

    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    const end = parser.current_token.span.end;

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

    while (parser.current_token.type == .dot and parser.current_token.type != .eof) {
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
