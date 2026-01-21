// WIP WIP WIP

const std = @import("std");
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const literals = @import("literals.zig");
const expressions = @import("expressions.zig");

pub fn parseJsxElement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const opening_element = try parseJsxOpeningElement(parser) orelse return null;

    // parser.setLexerMode(.jsx_text);

    const end = parser.current_token.span.end;

    return try parser.addNode(.{ .jsx_element = .{ .opening_element = opening_element, .children = ast.IndexRange.empty, .closing_element = ast.null_node } }, .{ .start = start, .end = end });
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

    if (!try parser.expect(.greater_than, "Expected '>' to close JSX opening element", "Add '>' to close the JSX tag")) return null;

    const end = parser.current_token.span.end;

    return try parser.addNode(.{
        .jsx_opening_element = .{
            .name = name,
            .attributes = attributes,
            .self_closing = self_closing,
        },
    }, .{ .start = start, .end = end });
}

pub fn parseJsxAttributes(parser: *Parser) Error!?ast.IndexRange {
    const attributes_checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(attributes_checkpoint);

    while (parser.current_token.type == .jsx_identifier and parser.current_token.type != .eof) {
        const attribute = try parseJsxAttribute(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), attribute);
    }

    return try parser.addExtra(parser.scratch_a.take(attributes_checkpoint));
}

pub fn parseJsxAttribute(parser: *Parser) Error!?ast.NodeIndex {
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

        name = try parser.addNode(.{ .jsx_namespaced_name = .{ .namespace = name, .name = namespace_name } }, .{ .start = parser.getSpan(name).start, .end = parser.current_token.span.end });
    }

    return name;
}

pub fn parseJsxAttributeValue(parser: *Parser) Error!?ast.NodeIndex {
    return switch (parser.current_token.type) {
        .string_literal => literals.parseStringLiteral(parser),
        .left_brace => {
            parser.setLexerMode(.normal);

            _ = try parseJsxExpressionContainer(parser) orelse return null;

            // validate, empty expression not allowed as attribute value

            parser.setLexerMode(.jsx_identifier);
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

    if (!try parser.expect(.right_brace, "", "")) return null;

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

        name = try parser.addNode(.{ .jsx_member_expression = .{ .object = name, .property = property } }, .{ .start = parser.getSpan(name).start, .end = parser.current_token.span.end });
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

        return try parser.addNode(.{ .jsx_namespaced_name = .{ .namespace = name, .name = namespace_name } }, .{ .start = parser.getSpan(name).start, .end = parser.current_token.span.end });
    }

    return name;
}
