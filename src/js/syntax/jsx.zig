// WIP WIP WIP

const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

pub fn parseJsxElement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const opening_element = try parseJsxOpeningElement(parser) orelse return null;

    const end = parser.current_token.span.end;

    return try parser.addNode(.{ .jsx_element = .{ .opening_element = opening_element, .children = ast.IndexRange.empty, .closing_element = ast.null_node } }, .{ .start = start, .end = end });
}

pub fn parseJsxOpeningElement(parser: *Parser) Error!?ast.NodeIndex {
    parser.setLexerMode(.jsx_identifier);

    const start = parser.current_token.span.start;

    try parser.advance() orelse return null; // consume '<'

    var self_closing = false;

    const name = try parseJsxElementName(parser) orelse return null;

    if (parser.current_token.type == .slash) {
        self_closing = true;
        try parser.advance() orelse return null;
    }

    if (!try parser.expect(.greater_than, "an error message here", "a help message")) {
        return null;
    }

    const end = parser.current_token.span.end;

    return try parser.addNode(.{
        .jsx_opening_element = .{
            .name = name,
            .attributes = ast.IndexRange.empty, // not implemented yet
            .self_closing = self_closing,
        },
    }, .{ .start = start, .end = end });
}

pub fn parseJsxElementName(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.type != .jsx_identifier) {
        // report error here

        return null;
    }

    var name = try parser.addNode(.{ .jsx_identifier = .{ .name_len = @intCast(parser.current_token.lexeme.len), .name_start = parser.current_token.span.start } }, parser.current_token.span);

    try parser.advance() orelse return null;

    if (parser.current_token.type == .dot) {
        name = try parseJsxMemberExpression(parser, name) orelse return null;
    }

    return name;
}

pub fn parseJsxMemberExpression(parser: *Parser, object: ast.NodeIndex) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume '.'

    if (parser.current_token.type != .jsx_identifier) {
        // report error here with proper message

        return null;
    }

    const property = try parser.addNode(.{ .jsx_identifier = .{ .name_len = @intCast(parser.current_token.lexeme.len), .name_start = parser.current_token.span.start } }, parser.current_token.span);

    try parser.advance() orelse return null;

    return try parser.addNode(.{ .jsx_member_expression = .{ .object = object, .property = property } }, .{ .start = parser.getSpan(object).start, .end = parser.current_token.span.end });
}
