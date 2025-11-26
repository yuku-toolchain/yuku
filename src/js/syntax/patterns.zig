const std = @import("std");
const token = @import("../token.zig");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const literals = @import("literals.zig");
const expressions = @import("expressions.zig");

pub fn parseBindingPattern(parser: *Parser) ?ast.NodeIndex {
    if (parser.current_token.type.isIdentifierLike()) {
        return parseBindingIdentifier(parser);
    }
    return switch (parser.current_token.type) {
        .LeftBracket => parseArrayPattern(parser),
        .LeftBrace => parseObjectPattern(parser),
        else => {
            parser.err(parser.current_token.span.start, parser.current_token.span.end, "Expected binding pattern", null);
            return null;
        },
    };
}

fn parseBindingIdentifier(parser: *Parser) ?ast.NodeIndex {
    if (!parser.current_token.type.isIdentifierLike()) {
        parser.err(parser.current_token.span.start, parser.current_token.span.end, "Expected identifier", null);
        return null;
    }
    const current = parser.current_token;
    if (!parser.ensureValidIdentifier(current, "as an identifier", "Choose a different name", .{})) {
        return null;
    }
    parser.advance();
    return parser.addNode(.{
        .binding_identifier = .{
            .name_start = current.span.start,
            .name_len = @intCast(current.lexeme.len),
        },
    }, current.span);
}

fn parseArrayPattern(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (!parser.expect(.LeftBracket, "Expected '['", null)) return null;

    var elements: [256]ast.NodeIndex = undefined;
    var length: usize = 0;

    while (parser.current_token.type != .RightBracket and parser.current_token.type != .EOF) {
        if (parser.current_token.type == .Spread) {
            elements[length] = parseRestElement(parser) orelse return null;
            length += 1;
            if (parser.current_token.type == .Comma) {
                parser.err(parser.getSpan(elements[length - 1]).start, parser.current_token.span.end, "Rest must be last", null);
                return null;
            }
            break;
        }
        if (parser.current_token.type == .Comma) {
            elements[length] = ast.null_node;
            length += 1;
            parser.advance();
        } else {
            elements[length] = parseArrayPatternElement(parser) orelse return null;
            length += 1;
            if (parser.current_token.type == .Comma) parser.advance() else break;
        }
    }

    if (parser.current_token.type != .RightBracket) {
        parser.err(start, parser.current_token.span.end, "Expected ']'", null);
        return null;
    }
    const end = parser.current_token.span.end;
    parser.advance();

    return parser.addNode(.{
        .array_pattern = .{ .elements = parser.addExtra(elements[0..length]) },
    }, .{ .start = start, .end = end });
}

fn parseArrayPatternElement(parser: *Parser) ?ast.NodeIndex {
    const pattern = parseBindingPattern(parser) orelse return null;
    if (parser.current_token.type == .Assign) {
        return parseAssignmentPatternDefault(parser, pattern);
    }
    return pattern;
}

fn parseRestElement(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (!parser.expect(.Spread, "Expected '...'", null)) return null;
    const argument = parseBindingPattern(parser) orelse return null;
    return parser.addNode(.{
        .rest_element = .{ .argument = argument },
    }, .{ .start = start, .end = parser.getSpan(argument).end });
}

fn parseObjectPattern(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (!parser.expect(.LeftBrace, "Expected '{'", null)) return null;

    var properties: [256]ast.NodeIndex = undefined;
    var length: usize = 0;

    while (parser.current_token.type != .RightBrace and parser.current_token.type != .EOF) {
        if (parser.current_token.type == .Spread) {
            properties[length] = parseObjectRestElement(parser) orelse return null;
            length += 1;
            if (parser.current_token.type == .Comma) {
                parser.err(parser.getSpan(properties[length - 1]).start, parser.current_token.span.end, "Rest must be last", null);
                return null;
            }
            break;
        }
        properties[length] = parseObjectPatternProperty(parser) orelse return null;
        length += 1;
        if (parser.current_token.type == .Comma) parser.advance() else break;
    }

    if (parser.current_token.type != .RightBrace) {
        parser.err(start, parser.current_token.span.end, "Expected '}'", null);
        return null;
    }
    const end = parser.current_token.span.end;
    parser.advance();

    return parser.addNode(.{
        .object_pattern = .{ .properties = parser.addExtra(properties[0..length]) },
    }, .{ .start = start, .end = end });
}

fn parseObjectPatternProperty(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var computed = false;
    var key: ast.NodeIndex = undefined;
    var key_span: ast.Span = undefined;
    var identifier_token: token.Token = undefined;

    if (parser.current_token.type == .LeftBracket) {
        computed = true;
        parser.advance();
        key = expressions.parseExpression(parser, 0) orelse return null;
        key_span = .{ .start = start, .end = parser.getSpan(key).end };
        if (parser.current_token.type != .RightBracket) {
            parser.err(start, parser.current_token.span.start, "Expected ']'", null);
            return null;
        }
        key_span.end = parser.current_token.span.end;
        parser.advance();
    } else if (parser.current_token.type.isIdentifierLike()) {
        identifier_token = parser.current_token;
        key = parser.addNode(.{
            .identifier_name = .{
                .name_start = parser.current_token.span.start,
                .name_len = @intCast(parser.current_token.lexeme.len),
            },
        }, parser.current_token.span);
        key_span = parser.current_token.span;
        parser.advance();
    } else if (parser.current_token.type.isNumericLiteral()) {
        key = literals.parseNumericLiteral(parser) orelse return null;
        key_span = parser.getSpan(key);
    } else if (parser.current_token.type == .StringLiteral) {
        key = literals.parseStringLiteral(parser) orelse return null;
        key_span = parser.getSpan(key);
    } else {
        parser.err(parser.current_token.span.start, parser.current_token.span.end, "Expected property key", null);
        return null;
    }

    const is_shorthand = parser.current_token.type == .Comma or
        parser.current_token.type == .RightBrace or
        parser.current_token.type == .Assign;

    var value: ast.NodeIndex = undefined;
    if (is_shorthand) {
        const data = parser.getData(key);
        if (data != .identifier_name) {
            parser.err(key_span.start, key_span.end, "Cannot use computed as shorthand", null);
            return null;
        }
        if (!parser.ensureValidIdentifier(identifier_token, "in shorthand", "Use full form", .{})) {
            return null;
        }
        value = parser.addNode(.{
            .binding_identifier = .{
                .name_start = data.identifier_name.name_start,
                .name_len = data.identifier_name.name_len,
            },
        }, key_span);
        if (parser.current_token.type == .Assign) {
            value = parseAssignmentPatternDefault(parser, value) orelse return null;
        }
    } else {
        if (parser.current_token.type != .Colon) {
            parser.err(key_span.start, parser.current_token.span.start, "Expected ':'", null);
            return null;
        }
        parser.advance();
        value = parseBindingPattern(parser) orelse return null;
        if (parser.current_token.type == .Assign) {
            value = parseAssignmentPatternDefault(parser, value) orelse return null;
        }
    }

    return parser.addNode(.{
        .binding_property = .{
            .key = key,
            .value = value,
            .shorthand = is_shorthand,
            .computed = computed,
        },
    }, .{ .start = start, .end = parser.getSpan(value).end });
}

fn parseObjectRestElement(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;
    parser.advance();
    const argument = parseBindingPattern(parser) orelse return null;
    if (parser.getData(argument) != .binding_identifier) {
        parser.err(parser.getSpan(argument).start, parser.getSpan(argument).end, "Rest must be identifier", null);
        return null;
    }
    return parser.addNode(.{
        .rest_element = .{ .argument = argument },
    }, .{ .start = start, .end = parser.getSpan(argument).end });
}

fn parseAssignmentPatternDefault(parser: *Parser, left: ast.NodeIndex) ?ast.NodeIndex {
    const start = parser.getSpan(left).start;
    if (parser.current_token.type != .Assign) return left;
    parser.advance();
    const right = expressions.parseExpression(parser, 0) orelse return null;
    return parser.addNode(.{
        .assignment_pattern = .{ .left = left, .right = right },
    }, .{ .start = start, .end = parser.getSpan(right).end });
}

pub fn isDestructuringPattern(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.getData(index)) {
        .array_pattern, .object_pattern => true,
        .assignment_pattern => |pattern| isDestructuringPattern(parser, pattern.left),
        else => false,
    };
}
