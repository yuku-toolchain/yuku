const std = @import("std");
const TokenTag = @import("../../token.zig").TokenTag;
const Parser = @import("../../parser.zig").Parser;

pub fn isCodeBlockStart(parser: *Parser) bool {
    std.debug.assert(parser.current_token.span.start <= parser.current_token.span.end);
    std.debug.assert(parser.current_token.span.end <= parser.tree.source.len);

    if (!parser.tree.isTsrx()) return false;
    if (parser.current_token.tag != .at) return false;

    const marker_end = parser.current_token.span.end;
    const next = parser.peekAhead() orelse return false;
    if (next.tag != .left_brace) return false;

    return next.span.start == marker_end;
}

pub fn isIfDirectiveStart(parser: *Parser) bool {
    return isDirectiveStart(parser, .@"if");
}

pub fn isForDirectiveStart(parser: *Parser) bool {
    return isDirectiveStart(parser, .@"for");
}

pub fn isSwitchDirectiveStart(parser: *Parser) bool {
    return isDirectiveStart(parser, .@"switch");
}

pub fn isTryDirectiveStart(parser: *Parser) bool {
    return isDirectiveStart(parser, .@"try");
}

pub fn isElseDirectiveStart(parser: *Parser) bool {
    return isDirectiveStart(parser, .@"else");
}

pub fn isEmptyDirectiveStart(parser: *Parser) bool {
    return isIdentifierDirectiveStart(parser, "empty");
}

pub fn isCaseDirectiveStart(parser: *Parser) bool {
    return isDirectiveStart(parser, .case);
}

pub fn isDefaultDirectiveStart(parser: *Parser) bool {
    return isDirectiveStart(parser, .default);
}

pub fn isPendingDirectiveStart(parser: *Parser) bool {
    return isIdentifierDirectiveStart(parser, "pending");
}

pub fn isCatchDirectiveStart(parser: *Parser) bool {
    return isDirectiveStart(parser, .@"catch");
}

pub fn isControlFlowDirectiveStart(parser: *Parser) bool {
    std.debug.assert(parser.current_token.span.start <= parser.current_token.span.end);
    std.debug.assert(parser.current_token.span.end <= parser.tree.source.len);

    if (!parser.tree.isTsrx()) return false;
    if (parser.current_token.tag != .at) return false;

    const marker_end = parser.current_token.span.end;
    const next = parser.peekAhead() orelse return false;
    if (next.span.start != marker_end) return false;

    return switch (next.tag) {
        .@"if",
        .@"for",
        .@"switch",
        .@"try",
        => true,
        else => false,
    };
}

fn isDirectiveStart(parser: *Parser, tag: TokenTag) bool {
    std.debug.assert(parser.current_token.span.start <= parser.current_token.span.end);
    std.debug.assert(parser.current_token.span.end <= parser.tree.source.len);

    if (!parser.tree.isTsrx()) return false;
    if (parser.current_token.tag != .at) return false;

    const marker_end = parser.current_token.span.end;
    const next = parser.peekAhead() orelse return false;
    if (next.tag != tag) return false;

    return next.span.start == marker_end;
}

fn isIdentifierDirectiveStart(parser: *Parser, text: []const u8) bool {
    std.debug.assert(parser.current_token.span.start <= parser.current_token.span.end);
    std.debug.assert(parser.current_token.span.end <= parser.tree.source.len);

    if (!parser.tree.isTsrx()) return false;
    if (parser.current_token.tag != .at) return false;

    const marker_end = parser.current_token.span.end;
    const text_end = marker_end + @as(u32, @intCast(text.len));
    if (text_end > parser.tree.source.len) return false;
    if (!std.mem.eql(u8, parser.tree.source[marker_end..text_end], text)) return false;

    if (text_end == parser.tree.source.len) return true;

    const next = parser.tree.source[text_end];
    return !std.ascii.isAlphanumeric(next) and next != '_' and next != '$';
}
