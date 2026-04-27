const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;

const functions = @import("../functions.zig");
const parenthesized = @import("../parenthesized.zig");
const types = @import("types.zig");

pub const ArrowHead = enum { no, yes, maybe };

pub fn classifyArrowHead(parser: *Parser) Error!ArrowHead {
    if (parser.current_token.tag != .left_paren) return .no;
    return classifyParenArrowHead(parser);
}

fn classifyParenArrowHead(parser: *Parser) Error!ArrowHead {
    const peek = try parser.peekAheadN(3);
    const second = peek[0] orelse return .no;

    switch (second.tag) {
        .spread => return .yes,
        // overlaps with array/object expression
        .left_bracket, .left_brace => return .maybe,
        // `()` only when `=>` or `: returnType` follows
        .right_paren => {
            const third = peek[1] orelse return .no;
            return switch (third.tag) {
                .arrow, .colon => .yes,
                else => .no,
            };
        },
        // `this` is a parameter only when annotated
        .this => {
            const third = peek[1] orelse return .no;
            return if (third.tag == .colon) .yes else .no;
        },
        else => {
            if (!second.tag.isIdentifierLike()) return .no;
            const third = peek[1] orelse return .no;
            return switch (third.tag) {
                .colon => .yes,
                .question => blk: {
                    const fourth = peek[2] orelse break :blk .no;
                    break :blk switch (fourth.tag) {
                        .colon, .comma, .assign, .right_paren => .yes,
                        else => .no,
                    };
                },
                // overlaps with paren or sequence expression
                .comma, .assign, .right_paren => .maybe,
                else => .no,
            };
        },
    }
}

pub fn parseArrow(parser: *Parser, is_async: bool, arrow_start: u32) Error!?ast.NodeIndex {
    const saved_await = parser.context.await_is_keyword;
    if (is_async) parser.context.await_is_keyword = true;
    defer parser.context.await_is_keyword = saved_await;

    const type_parameters = try types.parseTypeParameters(parser);

    if (parser.current_token.tag != .left_paren) return null;

    const params = try functions.parseFormalParameters(parser, .arrow_formal_parameters, false) orelse return null;

    const return_type: ast.NodeIndex = if (parser.current_token.tag == .colon)
        try types.parseReturnTypeAnnotation(parser) orelse return null
    else
        .null;

    if (parser.current_token.tag != .arrow or parser.current_token.hasLineTerminatorBefore()) return null;

    return parenthesized.buildArrowFunction(parser, params, is_async, arrow_start, type_parameters, return_type);
}

// rewinds on failure so the caller can fall through to the cover grammar,
// jsx, or a prefix type assertion. also rewinds an `(p): T => body` that
// was parsed under `!allow_arrow_return_type` (ternary consequent, case
// label) when the `:` belongs to the outer context.
pub fn tryParseArrow(parser: *Parser, is_async: bool, arrow_start: u32) Error!?ast.NodeIndex {
    const cp = parser.checkpoint();
    if (try parseArrow(parser, is_async, arrow_start)) |arrow| {
        const return_type = parser.tree.getData(arrow).arrow_function_expression.return_type;
        if (!parser.context.allow_arrow_return_type and return_type != .null and parser.current_token.tag != .colon) {
            parser.rewind(cp);
            return null;
        }
        return arrow;
    }
    parser.rewind(cp);
    return null;
}

// one-token peek skips the checkpoint/rewind on hot jsx / type-assertion paths.
pub fn tryParseGenericArrow(parser: *Parser, is_async: bool, arrow_start: u32) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .less_than);

    const next = try parser.peekAhead() orelse return null;
    if (!next.tag.isIdentifierLike() and next.tag != .@"const") return null;

    return tryParseArrow(parser, is_async, arrow_start);
}
