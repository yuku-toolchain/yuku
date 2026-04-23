const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;

const functions = @import("../functions.zig");
const parenthesized = @import("../parenthesized.zig");
const types = @import("types.zig");

// tristate classification for an arrow-head start at `(` or `<`. `.no`
// skips the arrow path, `.yes` commits via `parseArrow`, `.maybe`
// speculates via `tryParseArrow`.
pub const ArrowHead = enum { no, yes, maybe };

pub fn classifyArrowHead(parser: *Parser) Error!ArrowHead {
    return switch (parser.current_token.tag) {
        .left_paren => try classifyParenArrowHead(parser),
        .less_than => try classifyAngleArrowHead(parser),
        else => .no,
    };
}

// `<` is ambiguous with jsx and the prefix type assertion. the best we
// can do here is `.maybe` when the follow-up at least looks like a
// type-parameter head.
fn classifyAngleArrowHead(parser: *Parser) Error!ArrowHead {
    const second = try parser.peekAhead() orelse return .no;
    if (!second.tag.isIdentifierLike() and second.tag != .@"const") return .no;
    return .maybe;
}

// `(` commits to `.yes` for shapes only an arrow can have, `.maybe` for
// shapes that overlap with a parenthesized or sequence expression.
fn classifyParenArrowHead(parser: *Parser) Error!ArrowHead {
    const peek = try parser.peekAheadN(3);
    const second = peek[0] orelse return .no;

    switch (second.tag) {
        // `(...` rest is arrow only.
        .spread => return .yes,
        // destructuring overlaps with array or object expressions.
        .left_bracket, .left_brace => return .maybe,
        // `()` is an arrow when followed by `=>` or `: returnType`.
        .right_paren => {
            const third = peek[1] orelse return .no;
            return switch (third.tag) {
                .arrow, .colon => .yes,
                else => .no,
            };
        },
        // `this` is a parameter only when annotated.
        .this => {
            const third = peek[1] orelse return .no;
            return if (third.tag == .colon) .yes else .no;
        },
        else => {
            if (!second.tag.isIdentifierLike()) return .no;
            const third = peek[1] orelse return .no;
            return switch (third.tag) {
                // `(a : T` typed parameter.
                .colon => .yes,
                // `(a ?` is arrow only when the next token continues a param list.
                .question => blk: {
                    const fourth = peek[2] orelse break :blk .no;
                    break :blk switch (fourth.tag) {
                        .colon, .comma, .assign, .right_paren => .yes,
                        else => .no,
                    };
                },
                // overlaps with paren or sequence expression.
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

    const params = try functions.parseFormalParamaters(parser, .arrow_formal_parameters, false) orelse return null;

    const return_type: ast.NodeIndex = if (parser.current_token.tag == .colon)
        try types.parseReturnTypeAnnotation(parser) orelse return null
    else
        .null;

    if (parser.current_token.tag != .arrow or parser.current_token.hasLineTerminatorBefore()) return null;

    return parenthesized.buildArrowFunction(parser, params, is_async, arrow_start, type_parameters, return_type);
}

// speculative wrapper around `parseArrow`. rewinds on failure so the
// caller can fall through to the cover grammar, a jsx element, or a
// prefix type assertion. also rewinds when we parsed `(params): T => body`
// in a context that disallows the return-type form (ternary consequent,
// case label) and no disambiguating `:` follows the body.
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
