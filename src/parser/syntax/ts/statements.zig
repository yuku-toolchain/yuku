const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const TokenTag = @import("../../token.zig").TokenTag;
const literals = @import("../literals.zig");
const ts_types = @import("types.zig");

/// mirrors
/// `isStartOfTypeAliasDeclaration` in the typescript-go parser
/// (https://github.com/microsoft/typescript-go/blob/main/internal/parser/parser.go).
pub fn isTypeAliasStart(parser: *Parser) Error!?bool {
    std.debug.assert(parser.current_token.tag == .type);
    if (!parser.tree.isTs()) return false;

    const next = (try parser.peekAhead()) orelse return null;
    return next.tag.isIdentifierLike() and !next.hasLineTerminatorBefore();
}

pub fn isDeclareTypeAliasStart(parser: *Parser) Error!?bool {
    std.debug.assert(parser.current_token.tag == .declare);
    if (!parser.tree.isTs()) return false;

    const ahead = try parser.peekAheadN(2);
    const type_token = ahead[0] orelse return null;
    const name_token = ahead[1] orelse return null;

    return type_token.tag == .type and !type_token.hasLineTerminatorBefore() and
        name_token.tag.isIdentifierLike() and !name_token.hasLineTerminatorBefore();
}

/// `type Foo<T> = Bar<T>;`   `declare type Foo<T> = Bar<T>;`
/// when `is_declare` is true, `decl_start` is the span start of the `declare`
/// keyword and the caller has already consumed it.
pub fn parseTypeAliasDeclaration(parser: *Parser, is_declare: bool, decl_start: u32) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .type);

    const start = if (is_declare) decl_start else parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'type'

    const id = try literals.parseBindingIdentifier(parser) orelse return null;
    const type_parameters = try ts_types.parseTypeParameters(parser);

    if (!try parser.expect(
        .assign,
        "Expected '=' in type alias declaration",
        "A type alias is written 'type Foo = Bar'",
    )) return null;

    const type_annotation = try ts_types.parseType(parser) orelse return null;

    const end = try parser.eatSemicolon(parser.tree.getSpan(type_annotation).end) orelse return null;

    return try parser.tree.createNode(
        .{ .ts_type_alias_declaration = .{
            .id = id,
            .type_parameters = type_parameters,
            .type_annotation = type_annotation,
            .declare = is_declare,
        } },
        .{ .start = start, .end = end },
    );
}
