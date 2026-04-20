const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const TokenTag = @import("../../token.zig").TokenTag;
const literals = @import("../literals.zig");
const ts_types = @import("types.zig");

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

/// `interface Foo { }` at the current token, requires the interface name
/// to be identifier-like and on the same source line.
pub fn isInterfaceStart(parser: *Parser) Error!?bool {
    std.debug.assert(parser.current_token.tag == .interface);
    if (!parser.tree.isTs()) return false;

    const next = (try parser.peekAhead()) orelse return null;
    return next.tag.isIdentifierLike() and !next.hasLineTerminatorBefore();
}

/// `declare interface Foo { }` at the current token. requires `declare` to
/// be followed by `interface <name>` all on the same source line.
pub fn isDeclareInterfaceStart(parser: *Parser) Error!?bool {
    std.debug.assert(parser.current_token.tag == .declare);
    if (!parser.tree.isTs()) return false;

    const ahead = try parser.peekAheadN(2);
    const interface_token = ahead[0] orelse return null;
    const name_token = ahead[1] orelse return null;

    return interface_token.tag == .interface and !interface_token.hasLineTerminatorBefore() and
        name_token.tag.isIdentifierLike() and !name_token.hasLineTerminatorBefore();
}

/// `interface Foo<T> extends Bar, Baz<U> { ... }` or the `declare`-prefixed
/// ambient form.
/// when `is_declare` is true, `decl_start` is the span start of `declare` and
/// the caller has already consumed it.
pub fn parseInterfaceDeclaration(parser: *Parser, is_declare: bool, decl_start: u32) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .interface);

    const start = if (is_declare) decl_start else parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'interface'

    const id = try literals.parseBindingIdentifier(parser) orelse return null;
    const type_parameters = try ts_types.parseTypeParameters(parser);
    const extends = try parseInterfaceExtendsClause(parser) orelse return null;
    const body = try parseInterfaceBody(parser) orelse return null;

    return try parser.tree.createNode(
        .{ .ts_interface_declaration = .{
            .id = id,
            .type_parameters = type_parameters,
            .extends = extends,
            .body = body,
            .declare = is_declare,
        } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

/// `extends Bar, Baz<U>` or an empty range when the `extends` keyword is
/// absent. each heritage entry is a `TSInterfaceHeritage` holding an
/// expression and an optional `<T>` type argument list.
fn parseInterfaceExtendsClause(parser: *Parser) Error!?ast.IndexRange {
    if (parser.current_token.tag != .extends) return .empty;
    try parser.advance() orelse return null; // consume 'extends'

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (true) {
        const heritage = try parseInterfaceHeritage(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), heritage);
        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return null; // consume ','
    }

    return try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);
}

/// `Bar` or `Foo.Bar` or `Foo.Bar<U>`. the expression is an identifier-path
/// chain built from `IdentifierReference` heads and `MemberExpression` links,
/// matching the runtime-expression shape the ESTree output expects.
fn parseInterfaceHeritage(parser: *Parser) Error!?ast.NodeIndex {
    const expression = try parseInterfaceHeritageExpression(parser) orelse return null;
    const type_arguments = try ts_types.parseTypeArguments(parser);

    const start = parser.tree.getSpan(expression).start;
    const end = if (type_arguments != .null)
        parser.tree.getSpan(type_arguments).end
    else
        parser.tree.getSpan(expression).end;

    return try parser.tree.createNode(
        .{ .ts_interface_heritage = .{
            .expression = expression,
            .type_arguments = type_arguments,
        } },
        .{ .start = start, .end = end },
    );
}

/// consumes an `IdentifierReference` head and any number of `.<name>`
/// continuations, producing left-associative `MemberExpression` nodes. does
/// not accept calls, computed access, or optional chaining (these are
/// invalid in heritage position).
fn parseInterfaceHeritageExpression(parser: *Parser) Error!?ast.NodeIndex {
    var expression = try literals.parseIdentifier(parser) orelse return null;

    while (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null; // consume '.'
        const property = try literals.parseIdentifierName(parser) orelse return null;
        const object_start = parser.tree.getSpan(expression).start;
        const property_end = parser.tree.getSpan(property).end;
        expression = try parser.tree.createNode(
            .{ .member_expression = .{
                .object = expression,
                .property = property,
                .computed = false,
                .optional = false,
            } },
            .{ .start = object_start, .end = property_end },
        );
    }

    return expression;
}

/// `{ a: T; b(): U; [k: string]: V }` body of an interface. produces a
/// `TSInterfaceBody` wrapping the signature list.
fn parseInterfaceBody(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .left_brace) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '{' to start an interface body",
            .{ .help = "An interface body is written '{ member1; member2 }'" },
        );
        return null;
    }

    const start = parser.current_token.span.start;
    const members = try ts_types.parseObjectTypeMembers(parser) orelse return null;

    return try parser.tree.createNode(
        .{ .ts_interface_body = .{ .body = members } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}
