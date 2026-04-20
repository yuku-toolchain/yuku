const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const Precedence = @import("../../token.zig").Precedence;
const TokenTag = @import("../../token.zig").TokenTag;
const expressions = @import("../expressions.zig");
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

/// `enum Foo { ... }` at the current token, requires the enum name to be
/// identifier-like and on the same source line.
pub fn isEnumStart(parser: *Parser) Error!?bool {
    std.debug.assert(parser.current_token.tag == .@"enum");
    if (!parser.tree.isTs()) return false;

    const next = (try parser.peekAhead()) orelse return null;
    return next.tag.isIdentifierLike() and !next.hasLineTerminatorBefore();
}

/// `const enum Foo { ... }` at the current token. requires `const` to be
/// followed by `enum <name>` all on the same source line.
pub fn isConstEnumStart(parser: *Parser) Error!?bool {
    std.debug.assert(parser.current_token.tag == .@"const");
    if (!parser.tree.isTs()) return false;

    const ahead = try parser.peekAheadN(2);
    const enum_token = ahead[0] orelse return null;
    const name_token = ahead[1] orelse return null;

    return enum_token.tag == .@"enum" and !enum_token.hasLineTerminatorBefore() and
        name_token.tag.isIdentifierLike() and !name_token.hasLineTerminatorBefore();
}

/// `declare enum Foo { ... }` or `declare const enum Foo { ... }` at the
/// current token. requires `declare` to be followed by `enum <name>` or
/// `const enum <name>` all on the same source line. when the result is true.
pub fn isDeclareEnumStart(parser: *Parser, out_is_const: *bool) Error!?bool {
    std.debug.assert(parser.current_token.tag == .declare);
    if (!parser.tree.isTs()) return false;

    const ahead = try parser.peekAheadN(3);
    const second = ahead[0] orelse return null;
    if (second.hasLineTerminatorBefore()) return false;

    if (second.tag == .@"enum") {
        const name_token = ahead[1] orelse return null;
        if (!name_token.tag.isIdentifierLike() or name_token.hasLineTerminatorBefore()) return false;
        out_is_const.* = false;
        return true;
    }

    if (second.tag == .@"const") {
        const enum_token = ahead[1] orelse return null;
        if (enum_token.tag != .@"enum" or enum_token.hasLineTerminatorBefore()) return false;
        const name_token = ahead[2] orelse return null;
        if (!name_token.tag.isIdentifierLike() or name_token.hasLineTerminatorBefore()) return false;
        out_is_const.* = true;
        return true;
    }

    return false;
}

/// `enum Foo { A, B = 1 }`, `const enum Foo { ... }`, `declare enum Foo { ... }`,
/// or `declare const enum Foo { ... }`. when `is_declare` or `is_const` is true,
/// the caller has already consumed the matching modifier keywords, `decl_start`
/// is the span start of the leading modifier.
pub fn parseEnumDeclaration(parser: *Parser, is_declare: bool, is_const: bool, decl_start: u32) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"enum");

    const start = if (is_declare or is_const) decl_start else parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'enum'

    const id = try literals.parseBindingIdentifier(parser) orelse return null;
    const body = try parseEnumBody(parser) orelse return null;

    return try parser.tree.createNode(
        .{ .ts_enum_declaration = .{
            .id = id,
            .body = body,
            .is_const = is_const,
            .declare = is_declare,
        } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

/// `{ A, B = 1, }` body of an enum declaration. produces a `TSEnumBody`
/// wrapping the member list. members are comma separated with an optional
/// trailing comma.
fn parseEnumBody(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .left_brace) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '{' to start an enum body",
            .{ .help = "An enum body is written '{ A, B = 1 }'" },
        );
        return null;
    }

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '{'

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .right_brace and parser.current_token.tag != .eof) {
        const member = try parseEnumMember(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), member);

        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return null; // consume ','
    }

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close an enum body",
        "Each '{' in an enum must be matched by a '}'",
    )) return null;

    const members = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);

    return try parser.tree.createNode(
        .{ .ts_enum_body = .{ .members = members } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

/// one member of an enum body, a name (identifier, string, or template) with
/// an optional `= expr` initializer. the name may also appear inside `[...]`
/// to form a computed member.
fn parseEnumMember(parser: *Parser) Error!?ast.NodeIndex {
    const name = try parseEnumMemberName(parser) orelse return null;
    const start = parser.tree.getSpan(name.id).start;
    var end = parser.tree.getSpan(name.id).end;
    if (name.computed) end = parser.prev_token_end;

    var initializer: ast.NodeIndex = .null;
    if (parser.current_token.tag == .assign) {
        try parser.advance() orelse return null; // consume '='
        initializer = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        end = parser.tree.getSpan(initializer).end;
    }

    return try parser.tree.createNode(
        .{ .ts_enum_member = .{
            .id = name.id,
            .initializer = initializer,
            .computed = name.computed,
        } },
        .{ .start = start, .end = end },
    );
}

const EnumMemberName = struct {
    id: ast.NodeIndex,
    computed: bool,
};

/// identifier, string literal, no-substitution template literal, or a
/// computed `[stringLit]` / `[noSubTemplate]` wrapper.
fn parseEnumMemberName(parser: *Parser) Error!?EnumMemberName {
    const tag = parser.current_token.tag;

    if (tag == .left_bracket) {
        try parser.advance() orelse return null; // consume '['
        const inner = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        if (!try parser.expect(
            .right_bracket,
            "Expected ']' to close a computed enum member name",
            "A computed enum member name is written '[\"name\"]'",
        )) return null;
        return .{ .id = inner, .computed = true };
    }

    if (tag.isIdentifierLike()) {
        const id = try literals.parseIdentifierName(parser) orelse return null;
        return .{ .id = id, .computed = false };
    }

    if (tag == .string_literal) {
        const id = try literals.parseStringLiteral(parser) orelse return null;
        return .{ .id = id, .computed = false };
    }

    if (tag == .no_substitution_template) {
        const id = try literals.parseNoSubstitutionTemplate(parser, false) orelse return null;
        return .{ .id = id, .computed = false };
    }

    try parser.report(
        parser.current_token.span,
        try parser.fmt("Unexpected token '{s}' as enum member name", .{parser.describeToken(parser.current_token)}),
        .{ .help = "Enum member names must be identifiers, string literals, or computed expressions '[name]'." },
    );
    return null;
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
