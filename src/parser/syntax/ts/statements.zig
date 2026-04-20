const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const Precedence = @import("../../token.zig").Precedence;
const Token = @import("../../token.zig").Token;
const expressions = @import("../expressions.zig");
const literals = @import("../literals.zig");
const variables = @import("../variables.zig");
const functions = @import("../functions.zig");
const class = @import("../class.zig");
const ts_types = @import("types.zig");

/// modifier keywords eaten by `parseTsDeclaration` before dispatching to
/// the matching declaration parser. `is_const` is only meaningful on enums.
pub const Modifiers = struct {
    declare: bool = false,
    is_const: bool = false,
};

pub fn isStartOfTsDeclaration(parser: *Parser) Error!?bool {
    if (!parser.tree.isTs()) return false;

    const peek = try parser.peekAheadN(3);
    var cur = parser.current_token;
    var idx: usize = 0;
    var has_declare = false;

    // skip 'declare' modifier
    if (cur.tag == .declare) {
        cur = peek[idx] orelse return null;
        if (cur.hasLineTerminatorBefore()) return false;
        idx += 1;
        has_declare = true;
    }

    // `const enum`, `declare const enum`, and `declare const <binding>` all
    // start here. skip the `const` kind keyword for the enum forms so the
    // switch below dispatches on `enum`. plain `declare const x` short-circuits
    // here since there is no further routing needed beyond the binding check.
    if (cur.tag == .@"const") {
        const next = peek[idx] orelse return null;
        if (isConstEnumHead(next)) {
            cur = next;
            idx += 1;
        } else if (has_declare) {
            return !next.hasLineTerminatorBefore() and variables.canStartBinding(next.tag);
        } else {
            return false;
        }
    }

    switch (cur.tag) {
        .type, .interface, .@"enum", .namespace => {
            const name = peek[idx] orelse return null;
            return name.tag.isIdentifierLike() and !name.hasLineTerminatorBefore();
        },
        .module => {
            // `module` accepts an identifier name (deprecated namespace form)
            // or a string literal (ambient external module).
            const name = peek[idx] orelse return null;
            if (name.hasLineTerminatorBefore()) return false;
            return name.tag.isIdentifierLike() or name.tag == .string_literal;
        },
        .global => {
            // `global { ... }` is only a module augmentation under `declare`.
            if (!has_declare) return false;
            const next = peek[idx] orelse return null;
            return next.tag == .left_brace and !next.hasLineTerminatorBefore();
        },
        // every ambient binding form: `declare var/let/function/class` plus
        // the destructuring variants of `declare var/let`. the non-ambient
        // forms are handled by the regular statement dispatch.
        .@"var", .let, .function, .class => {
            if (!has_declare) return false;
            const name = peek[idx] orelse return null;
            if (name.hasLineTerminatorBefore()) return false;
            return switch (cur.tag) {
                .@"var", .let => variables.canStartBinding(name.tag),
                else => name.tag.isIdentifierLike(),
            };
        },
        else => return false,
    }
}

/// `const enum` and `declare const enum` share a two-token head. collapsing
/// the check lets `isStartOfTsDeclaration` and `parseTsDeclaration` agree on
/// the predicate without duplicating the newline rule.
fn isConstEnumHead(after_const: Token) bool {
    return after_const.tag == .@"enum" and !after_const.hasLineTerminatorBefore();
}

pub fn parseTsDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var mods: Modifiers = .{};

    if (parser.current_token.tag == .declare) {
        mods.declare = true;
        try parser.advance() orelse return null;
    }
    if (parser.current_token.tag == .@"const") {
        const next = try parser.peekAhead() orelse return null;
        if (isConstEnumHead(next)) {
            mods.is_const = true;
            try parser.advance() orelse return null;
        }
    }

    return switch (parser.current_token.tag) {
        .type => parseTypeAliasDeclaration(parser, mods, start),
        .interface => parseInterfaceDeclaration(parser, mods, start),
        .@"enum" => parseEnumDeclaration(parser, mods, start),
        .namespace => parseModuleDeclaration(parser, mods, start, .namespace),
        .module => parseModuleDeclaration(parser, mods, start, .module),
        .global => parseGlobalDeclaration(parser, mods, start),
        .@"var", .let, .@"const" => variables.parseVariableDeclaration(
            parser,
            .{ .is_declare = mods.declare },
            start,
        ),
        .function => functions.parseFunction(parser, .{ .is_declare = mods.declare }, start),
        .class => class.parseClass(parser, .{ .is_declare = mods.declare }, start),
        else => unreachable,
    };
}

/// `type Foo<T> = Bar<T>;` or the `declare`-prefixed ambient form.
pub fn parseTypeAliasDeclaration(parser: *Parser, mods: Modifiers, start: u32) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .type);
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
            .declare = mods.declare,
        } },
        .{ .start = start, .end = end },
    );
}

/// `interface Foo<T> extends Bar, Baz<U> { ... }` or the `declare`-prefixed
/// ambient form.
pub fn parseInterfaceDeclaration(parser: *Parser, mods: Modifiers, start: u32) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .interface);
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
            .declare = mods.declare,
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

/// `enum Foo { A, B = 1 }`, `const enum ...`, `declare enum ...`, or
/// `declare const enum ...`.
pub fn parseEnumDeclaration(parser: *Parser, mods: Modifiers, start: u32) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"enum");
    try parser.advance() orelse return null; // consume 'enum'

    const id = try literals.parseBindingIdentifier(parser) orelse return null;
    const body = try parseEnumBody(parser) orelse return null;

    return try parser.tree.createNode(
        .{ .ts_enum_declaration = .{
            .id = id,
            .body = body,
            .is_const = mods.is_const,
            .declare = mods.declare,
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

/// `namespace Foo { ... }`, `namespace A.B.C { ... }`, `module "./mod" { ... }`,
/// and their `declare`-prefixed ambient forms. the caller has already eaten
/// any `declare` modifier into `mods` and captured the leading `start` offset.
pub fn parseModuleDeclaration(
    parser: *Parser,
    mods: Modifiers,
    start: u32,
    kind: ast.TSModuleDeclarationKind,
) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume 'namespace' or 'module'

    const id: ast.NodeIndex = blk: {
        if (kind == .module and parser.current_token.tag == .string_literal) {
            break :blk try literals.parseStringLiteral(parser) orelse return null;
        }
        break :blk try parseModuleName(parser) orelse return null;
    };

    const body = try parseOptionalModuleBlock(parser) orelse return null;
    const end = if (body == .null)
        try parser.eatSemicolon(parser.tree.getSpan(id).end) orelse return null
    else
        parser.prev_token_end;

    return try parser.tree.createNode(
        .{ .ts_module_declaration = .{
            .id = id,
            .body = body,
            .kind = kind,
            .declare = mods.declare,
        } },
        .{ .start = start, .end = end },
    );
}

/// `declare global { ... }` augmentation. the caller has already eaten the
/// `declare` modifier into `mods` and captured the leading `start` offset.
pub fn parseGlobalDeclaration(parser: *Parser, mods: Modifiers, start: u32) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .global);

    const id = try literals.parseIdentifierName(parser) orelse return null;
    const body = try parseModuleBlock(parser) orelse return null;

    return try parser.tree.createNode(
        .{ .ts_global_declaration = .{
            .id = id,
            .body = body,
            .declare = mods.declare,
        } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

/// a `BindingIdentifier` or a left-associative `TSQualifiedName` chain formed
/// by dotted identifier parts. `namespace A.B.C { ... }` is sugar for a single
/// module declaration whose name is `TSQualifiedName(TSQualifiedName(A, B), C)`.
fn parseModuleName(parser: *Parser) Error!?ast.NodeIndex {
    var name = try literals.parseBindingIdentifier(parser) orelse return null;

    while (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null; // consume '.'
        const right = try literals.parseIdentifierName(parser) orelse return null;
        const left_start = parser.tree.getSpan(name).start;
        const right_end = parser.tree.getSpan(right).end;
        name = try parser.tree.createNode(
            .{ .ts_qualified_name = .{ .left = name, .right = right } },
            .{ .start = left_start, .end = right_end },
        );
    }

    return name;
}

/// an optional module body. returns `.null` when no `{` follows (forward
/// declaration forms like `declare module "foo";`). otherwise delegates to
/// `parseModuleBlock`.
fn parseOptionalModuleBlock(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .left_brace) return .null;
    return parseModuleBlock(parser);
}

/// `{ <statements> }` body of a module, namespace, or global declaration.
/// produces a `TSModuleBlock` whose `body` is a range of regular statements.
fn parseModuleBlock(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (!try parser.expect(
        .left_brace,
        "Expected '{' to start a module body",
        "A module body is written '{ <statements> }'",
    )) return null;

    const body = try parser.parseBody(.right_brace, .module_block);

    const end = parser.current_token.span.end;
    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close a module body",
        "Each '{' in a module must be matched by a '}'",
    )) return null;

    return try parser.tree.createNode(
        .{ .ts_module_block = .{ .body = body } },
        .{ .start = start, .end = end },
    );
}
