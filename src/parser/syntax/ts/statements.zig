const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const Precedence = @import("../../token.zig").Precedence;
const Token = @import("../../token.zig").Token;
const TokenTag = @import("../../token.zig").TokenTag;
const expressions = @import("../expressions.zig");
const literals = @import("../literals.zig");
const variables = @import("../variables.zig");
const functions = @import("../functions.zig");
const class = @import("../class.zig");
const ts_types = @import("types.zig");
const ts_signatures = @import("signatures.zig");

/// `is_const` only matters on enums.
pub const Modifiers = struct {
    declare: bool = false,
    abstract: bool = false,
    is_const: bool = false,
};

pub fn isStartOfTsDeclaration(parser: *Parser) Error!?bool {
    if (!parser.tree.isTs()) return false;

    const peek = try parser.peekAheadN(3);
    var cur = parser.current_token;
    var idx: usize = 0;
    var has_declare = false;
    var has_abstract = false;

    if (cur.tag == .declare) {
        cur = peek[idx] orelse return null;
        if (cur.hasLineTerminatorBefore()) return false;
        idx += 1;
        has_declare = true;
    }

    // `abstract` only precedes `class` here.
    if (cur.tag == .abstract) {
        const next = peek[idx] orelse return null;
        if (next.hasLineTerminatorBefore()) return false;
        if (next.tag != .class) return false;
        cur = next;
        idx += 1;
        has_abstract = true;
    }

    // `const enum`, `declare const enum`, `declare const <binding>`.
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
            // a reserved-word follower (e.g. `module in {}`) is not a
            // valid declaration name, so the head falls through to
            // expression parsing.
            const name = peek[idx] orelse return null;
            return isDeclarationName(name);
        },
        .module => {
            // identifier (namespace form) or string (ambient module).
            const name = peek[idx] orelse return null;
            if (name.hasLineTerminatorBefore()) return false;
            return isDeclarationName(name) or name.tag == .string_literal;
        },
        .global => {
            // `global { ... }` and `declare global { ... }`. a same-line
            // `{` is the only thing that gates it.
            const next = peek[idx] orelse return null;
            return next.tag == .left_brace and !next.hasLineTerminatorBefore();
        },
        // ambient `declare var/let/function/class` and `abstract class`.
        .@"var", .let, .function, .class => {
            if (!has_declare and !has_abstract) return false;
            const name = peek[idx] orelse return null;
            if (name.hasLineTerminatorBefore()) return false;
            return switch (cur.tag) {
                .@"var", .let => variables.canStartBinding(name.tag),
                else => isDeclarationName(name),
            };
        },
        // `declare async function f()` only.
        .async => {
            if (!has_declare) return false;
            const fn_token = peek[idx] orelse return null;
            if (fn_token.tag != .function or fn_token.hasLineTerminatorBefore()) return false;
            const name = peek[idx + 1] orelse return null;
            return isDeclarationName(name);
        },
        // `declare import x = ...`. only the equals form is valid here;
        // a regular `declare import x from "m"` is not a declaration head.
        .import => {
            if (!has_declare) return false;
            const name = peek[idx] orelse return null;
            if (!isDeclarationName(name)) return false;
            const eq = peek[idx + 1] orelse return null;
            return eq.tag == .assign and !eq.hasLineTerminatorBefore();
        },
        else => return false,
    }
}

fn isConstEnumHead(after_const: Token) bool {
    return after_const.tag == .@"enum" and !after_const.hasLineTerminatorBefore();
}

/// the token following a TS declaration head must be a same-line, non-reserved
/// identifier-like name. rejects `module in {}` and similar binding-name
/// false-positives so they fall through to expression parsing.
fn isDeclarationName(token: Token) bool {
    return token.tag.isIdentifierLike() and
        !token.tag.isUnconditionallyReserved() and
        !token.hasLineTerminatorBefore();
}

pub fn parseTsDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var mods: Modifiers = .{};

    if (parser.current_token.tag == .declare) {
        mods.declare = true;
        try parser.advance() orelse return null;
    }
    if (parser.current_token.tag == .abstract) {
        mods.abstract = true;
        try parser.advance() orelse return null;
    }
    if (parser.current_token.tag == .@"const") {
        const next = try parser.peekAhead() orelse return null;
        if (isConstEnumHead(next)) {
            mods.is_const = true;
            try parser.advance() orelse return null;
        }
    }

    const saved_ambient = parser.context.ambient;
    defer parser.context.ambient = saved_ambient;
    if (mods.declare) parser.context.ambient = true;

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
        .async => blk: {
            try parser.advance() orelse break :blk null; // consume 'async'
            break :blk functions.parseFunction(
                parser,
                .{ .is_declare = mods.declare, .is_async = true },
                start,
            );
        },
        .class => class.parseClass(
            parser,
            .{ .is_declare = mods.declare, .is_abstract = mods.abstract },
            start,
        ),
        .import => parseDeclareImportEquals(parser, start),
        else => unreachable,
    };
}

/// `declare import x = Foo.Bar` and `declare import x = require("m")`.
/// inline here because `modules.zig` already imports this file, and the
/// shape is otherwise identical to the `import x = ...` declaration.
fn parseDeclareImportEquals(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume 'import'

    const id = try literals.parseBindingIdentifier(parser) orelse return null;

    if (!try parser.expect(
        .assign,
        "Expected '=' after 'declare import' name",
        "A declare import equals declaration is written 'declare import x = Foo.Bar'",
    )) return null;

    const module_reference = try parseImportEqualsTarget(parser) orelse return null;
    const end = try parser.eatSemicolon(parser.tree.getSpan(module_reference).end) orelse return null;

    return try parser.tree.createNode(.{
        .ts_import_equals_declaration = .{
            .id = id,
            .module_reference = module_reference,
            .import_kind = .value,
        },
    }, .{ .start = start, .end = end });
}

/// `require("m")` or a dotted entity name on the RHS of `import x = ...`.
fn parseImportEqualsTarget(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .require) {
        const next = try parser.peekAhead() orelse return null;
        if (next.tag == .left_paren) return parseExternalModuleReference(parser);
    }

    const head = try literals.parseIdentifier(parser) orelse return null;
    return ts_types.extendQualifiedName(parser, head);
}

/// `require("m")` on the RHS of an `import x = ...` declaration.
fn parseExternalModuleReference(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'require'

    if (!try parser.expect(
        .left_paren,
        "Expected '(' after 'require'",
        "External module references are written 'require(\"module\")'",
    )) return null;

    const expression = try literals.parseStringLiteral(parser) orelse return null;

    const end = parser.current_token.span.end;
    if (!try parser.expect(.right_paren, "Expected ')' to close 'require'", null)) return null;

    return try parser.tree.createNode(.{
        .ts_external_module_reference = .{ .expression = expression },
    }, .{ .start = start, .end = end });
}

/// type Foo<T> = Bar<T>
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

    const type_annotation = try ts_types.parseTypeAliasBody(parser) orelse return null;
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

/// interface Foo<T> extends Bar, Baz<U> { ... }
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

const HeritageKind = enum { interface, class };

/// `extends ...` for interfaces or `implements ...` for classes.
/// returns an empty range when the keyword isn't there.
fn parseHeritageClause(
    parser: *Parser,
    comptime keyword: TokenTag,
    comptime kind: HeritageKind,
) Error!?ast.IndexRange {
    if (parser.current_token.tag != keyword) return .empty;
    try parser.advance() orelse return null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (true) {
        const entry = try parseHeritageEntry(parser, kind) orelse return null;
        try parser.scratch_a.append(parser.allocator(), entry);
        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return null;
    }

    return try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);
}

/// Bar    Foo.Bar    Foo.Bar<U>
fn parseHeritageEntry(parser: *Parser, comptime kind: HeritageKind) Error!?ast.NodeIndex {
    const expression = try parseHeritageExpression(parser) orelse return null;
    const type_arguments = try ts_types.parseTypeArguments(parser);

    const start = parser.tree.getSpan(expression).start;
    const end = if (type_arguments != .null)
        parser.tree.getSpan(type_arguments).end
    else
        parser.tree.getSpan(expression).end;

    const data: ast.NodeData = switch (kind) {
        .interface => .{ .ts_interface_heritage = .{
            .expression = expression,
            .type_arguments = type_arguments,
        } },
        .class => .{ .ts_class_implements = .{
            .expression = expression,
            .type_arguments = type_arguments,
        } },
    };

    return try parser.tree.createNode(data, .{ .start = start, .end = end });
}

inline fn parseInterfaceExtendsClause(parser: *Parser) Error!?ast.IndexRange {
    return parseHeritageClause(parser, .extends, .interface);
}

pub inline fn parseImplementsClause(parser: *Parser) Error!?ast.IndexRange {
    return parseHeritageClause(parser, .implements, .class);
}

/// `Foo.Bar.Baz` as a left-leaning `MemberExpression` chain. no calls,
/// no computed access, no optional chaining.
fn parseHeritageExpression(parser: *Parser) Error!?ast.NodeIndex {
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

/// enum Foo { A, B = 1 }
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

/// { A, B = 1, }
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

/// name [= initializer]
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

/// identifier, string, template, or `[expr]`.
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

/// `{ ... }` body of an interface.
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
    const members = try ts_signatures.parseObjectTypeMembers(parser) orelse return null;

    return try parser.tree.createNode(
        .{ .ts_interface_body = .{ .body = members } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

/// namespace Foo { ... }    namespace A.B.C { ... }    module "./m" { ... }
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

/// `global { ... }` and `declare global { ... }` global augmentations.
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

/// `Foo` or `A.B.C` as a left-leaning `TSQualifiedName` chain.
fn parseModuleName(parser: *Parser) Error!?ast.NodeIndex {
    const head = try literals.parseBindingIdentifier(parser) orelse return null;
    return ts_types.extendQualifiedName(parser, head);
}

/// returns `.null` when no `{` follows, e.g. `declare module "foo"`.
fn parseOptionalModuleBlock(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .left_brace) return .null;
    return parseModuleBlock(parser);
}

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
