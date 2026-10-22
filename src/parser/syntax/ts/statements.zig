const std = @import("std");
const spec = @import("spec");
const ast = @import("../../ast.zig");
const static_semantics = @import("spec").static_semantics;
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const Token = @import("../../token.zig").Token;
const TokenTag = @import("../../token.zig").TokenTag;
const Precedence = @import("../../token.zig").Precedence;

const literals = @import("../literals.zig");
const functions = @import("../functions.zig");
const expressions = @import("../expressions.zig");
const variables = @import("../variables.zig");
const class = @import("../class.zig");

const types = @import("types.zig");

/// Declaration modifiers. `is_const` only applies to enums.
pub const Modifiers = struct {
    declare: bool = false,
    abstract: bool = false,
    is_const: bool = false,
};

pub fn isStartOfTsDeclaration(parser: *Parser) bool {
    if (!parser.tree.isTs()) return false;

    var peek = parser.beginPeek();
    defer peek.end();

    var token = parser.current_token;
    var has_declare = false;
    var has_abstract = false;

    if (token.tag == .declare) {
        token = peek.next();
        if (token.hasLineTerminatorBefore()) return false;
        has_declare = true;
    }

    if (token.tag == .abstract) {
        token = peek.next();
        if (token.hasLineTerminatorBefore()) return false;
        // `abstract declare class` as well as `declare abstract class`
        if (token.tag == .declare and !has_declare) {
            token = peek.next();
            if (token.hasLineTerminatorBefore()) return false;
            has_declare = true;
        }
        if (token.tag != .class) return false;
        has_abstract = true;
    }

    switch (token.tag) {
        .@"const" => return has_declare or isConstEnumHead(peek.next()),
        .@"enum" => return true,
        .type, .interface, .namespace => return isDeclarationName(peek.next()),
        .module => {
            const name = peek.next();
            if (name.hasLineTerminatorBefore()) return false;
            return isDeclarationName(name) or name.tag == .string_literal;
        },
        .global => return peek.next().tag == .left_brace,
        .class => return has_declare or has_abstract,
        .function, .@"var", .let => return has_declare,
        .async => {
            if (!has_declare) return false;
            const function_token = peek.next();
            if (function_token.tag != .function) return false;
            if (function_token.hasLineTerminatorBefore()) return false;
            return isDeclarationName(peek.next());
        },
        .import => {
            if (!has_declare) return false;
            const name = peek.next();
            if (!isDeclarationName(name)) return false;
            const eq = peek.next();
            return eq.tag == .assign and !eq.hasLineTerminatorBefore();
        },
        else => return false,
    }
}

pub fn isConstEnumHead(after_const: Token) bool {
    return after_const.tag == .@"enum";
}

fn isDeclarationName(token: Token) bool {
    return token.tag.isIdentifierLike() and
        !token.tag.isUnconditionallyReserved() and
        !token.hasLineTerminatorBefore();
}

pub fn parseTsDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var modifiers: Modifiers = .{};

    if (parser.current_token.tag == .declare) {
        modifiers.declare = true;
        try parser.advance() orelse return null;
    }
    if (parser.current_token.tag == .abstract) {
        modifiers.abstract = true;
        try parser.advance() orelse return null;
        if (parser.current_token.tag == .declare and !modifiers.declare) {
            modifiers.declare = true;
            try parser.advance() orelse return null;
        }
    }
    if (parser.current_token.tag == .@"const" and isConstEnumHead(parser.peekAhead())) {
        modifiers.is_const = true;
        try parser.advance() orelse return null;
    }

    const saved_ambient = parser.ts_context.ambient;
    defer parser.ts_context.ambient = saved_ambient;
    if (modifiers.declare) parser.ts_context.ambient = true;

    return switch (parser.current_token.tag) {
        .type => parseTypeAliasDeclaration(parser, modifiers, start),
        .interface => parseInterfaceDeclaration(parser, modifiers, start),
        .@"enum" => parseEnumDeclaration(parser, modifiers, start),
        .namespace => parseModuleDeclaration(parser, modifiers, start, .namespace),
        .module => parseModuleDeclaration(parser, modifiers, start, .module),
        .global => parseGlobalDeclaration(parser, modifiers, start),
        .@"var", .let, .@"const" => variables.parseVariableDeclaration(
            parser,
            .{ .is_declare = modifiers.declare },
            start,
        ),
        .function => functions.parseFunction(parser, .{ .is_declare = modifiers.declare }, start),
        .async => {
            try parser.advance() orelse return null;
            return functions.parseFunction(
                parser,
                .{ .is_declare = modifiers.declare, .is_async = true },
                start,
            );
        },
        .class => class.parseClass(
            parser,
            .{ .is_declare = modifiers.declare, .is_abstract = modifiers.abstract },
            start,
        ),
        .import => {
            try parser.advance() orelse return null;
            return parseImportEqualsBody(parser, start, .value);
        },
        else => unreachable,
    };
}

// caller already consumed `import`
pub fn parseImportEqualsBody(
    parser: *Parser,
    start: u32,
    import_kind: ast.ImportOrExportKind,
) Error!?ast.NodeIndex {
    const id = try literals.parseBindingIdentifier(parser) orelse return null;

    if (!try parser.expect(
        .assign,
        "Expected '=' in import equals declaration",
        "An import equals declaration is written 'import x = Foo.Bar'" ++
            " or 'import x = require(\"m\")'",
    )) return null;

    const module_reference = try parseModuleReference(parser) orelse return null;

    // `import type x = require("m")` is legal, an alias of a local entity has nothing type-only
    if (import_kind == .type and
        parser.tree.data(module_reference) != .ts_external_module_reference)
    {
        try parser.report(
            .{ .start = start, .end = parser.tree.span(module_reference).end },
            "An import alias cannot use 'import type'",
            .{ .help = "Drop 'type', or import the type with 'import type x from \"m\"'." },
        );
    }

    const end = try parser.eatSemicolon(parser.tree.span(module_reference).end) orelse return null;

    return try parser.tree.addNode(.{ .ts_import_equals_declaration = .{
        .id = id,
        .module_reference = module_reference,
        .import_kind = import_kind,
    } }, .{ .start = start, .end = end });
}

fn parseModuleReference(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .require and parser.peekAhead().tag == .left_paren) {
        return parseExternalModuleReference(parser);
    }
    const head = try literals.parseIdentifier(parser) orelse return null;
    return types.extendQualifiedName(parser, head);
}

fn parseExternalModuleReference(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    if (!try parser.expect(
        .left_paren,
        "Expected '(' after 'require'",
        "External module references are written 'require(\"module\")'",
    )) return null;

    const expression = try literals.parseStringLiteral(parser) orelse return null;

    const end = parser.current_token.span.end;
    if (!try parser.expect(.right_paren, "Expected ')' to close 'require'", null)) return null;

    return try parser.tree.addNode(.{
        .ts_external_module_reference = .{ .expression = expression },
    }, .{ .start = start, .end = end });
}

fn parseTypeAliasDeclaration(
    parser: *Parser,
    modifiers: Modifiers,
    start: u32,
) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .type);
    try parser.advance() orelse return null;

    const id = try literals.parseBindingIdentifier(parser) orelse return null;
    const type_parameters = try types.parseTypeParameters(parser);

    if (!try parser.expect(
        .assign,
        "Expected '=' in type alias declaration",
        "A type alias is written 'type Foo = Bar'",
    )) return null;

    const type_annotation = try types.parseTypeAliasBody(parser) orelse return null;
    const end = try parser.eatSemicolon(parser.tree.span(type_annotation).end) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_type_alias_declaration = .{
            .id = id,
            .type_parameters = type_parameters,
            .type_annotation = type_annotation,
            .declare = modifiers.declare,
        } },
        .{ .start = start, .end = end },
    );
}

pub fn parseInterfaceDeclaration(
    parser: *Parser,
    modifiers: Modifiers,
    start: u32,
) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .interface);
    try parser.advance() orelse return null;

    const id = try literals.parseBindingIdentifier(parser) orelse return null;
    const type_parameters = try types.parseTypeParameters(parser);
    const extends = try parseHeritageClause(parser, .extends, .ts_interface_heritage) orelse
        return null;
    const body = try parseInterfaceBody(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_interface_declaration = .{
            .id = id,
            .type_parameters = type_parameters,
            .extends = extends,
            .body = body,
            .declare = modifiers.declare,
        } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

pub fn parseHeritageClause(
    parser: *Parser,
    comptime keyword: TokenTag,
    comptime tag: std.meta.Tag(ast.NodeData),
) Error!?ast.IndexRange {
    if (parser.current_token.tag != keyword) return .empty;
    try parser.advance() orelse return null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (true) {
        const expression = try parseHeritageExpression(parser) orelse return null;
        const type_arguments = try types.parseTypeArguments(parser);
        const expression_span = parser.tree.span(expression);
        const end = if (type_arguments != .null)
            parser.tree.span(type_arguments).end
        else
            expression_span.end;
        const entry = try parser.tree.addNode(@unionInit(ast.NodeData, @tagName(tag), .{
            .expression = expression,
            .type_arguments = type_arguments,
        }), .{ .start = expression_span.start, .end = end });
        try parser.scratch_a.append(parser.allocator(), entry);

        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return null;
    }

    return try parser.flushToExtras(&parser.scratch_a, checkpoint);
}

fn parseHeritageExpression(parser: *Parser) Error!?ast.NodeIndex {
    var expression = if (parser.current_token.tag == .this) blk: {
        const span = parser.current_token.span;
        try parser.advance() orelse return null;
        break :blk try parser.tree.addNode(.{ .this_expression = .{} }, span);
    } else try literals.parseIdentifier(parser) orelse return null;

    while (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null;
        const property = try literals.parseIdentifierName(parser) orelse return null;
        expression = try parser.tree.addNode(
            .{ .member_expression = .{
                .object = expression,
                .property = property,
                .computed = false,
                .optional = false,
            } },
            .{
                .start = parser.tree.span(expression).start,
                .end = parser.tree.span(property).end,
            },
        );
    }

    return expression;
}

fn parseEnumDeclaration(parser: *Parser, modifiers: Modifiers, start: u32) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .@"enum");
    try parser.advance() orelse return null;

    const id = try literals.parseBindingIdentifier(parser) orelse return null;
    const body = try parseEnumBody(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_enum_declaration = .{
            .id = id,
            .body = body,
            .is_const = modifiers.is_const,
            .declare = modifiers.declare,
        } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

fn parseEnumBody(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (!try parser.expect(
        .left_brace,
        "Expected '{' to start an enum body",
        "An enum body is written '{ A, B = 1 }'",
    )) return null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .right_brace and parser.current_token.tag != .eof) {
        const member = try parseEnumMember(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), member);

        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return null;
    }

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close an enum body",
        "Each '{' in an enum must be matched by a '}'",
    )) return null;

    const members = try parser.flushToExtras(&parser.scratch_a, checkpoint);
    return try parser.tree.addNode(
        .{ .ts_enum_body = .{ .members = members } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

fn parseEnumMember(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const id, const computed = try parseEnumMemberName(parser) orelse return null;
    const id_span = parser.tree.span(id);
    var end = if (computed) parser.prev_token_end else id_span.end;

    var initializer: ast.NodeIndex = .null;
    if (parser.current_token.tag == .assign) {
        try parser.advance() orelse return null;
        initializer = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
            return null;
        end = parser.tree.span(initializer).end;
    }

    std.debug.assert(start <= id_span.start);
    if (!computed) std.debug.assert(start == id_span.start);
    return try parser.tree.addNode(
        .{ .ts_enum_member = .{ .id = id, .initializer = initializer, .computed = computed } },
        .{ .start = start, .end = end },
    );
}

fn parseEnumMemberName(parser: *Parser) Error!?struct { ast.NodeIndex, bool } {
    const tag = parser.current_token.tag;

    if (tag == .left_bracket) {
        const start = parser.current_token.span.start;
        try parser.advance() orelse return null;
        const parenthesized = parser.current_token.tag == .left_paren;
        const inner = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
            return null;
        if (!try parser.expect(
            .right_bracket,
            "Expected ']' to close an enum member name",
            "Each '[' in an enum member name must be matched by a ']'",
        )) return null;

        const span: ast.Span = .{ .start = start, .end = parser.prev_token_end };
        switch (parseEnumMemberNameKind(&parser.tree, inner, parenthesized)) {
            .name => {},
            .numeric => try reportEnumMemberNumericName(parser, span),
            .computed => try parser.report(
                span,
                "Computed property names are not allowed in enums",
                .{ .help = "Name the member with an identifier or a string literal." },
            ),
        }
        return .{ inner, true };
    }

    if (tag.isIdentifierLike()) {
        const id = try literals.parseIdentifierName(parser) orelse return null;
        return .{ id, false };
    }
    if (tag == .string_literal) {
        const id = try literals.parseStringLiteral(parser) orelse return null;
        const name = parser.tree.string(parser.tree.data(id).string_literal.value);
        if (spec.number.isCanonicalString(name)) {
            try reportEnumMemberNumericName(parser, parser.tree.span(id));
        }
        return .{ id, false };
    }
    if (tag == .no_substitution_template) {
        const id = try literals.parseNoSubstitutionTemplate(parser, false) orelse return null;
        try parser.report(
            parser.tree.span(id),
            "Enum member names cannot be template literals",
            .{ .help = "Use a string literal, or put the template in brackets." },
        );
        return .{ id, false };
    }
    if (tag.isNumericLiteral()) {
        try reportEnumMemberNumericName(parser, parser.current_token.span);
        return null;
    }

    try parser.report(
        parser.current_token.span,
        try parser.fmt(
            "Unexpected token '{s}' as enum member name",
            .{parser.describeToken(parser.current_token)},
        ),
        .{ .help = "Enum member names must be identifiers or string literals." },
    );
    return null;
}

const EnumMemberNameKind = enum { name, numeric, computed };

fn parseEnumMemberNameKind(
    tree: *const ast.Tree,
    expression: ast.NodeIndex,
    parenthesized: bool,
) EnumMemberNameKind {
    std.debug.assert(expression != .null);
    if (parenthesized) return .computed;
    if (static_semantics.stringLiteralLike(tree, expression)) |name| {
        return if (spec.number.isCanonicalString(tree.string(name))) .numeric else .name;
    }
    return switch (tree.data(expression)) {
        .numeric_literal => |literal| {
            return if (std.math.isFinite(literal.value(tree))) .numeric else .name;
        },
        else => .computed,
    };
}

fn reportEnumMemberNumericName(parser: *Parser, span: ast.Span) Error!void {
    std.debug.assert(span.start < span.end);
    try parser.report(span, "An enum member cannot have a numeric name", .{
        .help = "Name the member with an identifier or a string that is not a number.",
    });
}

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
    const members = try types.parseObjectTypeMembers(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_interface_body = .{ .body = members } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

fn parseModuleDeclaration(
    parser: *Parser,
    modifiers: Modifiers,
    start: u32,
    kind: ast.TSModuleDeclarationKind,
) Error!?ast.NodeIndex {
    try parser.advance() orelse return null;

    const id = if (kind == .module and parser.current_token.tag == .string_literal)
        try literals.parseStringLiteral(parser) orelse return null
    else
        try parseModuleName(parser) orelse return null;

    const body: ast.NodeIndex = if (parser.current_token.tag == .left_brace)
        try parseModuleBlock(parser) orelse return null
    else
        .null;

    // only an ambient `declare module "m"` may omit its body
    if (body == .null and parser.tree.data(id) != .string_literal) {
        try parser.report(
            .{ .start = start, .end = parser.tree.span(id).end },
            switch (kind) {
                .namespace => "A namespace declaration requires a body",
                .module => "A module declaration requires a body",
            },
            .{ .help = "Add a '{ }' body after the name." },
        );
    }

    const end = if (body == .null)
        try parser.eatSemicolon(parser.tree.span(id).end) orelse return null
    else
        parser.prev_token_end;

    return try parser.tree.addNode(
        .{ .ts_module_declaration = .{
            .id = id,
            .body = body,
            .kind = kind,
            .declare = modifiers.declare,
        } },
        .{ .start = start, .end = end },
    );
}

fn parseGlobalDeclaration(parser: *Parser, modifiers: Modifiers, start: u32) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .global);

    const id = try literals.parseIdentifierName(parser) orelse return null;
    const body = try parseModuleBlock(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_global_declaration = .{ .id = id, .body = body, .declare = modifiers.declare } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

fn parseModuleName(parser: *Parser) Error!?ast.NodeIndex {
    const head = try literals.parseBindingIdentifier(parser) orelse return null;
    return types.extendQualifiedName(parser, head);
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

    return try parser.tree.addNode(
        .{ .ts_module_block = .{ .body = body } },
        .{ .start = start, .end = end },
    );
}
