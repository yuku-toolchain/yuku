const std = @import("std");
const spec = @import("spec");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const Token = @import("../../token.zig").Token;
const TokenTag = @import("../../token.zig").TokenTag;

const literals = @import("../literals.zig");
const functions = @import("../functions.zig");
const expressions = @import("../expressions.zig");
const variables = @import("../variables.zig");
const class = @import("../class.zig");
const types = @import("types.zig");

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
            return isDeclarationName(name) or
                (name.tag == .string_literal and !name.hasLineTerminatorBefore());
        },
        .global => return peek.next().tag == .left_brace,
        .class => return has_declare or has_abstract,
        .function, .@"var", .let => return has_declare,
        .async => {
            if (!has_declare) return false;
            const function = peek.next();
            return function.tag == .function and !function.hasLineTerminatorBefore() and
                isDeclarationName(peek.next());
        },
        .import => {
            if (!has_declare or !isDeclarationName(peek.next())) return false;
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
    modifiers.declare = try parser.eat(.declare) orelse return null;
    modifiers.abstract = try parser.eat(.abstract) orelse return null;
    if (modifiers.abstract and !modifiers.declare) {
        modifiers.declare = try parser.eat(.declare) orelse return null;
    }
    if (parser.at(.@"const") and isConstEnumHead(parser.peekAhead())) {
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
        .namespace, .module => parseModuleDeclaration(parser, modifiers, start),
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
        "Expected '=' in the import alias declaration",
        "Write an import alias as 'import x = N.y' or 'import x = require(\"m\")'",
    )) return null;

    const module_reference = try parseModuleReference(parser) orelse return null;
    // `import type x = require("m")` is legal, an alias of a local entity has nothing type-only
    if (import_kind == .type and
        parser.tree.data(module_reference) != .ts_external_module_reference)
    {
        try parser.report(
            .{ .start = start, .end = parser.tree.span(module_reference).end },
            "An import alias cannot use 'import type'",
            .{ .help = "Remove 'type', or use 'import type x from \"m\"'" },
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
    if (!parser.at(.require) or parser.peekAhead().tag != .left_paren) {
        const head = try literals.parseIdentifier(parser) orelse return null;
        return types.extendQualifiedName(parser, head);
    }

    const start = try parser.take(.require) orelse return null;
    _ = try parser.take(.left_paren) orelse return null;
    const expression = try literals.parseStringLiteral(parser) orelse return null;
    if (!try parser.expect(.right_paren, "Expected ')' to close the 'require' call", null)) {
        return null;
    }
    return try parser.finish(start, .{
        .ts_external_module_reference = .{ .expression = expression },
    });
}

fn parseTypeAliasDeclaration(
    parser: *Parser,
    modifiers: Modifiers,
    start: u32,
) Error!?ast.NodeIndex {
    _ = try parser.take(.type) orelse return null;
    const id = try literals.parseBindingIdentifier(parser) orelse return null;
    const type_parameters = try types.parseTypeParameters(parser);
    if (!try parser.expect(
        .assign,
        "Expected '=' in the type alias declaration",
        "Write a type alias as 'type Name = Type'",
    )) return null;

    const type_annotation = try types.parseTypeAliasBody(parser) orelse return null;
    const end = try parser.eatSemicolon(parser.tree.span(type_annotation).end) orelse return null;
    return try parser.tree.addNode(.{ .ts_type_alias_declaration = .{
        .id = id,
        .type_parameters = type_parameters,
        .type_annotation = type_annotation,
        .declare = modifiers.declare,
    } }, .{ .start = start, .end = end });
}

pub fn parseInterfaceDeclaration(
    parser: *Parser,
    modifiers: Modifiers,
    start: u32,
) Error!?ast.NodeIndex {
    _ = try parser.take(.interface) orelse return null;
    const id = try literals.parseBindingIdentifier(parser) orelse return null;
    const type_parameters = try types.parseTypeParameters(parser);
    const extends = try parseHeritageClause(parser, .extends, .ts_interface_heritage) orelse
        return null;

    if (!parser.at(.left_brace)) return parser.failExpected(
        "Expected '{' to start the interface body",
        .{ .help = "Write an interface body as '{ name: Type; }'" },
    );
    const body_start = parser.current_token.span.start;
    const members = try types.parseObjectTypeMembers(parser) orelse return null;
    const body = try parser.finish(body_start, .{ .ts_interface_body = .{ .body = members } });

    return try parser.finish(start, .{ .ts_interface_declaration = .{
        .id = id,
        .type_parameters = type_parameters,
        .extends = extends,
        .body = body,
        .declare = modifiers.declare,
    } });
}

pub fn parseHeritageClause(
    parser: *Parser,
    comptime keyword: TokenTag,
    comptime tag: std.meta.Tag(ast.NodeData),
) Error!?ast.IndexRange {
    if (!(try parser.eat(keyword) orelse return null)) return .empty;

    const list = parser.list();
    defer list.deinit();
    try list.parseSeparated(null, parseHeritage, .{tag}) orelse return null;
    return try list.finish();
}

fn parseHeritage(
    parser: *Parser,
    comptime tag: std.meta.Tag(ast.NodeData),
) Error!?ast.NodeIndex {
    const expression = try parseHeritageExpression(parser) orelse return null;
    const type_arguments = try types.parseTypeArguments(parser);
    return try parser.finishAt(
        parser.tree.span(expression).start,
        if (type_arguments != .null) type_arguments else expression,
        @unionInit(ast.NodeData, @tagName(tag), .{
            .expression = expression,
            .type_arguments = type_arguments,
        }),
    );
}

fn parseHeritageExpression(parser: *Parser) Error!?ast.NodeIndex {
    var expression = try (if (parser.at(.this))
        parser.leaf(.{ .this_expression = .{} })
    else
        literals.parseIdentifier(parser)) orelse return null;

    while (try parser.eat(.dot) orelse return null) {
        if (!parser.current_token.tag.isIdentifierLike()) return parser.failExpected(
            "Expected an identifier",
            .{ .help = "Identifiers start with a letter, '_', or '$'" },
        );
        const property = try literals.parseIdentifierName(parser) orelse return null;
        expression = try parser.finishAt(parser.tree.span(expression).start, property, .{
            .member_expression = .{
                .object = expression,
                .property = property,
                .computed = false,
                .optional = false,
            },
        });
    }
    return expression;
}

fn parseEnumDeclaration(parser: *Parser, modifiers: Modifiers, start: u32) Error!?ast.NodeIndex {
    _ = try parser.take(.@"enum") orelse return null;
    const id = try literals.parseBindingIdentifier(parser) orelse return null;

    const body_start = parser.current_token.span.start;
    if (!try parser.expect(
        .left_brace,
        "Expected '{' to start the enum body",
        "Write an enum body as '{ A, B = 1 }'",
    )) return null;
    const members = parser.list();
    defer members.deinit();
    try members.parseSeparated(.right_brace, parseEnumMember, .{}) orelse return null;
    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close the enum body",
        "Check that every '{' in the enum has a matching '}'",
    )) return null;
    const body = try parser.finish(body_start, .{
        .ts_enum_body = .{ .members = try members.finish() },
    });

    return try parser.finish(start, .{ .ts_enum_declaration = .{
        .id = id,
        .body = body,
        .is_const = modifiers.is_const,
        .declare = modifiers.declare,
    } });
}

fn parseEnumMember(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const id, const computed = try parseEnumMemberName(parser) orelse return null;

    var end = parser.prev_token_end;
    var initializer: ast.NodeIndex = .null;
    if (try parser.eat(.assign) orelse return null) {
        initializer = try expressions.parseAssignment(parser) orelse return null;
        end = parser.tree.span(initializer).end;
    }
    return try parser.tree.addNode(
        .{ .ts_enum_member = .{ .id = id, .initializer = initializer, .computed = computed } },
        .{ .start = start, .end = end },
    );
}

fn parseEnumMemberName(parser: *Parser) Error!?struct { ast.NodeIndex, bool } {
    const token = parser.current_token;
    switch (token.tag) {
        .left_bracket => {
            try parser.advance() orelse return null;
            const parenthesized = parser.at(.left_paren);
            const inner = try expressions.parseAssignment(parser) orelse return null;
            if (!try parser.expect(
                .right_bracket,
                "Expected ']' to close the enum member name",
                "Check that every '[' in the member name has a matching ']'",
            )) return null;

            const span: ast.Span = .{ .start = token.span.start, .end = parser.prev_token_end };
            switch (enumMemberNameKind(&parser.tree, inner, parenthesized)) {
                .name => {},
                .numeric => try reportEnumMemberNumericName(parser, span),
                .computed => try parser.report(
                    span,
                    "Computed property names are not allowed in enums",
                    .{ .help = "Name the member with an identifier or a string literal" },
                ),
            }
            return .{ inner, true };
        },
        .string_literal => {
            const id = try literals.parseStringLiteral(parser) orelse return null;
            const name = parser.tree.string(parser.tree.data(id).string_literal.value);
            if (spec.number.isCanonicalString(name)) {
                try reportEnumMemberNumericName(parser, token.span);
            }
            return .{ id, false };
        },
        .no_substitution_template => {
            const id = try literals.parseNoSubstitutionTemplate(parser, false) orelse return null;
            try parser.report(
                token.span,
                "An enum member cannot be named with a template literal",
                .{ .help = "Use a string literal instead" },
            );
            return .{ id, false };
        },
        else => {
            if (token.tag.isIdentifierLike()) {
                return .{ try literals.parseIdentifierName(parser) orelse return null, false };
            }
            if (token.tag.isNumericLiteral()) {
                try reportEnumMemberNumericName(parser, token.span);
            } else {
                try parser.reportExpected("Expected an enum member", .{
                    .help = "Name the member with an identifier or a string literal",
                });
            }
            return null;
        },
    }
}

fn enumMemberNameKind(
    tree: *const ast.Tree,
    expression: ast.NodeIndex,
    parenthesized: bool,
) enum { name, numeric, computed } {
    if (parenthesized) return .computed;
    if (spec.static_semantics.stringLiteralLike(tree, expression)) |name| {
        return if (spec.number.isCanonicalString(tree.string(name))) .numeric else .name;
    }
    return switch (tree.data(expression)) {
        .numeric_literal => |literal| if (std.math.isFinite(literal.value(tree)))
            .numeric
        else
            .name,
        else => .computed,
    };
}

fn reportEnumMemberNumericName(parser: *Parser, span: ast.Span) Error!void {
    try parser.report(span, "An enum member cannot have a numeric name", .{
        .help = "Name the member with an identifier or a non-numeric string",
    });
}

fn parseModuleDeclaration(
    parser: *Parser,
    modifiers: Modifiers,
    start: u32,
) Error!?ast.NodeIndex {
    const kind: ast.TSModuleDeclarationKind = if (parser.at(.module)) .module else .namespace;
    try parser.advance() orelse return null;

    const id = if (kind == .module and parser.at(.string_literal))
        try literals.parseStringLiteral(parser) orelse return null
    else name: {
        const head = try literals.parseBindingIdentifier(parser) orelse return null;
        break :name try types.extendQualifiedName(parser, head) orelse return null;
    };
    const body: ast.NodeIndex = if (parser.at(.left_brace))
        try parseModuleBlock(parser) orelse return null
    else
        .null;

    const data: ast.NodeData = .{ .ts_module_declaration = .{
        .id = id,
        .body = body,
        .kind = kind,
        .declare = modifiers.declare,
    } };
    if (body != .null) return try parser.finish(start, data);

    // only an ambient `declare module "m"` may omit its body
    if (parser.tree.data(id) != .string_literal) try parser.report(
        .{ .start = start, .end = parser.tree.span(id).end },
        switch (kind) {
            .namespace => "A namespace declaration requires a body",
            .module => "A module declaration requires a body",
        },
        .{ .help = "Add a body after the name, such as '{}'" },
    );
    return parser.finishStatement(start, data);
}

fn parseGlobalDeclaration(parser: *Parser, modifiers: Modifiers, start: u32) Error!?ast.NodeIndex {
    const id = try literals.parseIdentifierName(parser) orelse return null;
    const body = try parseModuleBlock(parser) orelse return null;
    return try parser.finish(start, .{
        .ts_global_declaration = .{ .id = id, .body = body, .declare = modifiers.declare },
    });
}

fn parseModuleBlock(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.left_brace) orelse return null;
    const body = try parser.parseBody(.right_brace, .module_block);
    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close the module body",
        "Check that every '{' in the module has a matching '}'",
    )) return null;
    return try parser.finish(start, .{ .ts_module_block = .{ .body = body } });
}
