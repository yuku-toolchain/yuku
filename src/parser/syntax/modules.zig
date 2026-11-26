const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Token = @import("../token.zig").Token;
const TokenTag = @import("../token.zig").TokenTag;

const expressions = @import("expressions.zig");
const literals = @import("literals.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const variables = @import("variables.zig");
const ts = @import("ts/statements.zig");

// `start_from_param` lets `export public import x =` span from the legacy modifier
pub fn parseImportDeclaration(parser: *Parser, start_from_param: ?u32) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    const keyword_start = try parser.take(.import) orelse return null;
    const start = start_from_param orelse keyword_start;

    if (parser.at(.string_literal)) return parseImportSource(parser, start, .empty, null, .value);

    var import_kind: ast.ImportOrExportKind = .value;
    const next = parser.peekAhead();

    if (is_ts and parser.at(.type) and isTypeImportModifier(parser, next)) {
        import_kind = .type;
        try parser.advanceWithoutEscapeCheck() orelse return null;
    }
    // before the phase forms so `import source = require("m")` binds `source`
    if (is_ts and parser.current_token.tag.isIdentifierLike()) {
        if (parser.peekAhead().tag == .assign) {
            return ts.parseImportEqualsBody(parser, start, import_kind);
        }
    }

    var phase: ?ast.ImportPhase = null;
    if (import_kind == .value) {
        if (parser.at(.source) and isPhaseImportBinding(parser, next)) {
            phase = .source;
            try parser.advance() orelse return null;
        } else if (parser.at(.@"defer") and
            (next.tag == .star or isPhaseImportBinding(parser, next)))
        {
            if (next.tag != .star) try parser.report(
                parser.current_token.span,
                if (next.tag == .left_brace)
                    "Named imports are not allowed in a deferred import"
                else
                    "Default imports are not allowed in a deferred import",
                .{ .help = "Use a namespace import, such as 'import defer * as ns from \"m\"'" },
            );
            phase = .@"defer";
            try parser.advance() orelse return null;
        }
    }

    const specifiers = try parseImportClause(parser, import_kind) orelse return null;

    if (!try parser.expect(
        .from,
        "Expected 'from' after the import clause",
        "Follow the import clause with 'from' and a module specifier",
    )) return null;

    return parseImportSource(parser, start, specifiers, phase, import_kind);
}

fn isTypeImportModifier(parser: *Parser, after_type: Token) bool {
    if (after_type.tag == .left_brace or after_type.tag == .star) return true;
    if (after_type.tag != .from) return after_type.tag.isIdentifierLike();
    const after_from = parser.peekSecond().tag;
    return after_from == .from or after_from == .assign;
}

// `import source x from "m"` binds x, `import source from "m"` binds
// `source` itself, and `import source from from "m"` binds `from`
fn isPhaseImportBinding(parser: *Parser, next: Token) bool {
    if (next.tag != .from) return next.tag.isIdentifierLike();
    return parser.peekSecond().tag == .from;
}

fn parseImportSource(
    parser: *Parser,
    start: u32,
    specifiers: ast.IndexRange,
    phase: ?ast.ImportPhase,
    import_kind: ast.ImportOrExportKind,
) Error!?ast.NodeIndex {
    const source = try parseModuleSpecifier(parser) orelse return null;
    const attributes = try parseWithClause(parser);

    return parser.finishStatement(start, .{ .import_declaration = .{
        .specifiers = specifiers,
        .source = source,
        .attributes = attributes,
        .phase = phase,
        .import_kind = import_kind,
    } });
}

fn parseImportClause(parser: *Parser, import_kind: ast.ImportOrExportKind) Error!?ast.IndexRange {
    if (parser.at(.left_brace)) return parseNamedImports(parser);

    const specifiers = parser.list();
    defer specifiers.deinit();

    var default_import: ast.NodeIndex = .null;
    if (!parser.at(.star)) {
        const local = try literals.parseBindingIdentifier(parser) orelse return null;
        default_import = try parser.tree.addNode(
            .{ .import_default_specifier = .{ .local = local } },
            parser.tree.span(local),
        );
        try specifiers.append(default_import);
        if (!(try parser.eat(.comma) orelse return null)) return try specifiers.finish();
    }

    if (parser.at(.star)) {
        const start = try parser.take(.star) orelse return null;
        if (!try parser.expect(
            .as,
            "Expected 'as' after '*' in the namespace import",
            "Write a namespace import as '* as name'",
        )) return null;
        const local = try literals.parseBindingIdentifier(parser) orelse return null;
        try specifiers.append(try parser.finish(start, .{
            .import_namespace_specifier = .{ .local = local },
        }));
    } else if (parser.at(.left_brace)) {
        const named = try parseNamedImports(parser) orelse return null;
        for (parser.tree.extra(named)) |specifier| try specifiers.append(specifier);
    } else {
        try parser.reportExpected(
            "Expected a namespace import or named imports after ','",
            .{ .help = "Write '* as name' or '{ name }' after the default import" },
        );
        return null;
    }

    if (default_import != .null and import_kind == .type) try parser.report(
        .{ .start = parser.tree.span(default_import).start, .end = parser.prev_token_end },
        "A type-only import can specify a default import or named bindings, but not both",
        .{ .help = "Split into two separate import declarations" },
    );

    return try specifiers.finish();
}

fn parseNamedImports(parser: *Parser) Error!?ast.IndexRange {
    _ = try parser.take(.left_brace) orelse return null;

    const specifiers = parser.list();
    defer specifiers.deinit();
    try specifiers.parseSeparated(.right_brace, parseImportSpecifier, .{}) orelse return null;

    if (!try parser.expect(.right_brace, "Expected '}' to close the named imports", null)) {
        return null;
    }
    return try specifiers.finish();
}

fn parseImportSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const parts = try parseSpecifierParts(parser) orelse return null;
    const name_span = parser.tree.span(parts.name);
    const name_data = parser.tree.data(parts.name);

    if (name_data == .string_literal) {
        if (parts.property_name != .null) {
            return parser.fail(name_span, "The local name of an import must be an identifier", .{});
        }
        return parser.fail(name_span, "A string literal import name requires an 'as' clause", .{
            .help = "Rename the import, such as 'import { \"a-b\" as ab } from \"m\"'",
        });
    }

    try literals.validateIdentifier(parser, "an imported binding", parts.name_token);

    const local = try parser.tree.addNode(.{
        .binding_identifier = .{ .name = name_data.identifier_name.name },
    }, name_span);

    return try parser.tree.addNode(.{ .import_specifier = .{
        .imported = if (parts.property_name != .null) parts.property_name else parts.name,
        .local = local,
        .import_kind = parts.kind,
    } }, .{ .start = start, .end = name_span.end });
}

// property_name is the imported name for imports and the local name for exports
const SpecifierParts = struct {
    property_name: ast.NodeIndex = .null,
    name: ast.NodeIndex,
    name_token: Token,
    kind: ast.ImportOrExportKind = .value,

    fn parseName(parts: *SpecifierParts, parser: *Parser) Error!?void {
        parts.name_token = parser.current_token;
        parts.name = try parseModuleExportName(parser) orelse return null;
    }
};

// `type` is a modifier only when a name follows, so `{ type as }` imports `as` type-only
// and `{ type as as X }` renames `as` to `X` type-only
fn parseSpecifierParts(parser: *Parser) Error!?SpecifierParts {
    const first_token = parser.current_token;
    const first = try parseModuleExportName(parser) orelse return null;

    var parts: SpecifierParts = .{ .name = first, .name_token = first_token };
    var may_rename = true;

    if (parser.tree.isTs() and
        first_token.tag == .type and
        parser.tree.data(first) == .identifier_name)
    {
        if (parser.at(.as)) {
            const first_as_token = parser.current_token;
            const first_as = try literals.parseIdentifierName(parser) orelse return null;

            if (parser.at(.as)) {
                const second_as_token = parser.current_token;
                const second_as = try literals.parseIdentifierName(parser) orelse return null;

                if (canStartModuleExportName(parser.current_token.tag)) {
                    // `type as as <name>`
                    parts.kind = .type;
                    parts.property_name = first_as;
                    try parts.parseName(parser) orelse return null;
                } else {
                    // `type as as`
                    parts.property_name = first;
                    parts.name = second_as;
                    parts.name_token = second_as_token;
                }
                may_rename = false;
            } else if (canStartModuleExportName(parser.current_token.tag)) {
                // `type as <name>`
                parts.property_name = first;
                try parts.parseName(parser) orelse return null;
                may_rename = false;
            } else {
                // `type as`
                parts.kind = .type;
                parts.name = first_as;
                parts.name_token = first_as_token;
            }
        } else if (canStartModuleExportName(parser.current_token.tag)) {
            // `type <name>`
            parts.kind = .type;
            try parts.parseName(parser) orelse return null;
        }
        // else `type` alone is the name
    }

    if (may_rename and (try parser.eat(.as) orelse return null)) {
        parts.property_name = parts.name;
        try parts.parseName(parser) orelse return null;
    }

    return parts;
}

fn canStartModuleExportName(tag: TokenTag) bool {
    return tag.isIdentifierLike() or tag == .string_literal;
}

pub fn parseExportDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    const start = try parser.take(.@"export") orelse return null;

    if (is_ts) switch (parser.current_token.tag) {
        .assign => return parseTSExportAssignment(parser, start),
        .as => return parseTSNamespaceExportDeclaration(parser, start),
        .type => switch (parser.peekAhead().tag) {
            .left_brace => {
                try parser.advance() orelse return null;
                return parseExportNamedFromClause(parser, start, .type);
            },
            .star => {
                try parser.advance() orelse return null;
                return parseExportAllDeclaration(parser, start, .type);
            },
            else => {},
        },
        else => {},
    };

    return switch (parser.current_token.tag) {
        .default => parseExportDefaultDeclaration(parser, start),
        .star => parseExportAllDeclaration(parser, start, .value),
        .left_brace => parseExportNamedFromClause(parser, start, .value),
        else => parseExportWithDeclaration(parser, start),
    };
}

fn parseTSExportAssignment(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    _ = try parser.take(.assign) orelse return null;
    const expression = try expressions.parseAssignment(parser) orelse return null;
    const end = try parser.eatSemicolon(parser.tree.span(expression).end) orelse return null;

    return try parser.tree.addNode(.{
        .ts_export_assignment = .{ .expression = expression },
    }, .{ .start = start, .end = end });
}

fn parseTSNamespaceExportDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    _ = try parser.take(.as) orelse return null;
    if (!try parser.expect(.namespace, "Expected 'namespace' after 'export as'", null)) {
        return null;
    }

    const id = try literals.parseIdentifierName(parser) orelse return null;
    return parser.finishStatement(start, .{ .ts_namespace_export_declaration = .{ .id = id } });
}

fn parseExportDefaultDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    _ = try parser.take(.default) orelse return null;
    const tag = parser.current_token.tag;
    const declaration_start = parser.current_token.span.start;

    var is_expression = false;
    const declaration = try blk: {
        if (tag == .function) {
            break :blk functions.parseFunction(parser, .{ .is_default_export = true }, null);
        }

        if (tag == .async and !parser.current_token.hasLineTerminatorBefore()) {
            const next = parser.peekAhead();
            if (next.tag == .function and !next.hasLineTerminatorBefore()) {
                try parser.advance() orelse return null;
                break :blk functions.parseFunction(
                    parser,
                    .{ .is_default_export = true, .is_async = true },
                    declaration_start,
                );
            }
        }

        const options: class.ParseClassOptions = .{ .is_default_export = true };
        if (tag == .class) break :blk class.parseClass(parser, options, null);
        if (tag == .at) break :blk class.parseDecoratedClass(parser, options);

        if (parser.tree.isTs()) {
            if (tag == .abstract and class.isAbstractClassNext(parser)) {
                try parser.advance() orelse return null;
                break :blk class.parseClass(parser, .{
                    .is_default_export = true,
                    .is_abstract = true,
                }, declaration_start);
            }
            if (tag == .interface) {
                break :blk ts.parseInterfaceDeclaration(parser, .{}, declaration_start);
            }
        }

        is_expression = true;
        break :blk expressions.parseAssignment(parser);
    } orelse return null;

    const declaration_end = parser.tree.span(declaration).end;
    const end = if (is_expression)
        try parser.eatSemicolon(declaration_end) orelse return null
    else
        declaration_end;

    return try parser.tree.addNode(
        .{ .export_default_declaration = .{ .declaration = declaration } },
        .{ .start = start, .end = end },
    );
}

// caller already consumed `export` and any `type`
fn parseExportAllDeclaration(
    parser: *Parser,
    start: u32,
    export_kind: ast.ImportOrExportKind,
) Error!?ast.NodeIndex {
    _ = try parser.take(.star) orelse return null;

    const exported: ast.NodeIndex = if (try parser.eat(.as) orelse return null)
        try parseModuleExportName(parser) orelse return null
    else
        .null;

    if (!try parser.expect(
        .from,
        "Expected 'from' after 'export *'",
        "Write the re-export as 'export * from \"m\"'",
    )) return null;

    const source = try parseModuleSpecifier(parser) orelse return null;
    const attributes = try parseWithClause(parser);

    return parser.finishStatement(start, .{ .export_all_declaration = .{
        .exported = exported,
        .source = source,
        .attributes = attributes,
        .export_kind = export_kind,
    } });
}

// caller already consumed `export` and any `type`
fn parseExportNamedFromClause(
    parser: *Parser,
    start: u32,
    export_kind: ast.ImportOrExportKind,
) Error!?ast.NodeIndex {
    _ = try parser.take(.left_brace) orelse return null;

    const list = parser.list();
    defer list.deinit();
    const first_tags = parser.sideList();
    defer first_tags.deinit();

    while (!parser.at(.right_brace) and !parser.at(.eof)) {
        try first_tags.append(@bitCast(parser.current_token.tag));
        try list.append(try parseExportSpecifier(parser) orelse return null);
        if (!(try parser.eat(.comma) orelse return null)) break;
    }

    if (!try parser.expect(.right_brace, "Expected '}' to close the named exports", null)) {
        return null;
    }
    const specifiers = try list.finish();

    var source: ast.NodeIndex = .null;
    var attributes: ast.IndexRange = .empty;
    if (try parser.eat(.from) orelse return null) {
        source = try parseModuleSpecifier(parser) orelse return null;
        attributes = try parseWithClause(parser);
    } else {
        try resolveLocalExportSpecifiers(parser, specifiers, first_tags.items());
    }

    return parser.finishStatement(start, .{ .export_named_declaration = .{
        .declaration = .null,
        .specifiers = specifiers,
        .source = source,
        .attributes = attributes,
        .export_kind = export_kind,
    } });
}

fn parseExportWithDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    const missing_declaration = "Expected a declaration after 'export'";

    if (is_ts and ts.isStartOfTsDeclaration(parser)) {
        const declaration = try ts.parseTsDeclaration(parser) orelse return null;
        const export_kind = exportKindForDeclaration(parser, declaration);
        return try addExportNamedDeclaration(parser, start, declaration, export_kind);
    }

    const declaration = try switch (parser.current_token.tag) {
        .@"var", .@"const", .let => variables.parseVariableDeclaration(parser, .{}, null),
        .function => functions.parseFunction(parser, .{}, null),
        .async => blk: {
            const async_start = try parser.take(.async) orelse return null;
            break :blk functions.parseFunction(parser, .{ .is_async = true }, async_start);
        },
        .class => class.parseClass(parser, .{}, null),
        .at => class.parseDecoratedClass(parser, .{}),
        .import => if (is_ts)
            parseImportDeclaration(parser, null)
        else
            return parser.failExpected(missing_declaration, .{}),
        // `export public import x =`, the legacy modifier is noise but starts the span
        .public, .private, .static => blk: {
            const next = parser.peekAhead();
            if (!is_ts or next.tag != .import or next.hasLineTerminatorBefore()) {
                return parser.failExpected(missing_declaration, .{});
            }
            const modifier_start = parser.current_token.span.start;
            try parser.advance() orelse return null;
            break :blk parseImportDeclaration(parser, modifier_start);
        },
        else => return parser.failExpected(missing_declaration, .{}),
    } orelse return null;

    if (parser.tree.data(declaration) == .import_declaration) {
        std.debug.assert(is_ts);
        try parser.report(
            parser.tree.span(declaration),
            "An import declaration cannot have modifiers",
            .{ .help = "Re-export with 'export { name } from \"m\"' instead" },
        );
    }

    return try addExportNamedDeclaration(parser, start, declaration, .value);
}

fn addExportNamedDeclaration(
    parser: *Parser,
    start: u32,
    declaration: ast.NodeIndex,
    export_kind: ast.ImportOrExportKind,
) Error!ast.NodeIndex {
    return parser.finishAt(start, declaration, .{ .export_named_declaration = .{
        .declaration = declaration,
        .specifiers = .empty,
        .source = .null,
        .attributes = .empty,
        .export_kind = export_kind,
    } });
}

pub fn parseExportDecorated(parser: *Parser, decorators: ast.IndexRange) Error!?ast.NodeIndex {
    std.debug.assert(decorators.len > 0);
    const start = try parser.take(.@"export") orelse return null;
    const is_default = try parser.eat(.default) orelse return null;

    var class_decorators = decorators;
    if (parser.at(.at)) {
        class_decorators = try class.parseDecorators(parser, decorators) orelse return null;
        try parser.report(
            parser.tree.span(parser.tree.extra(class_decorators)[decorators.len]),
            "Decorators may not appear after 'export' or 'export default' if they " ++
                "also appear before 'export'",
            .{ .help = "Keep the decorators on one side of 'export'" },
        );
    }

    if (!parser.at(.class) and !(parser.tree.isTs() and class.isClassModifiersNext(parser))) {
        return parser.failExpected("Expected 'class' after the decorators", .{});
    }

    const declaration = try class.parseClass(
        parser,
        .{ .is_default_export = is_default, .decorators = class_decorators },
        null,
    ) orelse return null;

    if (!is_default) return try addExportNamedDeclaration(parser, start, declaration, .value);
    return try parser.finishAt(start, declaration, .{
        .export_default_declaration = .{ .declaration = declaration },
    });
}

fn exportKindForDeclaration(parser: *Parser, declaration: ast.NodeIndex) ast.ImportOrExportKind {
    const declared = switch (parser.tree.data(declaration)) {
        .ts_interface_declaration, .ts_type_alias_declaration => return .type,
        inline .ts_enum_declaration,
        .ts_module_declaration,
        .ts_global_declaration,
        .variable_declaration,
        .function,
        .class,
        => |declared| declared.declare,
        else => return .value,
    };
    return if (declared) .type else .value;
}

fn resolveLocalExportSpecifiers(
    parser: *Parser,
    specifiers: ast.IndexRange,
    first_tags: []const ast.NodeIndex,
) Error!void {
    for (parser.tree.extra(specifiers), first_tags) |node, first_tag| {
        const specifier = parser.tree.data(node).export_specifier;
        const local_data = parser.tree.data(specifier.local);
        const local_span = parser.tree.span(specifier.local);

        if (local_data == .string_literal) {
            try parser.report(
                local_span,
                "A string literal cannot be used as an exported binding without 'from'",
                .{ .help = "Add a 'from' clause, or export a local binding with 'as'" },
            );
            continue;
        }

        if (@as(TokenTag, @bitCast(first_tag)).isReserved()) try parser.report(
            local_span,
            "A reserved word cannot be used as an exported binding without 'from'",
            .{ .help = try parser.fmt(
                "Add a 'from' clause, such as 'export {{ {s} }} from \"m\"'",
                .{parser.tree.string(local_data.identifier_name.name)},
            ) },
        );

        if (local_data == .identifier_name) {
            std.debug.assert(specifier.local != specifier.exported);
            parser.tree.setData(specifier.local, .{
                .identifier_reference = .{ .name = local_data.identifier_name.name },
            });
        }
    }
}

fn parseExportSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const parts = try parseSpecifierParts(parser) orelse return null;

    const exported = parts.name;
    const local = if (parts.property_name != .null)
        parts.property_name
    else
        try parser.tree.addNode(parser.tree.data(exported), parser.tree.span(exported));

    return try parser.finishAt(start, exported, .{ .export_specifier = .{
        .local = local,
        .exported = exported,
        .export_kind = parts.kind,
    } });
}

fn parseModuleExportName(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    if (token.tag == .string_literal) {
        if (token.hasLoneSurrogates()) {
            return parser.fail(
                token.span,
                "An export name cannot contain a lone Unicode surrogate",
                .{},
            );
        }
        return literals.parseStringLiteral(parser);
    }
    if (token.tag.isIdentifierLike()) return literals.parseIdentifierName(parser);
    return parser.failExpected("Expected an identifier or string literal", .{});
}

fn parseModuleSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    if (!parser.at(.string_literal)) {
        return parser.failExpected("Expected a module specifier", .{
            .help = "Write the module specifier as a string, such as './file.js'",
        });
    }
    return literals.parseStringLiteral(parser);
}

fn parseWithClause(parser: *Parser) Error!ast.IndexRange {
    const is_assert = parser.at(.assert) and !parser.current_token.hasLineTerminatorBefore();
    if (!parser.at(.with) and !is_assert) return .empty;
    try parser.advance() orelse return .empty;

    if (!try parser.expect(.left_brace, "Expected '{' to start the import attributes", null)) {
        return .empty;
    }

    const attributes = parser.list();
    defer attributes.deinit();
    try attributes.parseSeparated(.right_brace, parseImportAttribute, .{}) orelse return .empty;

    if (!try parser.expect(.right_brace, "Expected '}' to close the import attributes", null)) {
        return .empty;
    }
    return attributes.finish();
}

fn parseImportAttribute(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const tag = parser.current_token.tag;
    const key = if (tag == .string_literal)
        try literals.parseStringLiteral(parser) orelse return null
    else if (tag.isIdentifierLike())
        try literals.parseIdentifierName(parser) orelse return null
    else
        return parser.failExpected(
            "Expected an identifier or string literal for the attribute key",
            .{},
        );

    if (!try parser.expect(.colon, "Expected ':' after the import attribute key", null)) {
        return null;
    }

    if (!parser.at(.string_literal)) return parser.fail(
        parser.current_token.span,
        "Import attribute values must be string literal expressions",
        .{},
    );
    const value = try literals.parseStringLiteral(parser) orelse return null;

    return try parser.finish(start, .{ .import_attribute = .{ .key = key, .value = value } });
}

pub fn parseDynamicImport(
    parser: *Parser,
    import_keyword: ast.NodeIndex,
    phase: ?ast.ImportPhase,
) Error!?ast.NodeIndex {
    if (!try parser.expect(.left_paren, "Expected '(' after 'import'", null)) return null;

    const source = try expressions.parseAssignment(parser) orelse return null;

    var options: ast.NodeIndex = .null;
    if ((try parser.eat(.comma) orelse return null) and !parser.at(.right_paren)) {
        options = try expressions.parseAssignment(parser) orelse return null;
        _ = try parser.eat(.comma) orelse return null;
    }

    if (!try parser.expect(.right_paren, "Expected ')' to close the 'import()' call", null)) {
        return null;
    }

    return try parser.finish(parser.tree.span(import_keyword).start, .{ .import_expression = .{
        .source = source,
        .options = options,
        .phase = phase,
    } });
}
