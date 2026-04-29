const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Token = @import("../token.zig").Token;
const TokenTag = @import("../token.zig").TokenTag;
const Precedence = @import("../token.zig").Precedence;

const expressions = @import("expressions.zig");
const statements = @import("statements.zig");
const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const extensions = @import("extensions.zig");
const variables = @import("variables.zig");
const ts_statements = @import("ts/statements.zig");

pub fn parseImportDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    return parseImportDeclarationFrom(parser, parser.current_token.span.start);
}

// explicit start for `export` wrapped import equals so span includes legacy modifier not only `import`
pub fn parseImportDeclarationFrom(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    try parser.advance() orelse return null;

    // import 'module' side effect only
    if (parser.current_token.tag == .string_literal) {
        return parseSideEffectImport(parser, start, null);
    }

    var phase: ?ast.ImportPhase = null;
    var import_kind: ast.ImportOrExportKind = .value;

    const peek = try parser.peekAheadN(2);
    const next = peek[0] orelse return null;

    // ts `import type { }` etc not `import type from`
    if (is_ts and parser.current_token.tag == .type and
        isTypeImportModifier(next, peek[1]))
    {
        import_kind = .type;
        try parser.advance() orelse return null;
    }
    // import source x from
    else if (parser.current_token.tag == .source and next.tag.isIdentifierLike() and next.tag != .from) {
        phase = .source;
        try parser.advance() orelse return null;
    }
    // import defer * as x from
    else if (parser.current_token.tag == .@"defer" and next.tag == .star) {
        phase = .@"defer";
        try parser.advance() orelse return null;
    }

    // ts import x = rhs when id then assign, covers import type x = too
    if (is_ts and parser.current_token.tag.isIdentifierLike()) {
        const after_id = try parser.peekAhead() orelse return null;
        if (after_id.tag == .assign) {
            return ts_statements.parseImportEqualsBody(parser, start, import_kind);
        }
    }

    const specifiers = try parseImportClause(parser) orelse return null;

    if (parser.current_token.tag != .from) {
        try parser.reportExpected(parser.current_token.span, "Expected 'from' after import clause", .{
            .help = "Import statements require 'from' followed by a module specifier: import x from 'module'",
        });
        return null;
    }

    try parser.advance() orelse return null;

    const source = try parseModuleSpecifier(parser) orelse return null;

    const attributes = try parseWithClause(parser);

    const end = try parser.eatSemicolon(parser.tree.getSpan(source).end) orelse return null;

    return try parser.tree.createNode(.{
        .import_declaration = .{
            .specifiers = specifiers,
            .source = source,
            .attributes = attributes,
            .phase = phase,
            .import_kind = import_kind,
        },
    }, .{ .start = start, .end = end });
}

// `import type` modifier vs default binding named type
fn isTypeImportModifier(after_type: Token, after_after: ?Token) bool {
    if (after_type.tag == .left_brace or after_type.tag == .star) return true;
    if (!after_type.tag.isIdentifierLike()) return false;
    if (after_type.tag == .from) {
        const a2 = after_after orelse return false;
        return a2.tag == .from or a2.tag == .assign;
    }
    return true;
}

// import 'm' only, no bindings
fn parseSideEffectImport(parser: *Parser, start: u32, phase: ?ast.ImportPhase) Error!?ast.NodeIndex {
    const source = try parseModuleSpecifier(parser) orelse return null;
    const attributes = try parseWithClause(parser);
    const end = try parser.eatSemicolon(parser.tree.getSpan(source).end) orelse return null;

    return try parser.tree.createNode(.{
        .import_declaration = .{
            .specifiers = ast.IndexRange.empty,
            .source = source,
            .attributes = attributes,
            .phase = phase,
            .import_kind = .value,
        },
    }, .{ .start = start, .end = end });
}

// ImportClause: default, * as, { }, default comma star, default comma braces
fn parseImportClause(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    if (parser.current_token.tag == .star) {
        const ns = try parseImportNamespaceSpecifier(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), ns);
        return try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);
    }

    if (parser.current_token.tag == .left_brace) {
        return parseNamedImports(parser);
    }

    const default_import = try parseImportDefaultSpecifier(parser) orelse return null;

    try parser.scratch_a.append(parser.allocator(), default_import);

    // import foo, * as bar / import foo, { bar }
    if (parser.current_token.tag == .comma) {
        try parser.advance() orelse return null;

        if (parser.current_token.tag == .star) {
            const ns = try parseImportNamespaceSpecifier(parser) orelse return null;
            try parser.scratch_a.append(parser.allocator(), ns);
        } else if (parser.current_token.tag == .left_brace) {
            const named = try parseNamedImports(parser) orelse return null;
            for (parser.tree.getExtra(named)) |spec| {
                try parser.scratch_a.append(parser.allocator(), spec);
            }
        } else {
            try parser.reportExpected(parser.current_token.span, "Expected namespace import (* as name) or named imports ({...}) after ','", .{});
            return null;
        }
    }

    return try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);
}

// import foo from 'm'
//            ~~~
fn parseImportDefaultSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const local = try parseImportedBinding(parser) orelse return null;
    const end = parser.tree.getSpan(local).end;

    return try parser.tree.createNode(.{
        .import_default_specifier = .{ .local = local },
    }, .{ .start = start, .end = end });
}

// * as name
fn parseImportNamespaceSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (!try parser.expect(.star, "Expected '*' for namespace import", null)) return null;

    if (parser.current_token.tag != .as) {
        try parser.reportExpected(parser.current_token.span, "Expected 'as' after '*' in namespace import", .{
            .help = "Namespace imports must use the form: * as name",
        });
        return null;
    }
    try parser.advance() orelse return null;

    const local = try parseImportedBinding(parser) orelse return null;
    const end = parser.tree.getSpan(local).end;

    return try parser.tree.createNode(.{
        .import_namespace_specifier = .{ .local = local },
    }, .{ .start = start, .end = end });
}

// { foo, bar as baz }
fn parseNamedImports(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    if (!try parser.expect(.left_brace, "Expected '{' to start named imports", null)) return null;

    while (parser.current_token.tag != .right_brace and parser.current_token.tag != .eof) {
        const spec = try parseImportSpecifier(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), spec);

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
        } else {
            break;
        }
    }

    if (!try parser.expect(.right_brace, "Expected '}' to close named imports", null)) return null;

    return try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);
}

// member: name, rename, string rename, ts type forms
fn parseImportSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const parts = try parseSpecifierParts(parser) orelse return null;

    // module export name is property_name slot when rename or type as as split it
    const imported = if (parts.property_name != .null) parts.property_name else parts.name;

    const name_data = parser.tree.getData(parts.name);

    if (name_data == .string_literal) {
        if (parts.property_name == .null) {
            try parser.report(parser.tree.getSpan(parts.name), "String literal imports require an 'as' clause", .{
                .help = "Use: import { \"name\" as localName } from 'module'",
            });
        } else {
            try parser.report(parser.tree.getSpan(parts.name), "Import local binding must be an identifier", .{});
        }
        return null;
    }

    try literals.validateIdentifier(parser, "an imported binding", parts.name_token);

    const local = try parser.tree.createNode(.{
        .binding_identifier = .{ .name = name_data.identifier_name.name },
    }, parser.tree.getSpan(parts.name));

    const end = parser.tree.getSpan(local).end;

    return try parser.tree.createNode(.{
        .import_specifier = .{
            .imported = imported,
            .local = local,
            .import_kind = parts.kind,
        },
    }, .{ .start = start, .end = end });
}

// import: property_name is exported id when renamed. export: property_name is local when renamed
const SpecifierParts = struct {
    property_name: ast.NodeIndex = .null,
    name: ast.NodeIndex,
    // token for reserved checks on `name`
    name_token: Token,
    kind: ast.ImportOrExportKind = .value,
};

// head of import export specifier, ts `type` modifier tables:
// ```
// { type }             -> kind=value, name=type
// { type as }          -> kind=type,  name=as
// { type as as }       -> kind=value, propertyName=type, name=as
// { type as as X }     -> kind=type,  propertyName=as,   name=X
// { type as X }        -> kind=value, propertyName=type, name=X
// { type X }           -> kind=type,                     name=X
// { type X as Y }      -> kind=type,  propertyName=X,    name=Y
// ```
fn parseSpecifierParts(parser: *Parser) Error!?SpecifierParts {
    const first_token = parser.current_token;
    const first = try parseModuleExportName(parser) orelse return null;

    var parts: SpecifierParts = .{ .name = first, .name_token = first_token };

    // parseTypeSpecifierTail may eat the rename, then skip outer as
    const tail = if (parser.tree.isTs() and
        first_token.tag == .type and
        parser.tree.getData(first) == .identifier_name)
        try parseTypeSpecifierTail(parser, &parts, first) orelse return null
    else
        TypeTailResult.no_type_modifier;

    if (tail != .consumed_rename and parser.current_token.tag == .as) {
        try parser.advance() orelse return null;
        parts.property_name = parts.name;
        parts.name_token = parser.current_token;
        parts.name = try parseModuleExportName(parser) orelse return null;
    }

    return parts;
}

const TypeTailResult = enum {
    no_type_modifier,
    // rename done inside helper
    consumed_rename,
    // still need outer as name if present
    keep_outer_rename,
};

// ts only. mutates parts fields
fn parseTypeSpecifierTail(
    parser: *Parser,
    parts: *SpecifierParts,
    first: ast.NodeIndex,
) Error!?TypeTailResult {
    if (parser.current_token.tag == .as) {
        const first_as_token = parser.current_token;
        const first_as = try literals.parseIdentifierName(parser) orelse return null;

        if (parser.current_token.tag == .as) {
            const second_as_token = parser.current_token;
            const second_as = try literals.parseIdentifierName(parser) orelse return null;

            if (canStartModuleExportName(parser.current_token.tag)) {
                // `type as as <name>`
                parts.kind = .type;
                parts.property_name = first_as;
                parts.name_token = parser.current_token;
                parts.name = try parseModuleExportName(parser) orelse return null;
            } else {
                // `type as as`
                parts.property_name = first;
                parts.name = second_as;
                parts.name_token = second_as_token;
            }
            return .consumed_rename;
        }

        if (canStartModuleExportName(parser.current_token.tag)) {
            // `type as <name>`
            parts.property_name = first;
            parts.name_token = parser.current_token;
            parts.name = try parseModuleExportName(parser) orelse return null;
            return .consumed_rename;
        }

        // `type as`
        parts.kind = .type;
        parts.name = first_as;
        parts.name_token = first_as_token;
        return .keep_outer_rename;
    }

    if (canStartModuleExportName(parser.current_token.tag)) {
        // `type <name>`
        parts.kind = .type;
        parts.name_token = parser.current_token;
        parts.name = try parseModuleExportName(parser) orelse return null;
        return .keep_outer_rename;
    }

    // bare type token as name
    return .keep_outer_rename;
}

fn canStartModuleExportName(tag: TokenTag) bool {
    return tag.isIdentifierLike() or tag == .string_literal;
}

// local name in import clause, usual binding id rules
fn parseImportedBinding(parser: *Parser) Error!?ast.NodeIndex {
    return literals.parseBindingIdentifier(parser);
}

pub fn parseExportDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    if (is_ts) switch (parser.current_token.tag) {
        .assign => return parseTSExportAssignment(parser, start),
        .as => return parseTSNamespaceExportDeclaration(parser, start),
        // export type { } / export type *
        .type => {
            const next = try parser.peekAhead() orelse return null;
            if (next.tag == .left_brace) {
                try parser.advance() orelse return null;
                return parseExportNamedFromClause(parser, start, .type);
            }
            if (next.tag == .star) {
                try parser.advance() orelse return null;
                return parseExportAllDeclaration(parser, start, .type);
            }
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

// export = expr
fn parseTSExportAssignment(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null;

    const expression = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    const end = try parser.eatSemicolon(parser.tree.getSpan(expression).end) orelse return null;

    return try parser.tree.createNode(.{
        .ts_export_assignment = .{ .expression = expression },
    }, .{ .start = start, .end = end });
}

// export as namespace foo
fn parseTSNamespaceExportDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null;

    if (parser.current_token.tag != .namespace) {
        try parser.reportExpected(parser.current_token.span, "Expected 'namespace' after 'export as'", .{});
        return null;
    }

    try parser.advance() orelse return null;

    const id = try literals.parseIdentifierName(parser) orelse return null;
    const end = try parser.eatSemicolon(parser.tree.getSpan(id).end) orelse return null;

    return try parser.tree.createNode(.{
        .ts_namespace_export_declaration = .{ .id = id },
    }, .{ .start = start, .end = end });
}

// abstract then class on same line, like ts decl probe
fn isAbstractClassNext(parser: *Parser) Error!bool {
    const next = try parser.peekAhead() orelse return false;
    return next.tag == .class and !next.hasLineTerminatorBefore();
}

// legacy `export public import x =`, only import after modifier matters here
fn isLegacyAccessibilityImport(parser: *Parser) Error!bool {
    const next = try parser.peekAhead() orelse return false;
    return next.tag == .import and !next.hasLineTerminatorBefore();
}

// expr gets eaten semicolon after, decl does not
const DefaultExportPart = struct { declaration: ast.NodeIndex, needs_semi: bool };

fn parseExportDefaultDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null;

    const part = try parseExportDefaultPart(parser) orelse return null;
    const decl_span = parser.tree.getSpan(part.declaration);
    const end = if (part.needs_semi)
        try parser.eatSemicolon(decl_span.end) orelse return null
    else
        decl_span.end;

    return try parser.tree.createNode(
        .{ .export_default_declaration = .{ .declaration = part.declaration } },
        .{ .start = start, .end = end },
    );
}

fn parseExportDefaultPart(parser: *Parser) Error!?DefaultExportPart {
    const is_ts = parser.tree.isTs();
    const tag = parser.current_token.tag;

    if (tag == .function) {
        const decl = try functions.parseFunction(parser, .{ .is_default_export = true }, null) orelse return null;
        return .{ .declaration = decl, .needs_semi = false };
    }

    if (tag == .async and !parser.current_token.hasLineTerminatorBefore()) {
        const async_start = parser.current_token.span.start;
        try parser.advance() orelse return null;
        if (parser.current_token.tag == .function) {
            const decl = try functions.parseFunction(parser, .{ .is_default_export = true, .is_async = true }, async_start) orelse return null;
            return .{ .declaration = decl, .needs_semi = false };
        }
        // export default async as identifier value
        const async_end = async_start + 5;
        const id = try parser.tree.createNode(
            .{ .identifier_reference = .{ .name = parser.tree.sourceSlice(async_start, async_end) } },
            .{ .start = async_start, .end = async_end },
        );
        return .{ .declaration = id, .needs_semi = true };
    }

    if (tag == .class) {
        const decl = try class.parseClass(parser, .{ .is_default_export = true }, null) orelse return null;
        return .{ .declaration = decl, .needs_semi = false };
    }

    if (tag == .at) {
        const decorators_start = parser.current_token.span.start;
        const decorators = try extensions.parseDecorators(parser) orelse return null;
        const decl = try class.parseClassDecorated(parser, .{ .is_default_export = true }, decorators_start, decorators) orelse return null;
        return .{ .declaration = decl, .needs_semi = false };
    }

    if (is_ts) {
        if (tag == .abstract and try isAbstractClassNext(parser)) {
            const abstract_start = parser.current_token.span.start;
            try parser.advance() orelse return null;
            const decl = try class.parseClass(parser, .{
                .is_default_export = true,
                .is_abstract = true,
            }, abstract_start) orelse return null;
            return .{ .declaration = decl, .needs_semi = false };
        }

        if (tag == .interface) {
            const decl = try ts_statements.parseInterfaceDeclaration(parser, .{}, parser.current_token.span.start) orelse return null;
            return .{ .declaration = decl, .needs_semi = false };
        }
    }

    const expr = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
    return .{ .declaration = expr, .needs_semi = true };
}

// export * from m, optional as export name
fn parseExportAllDeclaration(parser: *Parser, start: u32, export_kind: ast.ImportOrExportKind) Error!?ast.NodeIndex {
    try parser.advance() orelse return null;

    var exported: ast.NodeIndex = .null;

    if (parser.current_token.tag == .as) {
        try parser.advance() orelse return null;
        exported = try parseModuleExportName(parser) orelse return null;
    }

    if (parser.current_token.tag != .from) {
        try parser.reportExpected(parser.current_token.span, "Expected 'from' after export *", .{
            .help = "Export all declarations require 'from': export * from 'module'",
        });
        return null;
    }
    try parser.advance() orelse return null;

    const source = try parseModuleSpecifier(parser) orelse return null;
    const attributes = try parseWithClause(parser);
    const end = try parser.eatSemicolon(parser.tree.getSpan(source).end) orelse return null;

    return try parser.tree.createNode(.{
        .export_all_declaration = .{
            .exported = exported,
            .source = source,
            .attributes = attributes,
            .export_kind = export_kind,
        },
    }, .{ .start = start, .end = end });
}

// export { } or export { } from m. caller already ate export type if needed
fn parseExportNamedFromClause(parser: *Parser, start: u32, export_kind: ast.ImportOrExportKind) Error!?ast.NodeIndex {
    const result = try parseExportSpecifiers(parser) orelse return null;
    const specifiers = result.specifiers;

    var source: ast.NodeIndex = .null;
    var attributes: ast.IndexRange = ast.IndexRange.empty;
    var end = parser.prev_token_end;

    // from m reexports. no from means local names, rewrite id names to refs
    if (parser.current_token.tag == .from) {
        try parser.advance() orelse return null;
        source = try parseModuleSpecifier(parser) orelse return null;
        attributes = try parseWithClause(parser);
        end = parser.tree.getSpan(source).end;
    } else {
        try resolveLocalExportSpecifiers(parser, result);
    }

    end = try parser.eatSemicolon(end) orelse return null;

    return try parser.tree.createNode(.{
        .export_named_declaration = .{
            .declaration = .null,
            .specifiers = specifiers,
            .source = source,
            .attributes = attributes,
            .export_kind = export_kind,
        },
    }, .{ .start = start, .end = end });
}

// export decl statement
fn parseExportWithDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();

    if (is_ts and ((try ts_statements.isStartOfTsDeclaration(parser)) orelse return null)) {
        const declaration = try ts_statements.parseTsDeclaration(parser) orelse return null;
        return try parser.tree.createNode(.{
            .export_named_declaration = .{
                .declaration = declaration,
                .specifiers = ast.IndexRange.empty,
                .source = .null,
                .attributes = ast.IndexRange.empty,
                .export_kind = exportKindForDeclaration(parser, declaration),
            },
        }, .{ .start = start, .end = parser.tree.getSpan(declaration).end });
    }

    const declaration: ast.NodeIndex = switch (parser.current_token.tag) {
        .@"var", .@"const", .let => try variables.parseVariableDeclaration(parser, .{}, null) orelse return null,
        .function => try functions.parseFunction(parser, .{}, null) orelse return null,
        .async => blk: {
            const async_start = parser.current_token.span.start;
            try parser.advance() orelse return null;
            break :blk try functions.parseFunction(parser, .{ .is_async = true }, async_start) orelse return null;
        },
        .class => try class.parseClass(parser, .{}, null) orelse return null,
        // inner class span starts at @
        .at => blk: {
            const decorators_start = parser.current_token.span.start;
            const decorators = try extensions.parseDecorators(parser) orelse return null;
            break :blk try class.parseClassDecorated(parser, .{}, decorators_start, decorators) orelse return null;
        },
        .import => if (is_ts) try parseImportDeclaration(parser) orelse return null else return reportMissingExportDeclaration(parser),
        // export public import x =, modifier is noise span kept from modifier token
        .public, .private, .static => blk: {
            if (!is_ts or !try isLegacyAccessibilityImport(parser)) return reportMissingExportDeclaration(parser);
            const modifier_start = parser.current_token.span.start;
            try parser.advance() orelse return null;
            break :blk try parseImportDeclarationFrom(parser, modifier_start) orelse return null;
        },
        else => return reportMissingExportDeclaration(parser),
    };

    return try parser.tree.createNode(.{
        .export_named_declaration = .{
            .declaration = declaration,
            .specifiers = ast.IndexRange.empty,
            .source = .null,
            .attributes = ast.IndexRange.empty,
            .export_kind = .value,
        },
    }, .{ .start = start, .end = parser.tree.getSpan(declaration).end });
}

fn reportMissingExportDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    try parser.reportExpected(parser.current_token.span, "Expected declaration after 'export'", .{
        .help = "Use 'export var', 'export let', 'export const', 'export function', or 'export class'",
    });
    return null;
}

// precollection skips leading @ on both export wrapper and inner class
pub fn parseExportDecorated(parser: *Parser, decorators: ast.IndexRange) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const is_default = parser.current_token.tag == .default;
    if (is_default) try parser.advance() orelse return null;

    const declaration = try class.parseClassDecorated(
        parser,
        .{ .is_default_export = is_default },
        null,
        decorators,
    ) orelse return null;
    const span: ast.Span = .{ .start = start, .end = parser.tree.getSpan(declaration).end };

    return try parser.tree.createNode(if (is_default) .{
        .export_default_declaration = .{ .declaration = declaration },
    } else .{
        .export_named_declaration = .{
            .declaration = declaration,
            .specifiers = ast.IndexRange.empty,
            .source = .null,
            .attributes = ast.IndexRange.empty,
            .export_kind = .value,
        },
    }, span);
}

// interface and type alias always type export kind. rest type only if declare
fn exportKindForDeclaration(parser: *Parser, declaration: ast.NodeIndex) ast.ImportOrExportKind {
    const declared = switch (parser.tree.getData(declaration)) {
        .ts_interface_declaration, .ts_type_alias_declaration => return .type,
        .ts_enum_declaration => |d| d.declare,
        .ts_module_declaration => |d| d.declare,
        .ts_global_declaration => |d| d.declare,
        .variable_declaration => |d| d.declare,
        .function => |d| d.declare,
        .class => |d| d.declare,
        else => return .value,
    };
    return if (declared) .type else .value;
}

const ExportSpecifiersResult = struct {
    specifiers: ast.IndexRange,
    local_tags: ast.IndexRange,
};

// export { } without from, locals become identifier refs in scope
fn resolveLocalExportSpecifiers(parser: *Parser, result: ExportSpecifiersResult) Error!void {
    const specs = parser.tree.getExtra(result.specifiers);
    const local_tags = parser.tree.getExtra(result.local_tags);

    for (specs, 0..) |spec_idx, i| {
        const specifier = parser.tree.getData(spec_idx).export_specifier;
        const local_data = parser.tree.getData(specifier.local);
        const local_span = parser.tree.getSpan(specifier.local);

        if (local_data == .string_literal) {
            try parser.report(local_span, "A string literal cannot be used as an exported binding without 'from'", .{
                .help = "Use: export { \"name\" } from 'some-module' or export { localName as \"name\" }",
            });
            continue;
        }

        const local_tag: TokenTag = @enumFromInt(@intFromEnum(local_tags[i]));
        if (local_tag.isReserved()) {
            const local_name = parser.tree.getString(local_data.identifier_name.name);
            try parser.report(
                local_span,
                "A reserved word cannot be used as an exported binding without 'from'",
                .{ .help = try parser.fmt("Did you mean `export {{ {s} as {s} }} from 'some-module'`?", .{ local_name, local_name }) },
            );
        }

        if (local_data == .identifier_name) {
            const new_local = try parser.tree.createNode(.{
                .identifier_reference = .{ .name = local_data.identifier_name.name },
            }, local_span);
            parser.tree.replaceData(spec_idx, .{
                .export_specifier = .{
                    .local = new_local,
                    .exported = specifier.exported,
                    .export_kind = specifier.export_kind,
                },
            });
        }
    }
}

// { foo, bar as baz } plus parallel local tag list
fn parseExportSpecifiers(parser: *Parser) Error!?ExportSpecifiersResult {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);
    const token_checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(token_checkpoint);

    if (!try parser.expect(.left_brace, "Expected '{' to start export specifiers", null)) return null;

    while (parser.current_token.tag != .right_brace and parser.current_token.tag != .eof) {
        const local_tag = parser.current_token.tag;

        const spec = try parseExportSpecifier(parser) orelse return null;

        try parser.scratch_a.append(parser.allocator(), spec);

        try parser.scratch_b.append(parser.allocator(), @enumFromInt(@as(u32, @intFromEnum(local_tag))));

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
        } else {
            break;
        }
    }

    if (!try parser.expect(.right_brace, "Expected '}' to close export specifiers", null)) return null;

    return .{
        .specifiers = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint),
        .local_tags = try parser.createExtraFromScratch(&parser.scratch_b, token_checkpoint),
    };
}

// local before as, exported after, ts type split like import
fn parseExportSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const parts = try parseSpecifierParts(parser) orelse return null;

    // local is before as slot, exported after, one node if no rename
    const local = if (parts.property_name != .null) parts.property_name else parts.name;
    const exported = parts.name;

    const end = parser.tree.getSpan(exported).end;

    return try parser.tree.createNode(.{
        .export_specifier = .{
            .local = local,
            .exported = exported,
            .export_kind = parts.kind,
        },
    }, .{ .start = start, .end = end });
}

// string or id
fn parseModuleExportName(parser: *Parser) Error!?ast.NodeIndex {
    const tag = parser.current_token.tag;

    if (tag == .string_literal) {
        if (parser.current_token.hasLoneSurrogates()) {
            try parser.report(parser.current_token.span, "An export name cannot include a unicode lone surrogate", .{});
            return null;
        }
        return literals.parseStringLiteral(parser);
    }

    if (tag.isIdentifierLike()) return literals.parseIdentifierName(parser);

    try parser.reportExpected(parser.current_token.span, "Expected identifier or string literal", .{});
    return null;
}

// module string
fn parseModuleSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .string_literal) {
        try parser.reportExpected(parser.current_token.span, "Expected module specifier", .{
            .help = "Module specifiers must be string literals, e.g., './module.js' or 'package'",
        });
        return null;
    }

    return literals.parseStringLiteral(parser);
}

// import attributes with { } or legacy assert { }
fn parseWithClause(parser: *Parser) Error!ast.IndexRange {
    if (parser.current_token.tag != .with and parser.current_token.tag != .assert) {
        return ast.IndexRange.empty;
    }

    try parser.advance() orelse return ast.IndexRange.empty;

    if (!try parser.expect(.left_brace, "Expected '{' after 'with' in import attributes", null)) {
        return ast.IndexRange.empty;
    }

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .right_brace and parser.current_token.tag != .eof) {
        const attr = try parseImportAttribute(parser) orelse return ast.IndexRange.empty;
        try parser.scratch_a.append(parser.allocator(), attr);

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return ast.IndexRange.empty;
        } else {
            break;
        }
    }

    if (!try parser.expect(.right_brace, "Expected '}' to close import attributes", null)) {
        return ast.IndexRange.empty;
    }

    return parser.createExtraFromScratch(&parser.scratch_a, checkpoint);
}

// attr key string value
fn parseImportAttribute(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const key = try parseAttributeKey(parser) orelse return null;

    if (!try parser.expect(.colon, "Expected ':' in import attribute", null)) return null;

    if (parser.current_token.tag != .string_literal) {
        try parser.report(parser.current_token.span, "Import attribute value must be a string literal", .{});
        return null;
    }

    const value = try literals.parseStringLiteral(parser) orelse return null;

    return try parser.tree.createNode(.{
        .import_attribute = .{
            .key = key,
            .value = value,
        },
    }, .{ .start = start, .end = parser.tree.getSpan(value).end });
}

fn parseAttributeKey(parser: *Parser) Error!?ast.NodeIndex {
    const tag = parser.current_token.tag;
    if (tag == .string_literal) return literals.parseStringLiteral(parser);
    if (tag.isIdentifierLike()) return literals.parseIdentifierName(parser);

    try parser.reportExpected(parser.current_token.span, "Expected identifier or string literal for attribute key", .{});
    return null;
}

// import(), import(,opts), import.source, import.defer
pub fn parseDynamicImport(parser: *Parser, import_keyword: ast.NodeIndex, phase: ?ast.ImportPhase) Error!?ast.NodeIndex {
    const start = parser.tree.getSpan(import_keyword).start;

    if (!try parser.expect(.left_paren, "Expected '(' after import", null)) return null;

    const source = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    var options: ast.NodeIndex = .null;

    // second arg only on plain import(), not import.source defer
    if (phase == null and parser.current_token.tag == .comma) {
        try parser.advance() orelse return null;

        if (parser.current_token.tag != .right_paren) {
            options = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

            if (parser.current_token.tag == .comma) {
                try parser.advance() orelse return null;
            }
        }
    }

    const end = parser.current_token.span.end;

    if (!try parser.expect(.right_paren, "Expected ')' after import()", "Dynamic import call must end with ')'")) return null;

    return try parser.tree.createNode(.{
        .import_expression = .{
            .source = source,
            .options = options,
            .phase = phase,
        },
    }, .{ .start = start, .end = end });
}
