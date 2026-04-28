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

/// `parseImportDeclaration` with an explicit start position. used by
/// `export <accessibility> import x = ...` so the inner declaration's
/// span begins at the legacy modifier instead of at `import`.
pub fn parseImportDeclarationFrom(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume 'import'

    // side-effect import: import 'module'
    if (parser.current_token.tag == .string_literal) {
        return parseSideEffectImport(parser, start, null);
    }

    var phase: ?ast.ImportPhase = null;
    var import_kind: ast.ImportOrExportKind = .value;

    const peek = try parser.peekAheadN(2);
    const next = peek[0] orelse return null;

    // import type ... (ts type-only import)
    if (parser.tree.isTs() and parser.current_token.tag == .type and
        isTypeImportModifier(next, peek[1]))
    {
        import_kind = .type;
        try parser.advance() orelse return null; // consume 'type'
    }
    // import source X from "X"
    else if (parser.current_token.tag == .source and next.tag.isIdentifierLike() and next.tag != .from) {
        phase = .source;
        try parser.advance() orelse return null;
    }
    // import defer * as X from "X"
    else if (parser.current_token.tag == .@"defer" and next.tag == .star) {
        phase = .@"defer";
        try parser.advance() orelse return null;
    }

    // `import x = <module reference>`. the equals form takes over
    // whenever an identifier head is followed by `=`, including the
    // type-only `import type x = ...` shape.
    if (parser.tree.isTs() and parser.current_token.tag.isIdentifierLike()) {
        const after_id = try parser.peekAhead() orelse return null;
        if (after_id.tag == .assign) {
            return parseImportEqualsDeclaration(parser, start, import_kind);
        }
    }

    // regular import, parse import clause (specifiers)
    const specifiers = try parseImportClause(parser) orelse return null;

    if (parser.current_token.tag != .from) {
        try parser.reportExpected(parser.current_token.span, "Expected 'from' after import clause", .{
            .help = "Import statements require 'from' followed by a module specifier: import x from 'module'",
        });
        return null;
    }

    try parser.advance() orelse return null; // consume 'from'

    const source = try parseModuleSpecifier(parser) orelse return null;

    // parse optional 'with' clause
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

/// decides whether a `type` token immediately after `import` is the type-only
/// modifier (as in `import type { X } from "m"`) or the default import name
/// (as in `import type from "m"`).
fn isTypeImportModifier(after_type: Token, after_after: ?Token) bool {
    if (after_type.tag == .left_brace or after_type.tag == .star) return true;
    if (!after_type.tag.isIdentifierLike()) return false;
    if (after_type.tag == .from) {
        const a2 = after_after orelse return false;
        return a2.tag == .from or a2.tag == .assign;
    }
    return true;
}

/// `import x = <module reference>`
fn parseImportEqualsDeclaration(
    parser: *Parser,
    start: u32,
    import_kind: ast.ImportOrExportKind,
) Error!?ast.NodeIndex {
    const id = try literals.parseBindingIdentifier(parser) orelse return null;

    if (!try parser.expect(
        .assign,
        "Expected '=' in import equals declaration",
        "An import equals declaration is written 'import x = Foo.Bar' or 'import x = require(\"m\")'",
    )) return null;

    const module_reference = try parseModuleReference(parser) orelse return null;
    const end = try parser.eatSemicolon(parser.tree.getSpan(module_reference).end) orelse return null;

    return try parser.tree.createNode(.{
        .ts_import_equals_declaration = .{
            .id = id,
            .module_reference = module_reference,
            .import_kind = import_kind,
        },
    }, .{ .start = start, .end = end });
}

/// ModuleReference : TSExternalModuleReference or EntityName. `require("m")`
/// is the external form, anything else is a dotted identifier chain.
fn parseModuleReference(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .require) {
        const next = try parser.peekAhead() orelse return null;
        if (next.tag == .left_paren) return parseExternalModuleReference(parser);
    }
    return parseEntityName(parser);
}

/// `require("m")` on the right hand side of an `import x = ...` declaration.
fn parseExternalModuleReference(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'require'

    if (!try parser.expect(
        .left_paren,
        "Expected '(' after 'require' in import equals declaration",
        "External module references are written 'require(\"module\")'",
    )) return null;

    const expression = try literals.parseStringLiteral(parser) orelse return null;

    const end = parser.current_token.span.end;
    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close 'require' call in import equals declaration",
        null,
    )) return null;

    return try parser.tree.createNode(.{
        .ts_external_module_reference = .{ .expression = expression },
    }, .{ .start = start, .end = end });
}

/// a dotted entity name used as the right hand side of an `import x = ...`
/// declaration
fn parseEntityName(parser: *Parser) Error!?ast.NodeIndex {
    var name = try literals.parseIdentifier(parser) orelse return null;

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

/// side-effect import: import 'module'
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

/// ImportClause :
///   ImportedDefaultBinding
///   NameSpaceImport
///   NamedImports
///   ImportedDefaultBinding , NameSpaceImport
///   ImportedDefaultBinding , NamedImports
fn parseImportClause(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    // namespace import: * as name
    if (parser.current_token.tag == .star) {
        const ns = try parseImportNamespaceSpecifier(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), ns);
        return try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);
    }

    // named imports: { foo, bar }
    if (parser.current_token.tag == .left_brace) {
        return parseNamedImports(parser);
    }

    // default import: import foo from 'module'
    const default_import = try parseImportDefaultSpecifier(parser) orelse return null;

    try parser.scratch_a.append(parser.allocator(), default_import);

    //     import foo, * as bar from 'module'
    // or: import foo, { bar } from 'module'
    if (parser.current_token.tag == .comma) {
        try parser.advance() orelse return null; // consume ','

        if (parser.current_token.tag == .star) {
            const ns = try parseImportNamespaceSpecifier(parser) orelse return null;
            try parser.scratch_a.append(parser.allocator(), ns);
        } else if (parser.current_token.tag == .left_brace) {
            const named = try parseNamedImports(parser) orelse return null;
            // append all named imports
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

/// default import specifier: import foo from 'module'
///                                  ~~~
fn parseImportDefaultSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const local = try parseImportedBinding(parser) orelse return null;
    const end = parser.tree.getSpan(local).end;

    return try parser.tree.createNode(.{
        .import_default_specifier = .{ .local = local },
    }, .{ .start = start, .end = end });
}

/// namespace import: * as name
fn parseImportNamespaceSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (!try parser.expect(.star, "Expected '*' for namespace import", null)) return null;

    if (parser.current_token.tag != .as) {
        try parser.reportExpected(parser.current_token.span, "Expected 'as' after '*' in namespace import", .{
            .help = "Namespace imports must use the form: * as name",
        });
        return null;
    }
    try parser.advance() orelse return null; // consume 'as'

    const local = try parseImportedBinding(parser) orelse return null;
    const end = parser.tree.getSpan(local).end;

    return try parser.tree.createNode(.{
        .import_namespace_specifier = .{ .local = local },
    }, .{ .start = start, .end = end });
}

/// named imports: { foo, bar as baz }
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

/// import specifier: foo or foo as bar or "string" as bar or type foo or type foo as bar
fn parseImportSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const parts = try parseSpecifierParts(parser) orelse return null;

    // imported is the module-side name. when `as` or `type as as <name>` promoted a
    // `propertyName`, use it, otherwise the single parsed name serves both roles.
    const imported = if (parts.property_name != .null) parts.property_name else parts.name;

    const name_data = parser.tree.getData(parts.name);

    // string literal cannot be used as a local binding name
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

/// shared parse result for import and export specifiers. `property_name` is
/// the `imported` (for import) or `local` (for export) side when the specifier
/// has an explicit `as` rename or `type as as <name>` disambiguation, otherwise
/// `.null`, meaning the single `name` serves both roles. `kind` carries the
/// `type` modifier resolved from the specifier-level disambiguation.
const SpecifierParts = struct {
    property_name: ast.NodeIndex = .null,
    name: ast.NodeIndex,
    /// the token whose tag governs `name` validation (reserved-word check etc.)
    name_token: Token,
    kind: ast.ImportOrExportKind = .value,
};

/// parses one import or export specifier head, disambiguating the
/// optional `type` modifier. the possibilities when the first token is
/// `type` are:
///
/// ```
/// { type }             -> kind=value, name=type
/// { type as }          -> kind=type,  name=as
/// { type as as }       -> kind=value, propertyName=type, name=as
/// { type as as X }     -> kind=type,  propertyName=as,   name=X
/// { type as X }        -> kind=value, propertyName=type, name=X
/// { type X }           -> kind=type,                     name=X
/// { type X as Y }      -> kind=type,  propertyName=X,    name=Y
/// ```
fn parseSpecifierParts(parser: *Parser) Error!?SpecifierParts {
    const is_ts = parser.tree.isTs();
    const first_token = parser.current_token;

    const first = try parseModuleExportName(parser) orelse return null;

    var parts: SpecifierParts = .{
        .name = first,
        .name_token = first_token,
    };

    var can_parse_outer_as = true;

    if (is_ts and first_token.tag == .type and parser.tree.getData(first) == .identifier_name) {
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
                    can_parse_outer_as = false;
                } else {
                    // `type as as`
                    parts.property_name = first;
                    parts.name = second_as;
                    parts.name_token = second_as_token;
                    can_parse_outer_as = false;
                }
            } else if (canStartModuleExportName(parser.current_token.tag)) {
                // `type as <name>`
                parts.property_name = first;
                parts.name_token = parser.current_token;
                parts.name = try parseModuleExportName(parser) orelse return null;
                can_parse_outer_as = false;
            } else {
                // `type as`
                parts.kind = .type;
                parts.name = first_as;
                parts.name_token = first_as_token;
            }
        } else if (canStartModuleExportName(parser.current_token.tag)) {
            // `type <name>`
            parts.kind = .type;
            parts.name_token = parser.current_token;
            parts.name = try parseModuleExportName(parser) orelse return null;
        }
        // else: `type` (terminator) - nothing to do, shorthand with name=first
    }

    if (can_parse_outer_as and parser.current_token.tag == .as) {
        try parser.advance() orelse return null; // consume 'as'
        parts.property_name = parts.name;
        parts.name_token = parser.current_token;
        parts.name = try parseModuleExportName(parser) orelse return null;
    }

    return parts;
}

fn canStartModuleExportName(tag: TokenTag) bool {
    return tag.isIdentifierLike() or tag == .string_literal;
}

/// ImportedBinding: BindingIdentifier[~Yield, +Await]
fn parseImportedBinding(parser: *Parser) Error!?ast.NodeIndex {
    return literals.parseBindingIdentifier(parser);
}

pub fn parseExportDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'export'

    // export = expression
    if (parser.tree.isTs() and parser.current_token.tag == .assign) {
        return parseTSExportAssignment(parser, start);
    }

    // export as namespace name
    if (parser.tree.isTs() and parser.current_token.tag == .as) {
        return parseTSNamespaceExportDeclaration(parser, start);
    }

    if (parser.tree.isTs() and parser.current_token.tag == .type) {
        const next = try parser.peekAhead() orelse return null;
        if (next.tag == .left_brace) {
            try parser.advance() orelse return null; // consume 'type'
            return parseExportNamedFromClause(parser, start, .type);
        }
        if (next.tag == .star) {
            try parser.advance() orelse return null; // consume 'type'
            return parseExportAllDeclaration(parser, start, .type);
        }
    }

    // export default ...
    if (parser.current_token.tag == .default) {
        return parseExportDefaultDeclaration(parser, start);
    }

    // export * from 'module'
    // export * as name from 'module'
    if (parser.current_token.tag == .star) {
        return parseExportAllDeclaration(parser, start, .value);
    }

    // export { foo, bar }
    // export { foo } from 'module'
    if (parser.current_token.tag == .left_brace) {
        return parseExportNamedFromClause(parser, start, .value);
    }

    // export var/let/const/function/class
    return parseExportWithDeclaration(parser, start);
}

/// export = expression
fn parseTSExportAssignment(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume '='

    const expression = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    const end = try parser.eatSemicolon(parser.tree.getSpan(expression).end) orelse return null;

    return try parser.tree.createNode(.{
        .ts_export_assignment = .{ .expression = expression },
    }, .{ .start = start, .end = end });
}

/// export as namespace name
fn parseTSNamespaceExportDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume 'as'

    if (parser.current_token.tag != .namespace) {
        try parser.reportExpected(parser.current_token.span, "Expected 'namespace' after 'export as'", .{});
        return null;
    }

    try parser.advance() orelse return null; // consume 'namespace'

    const id = try literals.parseIdentifierName(parser) orelse return null;
    const end = try parser.eatSemicolon(parser.tree.getSpan(id).end) orelse return null;

    return try parser.tree.createNode(.{
        .ts_namespace_export_declaration = .{ .id = id },
    }, .{ .start = start, .end = end });
}

/// peek lookahead for `abstract class` after an `abstract` head, sharing the
/// same-line rule used by `isStartOfTsDeclaration`.
fn isAbstractClassNext(parser: *Parser) Error!bool {
    const next = try parser.peekAhead() orelse return false;
    return next.tag == .class and !next.hasLineTerminatorBefore();
}

/// `export public import x = ...` and friends. legacy TS allows an
/// accessibility-style modifier here; only the `import` form is valid,
/// nothing else uses these tokens at export-position.
fn isLegacyAccessibilityImport(parser: *Parser) Error!bool {
    const next = try parser.peekAhead() orelse return false;
    return next.tag == .import and !next.hasLineTerminatorBefore();
}

/// export default declaration
fn parseExportDefaultDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume 'default'

    var declaration: ast.NodeIndex = undefined;
    var is_decl = false;

    // export default function [name]() {}
    if (parser.current_token.tag == .function) {
        declaration = try functions.parseFunction(parser, .{ .is_default_export = true }, null) orelse return null;
        is_decl = true;
    }

    // export default async function [name]() {}
    else if (parser.current_token.tag == .async and !parser.current_token.hasLineTerminatorBefore()) {
        const async_start = parser.current_token.span.start;
        try parser.advance() orelse return null; // consume 'async'
        if (parser.current_token.tag == .function) {
            declaration = try functions.parseFunction(parser, .{ .is_default_export = true, .is_async = true }, async_start) orelse return null;
            is_decl = true;
        } else {
            // if it's not a async function, it's an identifier
            // export default async;
            const async_end = async_start + 5;

            declaration = try parser.tree.createNode(.{
                .identifier_reference = .{
                    .name = parser.tree.sourceSlice(async_start, async_end),
                },
            }, .{ .start = async_start, .end = async_end });
        }
    }

    // export default class [name] {}
    else if (parser.current_token.tag == .class) {
        declaration = try class.parseClass(parser, .{ .is_default_export = true }, null) orelse return null;
        is_decl = true;
    }
    // export default @decorator class
    else if (parser.current_token.tag == .at) {
        const decorators_start = parser.current_token.span.start;
        const decorators = try extensions.parseDecorators(parser) orelse return null;
        declaration = try class.parseClassDecorated(parser, .{ .is_default_export = true }, decorators_start, decorators) orelse return null;
        is_decl = true;
    }
    // export default abstract class [name] {}
    else if (parser.tree.isTs() and parser.current_token.tag == .abstract and try isAbstractClassNext(parser)) {
        const abstract_start = parser.current_token.span.start;
        try parser.advance() orelse return null; // consume 'abstract'
        declaration = try class.parseClass(parser, .{
            .is_default_export = true,
            .is_abstract = true,
        }, abstract_start) orelse return null;
        is_decl = true;
    }
    // export default interface Foo {}
    else if (parser.tree.isTs() and parser.current_token.tag == .interface) {
        declaration = try ts_statements.parseInterfaceDeclaration(parser, .{}, parser.current_token.span.start) orelse return null;
        is_decl = true;
    }
    // export default expression
    else {
        declaration = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
    }

    const decl_span = parser.tree.getSpan(declaration);

    // function/class declarations don't need semicolon
    const end = if (is_decl)
        decl_span.end
    else
        try parser.eatSemicolon(decl_span.end) orelse return null;

    return try parser.tree.createNode(.{
        .export_default_declaration = .{ .declaration = declaration },
    }, .{ .start = start, .end = end });
}

/// export * from 'module' or export * as name from 'module'
fn parseExportAllDeclaration(parser: *Parser, start: u32, export_kind: ast.ImportOrExportKind) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume '*'

    var exported: ast.NodeIndex = .null;

    // export * as name from 'module'
    if (parser.current_token.tag == .as) {
        try parser.advance() orelse return null; // consume 'as'
        exported = try parseModuleExportName(parser) orelse return null;
    }

    // expect 'from'
    if (parser.current_token.tag != .from) {
        try parser.reportExpected(parser.current_token.span, "Expected 'from' after export *", .{
            .help = "Export all declarations require 'from': export * from 'module'",
        });
        return null;
    }
    try parser.advance() orelse return null; // consume 'from'

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

/// export { foo, bar } or export { foo } from 'module', with optional leading
/// `type` modifier consumed by the caller (`export_kind = .type`).
fn parseExportNamedFromClause(parser: *Parser, start: u32, export_kind: ast.ImportOrExportKind) Error!?ast.NodeIndex {
    const result = try parseExportSpecifiers(parser) orelse return null;
    const specifiers = result.specifiers;

    var source: ast.NodeIndex = .null;
    var attributes: ast.IndexRange = ast.IndexRange.empty;
    var end = parser.prev_token_end;

    // re-export: export { foo } from 'module'
    if (parser.current_token.tag == .from) {
        try parser.advance() orelse return null; // consume 'from'
        source = try parseModuleSpecifier(parser) orelse return null;
        attributes = try parseWithClause(parser);
        end = parser.tree.getSpan(source).end;
    } else {
        const specs = parser.tree.getExtra(specifiers);
        const local_tags = parser.tree.getExtra(result.local_tags);

        for (specs, 0..) |spec_idx, i| {
            const specifier = parser.tree.getData(spec_idx).export_specifier;
            const local_data = parser.tree.getData(specifier.local);
            const local_span = parser.tree.getSpan(specifier.local);

            if (local_data == .string_literal) {
                try parser.report(local_span, "A string literal cannot be used as an exported binding without 'from'", .{
                    .help = "Use: export { \"name\" } from 'some-module' or export { localName as \"name\" }",
                });
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

            // convert local from identifier_name to identifier_reference
            // since it references a local binding (not a re-export)
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

/// export var/let/const/function/class
fn parseExportWithDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    if (parser.tree.isTs()) {
        if ((try ts_statements.isStartOfTsDeclaration(parser)) orelse return null) {
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
    }

    var declaration: ast.NodeIndex = undefined;

    switch (parser.current_token.tag) {
        .@"var", .@"const", .let => {
            declaration = try variables.parseVariableDeclaration(parser, .{}, null) orelse return null;
        },
        .function => {
            declaration = try functions.parseFunction(parser, .{}, null) orelse return null;
        },
        .async => {
            const async_start = parser.current_token.span.start;
            try parser.advance() orelse return null; // consume 'async'
            declaration = try functions.parseFunction(parser, .{ .is_async = true }, async_start) orelse return null;
        },
        .class => {
            declaration = try class.parseClass(parser, .{}, null) orelse return null;
        },
        // `export @dec class`: inner `ClassDeclaration` spans from the `@`.
        .at => {
            const decorators_start = parser.current_token.span.start;
            const decorators = try extensions.parseDecorators(parser) orelse return null;
            declaration = try class.parseClassDecorated(parser, .{}, decorators_start, decorators) orelse return null;
        },
        // `export import x = ...`
        .import => if (parser.tree.isTs()) {
            declaration = try parseImportDeclaration(parser) orelse return null;
        } else {
            try parser.reportExpected(parser.current_token.span, "Expected declaration after 'export'", .{
                .help = "Use 'export var', 'export let', 'export const', 'export function', or 'export class'",
            });
            return null;
        },
        // legacy `export public/private/static import x = ...`. the
        // accessibility modifier carries no semantics on an import equals
        // declaration and is silently consumed; the inner declaration's
        // span starts at the modifier so the source range is preserved.
        .public, .private, .static => if (parser.tree.isTs() and try isLegacyAccessibilityImport(parser)) {
            const modifier_start = parser.current_token.span.start;
            try parser.advance() orelse return null; // consume modifier
            declaration = try parseImportDeclarationFrom(parser, modifier_start) orelse return null;
        } else {
            try parser.reportExpected(parser.current_token.span, "Expected declaration after 'export'", .{
                .help = "Use 'export var', 'export let', 'export const', 'export function', or 'export class'",
            });
            return null;
        },
        else => {
            try parser.reportExpected(parser.current_token.span, "Expected declaration after 'export'", .{
                .help = "Use 'export var', 'export let', 'export const', 'export function', or 'export class'",
            });
            return null;
        },
    }

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

/// `@dec export [default] class C`. `decorators` were collected at
/// statement position by the caller, both the wrapping export and the
/// inner `ClassDeclaration` exclude the leading `@`
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

fn exportKindForDeclaration(parser: *Parser, declaration: ast.NodeIndex) ast.ImportOrExportKind {
    return switch (parser.tree.getData(declaration)) {
        .ts_interface_declaration, .ts_type_alias_declaration => .type,
        .ts_enum_declaration => |d| if (d.declare) .type else .value,
        .ts_module_declaration => |d| if (d.declare) .type else .value,
        .ts_global_declaration => |d| if (d.declare) .type else .value,
        .variable_declaration => |d| if (d.declare) .type else .value,
        .function => |d| if (d.declare) .type else .value,
        .class => |d| if (d.declare) .type else .value,
        else => .value,
    };
}

const ExportSpecifiersResult = struct {
    specifiers: ast.IndexRange,
    local_tags: ast.IndexRange,
};

/// export specifiers: { foo, bar as baz }
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

/// export specifier: foo, foo as bar, type foo, or type foo as bar
fn parseExportSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const parts = try parseSpecifierParts(parser) orelse return null;

    // local (module-local binding) is the name before `as`, exported (module
    // public name) is the name after `as`. when there is no rename both roles
    // share the single parsed `name` node.
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

/// ModuleExportName: IdentifierName or StringLiteral
fn parseModuleExportName(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .string_literal) {
        if (parser.current_token.hasLoneSurrogates()) {
            try parser.report(parser.current_token.span, "An export name cannot include a unicode lone surrogate", .{});
            return null;
        }

        return literals.parseStringLiteral(parser);
    }

    if (parser.current_token.tag.isIdentifierLike()) {
        return try literals.parseIdentifierName(parser) orelse return null;
    }

    try parser.reportExpected(parser.current_token.span, "Expected identifier or string literal", .{});

    return null;
}

/// ModuleSpecifier: StringLiteral
fn parseModuleSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .string_literal) {
        try parser.reportExpected(parser.current_token.span, "Expected module specifier", .{
            .help = "Module specifiers must be string literals, e.g., './module.js' or 'package'",
        });
        return null;
    }

    return literals.parseStringLiteral(parser);
}

/// WithClause / ImportAttributes
/// WithClause :
///   with { }
///   with { WithEntries ,? }
fn parseWithClause(parser: *Parser) Error!ast.IndexRange {
    // check for 'with' or 'assert' keyword
    if (parser.current_token.tag != .with and parser.current_token.tag != .assert) {
        return ast.IndexRange.empty;
    }

    try parser.advance() orelse return ast.IndexRange.empty; // consume 'with' or 'assert'

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

/// ImportAttribute: key : value
fn parseImportAttribute(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    // IdentifierName or StringLiteral
    const key = try parseAttributeKey(parser) orelse return null;

    if (!try parser.expect(.colon, "Expected ':' in import attribute", null)) return null;

    // value (must be StringLiteral)
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

/// AttributeKey: IdentifierName or StringLiteral
fn parseAttributeKey(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .string_literal) {
        return literals.parseStringLiteral(parser);
    }

    if (parser.current_token.tag.isIdentifierLike()) {
        return try literals.parseIdentifierName(parser) orelse return null;
    }

    try parser.reportExpected(parser.current_token.span, "Expected identifier or string literal for attribute key", .{});
    return null;
}

/// dynamic import: import(source), import(source, options), import.source(source), import.defer(source)
pub fn parseDynamicImport(parser: *Parser, import_keyword: ast.NodeIndex, phase: ?ast.ImportPhase) Error!?ast.NodeIndex {
    const start = parser.tree.getSpan(import_keyword).start;

    if (!try parser.expect(.left_paren, "Expected '(' after import", null)) return null;

    // source expression
    const source = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    var options: ast.NodeIndex = .null;

    // check for options argument (only for regular imports, not phase imports)
    if (phase == null and parser.current_token.tag == .comma) {
        // allow trailing comma
        try parser.advance() orelse return null; // consume ','

        if (parser.current_token.tag != .right_paren) {
            options = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

            // allow trailing comma after options
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
