const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const token = @import("../token.zig");
const Precedence = @import("../token.zig").Precedence;

const expressions = @import("expressions.zig");
const statements = @import("statements.zig");
const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const extensions = @import("extensions.zig");
const variables = @import("variables.zig");

pub fn parseImportDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'import'

    // side-effect import: import 'module'
    if (parser.current_token.tag == .string_literal) {
        return parseSideEffectImport(parser, start, null);
    }

    var phase: ?ast.ImportPhase = null;

    const next = try parser.lookAhead() orelse return null;

    // import source X from "X"
    if (parser.current_token.tag == .source and next.tag.isIdentifierLike() and next.tag != .from) {
        phase = .source;
        try parser.advance() orelse return null;
    }
    // import defer * as X from "X"
    else if (parser.current_token.tag == .@"defer" and next.tag == .star) {
        phase = .@"defer";
        try parser.advance() orelse return null;
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

    const end = try parser.eatSemicolon(parser.getSpan(source).end) orelse return null;

    return try parser.addNode(.{
        .import_declaration = .{
            .specifiers = specifiers,
            .source = source,
            .attributes = attributes,
            .phase = phase,
        },
    }, .{ .start = start, .end = end });
}

/// side-effect import: import 'module'
fn parseSideEffectImport(parser: *Parser, start: u32, phase: ?ast.ImportPhase) Error!?ast.NodeIndex {
    const source = try parseModuleSpecifier(parser) orelse return null;
    const attributes = try parseWithClause(parser);
    const end = try parser.eatSemicolon(parser.getSpan(source).end) orelse return null;

    return try parser.addNode(.{
        .import_declaration = .{
            .specifiers = ast.IndexRange.empty,
            .source = source,
            .attributes = attributes,
            .phase = phase,
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
        return try parser.addExtraFromScratch(&parser.scratch_a, checkpoint);
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
            for (parser.getExtra(named)) |spec| {
                try parser.scratch_a.append(parser.allocator(), spec);
            }
        } else {
            try parser.reportExpected(parser.current_token.span, "Expected namespace import (* as name) or named imports ({...}) after ','", .{});
            return null;
        }
    }

    return try parser.addExtraFromScratch(&parser.scratch_a, checkpoint);
}

/// default import specifier: import foo from 'module'
///                                  ~~~
fn parseImportDefaultSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const local = try parseImportedBinding(parser) orelse return null;
    const end = parser.getSpan(local).end;

    return try parser.addNode(.{
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
    const end = parser.getSpan(local).end;

    return try parser.addNode(.{
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

    return try parser.addExtraFromScratch(&parser.scratch_a, checkpoint);
}

/// import specifier: foo or foo as bar or "string" as bar
///                   ~~~    ~~~~~~~~~~    ~~~~~~~~~~~~~~~
fn parseImportSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const imported_token = parser.current_token;

    // parse imported name
    const imported = try parseModuleExportName(parser) orelse return null;

    var local: ast.NodeIndex = undefined;

    // check for 'as' alias
    if (parser.current_token.tag == .as) {
        try parser.advance() orelse return null; // consume 'as'
        local = try parseImportedBinding(parser) orelse return null;
    } else {
        // no alias - local is the same as imported
        // but we need to convert IdentifierName to BindingIdentifier if it's not a string

        const imported_data = parser.getData(imported);

        if (imported_data == .string_literal) {
            try parser.report(parser.getSpan(imported), "String literal imports require an 'as' clause", .{
                .help = "Use: import { \"name\" as localName } from 'module'",
            });
            return null;
        }

        // convert identifier_name to binding_identifier
        // since it is now a binding identifier, we need to validate like reserved words, etc.
        if (!try literals.validateIdentifier(parser, "an imported binding", imported_token)) {
            return null;
        }

        const id_data = imported_data.identifier_name;

        parser.setData(imported, .{
            .binding_identifier = .{
                .name_start = id_data.name_start,
                .name_len = id_data.name_len,
            },
        });

        local = imported;
    }

    const end = parser.getSpan(local).end;

    return try parser.addNode(.{
        .import_specifier = .{
            .imported = imported,
            .local = local,
        },
    }, .{ .start = start, .end = end });
}

/// ImportedBinding: BindingIdentifier[~Yield, +Await]
fn parseImportedBinding(parser: *Parser) Error!?ast.NodeIndex {
    return patterns.parseBindingIdentifier(parser);
}

pub fn parseExportDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'export'

    // export = expression
    if (parser.isTs() and parser.current_token.tag == .assign) {
        return parseTSExportAssignment(parser, start);
    }

    // export as namespace name
    if (parser.isTs() and parser.current_token.tag == .as) {
        return parseTSNamespaceExportDeclaration(parser, start);
    }

    // export default ...
    if (parser.current_token.tag == .default) {
        return parseExportDefaultDeclaration(parser, start);
    }

    // export * from 'module'
    // export * as name from 'module'
    if (parser.current_token.tag == .star) {
        return parseExportAllDeclaration(parser, start);
    }

    // export { foo, bar }
    // export { foo } from 'module'
    if (parser.current_token.tag == .left_brace) {
        return parseExportNamedFromClause(parser, start);
    }

    // export var/let/const/function/class
    return parseExportWithDeclaration(parser, start);
}

/// export = expression
fn parseTSExportAssignment(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume '='

    const expression = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
    const end = try parser.eatSemicolon(parser.getSpan(expression).end) orelse return null;

    return try parser.addNode(.{
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
    const end = try parser.eatSemicolon(parser.getSpan(id).end) orelse return null;

    return try parser.addNode(.{
        .ts_namespace_export_declaration = .{ .id = id },
    }, .{ .start = start, .end = end });
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

            declaration = try parser.addNode(.{
                .identifier_reference = .{
                    .name_start = async_start,
                    .name_len = @intCast(async_end - async_start),
                },
            }, .{ .start = async_start, .end = async_end });
        }
    }

    // export default class [name] {}
    else if (parser.current_token.tag == .class) {
        declaration = try class.parseClass(parser, .{ .is_default_export = true }, null) orelse return null;
        is_decl = true;
    }
    // export default @decorator [declaration]
    else if (parser.current_token.tag == .at) {
        declaration = try extensions.parseDecorated(parser, .{ .is_default_export = true }) orelse return null;
        is_decl = true;
    }
    // export default expression
    else {
        declaration = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
    }

    const decl_span = parser.getSpan(declaration);

    // function/class declarations don't need semicolon
    const end = if (is_decl)
        decl_span.end
    else
        try parser.eatSemicolon(decl_span.end) orelse return null;

    return try parser.addNode(.{
        .export_default_declaration = .{ .declaration = declaration },
    }, .{ .start = start, .end = end });
}

/// export * from 'module' or export * as name from 'module'
fn parseExportAllDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume '*'

    var exported: ast.NodeIndex = ast.null_node;

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
    const end = try parser.eatSemicolon(parser.getSpan(source).end) orelse return null;

    return try parser.addNode(.{
        .export_all_declaration = .{
            .exported = exported,
            .source = source,
            .attributes = attributes,
        },
    }, .{ .start = start, .end = end });
}

/// export { foo, bar } or export { foo } from 'module'
fn parseExportNamedFromClause(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    const result = try parseExportSpecifiers(parser) orelse return null;
    const specifiers = result.specifiers;

    var source: ast.NodeIndex = ast.null_node;
    var attributes: ast.IndexRange = ast.IndexRange.empty;
    var end = parser.current_token.span.start;

    // re-export: export { foo } from 'module'
    if (parser.current_token.tag == .from) {
        try parser.advance() orelse return null; // consume 'from'
        source = try parseModuleSpecifier(parser) orelse return null;
        attributes = try parseWithClause(parser);
        end = parser.getSpan(source).end;
    } else {
        const specs = parser.getExtra(specifiers);
        const local_tags = parser.getExtra(result.local_tags);

        for (specs, 0..) |spec_idx, i| {
            const specifier = parser.getData(spec_idx).export_specifier;
            const local_data = parser.getData(specifier.local);
            const local_span = parser.getSpan(specifier.local);

            if (local_data == .string_literal) {
                try parser.report(local_span, "A string literal cannot be used as an exported binding without 'from'", .{
                    .help = "Use: export { \"name\" } from 'some-module' or export { localName as \"name\" }",
                });
            }

            const local_tag: token.TokenTag = @enumFromInt(local_tags[i]);

            if (local_tag.isReserved()) {
                const local_name = parser.getSourceText(local_data.identifier_name.name_start, local_data.identifier_name.name_len);

                try parser.reportFmt(
                    local_span,
                    "A reserved word cannot be used as an exported binding without 'from'",
                    .{},
                    .{ .help = try parser.formatMessage("Did you mean `export {{ {s} as {s} }} from 'some-module'`?", .{ local_name, local_name }) },
                );
            }
        }
    }

    end = try parser.eatSemicolon(end) orelse return null;

    return try parser.addNode(.{
        .export_named_declaration = .{
            .declaration = ast.null_node,
            .specifiers = specifiers,
            .source = source,
            .attributes = attributes,
        },
    }, .{ .start = start, .end = end });
}

/// export var/let/const/function/class
fn parseExportWithDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    var declaration: ast.NodeIndex = undefined;

    switch (parser.current_token.tag) {
        .@"var", .@"const", .let => {
            declaration = try variables.parseVariableDeclaration(parser, false, null) orelse return null;
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
        .at => {
            declaration = try extensions.parseDecorated(parser, .{}) orelse return null;
        },
        else => {
            try parser.reportExpected(parser.current_token.span, "Expected declaration after 'export'", .{
                .help = "Use 'export var', 'export let', 'export const', 'export function', or 'export class'",
            });
            return null;
        },
    }

    return try parser.addNode(.{
        .export_named_declaration = .{
            .declaration = declaration,
            .specifiers = ast.IndexRange.empty,
            .source = ast.null_node,
            .attributes = ast.IndexRange.empty,
        },
    }, .{ .start = start, .end = parser.getSpan(declaration).end });
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

        try parser.scratch_b.append(parser.allocator(), @intFromEnum(local_tag));

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
        } else {
            break;
        }
    }

    if (!try parser.expect(.right_brace, "Expected '}' to close export specifiers", null)) return null;

    return .{
        .specifiers = try parser.addExtraFromScratch(&parser.scratch_a, checkpoint),
        .local_tags = try parser.addExtraFromScratch(&parser.scratch_b, token_checkpoint),
    };
}

/// export specifier: foo or foo as bar
fn parseExportSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    // local name (can be identifier or string literal)
    const local = try parseModuleExportName(parser) orelse return null;

    var exported: ast.NodeIndex = undefined;

    if (parser.current_token.tag == .as) {
        try parser.advance() orelse return null; // consume 'as'
        exported = try parseModuleExportName(parser) orelse return null;
    } else {
        // exported is the same as local
        exported = local;
    }

    const end = parser.getSpan(exported).end;

    return try parser.addNode(.{
        .export_specifier = .{
            .local = local,
            .exported = exported,
        },
    }, .{ .start = start, .end = end });
}

/// ModuleExportName: IdentifierName or StringLiteral
fn parseModuleExportName(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .string_literal) {
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

    return parser.addExtraFromScratch(&parser.scratch_a, checkpoint);
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

    return try parser.addNode(.{
        .import_attribute = .{
            .key = key,
            .value = value,
        },
    }, .{ .start = start, .end = parser.getSpan(value).end });
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
    const start = parser.getSpan(import_keyword).start;

    if (!try parser.expect(.left_paren, "Expected '(' after import", null)) return null;

    // source expression
    const source = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    var options: ast.NodeIndex = ast.null_node;

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

    return try parser.addNode(.{
        .import_expression = .{
            .source = source,
            .options = options,
            .phase = phase,
        },
    }, .{ .start = start, .end = end });
}
