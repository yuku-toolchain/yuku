// typescript type parsing
//
// parses type annotations, type expressions, and type-level constructs.
// called from expression and statement parsing when typescript syntax is encountered.

const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const TokenTag = @import("../../token.zig").TokenTag;

/// parses any TSType node.
///
/// phase 1.3 scaffolding: concrete type nodes (keywords, references, unions, etc.) are added
/// incrementally starting in phase 2. until then this function reports a diagnostic at the
/// current token and returns null so callers unwind cleanly.
pub fn parseType(parser: *Parser) Error!?ast.NodeIndex {
    try parser.reportExpected(
        parser.current_token.span,
        "Expected a type",
        .{ .help = "TypeScript type parsing is being implemented incrementally; this type form is not supported yet" },
    );
    return null;
}

/// parses a `: Type` type annotation and returns a `TSTypeAnnotation` node.
/// the caller must have already confirmed the current token is `:` before calling.
pub fn parseTypeAnnotation(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .colon);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume ':'

    const type_node = try parseType(parser) orelse return null;

    return try parser.tree.createNode(
        .{ .ts_type_annotation = .{ .type_annotation = type_node } },
        .{ .start = start, .end = parser.tree.getSpan(type_node).end },
    );
}
