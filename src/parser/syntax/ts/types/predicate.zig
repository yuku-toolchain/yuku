const std = @import("std");
const ast = @import("../../../ast.zig");
const Parser = @import("../../../parser.zig").Parser;
const Error = @import("../../../parser.zig").Error;

const literals = @import("../../literals.zig");
const core = @import("core.zig");

// let x: string
//      ^^^^^^^^
pub fn parseTypeAnnotation(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .colon);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const type_node = try core.parseType(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_type_annotation = .{ .type_annotation = type_node } },
        .{ .start = start, .end = parser.tree.span(type_node).end },
    );
}

// function f(x): x is T { ... }   function f(x): asserts x is T { ... }
//                ^^^^^^                            ^^^^^^^^^^^^^^
pub fn parseReturnTypeAnnotation(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .colon);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const inner = try parseTypeOrTypePredicate(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_type_annotation = .{ .type_annotation = inner } },
        .{ .start = start, .end = parser.tree.span(inner).end },
    );
}

// only bare `id is T` needs lookahead here, other preds route via parseType
pub fn parseTypeOrTypePredicate(parser: *Parser) Error!?ast.NodeIndex {
    if (!try isIdentifierPredicateStart(parser)) return core.parseType(parser);

    const parameter_name = try literals.parseIdentifierName(parser) orelse return null;
    return finishTypePredicate(parser, parser.tree.span(parameter_name).start, parameter_name, false);
}

// this   this is T
// ^^^^   ^^^^^^^^^
pub fn parseThisTypeOrPredicate(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .this);

    const this_token = parser.current_token;
    try parser.advance() orelse return null;

    const this_type = try parser.tree.addNode(.{ .ts_this_type = .{} }, this_token.span);

    const next = parser.current_token;
    if (next.tag != .is or next.isEscaped() or next.hasLineTerminatorBefore()) return this_type;

    return finishTypePredicate(parser, this_token.span.start, this_type, false);
}

// asserts x   asserts x is T   asserts this is T
// ^^^^^^^^^   ^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^
pub fn parseAssertsTypePredicate(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .asserts);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const parameter_name = try parsePredicateParameterName(parser) orelse return null;
    return finishTypePredicate(parser, start, parameter_name, true);
}

// asserts target is `this` as node or identifier name
fn parsePredicateParameterName(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .this) return literals.parseIdentifierName(parser);

    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.tree.addNode(.{ .ts_this_type = .{} }, token.span);
}

// optional `is T` tail then predicate node
fn finishTypePredicate(
    parser: *Parser,
    start: u32,
    parameter_name: ast.NodeIndex,
    asserts: bool,
) Error!?ast.NodeIndex {
    var end = parser.tree.span(parameter_name).end;
    var type_annotation: ast.NodeIndex = .null;

    if (parser.current_token.tag == .is and !parser.current_token.isEscaped()) {
        try parser.advance() orelse return null;
        const inner = try core.parseType(parser) orelse return null;
        type_annotation = try parser.tree.addNode(
            .{ .ts_type_annotation = .{ .type_annotation = inner } },
            parser.tree.span(inner),
        );
        end = parser.tree.span(type_annotation).end;
    }

    return try parser.tree.addNode(
        .{ .ts_type_predicate = .{
            .parameter_name = parameter_name,
            .type_annotation = type_annotation,
            .asserts = asserts,
        } },
        .{ .start = start, .end = end },
    );
}

// asserts keyword then this or id on same line
pub fn isAssertsPredicateStart(parser: *Parser) Error!bool {
    if (parser.current_token.tag != .asserts or parser.current_token.isEscaped()) return false;

    const next = parser.peekAhead() orelse return false;
    if (next.hasLineTerminatorBefore()) return false;

    return next.tag == .this or next.tag.isIdentifierLike();
}

// `id is T` same line. `this is T` lives in primary type path already
fn isIdentifierPredicateStart(parser: *Parser) Error!bool {
    const current = parser.current_token;
    if (current.isEscaped() or current.tag == .this or !current.tag.isIdentifierLike()) return false;

    const next = parser.peekAhead() orelse return false;
    return next.tag == .is and !next.isEscaped() and !next.hasLineTerminatorBefore();
}

// write type annotation into pattern node and grow span, noop if no field for it
pub fn applyTypeAnnotationToPattern(parser: *Parser, pattern: ast.NodeIndex, annotation: ast.NodeIndex) void {
    var data = parser.tree.data(pattern);
    switch (data) {
        inline .binding_identifier, .object_pattern, .array_pattern, .assignment_pattern => |*v| v.type_annotation = annotation,
        else => return,
    }
    parser.tree.setData(pattern, data);
    extendSpanTo(parser, pattern, parser.tree.span(annotation).end);
}

// decorators hang on pattern node, span unchanged, empty range noop
pub fn applyDecoratorsToPattern(parser: *Parser, pattern: ast.NodeIndex, decorators: ast.IndexRange) void {
    if (decorators.len == 0) return;
    var data = parser.tree.data(pattern);
    switch (data) {
        inline .binding_identifier,
        .object_pattern,
        .array_pattern,
        .assignment_pattern,
        .binding_rest_element,
        => |*v| v.decorators = decorators,
        else => return,
    }
    parser.tree.setData(pattern, data);
}

// marks `x?`, `[...]?`, `{...}?` optional and stretches the span to `end`.
pub fn markPatternOptional(parser: *Parser, pattern: ast.NodeIndex, end: u32) void {
    var data = parser.tree.data(pattern);
    switch (data) {
        inline .binding_identifier, .object_pattern, .array_pattern, .assignment_pattern => |*v| v.optional = true,
        else => return,
    }
    parser.tree.setData(pattern, data);
    extendSpanTo(parser, pattern, end);
}

inline fn extendSpanTo(parser: *Parser, node: ast.NodeIndex, end: u32) void {
    const span = parser.tree.span(node);
    if (end > span.end) parser.tree.setSpan(node, .{ .start = span.start, .end = end });
}
