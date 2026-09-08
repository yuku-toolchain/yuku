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

// only bare `id is T` needs lookahead, `this is T` and `asserts` go through parseType
pub fn parseTypeOrTypePredicate(parser: *Parser) Error!?ast.NodeIndex {
    if (!try isIdentifierPredicateStart(parser)) return core.parseType(parser);

    const parameter_name = try literals.parseIdentifierName(parser) orelse return null;
    return finishTypePredicate(
        parser,
        parser.tree.span(parameter_name).start,
        parameter_name,
        false,
    );
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

fn parsePredicateParameterName(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .this) return literals.parseIdentifierName(parser);

    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.tree.addNode(.{ .ts_this_type = .{} }, token.span);
}

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

pub fn isAssertsPredicateStart(parser: *Parser) Error!bool {
    if (parser.current_token.tag != .asserts or parser.current_token.isEscaped()) return false;

    const next = parser.peekAhead();
    if (next.hasLineTerminatorBefore()) return false;

    return next.tag == .this or next.tag.isIdentifierLike();
}

// `this is T` is handled by the primary type path
fn isIdentifierPredicateStart(parser: *Parser) Error!bool {
    const current = parser.current_token;
    if (current.isEscaped() or current.tag == .this or !current.tag.isIdentifierLike()) {
        return false;
    }

    const next = parser.peekAhead();
    return next.tag == .is and !next.isEscaped() and !next.hasLineTerminatorBefore();
}

pub fn applyTypeAnnotationToPattern(
    parser: *Parser,
    pattern: ast.NodeIndex,
    annotation: ast.NodeIndex,
) void {
    var data = parser.tree.data(pattern);
    switch (data) {
        inline .binding_identifier,
        .object_pattern,
        .array_pattern,
        .assignment_pattern,
        => |*v| v.type_annotation = annotation,
        else => return,
    }
    parser.tree.setData(pattern, data);
    extendSpanTo(parser, pattern, parser.tree.span(annotation).end);
}

// only a rest element grows to cover its decorators, matching TS-ESTree ranges
pub fn applyDecoratorsToPattern(
    parser: *Parser,
    pattern: ast.NodeIndex,
    decorators: ast.IndexRange,
) void {
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

    if (data == .binding_rest_element) {
        const first = parser.tree.extra(decorators)[0];
        extendSpanFrom(parser, pattern, parser.tree.span(first).start);
    }
}

pub fn checkDefiniteAssignment(
    parser: *Parser,
    span: ast.Span,
    has_type_annotation: bool,
    has_initializer: bool,
) Error!void {
    if (has_initializer) {
        try parser.report(
            span,
            "A declaration with an initializer cannot also have a definite assignment assertion",
            .{ .help = "Remove the '!'. The initializer already assigns a value." },
        );
    } else if (!has_type_annotation) {
        try parser.report(
            span,
            "A declaration with a definite assignment assertion must also have a type annotation",
            .{ .help = "Annotate the declaration, for example 'x!: string'." },
        );
    }
}

pub fn markPatternOptional(parser: *Parser, pattern: ast.NodeIndex, end: u32) void {
    var data = parser.tree.data(pattern);
    switch (data) {
        inline .binding_identifier,
        .object_pattern,
        .array_pattern,
        .assignment_pattern,
        => |*v| v.optional = true,
        else => return,
    }
    parser.tree.setData(pattern, data);
    extendSpanTo(parser, pattern, end);
}

inline fn extendSpanTo(parser: *Parser, node: ast.NodeIndex, end: u32) void {
    const span = parser.tree.span(node);
    if (end > span.end) parser.tree.setSpan(node, .{ .start = span.start, .end = end });
}

inline fn extendSpanFrom(parser: *Parser, node: ast.NodeIndex, start: u32) void {
    const span = parser.tree.span(node);
    if (start < span.start) parser.tree.setSpan(node, .{ .start = start, .end = span.end });
}
