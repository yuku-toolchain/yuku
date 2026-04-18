const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const TokenTag = @import("../../token.zig").TokenTag;

/// Parses a `TSType`. The entry point for any type position.
pub fn parseType(parser: *Parser) Error!?ast.NodeIndex {
    const ty = try parsePrimaryType(parser) orelse {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected a type",
            .{},
        );
        return null;
    };

    return ty;
}

fn parsePrimaryType(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    if (!token.isEscaped()) {
        switch (token.tag) {
            .any,
            .bigint,
            .boolean,
            .intrinsic,
            .never,
            .null_literal,
            .number,
            .object,
            .string,
            .symbol,
            .this,
            .@"undefined",
            .unknown,
            .void,
            => return parseTypeKeyword(parser),
            else => {},
        }
    }

    if (token.tag.isIdentifierLike() and !token.tag.isUnconditionallyReserved()) {
        return parseTypeReference(parser);
    }

    return null;
}

/// parses a primitive type keyword (`any`, `string`, `void`, `null`, `this`,
/// ...) into its corresponding `TSKeyword` node.
inline fn parseTypeKeyword(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    const data: ast.NodeData = switch (token.tag) {
        .any => .{ .ts_any_keyword = .{} },
        .bigint => .{ .ts_bigint_keyword = .{} },
        .boolean => .{ .ts_boolean_keyword = .{} },
        .intrinsic => .{ .ts_intrinsic_keyword = .{} },
        .never => .{ .ts_never_keyword = .{} },
        .null_literal => .{ .ts_null_keyword = .{} },
        .number => .{ .ts_number_keyword = .{} },
        .object => .{ .ts_object_keyword = .{} },
        .string => .{ .ts_string_keyword = .{} },
        .symbol => .{ .ts_symbol_keyword = .{} },
        .this => .{ .ts_this_type = .{} },
        .@"undefined" => .{ .ts_undefined_keyword = .{} },
        .unknown => .{ .ts_unknown_keyword = .{} },
        .void => .{ .ts_void_keyword = .{} },
        else => unreachable,
    };

    try parser.advance() orelse return null;

    return try parser.tree.createNode(data, token.span);
}

/// parses a `TSTypeReference`: an identifier, an optional dotted tail forming a
/// `TSQualifiedName`, and an optional `<T, U>` type argument list. the caller
/// must have already verified via `parsePrimaryType`'s dispatch that the
/// leading token is identifier-like and not unconditionally reserved.
fn parseTypeReference(parser: *Parser) Error!?ast.NodeIndex {
    const first_token = parser.current_token;
    const start = first_token.span.start;

    var type_name = try parser.tree.createNode(.{
        .identifier_reference = .{ .name = try parser.identifierName(first_token) },
    }, first_token.span);

    try parser.advanceWithoutEscapeCheck() orelse return null;

    while (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null; // consume '.'

        const right_token = parser.current_token;
        if (!right_token.tag.isIdentifierLike()) {
            try parser.reportExpected(
                right_token.span,
                "Expected an identifier after '.'",
                .{ .help = "A qualified type name must end with an identifier" },
            );
            return null;
        }

        const right = try parser.tree.createNode(.{
            .identifier_name = .{ .name = try parser.identifierName(right_token) },
        }, right_token.span);

        try parser.advanceWithoutEscapeCheck() orelse return null;

        type_name = try parser.tree.createNode(
            .{ .ts_qualified_name = .{ .left = type_name, .right = right } },
            .{ .start = start, .end = right_token.span.end },
        );
    }

    const type_arguments = try parseTypeArguments(parser);

    const end = if (type_arguments != .null)
        parser.tree.getSpan(type_arguments).end
    else
        parser.tree.getSpan(type_name).end;

    return try parser.tree.createNode(
        .{ .ts_type_reference = .{
            .type_name = type_name,
            .type_arguments = type_arguments,
        } },
        .{ .start = start, .end = end },
    );
}

/// parses a `<T, U, ...>` type argument list into a `TSTypeParameterInstantiation`.
/// returns `.null` when the current token is not `<`.
pub fn parseTypeArguments(parser: *Parser) Error!ast.NodeIndex {
    if (parser.current_token.tag != .less_than) return .null;

    const start = parser.current_token.span.start;

    try parser.advance() orelse return .null; // consume '<'

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .greater_than and parser.current_token.tag != .eof) {
        const arg = try parseType(parser) orelse return .null;
        try parser.scratch_a.append(parser.allocator(), arg);

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return .null;
        } else {
            break;
        }
    }

    // closes the type argument list. a compound `>`-starting token (`>>`,
    // `>>>`, `>=`, `>>=`, `>>>=`) is re-scanned as a leading `>` so the
    // remainder stays in the token stream for the enclosing context.
    const end: u32 = switch (parser.current_token.tag) {
        .greater_than => blk: {
            const e = parser.current_token.span.end;
            try parser.advance() orelse return .null;
            break :blk e;
        },
        .right_shift, .unsigned_right_shift, .greater_than_equal, .right_shift_assign, .unsigned_right_shift_assign => blk: {
            const gt = parser.lexer.reScanGreaterThan(parser.current_token.span.start);
            try parser.advanceWithRescannedToken(gt) orelse return .null;
            break :blk gt.span.end;
        },
        else => {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected '>' to close a type argument list",
                .{ .help = "Each '<' in a type must be matched by a '>'" },
            );
            return .null;
        },
    };

    const params = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);

    return try parser.tree.createNode(
        .{ .ts_type_parameter_instantiation = .{ .params = params } },
        .{ .start = start, .end = end },
    );
}

/// `: Type`.
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

/// attaches a parsed `TSTypeAnnotation` to the inner binding pattern. mutates the
/// pattern node's data in place.
pub fn applyTypeAnnotationToPattern(parser: *Parser, pattern: ast.NodeIndex, annotation: ast.NodeIndex) void {
    var data = parser.tree.getData(pattern);

    switch (data) {
        .binding_identifier => |*v| v.type_annotation = annotation,
        .object_pattern => |*v| v.type_annotation = annotation,
        .array_pattern => |*v| v.type_annotation = annotation,
        .assignment_pattern => |*v| v.type_annotation = annotation,
        else => return,
    }

    parser.tree.replaceData(pattern, data);

    const pattern_span = parser.tree.getSpan(pattern);
    const annotation_end = parser.tree.getSpan(annotation).end;

    if (annotation_end > pattern_span.end) {
        parser.tree.replaceSpan(pattern, .{ .start = pattern_span.start, .end = annotation_end });
    }
}
