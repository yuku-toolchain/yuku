const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const TokenTag = @import("../../token.zig").TokenTag;
const literals = @import("../literals.zig");

pub fn parseType(parser: *Parser) Error!?ast.NodeIndex {
    return try parseUnionType(parser) orelse {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected a type",
            .{},
        );
        return null;
    };
}

/// | A | B | C
/// ^^^^^^^^^^^
fn parseUnionType(parser: *Parser) Error!?ast.NodeIndex {
    const leading_start: ?u32 = if (parser.current_token.tag == .bitwise_or)
        parser.current_token.span.start
    else
        null;

    if (leading_start != null) {
        try parser.advance() orelse return null; // consume leading '|'
    }

    const first = try parseIntersectionType(parser) orelse return null;

    if (leading_start == null and parser.current_token.tag != .bitwise_or) {
        return first;
    }

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    try parser.scratch_a.append(parser.allocator(), first);

    var last = first;
    while (parser.current_token.tag == .bitwise_or) {
        try parser.advance() orelse return null; // consume '|'
        last = try parseIntersectionType(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), last);
    }

    const types = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);
    const start = leading_start orelse parser.tree.getSpan(first).start;
    const end = parser.tree.getSpan(last).end;

    return try parser.tree.createNode(
        .{ .ts_union_type = .{ .types = types } },
        .{ .start = start, .end = end },
    );
}

/// & A & B & C
/// ^^^^^^^^^^^
fn parseIntersectionType(parser: *Parser) Error!?ast.NodeIndex {
    const leading_start: ?u32 = if (parser.current_token.tag == .bitwise_and)
        parser.current_token.span.start
    else
        null;
    if (leading_start != null) {
        try parser.advance() orelse return null; // consume leading '&'
    }

    const first = try parsePostfixType(parser) orelse return null;

    if (leading_start == null and parser.current_token.tag != .bitwise_and) {
        return first;
    }

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    try parser.scratch_a.append(parser.allocator(), first);

    var last = first;
    while (parser.current_token.tag == .bitwise_and) {
        try parser.advance() orelse return null; // consume '&'
        last = try parsePostfixType(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), last);
    }

    const types = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);
    const start = leading_start orelse parser.tree.getSpan(first).start;
    const end = parser.tree.getSpan(last).end;

    return try parser.tree.createNode(
        .{ .ts_intersection_type = .{ .types = types } },
        .{ .start = start, .end = end },
    );
}

/// T[]   T[K]
/// ^^^   ^^^^
fn parsePostfixType(parser: *Parser) Error!?ast.NodeIndex {
    var ty = try parsePrimaryType(parser) orelse return null;

    while (parser.current_token.tag == .left_bracket) {
        const start = parser.tree.getSpan(ty).start;
        try parser.advance() orelse return null; // consume '['

        if (parser.current_token.tag == .right_bracket) {
            const end = parser.current_token.span.end;
            try parser.advance() orelse return null; // consume ']'
            ty = try parser.tree.createNode(
                .{ .ts_array_type = .{ .element_type = ty } },
                .{ .start = start, .end = end },
            );
            continue;
        }

        const index = try parseType(parser) orelse return null;

        if (parser.current_token.tag != .right_bracket) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected ']' to close an indexed access type",
                .{ .help = "Each '[' in a type must be matched by a ']'" },
            );
            return null;
        }

        const end = parser.current_token.span.end;
        try parser.advance() orelse return null; // consume ']'

        ty = try parser.tree.createNode(
            .{ .ts_indexed_access_type = .{ .object_type = ty, .index_type = index } },
            .{ .start = start, .end = end },
        );
    }

    return ty;
}

/// string   42   Foo<T>
/// ^^^^^^   ^^   ^^^^^^
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
            .true,
            .false,
            .string_literal,
            .numeric_literal,
            .hex_literal,
            .octal_literal,
            .binary_literal,
            .bigint_literal,
            .no_substitution_template,
            .minus,
            .plus,
            => return parseLiteralType(parser),
            .left_paren => return parseParenthesizedType(parser),
            .left_bracket => return parseTupleType(parser),
            else => {},
        }
    }

    if (token.tag.isIdentifierLike() and !token.tag.isUnconditionallyReserved()) {
        return parseTypeReference(parser);
    }

    return null;
}

/// string   number   void
/// ^^^^^^   ^^^^^^   ^^^^
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

/// 42   "foo"   true   -1
/// ^^   ^^^^^   ^^^^   ^^
fn parseLiteralType(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    const start = token.span.start;

    const literal: ast.NodeIndex = switch (token.tag) {
        .true, .false => try literals.parseBooleanLiteral(parser) orelse return null,
        .string_literal => try literals.parseStringLiteral(parser) orelse return null,
        .numeric_literal,
        .hex_literal,
        .octal_literal,
        .binary_literal,
        .bigint_literal,
        => try literals.parseNumericLiteral(parser) orelse return null,
        .no_substitution_template => try literals.parseNoSubstitutionTemplate(parser, false) orelse return null,
        .minus, .plus => blk: {
            const next = (try parser.lookAhead()) orelse return null;
            if (!next.tag.isNumericLiteral()) return null;

            try parser.advance() orelse return null; // consume '-' or '+'

            const arg = try literals.parseNumericLiteral(parser) orelse return null;
            const arg_end = parser.tree.getSpan(arg).end;

            break :blk try parser.tree.createNode(
                .{ .unary_expression = .{
                    .argument = arg,
                    .operator = if (token.tag == .minus) .negate else .positive,
                } },
                .{ .start = start, .end = arg_end },
            );
        },
        else => unreachable,
    };

    const end = parser.tree.getSpan(literal).end;

    return try parser.tree.createNode(
        .{ .ts_literal_type = .{ .literal = literal } },
        .{ .start = start, .end = end },
    );
}

/// (A | B)   (Foo)
/// ^^^^^^^   ^^^^^
fn parseParenthesizedType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_paren);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '('

    const inner = try parseType(parser) orelse return null;

    if (parser.current_token.tag != .right_paren) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected ')' to close a parenthesized type",
            .{ .help = "Each '(' in a type must be matched by a ')'" },
        );
        return null;
    }

    const end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume ')'

    return try parser.tree.createNode(
        .{ .ts_parenthesized_type = .{ .type_annotation = inner } },
        .{ .start = start, .end = end },
    );
}

/// [A, B?, ...C]   [label: T, rest?: U]
/// ^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^
fn parseTupleType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_bracket);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '['

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .right_bracket and parser.current_token.tag != .eof) {
        const element = try parseTupleElement(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), element);

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
        } else {
            break;
        }
    }

    if (parser.current_token.tag != .right_bracket) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected ']' to close a tuple type",
            .{ .help = "Each '[' in a tuple type must be matched by a ']'" },
        );
        return null;
    }

    const end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume ']'

    const element_types = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);

    return try parser.tree.createNode(
        .{ .ts_tuple_type = .{ .element_types = element_types } },
        .{ .start = start, .end = end },
    );
}

/// one element of a tuple. handles the leading `...` (rest) and the
/// `label:` / `label?:` named forms, then falls back to a plain type with
/// an optional trailing `?` suffix
fn parseTupleElement(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .spread) {
        const start = parser.current_token.span.start;
        try parser.advance() orelse return null; // consume '...'
        const inner = try parseTupleElementBody(parser) orelse return null;
        const end = parser.tree.getSpan(inner).end;
        return try parser.tree.createNode(
            .{ .ts_rest_type = .{ .type_annotation = inner } },
            .{ .start = start, .end = end },
        );
    }

    return parseTupleElementBody(parser);
}

/// a tuple element without the `...` prefix. dispatches to the named form
/// when the next token reveals a `label:` or `label?:` pattern, otherwise
/// parses a plain type and wraps it in `TSOptionalType` if a trailing `?`
/// is present
fn parseTupleElementBody(parser: *Parser) Error!?ast.NodeIndex {
    if (try isNamedTupleElement(parser)) {
        return parseNamedTupleMember(parser);
    }

    const ty = try parseType(parser) orelse return null;

    if (parser.current_token.tag == .question) {
        const start = parser.tree.getSpan(ty).start;
        const end = parser.current_token.span.end;
        try parser.advance() orelse return null; // consume '?'
        return try parser.tree.createNode(
            .{ .ts_optional_type = .{ .type_annotation = ty } },
            .{ .start = start, .end = end },
        );
    }

    return ty;
}

/// peeks past the current identifier to see if the next token is `:` or
/// `?:`
fn isNamedTupleElement(parser: *Parser) Error!bool {
    if (!parser.current_token.tag.isIdentifierLike()) return false;

    const t1 = try parser.lookAheadAt(1) orelse return false;

    if (t1.tag == .colon) return true;
    if (t1.tag != .question) return false;

    const t2 = try parser.lookAheadAt(2) orelse return false;
    return t2.tag == .colon;
}

/// label: Type     label?: Type
/// ^^^^^^^^^^^     ^^^^^^^^^^^^
fn parseNamedTupleMember(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const label = try literals.parseIdentifierName(parser) orelse return null;

    var is_optional = false;
    if (parser.current_token.tag == .question) {
        is_optional = true;
        try parser.advance() orelse return null; // consume '?'
    }

    if (parser.current_token.tag != .colon) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected ':' after named tuple element label",
            .{},
        );
        return null;
    }
    try parser.advance() orelse return null; // consume ':'

    const element_type = try parseType(parser) orelse return null;
    const end = parser.tree.getSpan(element_type).end;

    return try parser.tree.createNode(
        .{ .ts_named_tuple_member = .{
            .label = label,
            .element_type = element_type,
            .optional = is_optional,
        } },
        .{ .start = start, .end = end },
    );
}

/// Foo.Bar.Baz<T, U>
/// ^^^^^^^^^^^^^^^^^
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

/// Foo<T, U, V>
///    ^^^^^^^^^
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

/// function f<T, U extends V>() {}
///           ^^^^^^^^^^^^^^^^
pub fn parseTypeParameters(parser: *Parser) Error!ast.NodeIndex {
    if (parser.current_token.tag != .less_than) return .null;

    const start = parser.current_token.span.start;

    try parser.advance() orelse return .null; // consume '<'

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .greater_than and parser.current_token.tag != .eof) {
        const param = try parseTypeParameter(parser) orelse return .null;
        try parser.scratch_a.append(parser.allocator(), param);

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return .null;
        } else {
            break;
        }
    }

    // closes the type parameter list. a compound `>`-starting token (`>>`,
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
                "Expected '>' to close a type parameter list",
                .{ .help = "Each '<' in a type must be matched by a '>'" },
            );
            return .null;
        },
    };

    const params = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);

    return try parser.tree.createNode(
        .{ .ts_type_parameter_declaration = .{ .params = params } },
        .{ .start = start, .end = end },
    );
}

/// const in out T extends U = V
/// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
fn parseTypeParameter(parser: *Parser) Error!?ast.NodeIndex {
    var is_const = false;
    var is_in = false;
    var is_out = false;
    var start: u32 = parser.current_token.span.start;
    var start_set = false;

    // modifier keywords are contextual. treat the current token as a modifier
    // only when the next token is an identifier-like token, so the modifier
    // word itself can still appear as a parameter name (e.g. `<out>`).
    while (true) {
        const tag = parser.current_token.tag;
        if (tag != .in and tag != .out and tag != .@"const") break;

        const next = (try parser.lookAhead()) orelse break;
        if (!next.tag.isIdentifierLike()) break;

        switch (tag) {
            .@"const" => is_const = true,
            .in => is_in = true,
            .out => is_out = true,
            else => unreachable,
        }

        if (!start_set) {
            start = parser.current_token.span.start;
            start_set = true;
        }

        try parser.advance() orelse return null;
    }

    const name_token = parser.current_token;
    const name = try literals.parseBindingIdentifier(parser) orelse return null;

    if (!start_set) start = name_token.span.start;
    var end = name_token.span.end;

    var constraint: ast.NodeIndex = .null;
    if (parser.current_token.tag == .extends) {
        try parser.advance() orelse return null;
        constraint = try parseType(parser) orelse return null;
        end = parser.tree.getSpan(constraint).end;
    }

    var default: ast.NodeIndex = .null;
    if (parser.current_token.tag == .assign) {
        try parser.advance() orelse return null;
        default = try parseType(parser) orelse return null;
        end = parser.tree.getSpan(default).end;
    }

    return try parser.tree.createNode(
        .{ .ts_type_parameter = .{
            .name = name,
            .constraint = constraint,
            .default = default,
            .in = is_in,
            .out = is_out,
            .@"const" = is_const,
        } },
        .{ .start = start, .end = end },
    );
}

/// let x: string
///      ^^^^^^^^
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
