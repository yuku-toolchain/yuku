const std = @import("std");
const ast = @import("../../../ast.zig");
const Parser = @import("../../../parser.zig").Parser;
const Error = @import("../../../parser.zig").Error;
const TokenTag = @import("../../../token.zig").TokenTag;
const Precedence = @import("../../../token.zig").Precedence;

const literals = @import("../../literals.zig");
const expressions = @import("../../expressions.zig");
const functions = @import("../../functions.zig");
const patterns = @import("../../patterns.zig");

const generics = @import("generics.zig");
const literal = @import("literal.zig");
const object = @import("object.zig");
const predicate = @import("predicate.zig");

pub fn parseType(parser: *Parser) Error!?ast.NodeIndex {
    const saved = parser.ts_context.disallow_conditional_types;
    parser.ts_context.disallow_conditional_types = false;
    defer parser.ts_context.disallow_conditional_types = saved;

    if (try isStartOfFunctionOrConstructorType(parser)) {
        return parseFunctionOrConstructorType(parser);
    }
    return try parseConditionalType(parser) orelse {
        try parser.reportExpected(parser.current_token.span, "Expected a type", .{});
        return null;
    };
}

fn parseTypeNoConditional(parser: *Parser) Error!?ast.NodeIndex {
    if (try isStartOfFunctionOrConstructorType(parser)) {
        return parseFunctionOrConstructorType(parser);
    }
    return try parseUnionType(parser) orelse {
        try parser.reportExpected(parser.current_token.span, "Expected a type", .{});
        return null;
    };
}

fn parseConditionalType(parser: *Parser) Error!?ast.NodeIndex {
    const check_type = try parseUnionType(parser) orelse return null;

    if (parser.current_token.tag != .extends or parser.current_token.hasLineTerminatorBefore()) {
        return check_type;
    }

    try parser.advance() orelse return null;

    // parentheses reset the flag through parseType
    const saved = parser.ts_context.disallow_conditional_types;
    parser.ts_context.disallow_conditional_types = true;
    defer parser.ts_context.disallow_conditional_types = saved;
    const extends_type = try parseTypeNoConditional(parser) orelse return null;

    if (!try parser.expect(
        .question,
        "Expected '?' in a conditional type",
        "A conditional type is written 'T extends U ? X : Y'",
    )) return null;

    const true_type = try parseType(parser) orelse return null;

    if (!try parser.expect(
        .colon,
        "Expected ':' in a conditional type",
        "A conditional type is written 'T extends U ? X : Y'",
    )) return null;

    const false_type = try parseType(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_conditional_type = .{
            .check_type = check_type,
            .extends_type = extends_type,
            .true_type = true_type,
            .false_type = false_type,
        } },
        .{
            .start = parser.tree.span(check_type).start,
            .end = parser.tree.span(false_type).end,
        },
    );
}

fn parseUnionType(parser: *Parser) Error!?ast.NodeIndex {
    return parseBinaryTypeChain(parser, .bitwise_or, parseIntersectionType);
}

fn parseIntersectionType(parser: *Parser) Error!?ast.NodeIndex {
    return parseBinaryTypeChain(parser, .bitwise_and, parsePostfixType);
}

fn parseBinaryTypeChain(
    parser: *Parser,
    comptime separator: TokenTag,
    comptime parseOperand: fn (*Parser) Error!?ast.NodeIndex,
) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const has_leading = parser.current_token.tag == separator;
    if (has_leading) try parser.advance() orelse return null;

    const first = try parseOperand(parser) orelse return null;
    if (!has_leading) std.debug.assert(parser.tree.span(first).start == start);
    if (!has_leading and parser.current_token.tag != separator) return first;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    try parser.scratch_a.append(parser.allocator(), first);

    var last = first;
    while (parser.current_token.tag == separator) {
        try parser.advance() orelse return null;
        last = try parseOperand(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), last);
    }

    const types = try parser.flushToExtras(&parser.scratch_a, checkpoint);
    const data: ast.NodeData = if (separator == .bitwise_or)
        .{ .ts_union_type = .{ .types = types } }
    else
        .{ .ts_intersection_type = .{ .types = types } };
    return try parser.tree.addNode(data, .{ .start = start, .end = parser.tree.span(last).end });
}

fn parsePostfixType(parser: *Parser) Error!?ast.NodeIndex {
    var ty = try parsePrimaryType(parser) orelse return null;

    while (!parser.current_token.hasLineTerminatorBefore()) {
        ty = try switch (parser.current_token.tag) {
            .left_bracket => parseArrayOrIndexedAccessType(parser, ty),
            // a postfix `?` only when no type follows, else the outer conditional owns it
            .question => if (isStartOfType(parser.peekAhead().tag))
                return ty
            else
                parseJSDocPostfix(parser, ty, .ts_jsdoc_nullable_type),
            .logical_not => parseJSDocPostfix(parser, ty, .ts_jsdoc_non_nullable_type),
            else => return ty,
        } orelse return null;
    }

    return ty;
}

fn parseArrayOrIndexedAccessType(parser: *Parser, element: ast.NodeIndex) Error!?ast.NodeIndex {
    const start = parser.tree.span(element).start;
    try parser.advance() orelse return null;

    if (parser.current_token.tag == .right_bracket) {
        const end = parser.current_token.span.end;
        try parser.advance() orelse return null;
        return try parser.tree.addNode(
            .{ .ts_array_type = .{ .element_type = element } },
            .{ .start = start, .end = end },
        );
    }

    const index = try parseType(parser) orelse return null;

    if (!try parser.expect(
        .right_bracket,
        "Expected ']' to close an indexed access type",
        "Each '[' in a type must be matched by a ']'",
    )) return null;

    return try parser.tree.addNode(
        .{ .ts_indexed_access_type = .{ .object_type = element, .index_type = index } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

fn parseJSDocPostfix(
    parser: *Parser,
    inner: ast.NodeIndex,
    comptime tag: std.meta.Tag(ast.NodeData),
) Error!?ast.NodeIndex {
    const start = parser.tree.span(inner).start;
    const end = parser.current_token.span.end;
    try parser.advance() orelse return null;
    return try parser.tree.addNode(
        @unionInit(ast.NodeData, @tagName(tag), .{ .type_annotation = inner, .postfix = true }),
        .{ .start = start, .end = end },
    );
}

fn parsePrimaryType(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    switch (token.tag) {
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
        => return literal.parseLiteralType(parser),
        // an escape in the head is template text, so this stays outside the `isEscaped` guard
        .template_head => return literal.parseTemplateLiteralType(parser),
        else => {},
    }

    if (!token.isEscaped()) {
        switch (token.tag) {
            .any,
            .bigint,
            .boolean,
            .never,
            .null_literal,
            .number,
            .object,
            .string,
            .symbol,
            .undefined,
            .unknown,
            .void,
            => if (!isQualifiedTypeContinuation(parser)) return parseTypeKeyword(parser),
            .this => return predicate.parseThisTypeOrPredicate(parser),
            .asserts => return if (predicate.isAssertsPredicateStart(parser))
                predicate.parseAssertsTypePredicate(parser)
            else
                parseTypeReference(parser),
            .left_paren => return parseParenthesizedType(parser),
            .left_bracket => return parseTupleType(parser),
            .left_brace => return if (object.isStartOfMappedType(parser))
                object.parseMappedType(parser)
            else
                object.parseTypeLiteral(parser),
            .keyof, .unique, .readonly => return parseTypeOperator(parser),
            .infer => return parseInferType(parser),
            .typeof => return parseTypeQuery(parser),
            .import => return parseImportType(parser),
            .question => return parseJSDocNullableOrUnknownType(parser),
            .logical_not => return parseJSDocNonNullableType(parser),
            else => {},
        }
    }

    if (token.tag.isIdentifierLike()) return parseTypeReference(parser);
    return null;
}

fn isQualifiedTypeContinuation(parser: *Parser) bool {
    const next = parser.peekAhead();
    return next.tag == .dot and !next.hasLineTerminatorBefore();
}

inline fn parseTypeKeyword(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    const data: ast.NodeData = switch (token.tag) {
        .any => .{ .ts_any_keyword = .{} },
        .bigint => .{ .ts_bigint_keyword = .{} },
        .boolean => .{ .ts_boolean_keyword = .{} },
        .never => .{ .ts_never_keyword = .{} },
        .null_literal => .{ .ts_null_keyword = .{} },
        .number => .{ .ts_number_keyword = .{} },
        .object => .{ .ts_object_keyword = .{} },
        .string => .{ .ts_string_keyword = .{} },
        .symbol => .{ .ts_symbol_keyword = .{} },
        .undefined => .{ .ts_undefined_keyword = .{} },
        .unknown => .{ .ts_unknown_keyword = .{} },
        .void => .{ .ts_void_keyword = .{} },
        else => unreachable,
    };

    try parser.advance() orelse return null;
    return try parser.tree.addNode(data, token.span);
}

// only a bare `type Name = intrinsic` is `TSIntrinsicKeyword`
pub fn parseTypeAliasBody(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .intrinsic and !parser.current_token.isEscaped()) {
        if (!continuesType(parser.peekAhead().tag)) {
            const span = parser.current_token.span;
            try parser.advance() orelse return null;
            return try parser.tree.addNode(.{ .ts_intrinsic_keyword = .{} }, span);
        }
    }
    return parseType(parser);
}

fn continuesType(tag: TokenTag) bool {
    return switch (tag) {
        .dot,
        .less_than,
        .left_shift,
        .left_bracket,
        .bitwise_or,
        .bitwise_and,
        .extends,
        .question,
        .logical_not,
        => true,
        else => false,
    };
}

fn parseJSDocNullableOrUnknownType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .question);

    const question = parser.current_token.span;
    try parser.advance() orelse return null;

    if (!isStartOfType(parser.current_token.tag)) {
        return try parser.tree.addNode(.{ .ts_jsdoc_unknown_type = .{} }, question);
    }

    const inner = try parseType(parser) orelse return null;
    return try parser.tree.addNode(
        .{ .ts_jsdoc_nullable_type = .{ .type_annotation = inner, .postfix = false } },
        .{ .start = question.start, .end = parser.tree.span(inner).end },
    );
}

// prefix `!` binds tighter than postfix, so `!T[]` is `(!T)[]`
fn parseJSDocNonNullableType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .logical_not);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const inner = try parsePrimaryType(parser) orelse return null;
    return try parser.tree.addNode(
        .{ .ts_jsdoc_non_nullable_type = .{ .type_annotation = inner, .postfix = false } },
        .{ .start = start, .end = parser.tree.span(inner).end },
    );
}

pub fn isStartOfType(tag: TokenTag) bool {
    return switch (tag) {
        .any,
        .bigint,
        .boolean,
        .never,
        .null_literal,
        .number,
        .object,
        .string,
        .symbol,
        .this,
        .undefined,
        .unknown,
        .void,
        .true,
        .false,
        .string_literal,
        .numeric_literal,
        .hex_literal,
        .octal_literal,
        .binary_literal,
        .bigint_literal,
        .no_substitution_template,
        .template_head,
        .minus,
        .plus,
        .left_paren,
        .left_bracket,
        .left_brace,
        .less_than,
        .left_shift,
        .keyof,
        .unique,
        .readonly,
        .infer,
        .typeof,
        .import,
        .new,
        .abstract,
        .asserts,
        .question,
        .logical_not,
        .spread,
        .bitwise_and,
        .bitwise_or,
        => true,
        else => tag.isIdentifierLike(),
    };
}

fn parseTypeOperator(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const operator: ast.TSTypeOperatorKind = switch (parser.current_token.tag) {
        .keyof => .keyof,
        .unique => .unique,
        .readonly => .readonly,
        else => unreachable,
    };

    try parser.advance() orelse return null;
    const inner = try parsePostfixType(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_type_operator = .{ .operator = operator, .type_annotation = inner } },
        .{ .start = start, .end = parser.tree.span(inner).end },
    );
}

fn parseInferType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .infer);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const name_span = parser.current_token.span;
    const name = try literals.parseBindingIdentifier(parser) orelse return null;

    const constraint = try parseInferConstraint(parser);
    const param_end = if (constraint != .null) parser.tree.span(constraint).end else name_span.end;

    const type_parameter = try parser.tree.addNode(
        .{ .ts_type_parameter = .{ .name = name, .constraint = constraint } },
        .{ .start = name_span.start, .end = param_end },
    );

    return try parser.tree.addNode(
        .{ .ts_infer_type = .{ .type_parameter = type_parameter } },
        .{ .start = start, .end = param_end },
    );
}

// rewinds when the `extends` belongs to an enclosing conditional
fn parseInferConstraint(parser: *Parser) Error!ast.NodeIndex {
    if (parser.current_token.tag != .extends) return .null;

    const extends_offset = parser.current_token.span.start;
    const conditional_allowed = !parser.ts_context.disallow_conditional_types;
    if (conditional_allowed and parser.ts_rejected_speculations.contains(extends_offset)) {
        return .null;
    }

    const cp = parser.checkpoint();
    try parser.advance() orelse return .null;

    parser.ts_context.disallow_conditional_types = true;
    const constraint = try parseTypeNoConditional(parser) orelse {
        parser.rewind(cp);
        return .null;
    };
    parser.ts_context.disallow_conditional_types = cp.ts_context.disallow_conditional_types;

    if (conditional_allowed and parser.current_token.tag == .question) {
        parser.rewind(cp);
        try parser.ts_rejected_speculations.putNoClobber(parser.allocator(), extends_offset, {});
        return .null;
    }

    return constraint;
}

fn isStartOfFunctionOrConstructorType(parser: *Parser) Error!bool {
    const tag = parser.current_token.tag;
    if (tag == .new) return true;
    if (tag == .abstract) return parser.peekAhead().tag == .new;
    if (generics.isAngleOpen(tag)) return true;
    if (tag != .left_paren) return false;

    var peek = parser.beginPeek();
    defer peek.end();

    const first = peek.next();
    switch (first.tag) {
        .right_paren => return peek.next().tag == .arrow,
        .spread => return true,
        .this => return peek.next().tag == .colon,
        // `{` or `[` may head a parameter pattern or a type
        .left_brace, .left_bracket => {
            peek.end();
            return isFunctionTypeAfterPattern(parser);
        },
        else => {},
    }

    if (!first.tag.isIdentifierLike()) return false;

    return switch (peek.next().tag) {
        .colon, .question, .comma, .assign => true,
        .right_paren => peek.next().tag == .arrow,
        else => false,
    };
}

fn isFunctionTypeAfterPattern(parser: *Parser) Error!bool {
    const cp = parser.checkpoint();
    defer parser.rewind(cp);

    try parser.advance() orelse return false;
    _ = try patterns.parseBindingPattern(parser) orelse return false;

    switch (parser.current_token.tag) {
        .colon, .comma, .question, .assign => return true,
        .right_paren => {
            try parser.advance() orelse return false;
            return parser.current_token.tag == .arrow;
        },
        else => return false,
    }
}

fn parseFunctionOrConstructorType(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const is_abstract = parser.current_token.tag == .abstract;
    if (is_abstract) try parser.advance() orelse return null;

    const is_constructor = parser.current_token.tag == .new;
    if (is_constructor) try parser.advance() orelse return null;

    const type_parameters = try generics.parseTypeParameters(parser);
    const params = try functions.parseFormalParameters(parser, .signature, false) orelse
        return null;

    const arrow_start = parser.current_token.span.start;
    if (!try parser.expect(
        .arrow,
        "Expected '=>' in function type",
        "A function type is written '(params) => ReturnType'",
    )) return null;

    const return_type_inner = try predicate.parseTypeOrTypePredicate(parser) orelse return null;
    const return_type_end = parser.tree.span(return_type_inner).end;

    const return_type = try parser.tree.addNode(
        .{ .ts_type_annotation = .{ .type_annotation = return_type_inner } },
        .{ .start = arrow_start, .end = return_type_end },
    );

    const data: ast.NodeData = if (is_constructor) .{ .ts_constructor_type = .{
        .type_parameters = type_parameters,
        .params = params,
        .return_type = return_type,
        .abstract = is_abstract,
    } } else .{ .ts_function_type = .{
        .type_parameters = type_parameters,
        .params = params,
        .return_type = return_type,
    } };

    return try parser.tree.addNode(data, .{ .start = start, .end = return_type_end });
}

fn parseParenthesizedType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_paren);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const inner = try parseType(parser) orelse return null;

    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close a parenthesized type",
        "Each '(' in a type must be matched by a ')'",
    )) return null;

    return try parser.tree.addNode(
        .{ .ts_parenthesized_type = .{ .type_annotation = inner } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

fn parseTupleType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_bracket);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .right_bracket and parser.current_token.tag != .eof) {
        const element = try parseTupleElement(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), element);

        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return null;
    }

    if (!try parser.expect(
        .right_bracket,
        "Expected ']' to close a tuple type",
        "Each '[' in a tuple type must be matched by a ']'",
    )) return null;

    const element_types = try parser.flushToExtras(&parser.scratch_a, checkpoint);
    try checkTupleElementOrder(parser, element_types);

    return try parser.tree.addNode(
        .{ .ts_tuple_type = .{ .element_types = element_types } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

fn checkTupleElementOrder(parser: *Parser, element_types: ast.IndexRange) Error!void {
    var seen_optional = false;
    for (parser.tree.extra(element_types)) |element| {
        const optional = switch (parser.tree.data(element)) {
            // a rest element neither satisfies nor clears a preceding optional
            .ts_rest_type => continue,
            .ts_optional_type => true,
            .ts_named_tuple_member => |member| member.optional,
            else => false,
        };
        if (optional) {
            seen_optional = true;
        } else if (seen_optional) return parser.report(
            parser.tree.span(element),
            "A required element cannot follow an optional element",
            .{ .help = "Mark this element optional, or move it before the optional ones." },
        );
    }
}

fn parseTupleElement(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .spread) return parseTupleElementBody(parser);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;
    const inner = try parseTupleElementBody(parser) orelse return null;
    return try parser.tree.addNode(
        .{ .ts_rest_type = .{ .type_annotation = inner } },
        .{ .start = start, .end = parser.tree.span(inner).end },
    );
}

// a postfix jsdoc `?` in a tuple slot is an optional element
fn parseTupleElementBody(parser: *Parser) Error!?ast.NodeIndex {
    if (isNamedTupleElement(parser)) return parseNamedTupleMember(parser);

    const ty = try parseType(parser) orelse return null;

    switch (parser.tree.data(ty)) {
        .ts_jsdoc_nullable_type => |nullable| if (nullable.postfix) return try parser.tree.addNode(
            .{ .ts_optional_type = .{ .type_annotation = nullable.type_annotation } },
            parser.tree.span(ty),
        ),
        else => {},
    }

    return ty;
}

fn isNamedTupleElement(parser: *Parser) bool {
    if (!parser.current_token.tag.isIdentifierLike()) return false;

    var peek = parser.beginPeek();
    defer peek.end();

    const next = peek.next();
    if (next.tag == .colon) return true;
    if (next.tag != .question) return false;
    return peek.next().tag == .colon;
}

fn parseNamedTupleMember(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const label = try literals.parseIdentifierName(parser) orelse return null;

    const is_optional = parser.current_token.tag == .question;
    if (is_optional) try parser.advance() orelse return null;

    if (!try parser.expect(.colon, "Expected ':' after named tuple element label", null)) {
        return null;
    }

    const element_type = try parseType(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_named_tuple_member = .{
            .label = label,
            .element_type = element_type,
            .optional = is_optional,
        } },
        .{ .start = start, .end = parser.tree.span(element_type).end },
    );
}

fn parseTypeReference(parser: *Parser) Error!?ast.NodeIndex {
    const type_name = try parseEntityName(parser) orelse return null;
    const name_span = parser.tree.span(type_name);
    const type_arguments = try generics.parseTypeArgumentsAfterEntityName(parser);
    const end = if (type_arguments != .null)
        parser.tree.span(type_arguments).end
    else
        name_span.end;

    return try parser.tree.addNode(
        .{ .ts_type_reference = .{ .type_name = type_name, .type_arguments = type_arguments } },
        .{ .start = name_span.start, .end = end },
    );
}

fn parseTypeQuery(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .typeof);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const expr_name = if (parser.current_token.tag == .import)
        try parseImportType(parser) orelse return null
    else
        try parseEntityName(parser) orelse return null;

    // an import type parses its own `<>`
    const type_arguments = if (parser.tree.data(expr_name) == .ts_import_type)
        .null
    else
        try generics.parseTypeArgumentsAfterEntityName(parser);

    const end = parser.tree.span(if (type_arguments != .null) type_arguments else expr_name).end;
    return try parser.tree.addNode(
        .{ .ts_type_query = .{ .expr_name = expr_name, .type_arguments = type_arguments } },
        .{ .start = start, .end = end },
    );
}

fn parseImportType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .import);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    if (!try parser.expect(
        .left_paren,
        "Expected '(' after 'import' in an import type",
        "An import type is written 'import(\"module\").Foo'",
    )) return null;

    const source = try literals.parseStringLiteral(parser) orelse return null;
    const options = try parseImportTypeOptions(parser) orelse return null;

    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close an import type",
        "An import type's argument list must end with ')'",
    )) return null;

    var qualifier: ast.NodeIndex = .null;
    if (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null;
        const head = try parseQualifiedSegment(
            parser,
            "Expected an identifier",
            "Each '.' in a qualified name must be followed by an identifier",
        ) orelse return null;
        qualifier = try extendQualifiedName(parser, head) orelse return null;
    }

    const type_arguments = try generics.parseTypeArguments(parser);

    const end: u32 = if (type_arguments != .null)
        parser.tree.span(type_arguments).end
    else if (qualifier != .null)
        parser.tree.span(qualifier).end
    else
        parser.prev_token_end;

    return try parser.tree.addNode(
        .{ .ts_import_type = .{
            .source = source,
            .options = options,
            .qualifier = qualifier,
            .type_arguments = type_arguments,
        } },
        .{ .start = start, .end = end },
    );
}

fn parseImportTypeOptions(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .comma) return .null;
    try parser.advance() orelse return null;

    if (parser.current_token.tag == .right_paren) return .null;

    const options = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
        return null;

    if (parser.current_token.tag == .comma) try parser.advance() orelse return null;
    return options;
}

fn parseEntityName(parser: *Parser) Error!?ast.NodeIndex {
    const first_token = parser.current_token;
    if (!first_token.tag.isIdentifierLike()) {
        try parser.reportExpected(
            first_token.span,
            "Expected an identifier",
            .{ .help = "An entity name must start with an identifier" },
        );
        return null;
    }

    const head = if (first_token.tag == .this)
        try parser.tree.addNode(.{ .this_expression = .{} }, first_token.span)
    else
        try parser.tree.addNode(.{
            .identifier_reference = .{ .name = try parser.identifierName(first_token) },
        }, first_token.span);

    try parser.advanceWithoutEscapeCheck() orelse return null;

    return extendQualifiedName(parser, head);
}

fn parseQualifiedSegment(
    parser: *Parser,
    comptime message: []const u8,
    comptime help: []const u8,
) Error!?ast.NodeIndex {
    const token = parser.current_token;
    if (!token.tag.isIdentifierLike()) {
        try parser.reportExpected(token.span, message, .{ .help = help });
        return null;
    }
    const node = try literals.addIdentifierName(parser, token);
    try parser.advanceWithoutEscapeCheck() orelse return null;
    return node;
}

pub fn extendQualifiedName(parser: *Parser, head: ast.NodeIndex) Error!?ast.NodeIndex {
    var name = head;
    while (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null;
        const right = try parseQualifiedSegment(
            parser,
            "Expected an identifier after '.'",
            "A qualified name must end with an identifier",
        ) orelse return null;
        name = try parser.tree.addNode(
            .{ .ts_qualified_name = .{ .left = name, .right = right } },
            .{ .start = parser.tree.span(name).start, .end = parser.tree.span(right).end },
        );
    }
    return name;
}
