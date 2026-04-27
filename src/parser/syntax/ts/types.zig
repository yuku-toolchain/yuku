const std = @import("std");
const ast = @import("../../ast.zig");
const lexer = @import("../../lexer.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const TokenTag = @import("../../token.zig").TokenTag;
const Precedence = @import("../../token.zig").Precedence;

const literals = @import("../literals.zig");
const functions = @import("../functions.zig");
const expressions = @import("../expressions.zig");
const patterns = @import("../patterns.zig");
const signatures = @import("signatures.zig");

pub fn parseType(parser: *Parser) Error!?ast.NodeIndex {
    if (try isStartOfFunctionOrConstructorType(parser)) {
        return try parseFunctionOrConstructorType(parser);
    }

    return try parseConditionalType(parser) orelse {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected a type",
            .{},
        );
        return null;
    };
}

/// extends slot of `T extends U ? X : Y`; refuses to start a nested
/// conditional so the outer `? :` survives.
fn parseTypeNoConditional(parser: *Parser) Error!?ast.NodeIndex {
    if (try isStartOfFunctionOrConstructorType(parser)) {
        return try parseFunctionOrConstructorType(parser);
    }

    return try parseUnionType(parser) orelse {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected a type",
            .{},
        );
        return null;
    };
}

/// T extends U ? X : Y
/// ^^^^^^^^^^^^^^^^^^^
fn parseConditionalType(parser: *Parser) Error!?ast.NodeIndex {
    const check_type = try parseUnionType(parser) orelse return null;

    if (parser.current_token.tag != .extends) return check_type;

    try parser.advance() orelse return null; // consume 'extends'

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

    const start = parser.tree.getSpan(check_type).start;
    const end = parser.tree.getSpan(false_type).end;

    return try parser.tree.createNode(
        .{ .ts_conditional_type = .{
            .check_type = check_type,
            .extends_type = extends_type,
            .true_type = true_type,
            .false_type = false_type,
        } },
        .{ .start = start, .end = end },
    );
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

/// T[]   T[K]   T?   T!
/// ^^^   ^^^^   ^^   ^^
fn parsePostfixType(parser: *Parser) Error!?ast.NodeIndex {
    var ty = try parsePrimaryType(parser) orelse return null;

    while (!parser.current_token.hasLineTerminatorBefore()) {
        switch (parser.current_token.tag) {
            .left_bracket => ty = try parseArrayOrIndexedAccessType(parser, ty) orelse return null,
            .question => {
                if (!try isPostfixNullable(parser)) return ty;
                ty = try parseJSDocPostfix(parser, ty, .nullable) orelse return null;
            },
            .logical_not => ty = try parseJSDocPostfix(parser, ty, .non_nullable) orelse return null,
            else => return ty,
        }
    }

    return ty;
}

/// T[]   T[K]
/// ^^^   ^^^^
fn parseArrayOrIndexedAccessType(parser: *Parser, element: ast.NodeIndex) Error!?ast.NodeIndex {
    const start = parser.tree.getSpan(element).start;
    try parser.advance() orelse return null; // consume '['

    if (parser.current_token.tag == .right_bracket) {
        const end = parser.current_token.span.end;
        try parser.advance() orelse return null; // consume ']'
        return try parser.tree.createNode(
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

    return try parser.tree.createNode(
        .{ .ts_indexed_access_type = .{ .object_type = element, .index_type = index } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

/// T?   T!
/// ^^   ^^
fn parseJSDocPostfix(parser: *Parser, inner: ast.NodeIndex, comptime kind: JSDocKind) Error!?ast.NodeIndex {
    const start = parser.tree.getSpan(inner).start;
    const end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume '?' or '!'
    return try parser.tree.createNode(
        jsdocNodeData(inner, kind, true),
        .{ .start = start, .end = end },
    );
}

/// true when a postfix `?` should be consumed; false when a type starts
/// after it (the `?` belongs to an outer conditional).
fn isPostfixNullable(parser: *Parser) Error!bool {
    const next = (try parser.peekAhead()) orelse return false;
    return !isStartOfType(next.tag);
}

/// string   42   Foo<T>   this is T   asserts x is T
/// ^^^^^^   ^^   ^^^^^^   ^^^^^^^^^   ^^^^^^^^^^^^^^
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
            .undefined,
            .unknown,
            .void,
            => return parseTypeKeyword(parser),
            .this => return parseThisTypeOrPredicate(parser),
            .asserts => {
                if (try isAssertsPredicateStart(parser)) return parseAssertsTypePredicate(parser);
                return parseTypeReference(parser);
            },
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
            .left_brace => return if (try signatures.isStartOfMappedType(parser))
                signatures.parseMappedType(parser)
            else
                signatures.parseTypeLiteral(parser),
            .keyof, .unique, .readonly => return parseTypeOperator(parser),
            .infer => return parseInferType(parser),
            .template_head => return parseTemplateLiteralType(parser),
            .typeof => return parseTypeQuery(parser),
            .import => return parseImportType(parser),
            .question => return parseJSDocNullableOrUnknownType(parser),
            .logical_not => return parseJSDocNonNullableType(parser),
            else => {},
        }
    }

    // `const` is allowed here for `as const`.
    if ((token.tag.isIdentifierLike() and !token.tag.isUnconditionallyReserved()) or token.tag == .@"const") {
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
        .undefined => .{ .ts_undefined_keyword = .{} },
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
            const next = (try parser.peekAhead()) orelse return null;
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

/// `Hello, ${World}!`
/// ^^^^^^^^^^^^^^^^^^
fn parseTemplateLiteralType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .template_head);

    const start = parser.current_token.span.start;

    const quasis_checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(quasis_checkpoint);
    const types_checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(types_checkpoint);

    const head = parser.current_token;
    try parser.scratch_a.append(
        parser.allocator(),
        try literals.addTemplateElement(parser, head, false, false),
    );
    try parser.advance() orelse return null;

    var end = head.span.end;

    while (true) {
        const ty = try parseType(parser) orelse return null;
        try parser.scratch_b.append(parser.allocator(), ty);

        // re-scan `}` as a template continuation so the following text is
        // lexed as template characters, not source.
        if (parser.current_token.tag != .right_brace) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected '}' to close template type interpolation",
                .{ .help = "Template type interpolations must be closed with '}'" },
            );
            return null;
        }

        const right_brace = parser.current_token;
        const template_token = parser.lexer.reScanTemplateContinuation(right_brace.span.start) catch |e| {
            try parser.report(
                right_brace.span,
                lexer.getLexicalErrorMessage(e),
                .{ .help = lexer.getLexicalErrorHelp(e) },
            );
            return null;
        };

        const is_tail = template_token.tag == .template_tail;

        try parser.scratch_a.append(
            parser.allocator(),
            try literals.addTemplateElement(parser, template_token, is_tail, false),
        );

        end = template_token.span.end;
        try parser.advanceWithRescannedToken(template_token) orelse return null;

        if (is_tail) break;
    }

    return try parser.tree.createNode(
        .{ .ts_template_literal_type = .{
            .quasis = try parser.createExtraFromScratch(&parser.scratch_a, quasis_checkpoint),
            .types = try parser.createExtraFromScratch(&parser.scratch_b, types_checkpoint),
        } },
        .{ .start = start, .end = end },
    );
}

/// ?T   ?
/// ^^   ^
fn parseJSDocNullableOrUnknownType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .question);

    const start = parser.current_token.span.start;
    const q_end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume '?'

    if (!isStartOfType(parser.current_token.tag)) {
        return try parser.tree.createNode(
            .{ .ts_jsdoc_unknown_type = .{} },
            .{ .start = start, .end = q_end },
        );
    }

    const inner = try parseType(parser) orelse return null;
    return try parser.tree.createNode(
        jsdocNodeData(inner, .nullable, false),
        .{ .start = start, .end = parser.tree.getSpan(inner).end },
    );
}

/// !T   (operand is a primary type, so `!T[]` groups as `(!T)[]`)
/// ^^
fn parseJSDocNonNullableType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .logical_not);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '!'

    const inner = try parsePrimaryType(parser) orelse return null;
    return try parser.tree.createNode(
        jsdocNodeData(inner, .non_nullable, false),
        .{ .start = start, .end = parser.tree.getSpan(inner).end },
    );
}

const JSDocKind = enum { nullable, non_nullable };

fn jsdocNodeData(inner: ast.NodeIndex, comptime kind: JSDocKind, postfix: bool) ast.NodeData {
    return switch (kind) {
        .nullable => .{ .ts_jsdoc_nullable_type = .{ .type_annotation = inner, .postfix = postfix } },
        .non_nullable => .{ .ts_jsdoc_non_nullable_type = .{ .type_annotation = inner, .postfix = postfix } },
    };
}

fn isStartOfType(tag: TokenTag) bool {
    return switch (tag) {
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
        => true,
        else => tag.isIdentifierLike(),
    };
}

/// keyof T   unique symbol   readonly T[]
/// ^^^^^^^   ^^^^^^^^^^^^^   ^^^^^^^^^^^^
fn parseTypeOperator(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    const start = token.span.start;

    const operator: ast.TSTypeOperatorKind = switch (token.tag) {
        .keyof => .keyof,
        .unique => .unique,
        .readonly => .readonly,
        else => unreachable,
    };

    try parser.advance() orelse return null; // consume the operator keyword

    const inner = try parsePostfixType(parser) orelse return null;
    const end = parser.tree.getSpan(inner).end;

    return try parser.tree.createNode(
        .{ .ts_type_operator = .{
            .operator = operator,
            .type_annotation = inner,
        } },
        .{ .start = start, .end = end },
    );
}

/// infer T   infer T extends U
/// ^^^^^^^   ^^^^^^^^^^^^^^^^^
fn parseInferType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .infer);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'infer'

    const name_token = parser.current_token;
    const name = try literals.parseBindingIdentifier(parser) orelse return null;

    var constraint: ast.NodeIndex = .null;
    var param_end: u32 = name_token.span.end;

    if (parser.current_token.tag == .extends) {
        try parser.advance() orelse return null; // consume 'extends'
        constraint = try parseUnionType(parser) orelse return null;
        param_end = parser.tree.getSpan(constraint).end;
    }

    const type_parameter = try parser.tree.createNode(
        .{ .ts_type_parameter = .{
            .name = name,
            .constraint = constraint,
        } },
        .{ .start = name_token.span.start, .end = param_end },
    );

    return try parser.tree.createNode(
        .{ .ts_infer_type = .{ .type_parameter = type_parameter } },
        .{ .start = start, .end = param_end },
    );
}

fn isStartOfFunctionOrConstructorType(parser: *Parser) Error!bool {
    const tag = parser.current_token.tag;

    if (tag == .new) return true;

    if (tag == .abstract) {
        const next = (try parser.peekAhead()) orelse return false;
        return next.tag == .new;
    }

    if (tag == .less_than) return true;

    if (tag != .left_paren) return false;

    const peek = try parser.peekAheadN(3);

    const t1 = peek[0] orelse return false;

    switch (t1.tag) {
        .right_paren => {
            const t2 = peek[1] orelse return false;
            return t2.tag == .arrow;
        },
        .spread => return true,
        .this => {
            const t2 = peek[1] orelse return false;
            return t2.tag == .colon;
        },
        // `({...` or `([...` is ambiguous between a destructuring param
        // and a parenthesized type literal / tuple.
        .left_brace, .left_bracket => return isFunctionTypeAfterPattern(parser),
        else => {},
    }

    if (!t1.tag.isIdentifierLike()) return false;

    const t2 = peek[1] orelse return false;

    return switch (t2.tag) {
        .colon, .question, .comma, .assign => true,
        .right_paren => blk: {
            const t3 = peek[2] orelse break :blk false;
            break :blk t3.tag == .arrow;
        },
        else => false,
    };
}

/// speculatively parses `(BindingPattern` and checks the follow set to
/// disambiguate a function type from a parenthesized type.
fn isFunctionTypeAfterPattern(parser: *Parser) Error!bool {
    const cp = parser.checkpoint();
    defer parser.rewind(cp);

    try parser.advance() orelse return false; // consume '('

    _ = try patterns.parseBindingPattern(parser) orelse return false;

    return switch (parser.current_token.tag) {
        .colon, .comma, .question, .assign => true,
        .right_paren => blk: {
            try parser.advance() orelse break :blk false;
            break :blk parser.current_token.tag == .arrow;
        },
        else => false,
    };
}

/// (a: T) => R       new (x: T) => R       abstract new (x: T) => R
/// ^^^^^^^^^^^       ^^^^^^^^^^^^^^^       ^^^^^^^^^^^^^^^^^^^^^^^^
fn parseFunctionOrConstructorType(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    var is_abstract = false;
    if (parser.current_token.tag == .abstract) {
        is_abstract = true;
        try parser.advance() orelse return null; // consume 'abstract'
    }

    var is_constructor = false;
    if (parser.current_token.tag == .new) {
        is_constructor = true;
        try parser.advance() orelse return null; // consume 'new'
    }

    const type_parameters = try parseTypeParameters(parser);

    const params = try functions.parseFormalParameters(parser, .signature, false) orelse return null;

    if (parser.current_token.tag != .arrow) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '=>' in function type",
            .{ .help = "A function type is written '(params) => ReturnType'" },
        );
        return null;
    }

    const arrow_start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '=>'

    const return_type_inner = try parseTypeOrTypePredicate(parser) orelse return null;

    const return_type_end = parser.tree.getSpan(return_type_inner).end;

    const return_type = try parser.tree.createNode(
        .{ .ts_type_annotation = .{ .type_annotation = return_type_inner } },
        .{ .start = arrow_start, .end = return_type_end },
    );

    if (is_constructor) {
        return try parser.tree.createNode(
            .{ .ts_constructor_type = .{
                .type_parameters = type_parameters,
                .params = params,
                .return_type = return_type,
                .abstract = is_abstract,
            } },
            .{ .start = start, .end = return_type_end },
        );
    }

    return try parser.tree.createNode(
        .{ .ts_function_type = .{
            .type_parameters = type_parameters,
            .params = params,
            .return_type = return_type,
        } },
        .{ .start = start, .end = return_type_end },
    );
}

/// (A | B)   (Foo)
/// ^^^^^^^   ^^^^^
fn parseParenthesizedType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_paren);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '('

    const inner = try parseType(parser) orelse return null;

    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close a parenthesized type",
        "Each '(' in a type must be matched by a ')'",
    )) return null;

    return try parser.tree.createNode(
        .{ .ts_parenthesized_type = .{ .type_annotation = inner } },
        .{ .start = start, .end = parser.prev_token_end },
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

    if (!try parser.expect(
        .right_bracket,
        "Expected ']' to close a tuple type",
        "Each '[' in a tuple type must be matched by a ']'",
    )) return null;

    const element_types = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);

    return try parser.tree.createNode(
        .{ .ts_tuple_type = .{ .element_types = element_types } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

/// `...T` rest, named `label[?]: T`, or a plain type.
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

/// rewrites a trailing postfix `T?` into `TSOptionalType` since
/// `parseType` always consumes it as a `TSJSDocNullableType`.
fn parseTupleElementBody(parser: *Parser) Error!?ast.NodeIndex {
    if (try isNamedTupleElement(parser)) {
        return parseNamedTupleMember(parser);
    }

    const ty = try parseType(parser) orelse return null;

    switch (parser.tree.getData(ty)) {
        .ts_jsdoc_nullable_type => |n| if (n.postfix) return try parser.tree.createNode(
            .{ .ts_optional_type = .{ .type_annotation = n.type_annotation } },
            parser.tree.getSpan(ty),
        ),
        else => {},
    }

    return ty;
}

fn isNamedTupleElement(parser: *Parser) Error!bool {
    if (!parser.current_token.tag.isIdentifierLike()) return false;

    const peek = try parser.peekAheadN(2);
    const t1 = peek[0] orelse return false;

    if (t1.tag == .colon) return true;
    if (t1.tag != .question) return false;

    const t2 = peek[1] orelse return false;
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

    if (!try parser.expect(.colon, "Expected ':' after named tuple element label", null)) return null;

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
    const start = parser.current_token.span.start;

    const type_name = try parseEntityName(parser) orelse return null;

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

/// typeof x   typeof x.y   typeof Err<T>   typeof import("m").Foo
/// ^^^^^^^^   ^^^^^^^^^^   ^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^
fn parseTypeQuery(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .typeof);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'typeof'

    const expr_name = if (parser.current_token.tag == .import)
        try parseImportType(parser) orelse return null
    else
        try parseEntityName(parser) orelse return null;

    // `import("m").Foo<T>` absorbs its own type arguments; only an
    // entity name still needs a top-level `<T>`.
    const type_arguments = if (parser.tree.getData(expr_name) == .ts_import_type)
        .null
    else
        try parseTypeArguments(parser);

    const end = if (type_arguments != .null)
        parser.tree.getSpan(type_arguments).end
    else
        parser.tree.getSpan(expr_name).end;

    return try parser.tree.createNode(
        .{ .ts_type_query = .{
            .expr_name = expr_name,
            .type_arguments = type_arguments,
        } },
        .{ .start = start, .end = end },
    );
}

/// import("module")    import("module").Foo.Bar<T>    import("m", { with: ... })
/// ^^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^^^^^^^^
fn parseImportType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .import);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'import'

    if (!try parser.expect(
        .left_paren,
        "Expected '(' after 'import' in an import type",
        "An import type is written 'import(\"module\").Foo'",
    )) return null;

    const source = try literals.parseStringLiteral(parser) orelse return null;

    var options: ast.NodeIndex = .null;
    if (parser.current_token.tag == .comma) {
        try parser.advance() orelse return null; // consume ','
        if (parser.current_token.tag != .right_paren) {
            options = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
            if (parser.current_token.tag == .comma) {
                try parser.advance() orelse return null; // trailing comma after options
            }
        }
    }

    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close an import type",
        "An import type's argument list must end with ')'",
    )) return null;

    var qualifier: ast.NodeIndex = .null;
    if (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null; // consume '.'
        qualifier = try parseImportTypeQualifier(parser) orelse return null;
    }

    const type_arguments = try parseTypeArguments(parser);

    const end = if (type_arguments != .null)
        parser.tree.getSpan(type_arguments).end
    else if (qualifier != .null)
        parser.tree.getSpan(qualifier).end
    else
        parser.prev_token_end;

    return try parser.tree.createNode(
        .{ .ts_import_type = .{
            .source = source,
            .options = options,
            .qualifier = qualifier,
            .type_arguments = type_arguments,
        } },
        .{ .start = start, .end = end },
    );
}

/// dotted name after `import("m").`; every segment is an `IdentifierName`.
fn parseImportTypeQualifier(parser: *Parser) Error!?ast.NodeIndex {
    const head = try parseIdentifierNameHead(parser) orelse return null;
    return extendQualifiedName(parser, head);
}

/// `A.B.C`; head is `IdentifierReference` (or `ThisExpression`), each
/// tail segment is `IdentifierName`.
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
        try parser.tree.createNode(.{ .this_expression = .{} }, first_token.span)
    else
        try parser.tree.createNode(.{
            .identifier_reference = .{ .name = try parser.identifierName(first_token) },
        }, first_token.span);

    try parser.advanceWithoutEscapeCheck() orelse return null;

    return extendQualifiedName(parser, head);
}

fn parseIdentifierNameHead(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    if (!token.tag.isIdentifierLike()) {
        try parser.reportExpected(
            token.span,
            "Expected an identifier",
            .{ .help = "Each '.' in a qualified name must be followed by an identifier" },
        );
        return null;
    }
    const head = try parser.tree.createNode(
        .{ .identifier_name = .{ .name = try parser.identifierName(token) } },
        token.span,
    );
    try parser.advanceWithoutEscapeCheck() orelse return null;
    return head;
}

/// extends `head` with `.Id` continuations, each wrapped in `TSQualifiedName`.
pub fn extendQualifiedName(parser: *Parser, head: ast.NodeIndex) Error!?ast.NodeIndex {
    var name = head;
    while (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null; // consume '.'

        const right_token = parser.current_token;
        if (!right_token.tag.isIdentifierLike()) {
            try parser.reportExpected(
                right_token.span,
                "Expected an identifier after '.'",
                .{ .help = "A qualified name must end with an identifier" },
            );
            return null;
        }

        const right = try parser.tree.createNode(
            .{ .identifier_name = .{ .name = try parser.identifierName(right_token) } },
            right_token.span,
        );
        try parser.advanceWithoutEscapeCheck() orelse return null;

        name = try parser.tree.createNode(
            .{ .ts_qualified_name = .{ .left = name, .right = right } },
            .{ .start = parser.tree.getSpan(name).start, .end = right_token.span.end },
        );
    }
    return name;
}

/// Foo<T, U, V>
///    ^^^^^^^^^
pub fn parseTypeArguments(parser: *Parser) Error!ast.NodeIndex {
    return parseAngleList(parser, .arguments);
}

/// function f<T, U extends V>() {}
///           ^^^^^^^^^^^^^^^^
pub fn parseTypeParameters(parser: *Parser) Error!ast.NodeIndex {
    return parseAngleList(parser, .parameters);
}

const AngleListKind = enum { arguments, parameters };

/// `.null` when no leading `<`.
fn parseAngleList(parser: *Parser, comptime kind: AngleListKind) Error!ast.NodeIndex {
    if (parser.current_token.tag != .less_than) return .null;

    const start = parser.current_token.span.start;
    try parser.advance() orelse return .null; // consume '<'

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .greater_than and parser.current_token.tag != .eof) {
        const elem = switch (kind) {
            .arguments => try parseType(parser) orelse return .null,
            .parameters => try parseTypeParameter(parser) orelse return .null,
        };
        try parser.scratch_a.append(parser.allocator(), elem);
        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return .null;
    }

    const end = try consumeAngleClose(parser, kind) orelse return .null;
    const params = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);

    const data: ast.NodeData = switch (kind) {
        .arguments => .{ .ts_type_parameter_instantiation = .{ .params = params } },
        .parameters => .{ .ts_type_parameter_declaration = .{ .params = params } },
    };
    return try parser.tree.createNode(data, .{ .start = start, .end = end });
}

/// re-scans a compound `>` token (`>>`, `>>=`, etc.) into a leading `>`
/// so the remainder stays in the token stream.
fn consumeAngleClose(parser: *Parser, comptime kind: AngleListKind) Error!?u32 {
    switch (parser.current_token.tag) {
        .greater_than => {
            const end = parser.current_token.span.end;
            try parser.advance() orelse return null;
            return end;
        },
        .right_shift, .unsigned_right_shift, .greater_than_equal, .right_shift_assign, .unsigned_right_shift_assign => {
            const gt = parser.lexer.reScanGreaterThan(parser.current_token.span.start);
            try parser.advanceWithRescannedToken(gt) orelse return null;
            return gt.span.end;
        },
        else => {
            try parser.reportExpected(
                parser.current_token.span,
                switch (kind) {
                    .arguments => "Expected '>' to close a type argument list",
                    .parameters => "Expected '>' to close a type parameter list",
                },
                .{ .help = "Each '<' in a type must be matched by a '>'" },
            );
            return null;
        },
    }
}

/// const in out T extends U = V
/// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
fn parseTypeParameter(parser: *Parser) Error!?ast.NodeIndex {
    var is_const = false;
    var is_in = false;
    var is_out = false;
    var start: u32 = parser.current_token.span.start;
    var start_set = false;

    // contextual: treat as a modifier only when followed by an identifier
    // (so `<out>` still names a parameter). order is `const in out`,
    // each at most once.
    while (true) {
        const token = parser.current_token;
        const tag = token.tag;
        if (tag != .in and tag != .out and tag != .@"const") break;

        const next = (try parser.peekAhead()) orelse break;
        if (!next.tag.isIdentifierLike()) break;

        const already_present = switch (tag) {
            .@"const" => is_const,
            .in => is_in,
            .out => is_out,
            else => unreachable,
        };
        const out_of_order = switch (tag) {
            .@"const" => is_in or is_out,
            .in => is_out,
            .out => false,
            else => unreachable,
        };

        if (already_present) {
            try parser.report(
                token.span,
                try parser.fmt("Duplicate '{s}' modifier on type parameter", .{tag.toString().?}),
                .{},
            );
        } else if (out_of_order) {
            try parser.report(
                token.span,
                "Type parameter modifiers must appear in the order 'const in out'",
                .{},
            );
        }

        switch (tag) {
            .@"const" => is_const = true,
            .in => is_in = true,
            .out => is_out = true,
            else => unreachable,
        }

        if (!start_set) {
            start = token.span.start;
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

/// function f(x): x is T { ... }   function f(x): asserts x is T { ... }
///                ^^^^^^                            ^^^^^^^^^^^^^^
pub fn parseReturnTypeAnnotation(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .colon);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume ':'

    const inner = try parseTypeOrTypePredicate(parser) orelse return null;

    return try parser.tree.createNode(
        .{ .ts_type_annotation = .{ .type_annotation = inner } },
        .{ .start = start, .end = parser.tree.getSpan(inner).end },
    );
}

// type predicates: three forms, one finalizer.
//   this is T          primary type
//   asserts X [is T]   primary type, narrowing is optional
//   Id is T            return-type only, needs cover disambiguation

/// return-type entry; `this is T` and `asserts ...` go through `parseType`.
pub fn parseTypeOrTypePredicate(parser: *Parser) Error!?ast.NodeIndex {
    if (!try isIdentifierPredicateStart(parser)) return parseType(parser);

    const parameter_name = try literals.parseIdentifierName(parser) orelse return null;
    return finishTypePredicate(parser, parser.tree.getSpan(parameter_name).start, parameter_name, false);
}

/// this   this is T
/// ^^^^   ^^^^^^^^^
fn parseThisTypeOrPredicate(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .this);

    const this_token = parser.current_token;
    try parser.advance() orelse return null; // consume 'this'

    const this_type = try parser.tree.createNode(.{ .ts_this_type = .{} }, this_token.span);

    const next = parser.current_token;
    if (next.tag != .is or next.isEscaped() or next.hasLineTerminatorBefore()) return this_type;

    return finishTypePredicate(parser, this_token.span.start, this_type, false);
}

/// asserts x   asserts x is T   asserts this is T
/// ^^^^^^^^^   ^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^
fn parseAssertsTypePredicate(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .asserts);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'asserts'

    const parameter_name = try parsePredicateParameterName(parser) orelse return null;
    return finishTypePredicate(parser, start, parameter_name, true);
}

/// `TSThisType` for `this`, otherwise `IdentifierName`.
fn parsePredicateParameterName(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .this) return literals.parseIdentifierName(parser);

    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.tree.createNode(.{ .ts_this_type = .{} }, token.span);
}

/// consumes the optional `is T` narrowing and builds the predicate node.
fn finishTypePredicate(
    parser: *Parser,
    start: u32,
    parameter_name: ast.NodeIndex,
    asserts: bool,
) Error!?ast.NodeIndex {
    var end = parser.tree.getSpan(parameter_name).end;
    var type_annotation: ast.NodeIndex = .null;

    if (parser.current_token.tag == .is and !parser.current_token.isEscaped()) {
        try parser.advance() orelse return null; // consume 'is'
        const inner = try parseType(parser) orelse return null;
        type_annotation = try parser.tree.createNode(
            .{ .ts_type_annotation = .{ .type_annotation = inner } },
            parser.tree.getSpan(inner),
        );
        end = parser.tree.getSpan(type_annotation).end;
    }

    return try parser.tree.createNode(
        .{ .ts_type_predicate = .{
            .parameter_name = parameter_name,
            .type_annotation = type_annotation,
            .asserts = asserts,
        } },
        .{ .start = start, .end = end },
    );
}

/// `asserts` followed on the same line by `this` or an identifier.
fn isAssertsPredicateStart(parser: *Parser) Error!bool {
    if (parser.current_token.tag != .asserts or parser.current_token.isEscaped()) return false;

    const next = (try parser.peekAhead()) orelse return false;
    if (next.hasLineTerminatorBefore()) return false;

    return next.tag == .this or next.tag.isIdentifierLike();
}

/// `Id is T` on the same line. excludes `this` (handled in `parsePrimaryType`).
fn isIdentifierPredicateStart(parser: *Parser) Error!bool {
    const current = parser.current_token;
    if (current.isEscaped() or current.tag == .this or !current.tag.isIdentifierLike()) return false;

    const next = (try parser.peekAhead()) orelse return false;
    return next.tag == .is and !next.isEscaped() and !next.hasLineTerminatorBefore();
}

/// in place
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

/// in place
pub fn applyDecoratorsToPattern(parser: *Parser, pattern: ast.NodeIndex, decorators: ast.IndexRange) void {
    if (decorators.len == 0) return;

    var data = parser.tree.getData(pattern);

    switch (data) {
        .binding_identifier => |*v| v.decorators = decorators,
        .object_pattern => |*v| v.decorators = decorators,
        .array_pattern => |*v| v.decorators = decorators,
        .assignment_pattern => |*v| v.decorators = decorators,
        .binding_rest_element => |*v| v.decorators = decorators,
        else => return,
    }

    parser.tree.replaceData(pattern, data);
}

/// marks `x?`, `[...]?`, `{...}?` optional in place; extends span to `end`.
pub fn markPatternOptional(parser: *Parser, pattern: ast.NodeIndex, end: u32) void {
    var data = parser.tree.getData(pattern);

    switch (data) {
        .binding_identifier => |*v| v.optional = true,
        .object_pattern => |*v| v.optional = true,
        .array_pattern => |*v| v.optional = true,
        .assignment_pattern => |*v| v.optional = true,
        else => return,
    }

    parser.tree.replaceData(pattern, data);

    const pattern_span = parser.tree.getSpan(pattern);
    if (end > pattern_span.end) {
        parser.tree.replaceSpan(pattern, .{ .start = pattern_span.start, .end = end });
    }
}
