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

/// `parseType` variant for the extends slot of a conditional type. refuses
/// to start a bare conditional so the enclosing `?` / `:` survive for the
/// outer pattern. a nested function or constructor type still parses its
/// own return type with the full grammar.
///
///   type R = T extends () => A ? X : Y
///   //                 ^^^^^^^^            extends slot
///   //                         ^^^^^^^     outer `? X : Y`
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

        if (!try parser.expect(
            .right_bracket,
            "Expected ']' to close an indexed access type",
            "Each '[' in a type must be matched by a ']'",
        )) return null;

        ty = try parser.tree.createNode(
            .{ .ts_indexed_access_type = .{ .object_type = ty, .index_type = index } },
            .{ .start = start, .end = parser.prev_token_end },
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
            .undefined,
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
            .left_brace => return if (try signatures.isStartOfMappedType(parser))
                signatures.parseMappedType(parser)
            else
                signatures.parseTypeLiteral(parser),
            .keyof, .unique, .readonly => return parseTypeOperator(parser),
            .infer => return parseInferType(parser),
            .template_head => return parseTemplateLiteralType(parser),
            .typeof => return parseTypeQuery(parser),
            .import => return parseImportType(parser),
            else => {},
        }
    }

    // `const` is reserved but appears as a type reference in `as const`.
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
        .this => .{ .ts_this_type = .{} },
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
///
/// template literal in type position with one or more interpolations. the
/// no-substitution form stays in `parseLiteralType`.
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

        // re-scan the closing `}` as a template continuation so the lexer
        // reads the following text as template characters, not source.
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

/// classifies whether the current position starts a function or
/// constructor type, by peeking up to three tokens.
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

    const params = try functions.parseFormalParamaters(parser, .signature, false) orelse return null;

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

/// one tuple element. handles `...T` rest, `label: T` / `label?: T`
/// named forms, and a plain type with an optional trailing `?`.
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

/// tuple element without the `...` prefix. picks the named form on
/// `label:` or `label?:`, otherwise parses a plain type and wraps a
/// trailing `?` in `TSOptionalType`.
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

/// true when the current identifier is followed by `:` or `?:`.
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
///
/// the `typeof` type operator, distinct from the unary `typeof`
/// expression. takes a dotted entity name or a `TSImportType` with
/// optional type arguments.
fn parseTypeQuery(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .typeof);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'typeof'

    const expr_name = if (parser.current_token.tag == .import)
        try parseImportType(parser) orelse return null
    else
        try parseEntityName(parser) orelse return null;

    // `import("m").Foo<T>` absorbs its own type arguments inside
    // `parseImportType`. a top-level `<T>` only binds to an entity name.
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
///
/// handles only the bare `import(...)` head. the `typeof import(...)`
/// form is dispatched by `parseTypeQuery` which wraps the result as
/// `expr_name`.
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

/// the dotted name after `import("m").`. every segment is an
/// `IdentifierName` because these are property accesses on the imported
/// module type, not references to bindings in scope.
fn parseImportTypeQualifier(parser: *Parser) Error!?ast.NodeIndex {
    const first_token = parser.current_token;
    if (!first_token.tag.isIdentifierLike()) {
        try parser.reportExpected(
            first_token.span,
            "Expected an identifier after '.' in an import type",
            .{ .help = "An import type qualifier is written 'import(\"m\").Foo' or 'import(\"m\").Foo.Bar'" },
        );
        return null;
    }

    const start = first_token.span.start;

    var name = try parser.tree.createNode(.{
        .identifier_name = .{ .name = try parser.identifierName(first_token) },
    }, first_token.span);

    try parser.advanceWithoutEscapeCheck() orelse return null;

    while (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null; // consume '.'

        const right_token = parser.current_token;
        if (!right_token.tag.isIdentifierLike()) {
            try parser.reportExpected(
                right_token.span,
                "Expected an identifier after '.' in an import type qualifier",
                .{ .help = "Each '.' in an import type qualifier must be followed by an identifier" },
            );
            return null;
        }

        const right = try parser.tree.createNode(.{
            .identifier_name = .{ .name = try parser.identifierName(right_token) },
        }, right_token.span);

        try parser.advanceWithoutEscapeCheck() orelse return null;

        name = try parser.tree.createNode(
            .{ .ts_qualified_name = .{ .left = name, .right = right } },
            .{ .start = start, .end = right_token.span.end },
        );
    }

    return name;
}

/// dotted entity name `A.B.C`. head is an `IdentifierReference` (or
/// `ThisExpression` for `this`), each tail segment is an `IdentifierName`
/// so reserved words and contextual keywords are accepted after `.`.
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

    const start = first_token.span.start;

    var name = if (first_token.tag == .this)
        try parser.tree.createNode(.{ .this_expression = .{} }, first_token.span)
    else
        try parser.tree.createNode(.{
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
                .{ .help = "A qualified entity name must end with an identifier" },
            );
            return null;
        }

        const right = try parser.tree.createNode(.{
            .identifier_name = .{ .name = try parser.identifierName(right_token) },
        }, right_token.span);

        try parser.advanceWithoutEscapeCheck() orelse return null;

        name = try parser.tree.createNode(
            .{ .ts_qualified_name = .{ .left = name, .right = right } },
            .{ .start = start, .end = right_token.span.end },
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

/// shared `<...>` list parser for type arguments and type parameters.
/// returns `.null` when the list is absent (no leading `<`).
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

/// consumes the closing `>` of an angle list. a compound `>` token
/// (`>>`, `>>>`, `>=`, `>>=`, `>>>=`) is re-scanned as a leading `>` so
/// the remainder stays in the token stream.
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

    // modifier keywords are contextual. treat the current token as a
    // modifier only when an identifier-like token follows, so the word
    // itself can still be a parameter name (e.g. `<out>`).
    while (true) {
        const tag = parser.current_token.tag;
        if (tag != .in and tag != .out and tag != .@"const") break;

        const next = (try parser.peekAhead()) orelse break;
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

/// function f(x): x is T { ... }   function f(x): asserts x is T { ... }
///                ^^^^^^                            ^^^^^^^^^^^^^^
///
/// wraps a plain type or a `TSTypePredicate` in a `TSTypeAnnotation`,
/// starting at the leading `:`.
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

/// plain type or `TSTypePredicate`. a predicate is produced for
/// `asserts Id`, `asserts Id is T`, `asserts this is T`, `Id is T`, or
/// `this is T`. `asserts` and `is` are contextual so the follow-up must
/// sit on the same line, anything else falls through to the plain type.
pub fn parseTypeOrTypePredicate(parser: *Parser) Error!?ast.NodeIndex {
    if (try isAssertsPredicateStart(parser)) {
        return parseAssertsTypePredicate(parser);
    }

    if (try isTypePredicatePrefix(parser)) {
        return parseSimpleTypePredicate(parser);
    }

    return parseType(parser);
}

/// true when `asserts` starts a predicate. must be followed on the same
/// line by `this` or an identifier-like token, otherwise `asserts` is
/// just a type reference.
fn isAssertsPredicateStart(parser: *Parser) Error!bool {
    if (parser.current_token.tag != .asserts) return false;
    if (parser.current_token.isEscaped()) return false;

    const next = (try parser.peekAhead()) orelse return false;
    if (next.hasLineTerminatorBefore()) return false;

    return next.tag == .this or next.tag.isIdentifierLike();
}

/// true when `Id is T` or `this is T` is coming up. the `is` keyword
/// must sit on the same line and be unescaped.
fn isTypePredicatePrefix(parser: *Parser) Error!bool {
    const current = parser.current_token;
    if (current.isEscaped()) return false;
    if (current.tag != .this and !current.tag.isIdentifierLike()) return false;

    const next = (try parser.peekAhead()) orelse return false;
    if (next.tag != .is) return false;
    if (next.isEscaped()) return false;
    if (next.hasLineTerminatorBefore()) return false;

    return true;
}

/// asserts x   asserts x is T   asserts this is T
/// ^^^^^^^^^   ^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^
fn parseAssertsTypePredicate(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .asserts);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'asserts'

    const parameter_name = try parseTypePredicateName(parser) orelse return null;
    var end = parser.tree.getSpan(parameter_name).end;

    var type_annotation: ast.NodeIndex = .null;
    if (parser.current_token.tag == .is and !parser.current_token.isEscaped()) {
        try parser.advance() orelse return null; // consume 'is'
        type_annotation = try parseTypePredicateTypeAnnotation(parser) orelse return null;
        end = parser.tree.getSpan(type_annotation).end;
    }

    return try parser.tree.createNode(
        .{ .ts_type_predicate = .{
            .parameter_name = parameter_name,
            .type_annotation = type_annotation,
            .asserts = true,
        } },
        .{ .start = start, .end = end },
    );
}

/// x is T   this is T
/// ^^^^^^   ^^^^^^^^^
fn parseSimpleTypePredicate(parser: *Parser) Error!?ast.NodeIndex {
    const parameter_name = try parseTypePredicateName(parser) orelse return null;
    const start = parser.tree.getSpan(parameter_name).start;

    std.debug.assert(parser.current_token.tag == .is);
    try parser.advance() orelse return null; // consume 'is'

    const type_annotation = try parseTypePredicateTypeAnnotation(parser) orelse return null;

    return try parser.tree.createNode(
        .{ .ts_type_predicate = .{
            .parameter_name = parameter_name,
            .type_annotation = type_annotation,
        } },
        .{ .start = start, .end = parser.tree.getSpan(type_annotation).end },
    );
}

/// the parameter name of a type predicate. an `IdentifierName` for a
/// real parameter, or `TSThisType` for the implicit `this` receiver.
fn parseTypePredicateName(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    if (token.tag == .this) {
        try parser.advance() orelse return null;
        return try parser.tree.createNode(.{ .ts_this_type = .{} }, token.span);
    }

    return literals.parseIdentifierName(parser);
}

/// wraps the narrowed type of a predicate in a `TSTypeAnnotation` whose
/// span starts at the type itself, not at a leading token.
fn parseTypePredicateTypeAnnotation(parser: *Parser) Error!?ast.NodeIndex {
    const type_node = try parseType(parser) orelse return null;
    const span = parser.tree.getSpan(type_node);

    return try parser.tree.createNode(
        .{ .ts_type_annotation = .{ .type_annotation = type_node } },
        span,
    );
}

/// attaches a `TSTypeAnnotation` to the inner binding pattern in place.
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

/// marks the inner binding pattern optional (`x?`, `[...]?`, `{...}?`)
/// and extends its span to cover the trailing `?`. in place.
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
