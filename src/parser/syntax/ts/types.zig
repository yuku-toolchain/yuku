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

/// parseType variant that refuses to start a bare conditional, so the
/// enclosing conditional's `?` and `:` survive for its own pattern. used
/// only for the extends slot of a conditional type.
///
/// a function or constructor type dispatched from here still parses its
/// own return type with the full grammar (matching TypeScript compiler's
/// `parseReturnType`, which unconditionally clears the
/// `DisallowConditionalTypes` flag). only the immediate extends operand
/// is restricted.
///
/// example:
///
///   type R = T extends () => A ? X : Y;
///   //                 ^^^^^^^^            extends slot (function type)
///   //                         ^^^^^^^     left for the outer `? X : Y`
///
/// parseUnionType stops one level below conditional, so any trailing
/// `extends` / `?` / `:` stays in the stream for the caller to consume.
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
            .left_brace => return parseTypeLiteral(parser),
            .keyof, .unique => return parseTypeOperator(parser),
            .readonly => {
                // `readonly` is a type operator in most positions (`readonly T[]`).
                // at the start of a type literal member it is a modifier, but the
                // dispatch to `parseTypeLiteral` is resolved one level up before
                // calling `parsePrimaryType` on a member. as a standalone type the
                // operator reading is always correct.
                return parseTypeOperator(parser);
            },
            .infer => return parseInferType(parser),
            .template_head => return parseTemplateLiteralType(parser),
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
/// template literal with one or more interpolations in type position.
/// the no-substitution case stays in `parseLiteralType` and produces a
/// `TSLiteralType` wrapping a `TemplateLiteral`.
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

        // the expression slot closes with a `}` which was lexed as a plain
        // right brace. re-scan it as a template continuation so the lexer
        // treats the following text as template characters rather than source.
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

/// references `isStartOfFunctionTypeOrConstructorType` and
/// `nextIsUnambiguouslyStartOfFunctionType` in the typescript-go
/// parser:
/// https://github.com/microsoft/typescript-go/blob/1de8d68230f8759af6fb71d9cf0f9c37d2c65507/internal/parser/parser.go#L3768
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

    if (!try parser.expect(
        .left_paren,
        "Expected '(' to start function type parameters",
        "A function or constructor type requires a parenthesized parameter list",
    )) return null;

    const params = try functions.parseFormalParamaters(parser, .signature) orelse return null;

    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close function type parameters",
        "Each '(' in a function type must be matched by a ')'",
    )) return null;

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

    const return_type_inner = try parseType(parser) orelse return null;

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

/// { x: T; foo(): U; [k: string]: V }
/// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
///
/// references `parseObjectTypeMembers` and `parseTypeMember` in the
/// typescript-go parser:
/// https://github.com/microsoft/typescript-go/blob/1de8d68230f8759af6fb71d9cf0f9c37d2c65507/internal/parser/parser.go
fn parseTypeLiteral(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume '{'

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .right_brace and parser.current_token.tag != .eof) {
        const member = try parseTypeMember(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), member);

        // members are separated by `;`, `,`, or an automatic newline. when a
        // `;` or `,` follows, it is folded into the member's span (TSC
        // includes the terminator in the member range). a bare newline is
        // accepted without adjusting the span.
        const sep = parser.current_token.tag;
        if (sep == .semicolon or sep == .comma) {
            const sep_end = parser.current_token.span.end;
            const member_span = parser.tree.getSpan(member);
            parser.tree.replaceSpan(member, .{ .start = member_span.start, .end = sep_end });
            try parser.advance() orelse return null;
        } else if (sep != .right_brace and !parser.current_token.hasLineTerminatorBefore()) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected ';', ',', or newline between type literal members",
                .{ .help = "Separate type literal members with ';', ',', or a newline, or close the type with '}'" },
            );
            return null;
        }
    }

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close a type literal",
        "Each '{' in a type must be matched by a '}'",
    )) return null;

    const members = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint);

    return try parser.tree.createNode(
        .{ .ts_type_literal = .{ .members = members } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

/// dispatches a single type literal member. accepts:
///
/// - call signatures      `(x: T): R`
/// - call signatures      `<T>(x: T): R`
/// - construct signatures `new (x: T): R`
/// - index signatures     `[k: string]: V` or `readonly [k: string]: V`
/// - property signatures  `key: T`, `readonly key?: T`
/// - method signatures    `key(x: T): R`, `get key(): R`, `set key(v: T)`
fn parseTypeMember(parser: *Parser) Error!?ast.NodeIndex {
    const tag = parser.current_token.tag;

    // call signature with no generics: starts with '('
    if (tag == .left_paren) return parseCallSignature(parser);

    // call signature with generics: starts with '<'
    if (tag == .less_than) return parseCallSignature(parser);

    // construct signature: `new` followed by '(', '<', or anything (treated
    // as a construct signature by default). if the next token is an
    // identifier-like and then `:`, it would be a property named `new`,
    // but TSC treats `new` as the construct signature keyword in member
    // position.
    if (tag == .new) {
        const next = (try parser.peekAhead()) orelse return null;
        if (next.tag == .left_paren or next.tag == .less_than) {
            return parseConstructSignature(parser);
        }
    }

    // `readonly` modifier. applies to property signatures and index
    // signatures. treat as a modifier only when followed by a property-key
    // start or a `[` (for an index signature). otherwise it is the property
    // name itself.
    if (tag == .readonly) {
        const next = (try parser.peekAhead()) orelse return null;
        if (isPropertyKeyStart(next.tag) or next.tag == .left_bracket) {
            const readonly_start = parser.current_token.span.start;
            try parser.advance() orelse return null; // consume 'readonly'
            if (parser.current_token.tag == .left_bracket and try isIndexSignatureStart(parser)) {
                return parseIndexSignature(parser, readonly_start, true);
            }
            return parsePropertyOrMethodSignature(parser, readonly_start, true);
        }
    }

    // index signature without `readonly`: `[k: T]: V`. must be distinguished
    // from a computed property key `[expr]: T`.
    if (tag == .left_bracket and try isIndexSignatureStart(parser)) {
        const start = parser.current_token.span.start;
        return parseIndexSignature(parser, start, false);
    }

    // property or method signature with no modifier.
    const start = parser.current_token.span.start;
    return parsePropertyOrMethodSignature(parser, start, false);
}

/// distinguishes an index signature `[k: T]: V` from a computed property key
/// `[expr]: T`. peeks for an `identifier-like` token followed by `:` or `,`.
fn isIndexSignatureStart(parser: *Parser) Error!bool {
    std.debug.assert(parser.current_token.tag == .left_bracket);

    const peek = try parser.peekAheadN(2);

    const t1 = peek[0] orelse return false;
    if (!t1.tag.isIdentifierLike()) return false;

    const t2 = peek[1] orelse return false;
    return t2.tag == .colon or t2.tag == .comma;
}

inline fn isPropertyKeyStart(tag: TokenTag) bool {
    return tag == .left_bracket or
        tag.isIdentifierLike() or
        tag == .string_literal or
        tag.isNumericLiteral();
}

/// (params): R    <T>(params): R
/// ^^^^^^^^^^^    ^^^^^^^^^^^^^^
fn parseCallSignature(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const type_parameters = try parseTypeParameters(parser);

    if (!try parser.expect(
        .left_paren,
        "Expected '(' to start call signature parameters",
        "A call signature is written '(params): ReturnType'",
    )) return null;

    const params = try functions.parseFormalParamaters(parser, .signature) orelse return null;

    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close call signature parameters",
        "Each '(' in a call signature must be matched by a ')'",
    )) return null;

    const return_type = try parseSignatureReturnType(parser);

    const end = if (return_type != .null)
        parser.tree.getSpan(return_type).end
    else
        parser.prev_token_end;

    return try parser.tree.createNode(
        .{ .ts_call_signature_declaration = .{
            .type_parameters = type_parameters,
            .params = params,
            .return_type = return_type,
        } },
        .{ .start = start, .end = end },
    );
}

/// new (params): R    new <T>(params): R
/// ^^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^
fn parseConstructSignature(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .new);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'new'

    const type_parameters = try parseTypeParameters(parser);

    if (!try parser.expect(
        .left_paren,
        "Expected '(' to start construct signature parameters",
        "A construct signature is written 'new (params): ReturnType'",
    )) return null;

    const params = try functions.parseFormalParamaters(parser, .signature) orelse return null;

    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close construct signature parameters",
        "Each '(' in a construct signature must be matched by a ')'",
    )) return null;

    const return_type = try parseSignatureReturnType(parser);

    const end = if (return_type != .null)
        parser.tree.getSpan(return_type).end
    else
        parser.prev_token_end;

    return try parser.tree.createNode(
        .{ .ts_construct_signature_declaration = .{
            .type_parameters = type_parameters,
            .params = params,
            .return_type = return_type,
        } },
        .{ .start = start, .end = end },
    );
}

/// [k: T]: V
/// ^^^^^^^^^
///
/// `readonly` is consumed by the caller when present; `start` points at the
/// modifier keyword start in that case, otherwise at the `[`.
fn parseIndexSignature(parser: *Parser, start: u32, is_readonly: bool) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_bracket);
    try parser.advance() orelse return null; // consume '['

    const params_checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(params_checkpoint);

    while (parser.current_token.tag != .right_bracket and parser.current_token.tag != .eof) {
        const param = try parseIndexSignatureParameter(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), param);
        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
        } else break;
    }

    if (!try parser.expect(
        .right_bracket,
        "Expected ']' to close an index signature parameter list",
        "An index signature is written '[name: KeyType]: ValueType'",
    )) return null;

    if (parser.current_token.tag != .colon) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected ':' after index signature parameters",
            .{ .help = "An index signature requires a value type: '[k: string]: number'" },
        );
        return null;
    }

    const type_annotation = try parseTypeAnnotation(parser) orelse return null;
    const end = parser.tree.getSpan(type_annotation).end;

    const parameters = try parser.createExtraFromScratch(&parser.scratch_a, params_checkpoint);

    return try parser.tree.createNode(
        .{ .ts_index_signature = .{
            .parameters = parameters,
            .type_annotation = type_annotation,
            .readonly = is_readonly,
        } },
        .{ .start = start, .end = end },
    );
}

/// one entry of an index signature parameter list. a plain binding identifier
/// with a type annotation attached. rejected without an annotation since the
/// index signature grammar requires it.
fn parseIndexSignatureParameter(parser: *Parser) Error!?ast.NodeIndex {
    const name = try literals.parseBindingIdentifier(parser) orelse return null;

    if (parser.current_token.tag != .colon) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected ':' after index signature parameter name",
            .{ .help = "Each index signature parameter requires a type: '[k: string]'" },
        );
        return null;
    }

    const annotation = try parseTypeAnnotation(parser) orelse return null;
    applyTypeAnnotationToPattern(parser, name, annotation);
    return name;
}

/// key: T    key?: T    get key(): T    set key(v: T)    key(x: T): R
/// ^^^^^^    ^^^^^^^    ^^^^^^^^^^^^    ^^^^^^^^^^^^^    ^^^^^^^^^^^^
///
/// `start` and `is_readonly` are passed in so this function can be called
/// either with the modifier already consumed or directly.
fn parsePropertyOrMethodSignature(parser: *Parser, start: u32, is_readonly: bool) Error!?ast.NodeIndex {
    // `get` / `set` accessor signatures. recognized only when followed by
    // another property-key start, so the name `get` or `set` itself can still
    // be used as an ordinary property name.
    const cur_tag = parser.current_token.tag;
    if (cur_tag == .get or cur_tag == .set) {
        const next = (try parser.peekAhead()) orelse return null;
        if (isPropertyKeyStart(next.tag)) {
            const kind: ast.TSMethodSignatureKind = if (cur_tag == .get) .get else .set;
            try parser.advance() orelse return null; // consume 'get' / 'set'
            return parseMethodSignatureTail(parser, start, kind, false, false);
        }
    }

    const key_result = try parsePropertyKey(parser) orelse return null;
    const key = key_result.key;
    const computed = key_result.computed;

    var is_optional = false;
    var end = parser.tree.getSpan(key).end;
    if (parser.current_token.tag == .question) {
        is_optional = true;
        end = parser.current_token.span.end;
        try parser.advance() orelse return null; // consume '?'
    }

    // method signature: key(...) or key<T>(...)
    if (parser.current_token.tag == .left_paren or parser.current_token.tag == .less_than) {
        return parseMethodSignatureBody(parser, start, key, .method, computed, is_optional);
    }

    // property signature
    var type_annotation: ast.NodeIndex = .null;
    if (parser.current_token.tag == .colon) {
        type_annotation = try parseTypeAnnotation(parser) orelse return null;
        end = parser.tree.getSpan(type_annotation).end;
    }

    return try parser.tree.createNode(
        .{ .ts_property_signature = .{
            .key = key,
            .type_annotation = type_annotation,
            .computed = computed,
            .optional = is_optional,
            .readonly = is_readonly,
        } },
        .{ .start = start, .end = end },
    );
}

/// tail of an accessor method signature. the `get` or `set` keyword has been
/// consumed by the caller; the next token is the key.
fn parseMethodSignatureTail(
    parser: *Parser,
    start: u32,
    kind: ast.TSMethodSignatureKind,
    computed_caller: bool,
    optional_caller: bool,
) Error!?ast.NodeIndex {
    _ = computed_caller;
    _ = optional_caller;

    const key_result = try parsePropertyKey(parser) orelse return null;
    const key = key_result.key;
    const computed = key_result.computed;

    var is_optional = false;
    if (parser.current_token.tag == .question) {
        is_optional = true;
        try parser.advance() orelse return null;
    }

    return parseMethodSignatureBody(parser, start, key, kind, computed, is_optional);
}

/// the parameter list + optional return type tail shared by method, getter,
/// and setter signatures.
fn parseMethodSignatureBody(
    parser: *Parser,
    start: u32,
    key: ast.NodeIndex,
    kind: ast.TSMethodSignatureKind,
    computed: bool,
    is_optional: bool,
) Error!?ast.NodeIndex {
    const type_parameters = try parseTypeParameters(parser);

    if (!try parser.expect(
        .left_paren,
        "Expected '(' to start method signature parameters",
        "A method signature is written 'name(params): ReturnType'",
    )) return null;

    const params = try functions.parseFormalParamaters(parser, .signature) orelse return null;

    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close method signature parameters",
        "Each '(' in a method signature must be matched by a ')'",
    )) return null;

    // setters carry no return type; getters and plain methods optionally do.
    const return_type: ast.NodeIndex = if (kind == .set)
        .null
    else
        try parseSignatureReturnType(parser);

    const end = if (return_type != .null)
        parser.tree.getSpan(return_type).end
    else
        parser.prev_token_end;

    return try parser.tree.createNode(
        .{ .ts_method_signature = .{
            .key = key,
            .type_parameters = type_parameters,
            .params = params,
            .return_type = return_type,
            .kind = kind,
            .computed = computed,
            .optional = is_optional,
        } },
        .{ .start = start, .end = end },
    );
}

const PropertyKeyResult = struct {
    key: ast.NodeIndex,
    computed: bool,
};

/// parse a signature property key. identifier-like names decode as
/// `IdentifierName` (property keys don't reference bindings), literals are
/// wrapped directly, and `[expr]` produces a computed key with the raw
/// expression inside.
fn parsePropertyKey(parser: *Parser) Error!?PropertyKeyResult {
    const tag = parser.current_token.tag;

    if (tag == .left_bracket) {
        try parser.advance() orelse return null; // consume '['
        const expr = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        if (!try parser.expect(
            .right_bracket,
            "Expected ']' to close a computed property key",
            "A computed property key is written '[expr]'",
        )) return null;
        return .{ .key = expr, .computed = true };
    }

    if (tag.isIdentifierLike()) {
        const key = try literals.parseIdentifierName(parser) orelse return null;
        return .{ .key = key, .computed = false };
    }

    if (tag == .string_literal) {
        const key = try literals.parseStringLiteral(parser) orelse return null;
        return .{ .key = key, .computed = false };
    }

    if (tag.isNumericLiteral()) {
        const key = try literals.parseNumericLiteral(parser) orelse return null;
        return .{ .key = key, .computed = false };
    }

    try parser.report(
        parser.current_token.span,
        try parser.fmt("Unexpected token '{s}' as signature key", .{parser.describeToken(parser.current_token)}),
        .{ .help = "Signature keys must be identifiers, strings, numbers, or computed expressions [expr]." },
    );
    return null;
}

/// the `: ReturnType` tail that follows a signature parameter list. optional;
/// returns `.null` when absent.
fn parseSignatureReturnType(parser: *Parser) Error!ast.NodeIndex {
    if (parser.current_token.tag != .colon) return .null;
    return try parseTypeAnnotation(parser) orelse .null;
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
