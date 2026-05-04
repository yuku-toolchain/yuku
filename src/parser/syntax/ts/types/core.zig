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

// `extends` arm in conditional type, no nested `? :` here
fn parseTypeNoConditional(parser: *Parser) Error!?ast.NodeIndex {
    if (try isStartOfFunctionOrConstructorType(parser)) {
        return parseFunctionOrConstructorType(parser);
    }
    return try parseUnionType(parser) orelse {
        try parser.reportExpected(parser.current_token.span, "Expected a type", .{});
        return null;
    };
}

// T extends U ? X : Y
// ^^^^^^^^^^^^^^^^^^^
fn parseConditionalType(parser: *Parser) Error!?ast.NodeIndex {
    const check_type = try parseUnionType(parser) orelse return null;

    if (parser.current_token.tag != .extends or parser.current_token.hasLineTerminatorBefore()) {
        return check_type;
    }

    try parser.advance() orelse return null;

    // no inner conditional in this slot. infer constraint sticks. nested paren resets in parseType
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

// | A | B | C
// ^^^^^^^^^^^
fn parseUnionType(parser: *Parser) Error!?ast.NodeIndex {
    return parseBinaryTypeChain(parser, .bitwise_or, parseIntersectionType, .union_type);
}

// & A & B & C
// ^^^^^^^^^^^
fn parseIntersectionType(parser: *Parser) Error!?ast.NodeIndex {
    return parseBinaryTypeChain(parser, .bitwise_and, parsePostfixType, .intersection_type);
}

const BinaryTypeKind = enum { union_type, intersection_type };

// optional leading `|` or `&`, flattens repeats, else unwraps one operand
fn parseBinaryTypeChain(
    parser: *Parser,
    comptime separator: TokenTag,
    comptime parseOperand: fn (*Parser) Error!?ast.NodeIndex,
    comptime kind: BinaryTypeKind,
) Error!?ast.NodeIndex {
    const leading_start: ?u32 = if (parser.current_token.tag == separator) blk: {
        const start = parser.current_token.span.start;
        try parser.advance() orelse return null;
        break :blk start;
    } else null;

    const first = try parseOperand(parser) orelse return null;
    if (leading_start == null and parser.current_token.tag != separator) return first;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    try parser.scratch_a.append(parser.allocator(), first);

    var last = first;
    while (parser.current_token.tag == separator) {
        try parser.advance() orelse return null;
        last = try parseOperand(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), last);
    }

    const types = try parser.addExtraFromScratch(&parser.scratch_a, checkpoint);
    const span: ast.Span = .{
        .start = leading_start orelse parser.tree.span(first).start,
        .end = parser.tree.span(last).end,
    };
    const data: ast.NodeData = switch (kind) {
        .union_type => .{ .ts_union_type = .{ .types = types } },
        .intersection_type => .{ .ts_intersection_type = .{ .types = types } },
    };
    return try parser.tree.addNode(data, span);
}

// T[]   T[K]   T?   T!
// ^^^   ^^^^   ^^   ^^
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

// T[]   T[K]
// ^^^   ^^^^
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

// T?   T!
// ^^   ^^
fn parseJSDocPostfix(parser: *Parser, inner: ast.NodeIndex, comptime kind: JSDocKind) Error!?ast.NodeIndex {
    const start = parser.tree.span(inner).start;
    const end = parser.current_token.span.end;
    try parser.advance() orelse return null;
    return try parser.tree.addNode(
        jsdocNodeData(inner, kind, true),
        .{ .start = start, .end = end },
    );
}

// postfix `?` only when no type follows, else outer conditional eats it
fn isPostfixNullable(parser: *Parser) Error!bool {
    const next = parser.peekAhead() orelse return false;
    return !isStartOfType(next.tag);
}

// string   42   Foo<T>   this is T   asserts x is T
// ^^^^^^   ^^   ^^^^^^   ^^^^^^^^^   ^^^^^^^^^^^^^^
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
            => if (!try isQualifiedTypeContinuation(parser)) return parseTypeKeyword(parser),
            .this => return predicate.parseThisTypeOrPredicate(parser),
            .asserts => {
                if (try predicate.isAssertsPredicateStart(parser)) return predicate.parseAssertsTypePredicate(parser);
                return parseTypeReference(parser);
            },
            .left_paren => return parseParenthesizedType(parser),
            .left_bracket => return parseTupleType(parser),
            .left_brace => return if (object.isStartOfMappedType(parser))
                object.parseMappedType(parser)
            else
                object.parseTypeLiteral(parser),
            .keyof, .unique, .readonly => return parseTypeOperator(parser),
            .infer => return parseInferType(parser),
            .template_head => return literal.parseTemplateLiteralType(parser),
            .typeof => return parseTypeQuery(parser),
            .import => return parseImportType(parser),
            .question => return parseJSDocNullableOrUnknownType(parser),
            .logical_not => return parseJSDocNonNullableType(parser),
            else => {},
        }
    }

    // only `const` bypasses reserved guard, `as const`
    if ((token.tag.isIdentifierLike() and !token.tag.isUnconditionallyReserved()) or token.tag == .@"const") {
        return parseTypeReference(parser);
    }

    return null;
}

// more qualified name when `.` same line after keyword type
fn isQualifiedTypeContinuation(parser: *Parser) Error!bool {
    const next = parser.peekAhead() orelse return false;
    return next.tag == .dot and !next.hasLineTerminatorBefore();
}

// string   number   void
// ^^^^^^   ^^^^^^   ^^^^
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

// only `type Name = intrinsic` uses `TSIntrinsicKeyword`, else normal ref
pub fn parseTypeAliasBody(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .intrinsic and !parser.current_token.isEscaped()) {
        const next = parser.peekAhead() orelse return null;
        if (!continuesType(next.tag)) {
            const span = parser.current_token.span;
            try parser.advance() orelse return null;
            return try parser.tree.addNode(.{ .ts_intrinsic_keyword = .{} }, span);
        }
    }
    return parseType(parser);
}

// postfix starters after a primary type
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

// ?T   ?
// ^^   ^
fn parseJSDocNullableOrUnknownType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .question);

    const start = parser.current_token.span.start;
    const q_end = parser.current_token.span.end;
    try parser.advance() orelse return null;

    if (!isStartOfType(parser.current_token.tag)) {
        return try parser.tree.addNode(
            .{ .ts_jsdoc_unknown_type = .{} },
            .{ .start = start, .end = q_end },
        );
    }

    const inner = try parseType(parser) orelse return null;
    return try parser.tree.addNode(
        jsdocNodeData(inner, .nullable, false),
        .{ .start = start, .end = parser.tree.span(inner).end },
    );
}

// `!` then primary so `!T[]` is `(!T)[]`
fn parseJSDocNonNullableType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .logical_not);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const inner = try parsePrimaryType(parser) orelse return null;
    return try parser.tree.addNode(
        jsdocNodeData(inner, .non_nullable, false),
        .{ .start = start, .end = parser.tree.span(inner).end },
    );
}

const JSDocKind = enum { nullable, non_nullable };

fn jsdocNodeData(inner: ast.NodeIndex, comptime kind: JSDocKind, postfix: bool) ast.NodeData {
    return switch (kind) {
        .nullable => .{ .ts_jsdoc_nullable_type = .{ .type_annotation = inner, .postfix = postfix } },
        .non_nullable => .{ .ts_jsdoc_non_nullable_type = .{ .type_annotation = inner, .postfix = postfix } },
    };
}

pub fn isStartOfType(tag: TokenTag) bool {
    return switch (tag) {
        // primitives
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
        // literals
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
        // grouping starters
        .left_paren,
        .left_bracket,
        .left_brace,
        .less_than,
        .left_shift,
        // prefix ops
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

// keyof T   unique symbol   readonly T[]
// ^^^^^^^   ^^^^^^^^^^^^^   ^^^^^^^^^^^^
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
        .{ .ts_type_operator = .{
            .operator = operator,
            .type_annotation = inner,
        } },
        .{ .start = start, .end = parser.tree.span(inner).end },
    );
}

// infer T   infer T extends U
// ^^^^^^^   ^^^^^^^^^^^^^^^^^
fn parseInferType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .infer);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const name_span = parser.current_token.span;
    const name = try literals.parseBindingIdentifier(parser) orelse return null;

    const constraint = try parseInferConstraint(parser);
    const param_end = if (constraint != .null)
        parser.tree.span(constraint).end
    else
        name_span.end;

    const type_parameter = try parser.tree.addNode(
        .{ .ts_type_parameter = .{
            .name = name,
            .constraint = constraint,
        } },
        .{ .start = name_span.start, .end = param_end },
    );

    return try parser.tree.addNode(
        .{ .ts_infer_type = .{ .type_parameter = type_parameter } },
        .{ .start = start, .end = param_end },
    );
}

// `extends` bound after `infer Name`. parsed with conditionals off. may rewind when outer needs `? :`
fn parseInferConstraint(parser: *Parser) Error!ast.NodeIndex {
    if (parser.current_token.tag != .extends) return .null;

    const cp = parser.checkpoint();
    try parser.advance() orelse return .null;

    parser.ts_context.disallow_conditional_types = true;
    const constraint = try parseUnionType(parser) orelse {
        parser.rewind(cp);
        return .null;
    };
    parser.ts_context.disallow_conditional_types = cp.ts_context.disallow_conditional_types;

    // if this `extends` is really start of `?` branch, rewind and let outer parse it
    const yields_to_conditional =
        !cp.ts_context.disallow_conditional_types and
        parser.current_token.tag == .question;

    if (yields_to_conditional) {
        parser.rewind(cp);
        return .null;
    }

    return constraint;
}

fn isStartOfFunctionOrConstructorType(parser: *Parser) Error!bool {
    const tag = parser.current_token.tag;

    if (tag == .new) return true;

    if (tag == .abstract) {
        const next = parser.peekAhead() orelse return false;
        return next.tag == .new;
    }

    if (generics.isAngleOpen(tag)) return true;

    if (tag != .left_paren) return false;

    var peek = parser.beginPeek();
    defer peek.end();

    const t1 = peek.next() orelse return false;

    switch (t1.tag) {
        .right_paren => {
            const t2 = peek.next() orelse return false;
            return t2.tag == .arrow;
        },
        .spread => return true,
        .this => {
            const t2 = peek.next() orelse return false;
            return t2.tag == .colon;
        },
        // object or tuple pattern head might be value param or type tuple
        .left_brace, .left_bracket => {
            peek.end();
            return isFunctionTypeAfterPattern(parser);
        },
        else => {},
    }

    if (!t1.tag.isIdentifierLike()) return false;

    const t2 = peek.next() orelse return false;

    return switch (t2.tag) {
        .colon, .question, .comma, .assign => true,
        .right_paren => blk: {
            const t3 = peek.next() orelse break :blk false;
            break :blk t3.tag == .arrow;
        },
        else => false,
    };
}

// peek `(pattern` and use follow set to see function type not paren type
fn isFunctionTypeAfterPattern(parser: *Parser) Error!bool {
    const cp = parser.checkpoint();
    defer parser.rewind(cp);

    try parser.advance() orelse return false;

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

// (a: T) => R       new (x: T) => R       abstract new (x: T) => R
// ^^^^^^^^^^^       ^^^^^^^^^^^^^^^       ^^^^^^^^^^^^^^^^^^^^^^^^
fn parseFunctionOrConstructorType(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const is_abstract = parser.current_token.tag == .abstract;
    if (is_abstract) try parser.advance() orelse return null;

    const is_constructor = parser.current_token.tag == .new;
    if (is_constructor) try parser.advance() orelse return null;

    const type_parameters = try generics.parseTypeParameters(parser);
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
    try parser.advance() orelse return null;

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

// (A | B)   (Foo)
// ^^^^^^^   ^^^^^
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

// [A, B?, ...C]   [label: T, rest?: U]
// ^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^
fn parseTupleType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_bracket);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

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

    const element_types = try parser.addExtraFromScratch(&parser.scratch_a, checkpoint);

    return try parser.tree.addNode(
        .{ .ts_tuple_type = .{ .element_types = element_types } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

// `...T` rest, named `label[?]: T`, or a plain type.
fn parseTupleElement(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag == .spread) {
        const start = parser.current_token.span.start;
        try parser.advance() orelse return null;
        const inner = try parseTupleElementBody(parser) orelse return null;
        const end = parser.tree.span(inner).end;
        return try parser.tree.addNode(
            .{ .ts_rest_type = .{ .type_annotation = inner } },
            .{ .start = start, .end = end },
        );
    }

    return parseTupleElementBody(parser);
}

// tuple slot rewrites postfix jsdoc `?` to optional type
fn parseTupleElementBody(parser: *Parser) Error!?ast.NodeIndex {
    if (isNamedTupleElement(parser)) return parseNamedTupleMember(parser);

    const ty = try parseType(parser) orelse return null;

    switch (parser.tree.data(ty)) {
        .ts_jsdoc_nullable_type => |n| if (n.postfix) return try parser.tree.addNode(
            .{ .ts_optional_type = .{ .type_annotation = n.type_annotation } },
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

    const t1 = peek.next() orelse return false;

    if (t1.tag == .colon) return true;
    if (t1.tag != .question) return false;

    const t2 = peek.next() orelse return false;
    return t2.tag == .colon;
}

// label: Type     label?: Type
// ^^^^^^^^^^^     ^^^^^^^^^^^^
fn parseNamedTupleMember(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const label = try literals.parseIdentifierName(parser) orelse return null;

    const is_optional = parser.current_token.tag == .question;
    if (is_optional) try parser.advance() orelse return null;

    if (!try parser.expect(.colon, "Expected ':' after named tuple element label", null)) return null;

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

// Foo.Bar.Baz<T, U>
// ^^^^^^^^^^^^^^^^^
fn parseTypeReference(parser: *Parser) Error!?ast.NodeIndex {
    const type_name = try parseEntityName(parser) orelse return null;
    const name_span = parser.tree.span(type_name);
    const type_arguments = try generics.parseTypeArgumentsAfterEntityName(parser);
    const end = if (type_arguments != .null) parser.tree.span(type_arguments).end else name_span.end;

    return try parser.tree.addNode(
        .{ .ts_type_reference = .{
            .type_name = type_name,
            .type_arguments = type_arguments,
        } },
        .{ .start = name_span.start, .end = end },
    );
}

// typeof x   typeof x.y   typeof Err<T>   typeof import("m").Foo
// ^^^^^^^^   ^^^^^^^^^^   ^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^
fn parseTypeQuery(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .typeof);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const expr_name = if (parser.current_token.tag == .import)
        try parseImportType(parser) orelse return null
    else
        try parseEntityName(parser) orelse return null;

    // import type parsed its own `<>` already
    const type_arguments = if (parser.tree.data(expr_name) == .ts_import_type)
        .null
    else
        try generics.parseTypeArgumentsAfterEntityName(parser);

    const end = parser.tree.span(if (type_arguments != .null) type_arguments else expr_name).end;
    return try parser.tree.addNode(
        .{ .ts_type_query = .{
            .expr_name = expr_name,
            .type_arguments = type_arguments,
        } },
        .{ .start = start, .end = end },
    );
}

// import("module")    import("module").Foo.Bar<T>    import("m", { with: ... })
// ^^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^^^^^^^^
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
        qualifier = try parseImportTypeQualifier(parser) orelse return null;
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

// optional second `import()` arg, null when skipped or `, )` empty slot
fn parseImportTypeOptions(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .comma) return ast.NodeIndex.null;
    try parser.advance() orelse return null;

    if (parser.current_token.tag == .right_paren) return ast.NodeIndex.null;

    const options = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    if (parser.current_token.tag == .comma) {
        try parser.advance() orelse return null;
    }
    return options;
}

// dotted tail after import parens
fn parseImportTypeQualifier(parser: *Parser) Error!?ast.NodeIndex {
    const head = try parseQualifiedSegment(
        parser,
        "Expected an identifier",
        "Each '.' in a qualified name must be followed by an identifier",
    ) orelse return null;
    return extendQualifiedName(parser, head);
}

// first id or this then qualified tail
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

// one `IdentifierName` step
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
    const node = try parser.tree.addNode(
        .{ .identifier_name = .{ .name = try parser.identifierName(token) } },
        token.span,
    );
    try parser.advanceWithoutEscapeCheck() orelse return null;
    return node;
}

// nest `TSQualifiedName` for each `.foo`
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
