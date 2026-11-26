const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const TokenTag = @import("../../token.zig").TokenTag;

const literals = @import("../literals.zig");
const expressions = @import("../expressions.zig");
const functions = @import("../functions.zig");
const patterns = @import("../patterns.zig");
const object = @import("../object.zig");
const Speculation = @import("expressions.zig").Speculation;

pub fn parseType(parser: *Parser) Error!?ast.NodeIndex {
    const saved = parser.ts_context.disallow_conditional_types;
    parser.ts_context.disallow_conditional_types = false;
    defer parser.ts_context.disallow_conditional_types = saved;

    if (try startsFunctionOrConstructorType(parser)) return parseFunctionOrConstructorType(parser);
    return try parseConditionalType(parser) orelse
        return parser.failExpected("Expected a type", .{});
}

inline fn parseTypeInline(parser: *Parser) Error!?ast.NodeIndex {
    return @call(.always_inline, parseType, .{parser});
}

fn parseTypeNoConditional(parser: *Parser) Error!?ast.NodeIndex {
    if (try startsFunctionOrConstructorType(parser)) return parseFunctionOrConstructorType(parser);
    return try parseUnionType(parser) orelse return parser.failExpected("Expected a type", .{});
}

inline fn startsFunctionOrConstructorType(parser: *Parser) Error!bool {
    const tag = parser.current_token.tag;
    return tag == .new or isAngleOpen(tag) or
        ((tag == .abstract or tag == .left_paren) and try isFunctionTypeAhead(parser));
}

inline fn parseConditionalType(parser: *Parser) Error!?ast.NodeIndex {
    const check_type = try parseUnionType(parser) orelse return null;
    if (!parser.at(.extends) or parser.current_token.hasLineTerminatorBefore()) return check_type;

    try parser.descend();
    defer parser.ascend();
    try parser.advance() orelse return null;

    // parentheses reset the flag through parseType
    const saved = parser.ts_context.disallow_conditional_types;
    parser.ts_context.disallow_conditional_types = true;
    defer parser.ts_context.disallow_conditional_types = saved;
    const extends_type = try parseTypeNoConditional(parser) orelse return null;

    const help = "Write a conditional type as 'T extends U ? X : Y'";
    if (!try parser.expect(.question, "Expected '?' in the conditional type", help)) return null;
    const true_type = try parseType(parser) orelse return null;
    if (!try parser.expect(.colon, "Expected ':' in the conditional type", help)) return null;
    const false_type = try parseType(parser) orelse return null;

    return try parser.finishAt(parser.tree.span(check_type).start, false_type, .{
        .ts_conditional_type = .{
            .check_type = check_type,
            .extends_type = extends_type,
            .true_type = true_type,
            .false_type = false_type,
        },
    });
}

inline fn parseUnionType(parser: *Parser) Error!?ast.NodeIndex {
    return parseBinaryTypeChain(parser, .bitwise_or, parseIntersectionType);
}

inline fn parseIntersectionType(parser: *Parser) Error!?ast.NodeIndex {
    return parseBinaryTypeChain(parser, .bitwise_and, parsePostfixType);
}

inline fn parseBinaryTypeChain(
    parser: *Parser,
    comptime separator: TokenTag,
    comptime parseOperand: anytype,
) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const has_leading = try parser.eat(separator) orelse return null;

    const first = try parseOperand(parser) orelse return null;
    if (!has_leading) std.debug.assert(parser.tree.span(first).start == start);
    if (!has_leading and !parser.at(separator)) return first;

    const operands = parser.list();
    defer operands.deinit();
    try operands.append(first);
    var last = first;
    while (try parser.eat(separator) orelse return null) {
        last = try parseOperand(parser) orelse return null;
        try operands.append(last);
    }

    const types = try operands.finish();
    return try parser.finishAt(start, last, if (separator == .bitwise_or)
        .{ .ts_union_type = .{ .types = types } }
    else
        .{ .ts_intersection_type = .{ .types = types } });
}

inline fn parsePostfixType(parser: *Parser) Error!?ast.NodeIndex {
    var ty = try parsePrimaryType(parser) orelse return null;

    while (!parser.current_token.hasLineTerminatorBefore()) {
        ty = try switch (parser.current_token.tag) {
            .left_bracket => parseArrayOrIndexedAccessType(parser, ty),
            // a postfix `?` only when no type follows, else the outer conditional owns it
            .question => if (isTypeNext(parser))
                return ty
            else
                parseJSDocPostfix(parser, ty, .ts_jsdoc_nullable_type),
            .logical_not => parseJSDocPostfix(parser, ty, .ts_jsdoc_non_nullable_type),
            else => return ty,
        } orelse return null;
    }

    return ty;
}

noinline fn isTypeNext(parser: *Parser) bool {
    return isStartOfType(parser.peekAhead().tag);
}

fn parseArrayOrIndexedAccessType(parser: *Parser, element: ast.NodeIndex) Error!?ast.NodeIndex {
    try parser.descend();
    defer parser.ascend();
    const start = parser.tree.span(element).start;
    _ = try parser.take(.left_bracket) orelse return null;

    if (try parser.eat(.right_bracket) orelse return null) {
        return try parser.finish(start, .{ .ts_array_type = .{ .element_type = element } });
    }

    const index = try parseType(parser) orelse return null;
    if (!try parser.expect(
        .right_bracket,
        "Expected ']' to close the indexed access type",
        "Check that every '[' in the type has a matching ']'",
    )) return null;
    return try parser.finish(start, .{
        .ts_indexed_access_type = .{ .object_type = element, .index_type = index },
    });
}

fn parseJSDocPostfix(
    parser: *Parser,
    inner: ast.NodeIndex,
    comptime tag: std.meta.Tag(ast.NodeData),
) Error!?ast.NodeIndex {
    try parser.advance() orelse return null;
    return try parser.finish(parser.tree.span(inner).start, @unionInit(
        ast.NodeData,
        @tagName(tag),
        .{ .type_annotation = inner, .postfix = true },
    ));
}

inline fn parsePrimaryType(parser: *Parser) Error!?ast.NodeIndex {
    try parser.descend();
    defer parser.ascend();
    const token = parser.current_token;
    if (token.tag == .identifier) return @call(.always_inline, parseTypeReference, .{parser});

    keyword: {
        const keyword: ast.NodeData = switch (token.tag) {
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
            else => break :keyword,
        };
        if (token.isEscaped() or (parser.nextMayStartWith('.') and isDotNext(parser))) {
            break :keyword;
        }
        try parser.advance() orelse return null;
        return try parser.tree.addNode(keyword, token.span);
    }
    return parsePrimaryTypeSlow(parser);
}

fn isDotNext(parser: *Parser) bool {
    const next = parser.peekAhead();
    return next.tag == .dot and !next.hasLineTerminatorBefore();
}

fn parsePrimaryTypeSlow(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    literal: {
        const literal = try switch (token.tag) {
            .true, .false => literals.parseBooleanLiteral(parser),
            .string_literal => literals.parseStringLiteral(parser),
            .no_substitution_template => literals.parseNoSubstitutionTemplate(parser, false),
            .minus, .plus => parseSignedNumericLiteral(parser),
            .template_head => return parseTemplateLiteralType(parser),
            else => if (token.tag.isNumericLiteral())
                literals.parseNumericLiteral(parser)
            else
                break :literal,
        } orelse return null;
        return try parser.finishAt(token.span.start, literal, .{
            .ts_literal_type = .{ .literal = literal },
        });
    }

    if (!token.isEscaped()) switch (token.tag) {
        .this => return parseThisTypeOrPredicate(parser),
        .asserts => {
            const next = parser.peekAhead();
            if (next.tag.isIdentifierLike() and !next.hasLineTerminatorBefore()) {
                return parseAssertsTypePredicate(parser);
            }
        },
        .left_paren => return parseParenthesizedType(parser),
        .left_bracket => return parseTupleType(parser),
        .left_brace => return if (isStartOfMappedType(parser))
            parseMappedType(parser)
        else
            parseTypeLiteral(parser),
        .keyof, .unique, .readonly => return parseTypeOperator(parser),
        .infer => return parseInferType(parser),
        .typeof => return parseTypeQuery(parser),
        .import => return parseImportType(parser),
        .question => return parseJSDocNullableOrUnknownType(parser),
        .logical_not => return parseJSDocNonNullableType(parser),
        else => {},
    };

    if (token.tag.isIdentifierLike()) return parseTypeReference(parser);
    return null;
}

fn parseSignedNumericLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const sign = parser.current_token;
    if (!parser.peekAhead().tag.isNumericLiteral()) return null;
    try parser.advance() orelse return null;

    const argument = try literals.parseNumericLiteral(parser) orelse return null;
    return try parser.finishAt(sign.span.start, argument, .{
        .unary_expression = .{ .argument = argument, .operator = .fromToken(sign.tag) },
    });
}

fn parseTemplateLiteralType(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const parts = try literals.parseTemplateParts(parser, false, parseType) orelse return null;
    return try parser.finish(start, .{
        .ts_template_literal_type = .{ .quasis = parts.quasis, .types = parts.substitutions },
    });
}

// only a bare `type Name = intrinsic` is `TSIntrinsicKeyword`
pub fn parseTypeAliasBody(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.at(.intrinsic) and !parser.current_token.isEscaped()) {
        if (!continuesType(parser.peekAhead().tag)) {
            return parser.leaf(.{ .ts_intrinsic_keyword = .{} });
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
    const question = parser.current_token.span;
    _ = try parser.take(.question) orelse return null;

    if (!isStartOfType(parser.current_token.tag)) {
        return try parser.tree.addNode(.{ .ts_jsdoc_unknown_type = .{} }, question);
    }
    const inner = try parseType(parser) orelse return null;
    return try parser.finishAt(question.start, inner, .{
        .ts_jsdoc_nullable_type = .{ .type_annotation = inner, .postfix = false },
    });
}

// prefix `!` binds tighter than postfix, so `!T[]` is `(!T)[]`
fn parseJSDocNonNullableType(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.logical_not) orelse return null;
    const inner = try parsePrimaryType(parser) orelse return null;
    return try parser.finishAt(start, inner, .{
        .ts_jsdoc_non_nullable_type = .{ .type_annotation = inner, .postfix = false },
    });
}

pub fn isStartOfType(tag: TokenTag) bool {
    return switch (tag) {
        .string_literal,
        .no_substitution_template,
        .template_head,
        .minus,
        .plus,
        .left_paren,
        .left_bracket,
        .left_brace,
        .less_than,
        .left_shift,
        .question,
        .logical_not,
        .spread,
        .bitwise_and,
        .bitwise_or,
        => true,
        else => tag.isIdentifierLike() or tag.isNumericLiteral(),
    };
}

fn parseTypeOperator(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const operator: ast.TSTypeOperatorKind = switch (parser.current_token.tag) {
        inline .keyof, .unique, .readonly => |tag| @field(ast.TSTypeOperatorKind, @tagName(tag)),
        else => unreachable,
    };
    try parser.advance() orelse return null;

    const inner = try parsePostfixType(parser) orelse return null;
    return try parser.finishAt(start, inner, .{
        .ts_type_operator = .{ .operator = operator, .type_annotation = inner },
    });
}

fn parseInferType(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.infer) orelse return null;
    const name = try literals.parseBindingIdentifier(parser) orelse return null;
    const constraint = try parseInferConstraint(parser);

    const type_parameter = try parser.finishAt(
        parser.tree.span(name).start,
        if (constraint != .null) constraint else name,
        .{ .ts_type_parameter = .{ .name = name, .constraint = constraint } },
    );
    return try parser.finishAt(start, type_parameter, .{
        .ts_infer_type = .{ .type_parameter = type_parameter },
    });
}

// rewinds when the `extends` belongs to an enclosing conditional
fn parseInferConstraint(parser: *Parser) Error!ast.NodeIndex {
    if (!parser.at(.extends)) return .null;

    const speculation: Speculation = .at(parser, .infer_constraint, .{});
    const conditional_allowed = !parser.ts_context.disallow_conditional_types;
    if (conditional_allowed and speculation.isRejected(parser)) return .null;

    const cp = parser.checkpoint();
    try parser.advance() orelse return .null;

    parser.ts_context.disallow_conditional_types = true;
    const constraint = try parseTypeNoConditional(parser) orelse {
        parser.rewind(cp);
        return .null;
    };
    parser.ts_context.disallow_conditional_types = cp.ts_context.disallow_conditional_types;

    if (conditional_allowed and parser.at(.question)) {
        parser.rewind(cp);
        try speculation.reject(parser);
        return .null;
    }
    return constraint;
}

fn isFunctionTypeAhead(parser: *Parser) Error!bool {
    var peek = parser.beginPeek();
    defer peek.end();

    const first = peek.next();
    if (parser.at(.abstract)) return first.tag == .new;
    switch (first.tag) {
        .right_paren => return peek.next().tag == .arrow,
        .spread => return true,
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
        .right_paren => (first.tag == .this or !first.tag.isUnconditionallyReserved()) and
            peek.next().tag == .arrow,
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
            return parser.at(.arrow);
        },
        else => return false,
    }
}

fn parseFunctionOrConstructorType(parser: *Parser) Error!?ast.NodeIndex {
    try parser.descend();
    defer parser.ascend();
    const start = parser.current_token.span.start;
    const is_abstract = try parser.eat(.abstract) orelse return null;
    const is_constructor = try parser.eat(.new) orelse return null;

    const type_parameters = try parseTypeParameters(parser);
    const params = try functions.parseFormalParameters(parser, .signature, false) orelse
        return null;

    const arrow_start = parser.current_token.span.start;
    if (!try parser.expect(
        .arrow,
        "Expected '=>' in the function type",
        "Write a function type as '(params) => ReturnType'",
    )) return null;
    const returned = try parseTypeOrTypePredicate(parser) orelse return null;
    const return_type = try parser.finishAt(arrow_start, returned, .{
        .ts_type_annotation = .{ .type_annotation = returned },
    });

    if (is_constructor) return try parser.finishAt(start, return_type, .{ .ts_constructor_type = .{
        .type_parameters = type_parameters,
        .params = params,
        .return_type = return_type,
        .abstract = is_abstract,
    } });
    return try parser.finishAt(start, return_type, .{ .ts_function_type = .{
        .type_parameters = type_parameters,
        .params = params,
        .return_type = return_type,
    } });
}

fn parseParenthesizedType(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.left_paren) orelse return null;
    const inner = try parseType(parser) orelse return null;
    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close the parenthesized type",
        "Check that every '(' in the type has a matching ')'",
    )) return null;
    return try parser.finish(start, .{ .ts_parenthesized_type = .{ .type_annotation = inner } });
}

fn parseTupleType(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.left_bracket) orelse return null;
    const elements = parser.list();
    defer elements.deinit();
    try elements.parseSeparated(.right_bracket, parseTupleElement, .{}) orelse return null;
    if (!try parser.expect(
        .right_bracket,
        "Expected ']' to close the tuple type",
        "Check that every '[' in the tuple has a matching ']'",
    )) return null;

    const element_types = try elements.finish();
    try checkTupleElementOrder(parser, element_types);
    return try parser.finish(start, .{ .ts_tuple_type = .{ .element_types = element_types } });
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
            .{ .help = "Mark the element optional, or move it before the optional elements" },
        );
    }
}

fn parseTupleElement(parser: *Parser) Error!?ast.NodeIndex {
    if (!parser.at(.spread)) return parseTupleElementBody(parser);

    const start = try parser.take(.spread) orelse return null;
    const inner = try parseTupleElementBody(parser) orelse return null;
    return try parser.finishAt(start, inner, .{ .ts_rest_type = .{ .type_annotation = inner } });
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
    const next = peek.next().tag;
    return next == .colon or (next == .question and peek.next().tag == .colon);
}

fn parseNamedTupleMember(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const label = try literals.parseIdentifierName(parser) orelse return null;
    const optional = try parser.eat(.question) orelse return null;
    if (!try parser.expect(.colon, "Expected ':' after the tuple element name", null)) {
        return null;
    }

    const element_type = try parseType(parser) orelse return null;
    return try parser.finishAt(start, element_type, .{ .ts_named_tuple_member = .{
        .label = label,
        .element_type = element_type,
        .optional = optional,
    } });
}

fn parseTypeReference(parser: *Parser) Error!?ast.NodeIndex {
    const type_name = try @call(.always_inline, parseEntityName, .{parser}) orelse return null;
    const type_arguments = try parseTypeArgumentsAfterEntityName(parser);
    return try parser.finishAt(
        parser.tree.span(type_name).start,
        if (type_arguments != .null) type_arguments else type_name,
        .{ .ts_type_reference = .{ .type_name = type_name, .type_arguments = type_arguments } },
    );
}

fn parseTypeQuery(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.typeof) orelse return null;
    const expr_name = try (if (parser.at(.import))
        parseImportType(parser)
    else
        parseEntityName(parser)) orelse return null;

    // an import type parses its own `<>`
    const type_arguments = if (parser.tree.data(expr_name) == .ts_import_type)
        .null
    else
        try parseTypeArgumentsAfterEntityName(parser);

    return try parser.finishAt(start, if (type_arguments != .null) type_arguments else expr_name, .{
        .ts_type_query = .{ .expr_name = expr_name, .type_arguments = type_arguments },
    });
}

fn parseImportType(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.import) orelse return null;
    if (!try parser.expect(
        .left_paren,
        "Expected '(' after 'import' in the import type",
        "Write an import type as 'import(\"m\").Name'",
    )) return null;

    const source = try literals.parseStringLiteral(parser) orelse return null;
    var options: ast.NodeIndex = .null;
    if ((try parser.eat(.comma) orelse return null) and !parser.at(.right_paren)) {
        options = try expressions.parseAssignment(parser) orelse return null;
        _ = try parser.eat(.comma) orelse return null;
    }
    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close the import type",
        "Check that every '(' in the import type has a matching ')'",
    )) return null;

    var qualifier: ast.NodeIndex = .null;
    if (try parser.eat(.dot) orelse return null) {
        const head = try parseQualifiedSegment(parser) orelse return null;
        qualifier = try extendQualifiedName(parser, head) orelse return null;
    }
    const type_arguments = try parseTypeArguments(parser);

    const end = if (type_arguments != .null)
        parser.tree.span(type_arguments).end
    else if (qualifier != .null)
        parser.tree.span(qualifier).end
    else
        parser.prev_token_end;
    return try parser.tree.addNode(.{ .ts_import_type = .{
        .source = source,
        .options = options,
        .qualifier = qualifier,
        .type_arguments = type_arguments,
    } }, .{ .start = start, .end = end });
}

fn parseEntityName(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    if (!token.tag.isIdentifierLike()) return parser.failExpected(
        "Expected an identifier",
        .{ .help = "Start the type name with an identifier" },
    );

    const head = try parser.tree.addNode(if (token.tag == .this)
        .{ .this_expression = .{} }
    else
        .{ .identifier_reference = .{ .name = try parser.identifierName(token) } }, token.span);
    try parser.advanceWithoutEscapeCheck() orelse return null;

    if (!parser.at(.dot)) return head;
    return extendQualifiedName(parser, head);
}

fn parseQualifiedSegment(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    if (!token.tag.isIdentifierLike()) return parser.failExpected(
        "Expected an identifier after '.'",
        .{ .help = "Follow each '.' in a qualified name with an identifier" },
    );
    const node = try literals.addIdentifierName(parser, token);
    try parser.advanceWithoutEscapeCheck() orelse return null;
    return node;
}

pub fn extendQualifiedName(parser: *Parser, head: ast.NodeIndex) Error!?ast.NodeIndex {
    var name = head;
    while (try parser.eat(.dot) orelse return null) {
        const right = try parseQualifiedSegment(parser) orelse return null;
        name = try parser.finishAt(parser.tree.span(name).start, right, .{
            .ts_qualified_name = .{ .left = name, .right = right },
        });
    }
    return name;
}

pub fn parseTypeArguments(parser: *Parser) Error!ast.NodeIndex {
    if (!isAngleOpen(parser.current_token.tag)) return .null;
    const speculation: Speculation = .at(parser, .type_arguments, .{});
    const arguments = try parseAngleList(parser, .argument, .peel);
    if (arguments == .null) try speculation.reject(parser);
    return arguments;
}

// a fused `>=` or `>>` fails so `f<T>= x` stays relational like tsc
pub inline fn parseTypeArgumentsInExpression(parser: *Parser) Error!ast.NodeIndex {
    return parseAngleList(parser, .argument, .exact);
}

// a newline before `<` keeps `typeof a` apart from a generic function type after it
inline fn parseTypeArgumentsAfterEntityName(parser: *Parser) Error!ast.NodeIndex {
    if (!isAngleOpen(parser.current_token.tag)) return .null;
    if (parser.current_token.hasLineTerminatorBefore()) return .null;
    return parseTypeArguments(parser);
}

pub inline fn parseTypeParameters(parser: *Parser) Error!ast.NodeIndex {
    if (!isAngleOpen(parser.current_token.tag)) return .null;
    return parseAngleList(parser, .parameter, .peel);
}

fn parseAngleList(
    parser: *Parser,
    comptime kind: enum { argument, parameter },
    comptime fused: enum { peel, exact },
) Error!ast.NodeIndex {
    const open = parser.current_token;
    switch (open.tag) {
        .less_than => try parser.advance() orelse return .null,
        // fused `<<` opens nested instantiations like `Foo<<T>(x: T) => R>`
        .left_shift => try parser.advanceWithRescannedToken(
            parser.lexer.reScanLessThan(open.span.start),
        ) orelse return .null,
        else => unreachable,
    }

    const list = parser.list();
    defer list.deinit();
    while (!isAngleClose(parser.current_token.tag) and !parser.at(.eof)) {
        const element = try switch (kind) {
            .argument => parseType(parser),
            .parameter => parseTypeParameter(parser),
        } orelse return .null;
        try list.append(element);
        if (!(try parser.eat(.comma) orelse return .null)) break;
    }
    if (list.items().len == 0) try parser.report(
        parser.current_token.span,
        "Type " ++ @tagName(kind) ++ " list cannot be empty",
        .{ .help = "Remove the empty '<>'" },
    );

    const close = parser.current_token;
    if (close.tag == .greater_than) {
        try parser.advance() orelse return .null;
    } else if (!isAngleClose(close.tag)) {
        try parser.reportExpected(
            "Expected '>' to close the type " ++ @tagName(kind) ++ " list",
            .{ .help = "Check that every '<' in the type has a matching '>'" },
        );
        return .null;
    } else if (fused == .exact) {
        return .null;
    } else {
        try parser.advanceWithRescannedToken(
            parser.lexer.reScanGreaterThan(close.span.start),
        ) orelse return .null;
    }

    const params = try list.finish();
    return try parser.finish(open.span.start, switch (kind) {
        .argument => .{ .ts_type_parameter_instantiation = .{ .params = params } },
        .parameter => .{ .ts_type_parameter_declaration = .{ .params = params } },
    });
}

pub inline fn isAngleOpen(tag: TokenTag) bool {
    return tag == .less_than or tag == .left_shift;
}

inline fn isAngleClose(tag: TokenTag) bool {
    return switch (tag) {
        .greater_than,
        .right_shift,
        .unsigned_right_shift,
        .greater_than_equal,
        .right_shift_assign,
        .unsigned_right_shift_assign,
        => true,
        else => false,
    };
}

fn parseTypeParameter(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const modifiers = [_]TokenTag{ .@"const", .in, .out };
    var seen: [modifiers.len]bool = @splat(false);
    var last_seen: usize = 0;

    // a modifier only when a name follows, else `<out>` is a parameter named out
    while (std.mem.findScalar(TokenTag, &modifiers, parser.current_token.tag)) |index| {
        if (!parser.peekAhead().tag.isIdentifierLike()) break;

        const keyword = parser.current_token.tag.toString().?;
        if (seen[index]) {
            try parser.report(
                parser.current_token.span,
                try parser.fmt("'{s}' modifier already seen", .{keyword}),
                .{ .help = "Remove the repeated modifier" },
            );
        } else if (index < last_seen) {
            try parser.report(parser.current_token.span, try parser.fmt(
                "'{s}' modifier must precede '{s}' modifier",
                .{ keyword, modifiers[last_seen].toString().? },
            ), .{});
        }
        seen[index] = true;
        last_seen = @max(last_seen, index);
        try parser.advance() orelse return null;
    }

    const name = try literals.parseBindingIdentifier(parser) orelse return null;
    const constraint: ast.NodeIndex = if (try parser.eat(.extends) orelse return null)
        try parseType(parser) orelse return null
    else
        .null;
    const default: ast.NodeIndex = if (try parser.eat(.assign) orelse return null)
        try parseType(parser) orelse return null
    else
        .null;

    const last = if (default != .null) default else if (constraint != .null) constraint else name;
    return try parser.finishAt(start, last, .{ .ts_type_parameter = .{
        .name = name,
        .constraint = constraint,
        .default = default,
        .@"const" = seen[0],
        .in = seen[1],
        .out = seen[2],
    } });
}

fn parseTypeLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const members = try parseObjectTypeMembers(parser) orelse return null;
    return try parser.finish(start, .{ .ts_type_literal = .{ .members = members } });
}

pub fn parseObjectTypeMembers(parser: *Parser) Error!?ast.IndexRange {
    _ = try parser.take(.left_brace) orelse return null;

    const members = parser.list();
    defer members.deinit();
    while (!parser.at(.right_brace) and !parser.at(.eof)) {
        const member = try @call(.always_inline, parseTypeMember, .{parser}) orelse return null;
        try members.append(member);

        switch (parser.current_token.tag) {
            // the separator joins the member span, matching TS-ESTree
            .semicolon, .comma => {
                const span = parser.tree.span(member);
                parser.tree.setSpan(member, .{
                    .start = span.start,
                    .end = parser.current_token.span.end,
                });
                try parser.advance() orelse return null;
            },
            .right_brace => {},
            else => if (!parser.current_token.hasLineTerminatorBefore()) {
                try parser.reportExpected(
                    "Expected ';', ',', or a line break between type members",
                    .{ .help = "Separate the members with ';', or close the type with '}'" },
                );
                return null;
            },
        }
    }

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close the object type",
        "Check that every '{' in the type has a matching '}'",
    )) return null;
    return try members.finish();
}

fn isStartOfMappedType(parser: *Parser) bool {
    var peek = parser.beginPeek();
    defer peek.end();

    var token = peek.next();
    if (token.tag == .plus or token.tag == .minus) {
        if (peek.next().tag != .readonly) return false;
        token = peek.next();
    } else if (token.tag == .readonly) {
        token = peek.next();
    }
    if (token.tag != .left_bracket) return false;
    return peek.next().tag.isIdentifierLike() and peek.next().tag == .in;
}

fn parseMappedType(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.left_brace) orelse return null;
    const readonly = try parseMappedModifier(parser, .readonly) orelse return null;

    _ = try parser.take(.left_bracket) orelse return null;
    const key = try literals.parseBindingIdentifier(parser) orelse return null;
    _ = try parser.take(.in) orelse return null;
    const constraint = try parseType(parser) orelse return null;
    const name_type: ast.NodeIndex = if (try parser.eat(.as) orelse return null)
        try parseType(parser) orelse return null
    else
        .null;
    if (!try parser.expect(
        .right_bracket,
        "Expected ']' to close the mapped type parameter",
        "Check that every '[' in the mapped type has a matching ']'",
    )) return null;

    const optional = try parseMappedModifier(parser, .question) orelse return null;
    const type_annotation: ast.NodeIndex = if (try parser.eat(.colon) orelse return null)
        try parseType(parser) orelse return null
    else
        .null;

    if (parser.at(.semicolon) or parser.at(.comma)) try parser.advance() orelse return null;
    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close the mapped type",
        "Check that every '{' in the mapped type has a matching '}'",
    )) return null;
    return try parser.finish(start, .{ .ts_mapped_type = .{
        .key = key,
        .constraint = constraint,
        .name_type = name_type,
        .type_annotation = type_annotation,
        .optional = optional,
        .readonly = readonly,
    } });
}

fn parseMappedModifier(
    parser: *Parser,
    comptime modified: TokenTag,
) Error!?ast.TSMappedTypeModifier {
    if (try parser.eat(modified) orelse return null) return .true;

    const sign: ast.TSMappedTypeModifier = switch (parser.current_token.tag) {
        .plus => .plus,
        .minus => .minus,
        else => return .none,
    };
    try parser.advance() orelse return null;
    if (!try parser.expect(
        modified,
        "Expected '" ++ (comptime modified.toString().?) ++ "' after '+' or '-' in the mapped type",
        "Mapped type '+' and '-' modifiers can only apply to 'readonly' or '?'",
    )) return null;
    return sign;
}

fn parseTypeMember(parser: *Parser) Error!?ast.NodeIndex {
    const tag = parser.current_token.tag;
    const start = parser.current_token.span.start;

    if (tag == .left_paren or tag == .less_than) {
        return parseCallOrConstructSignature(parser, false);
    }
    if (tag == .new) {
        const next = parser.peekAhead();
        if (next.tag == .left_paren or next.tag == .less_than) {
            return parseCallOrConstructSignature(parser, true);
        }
    }

    // `readonly` alone is a property name
    var is_readonly = false;
    if (tag == .readonly) {
        const next = parser.peekAhead();
        if (!next.hasLineTerminatorBefore() and canFollowReadonlyModifier(next.tag)) {
            try parser.advance() orelse return null;
            is_readonly = true;
        }
    }
    if (parser.at(.left_bracket) and isIndexSignatureStart(parser)) {
        return parseIndexSignature(parser, start, .{ .readonly = is_readonly });
    }
    return parsePropertyOrMethodSignature(parser, start, is_readonly);
}

pub fn isIndexSignatureStart(parser: *Parser) bool {
    std.debug.assert(parser.at(.left_bracket));

    var peek = parser.beginPeek();
    defer peek.end();
    if (!peek.next().tag.isIdentifierLike()) return false;
    const after_name = peek.next().tag;
    return after_name == .colon or after_name == .comma;
}

inline fn canFollowAccessorKeyword(tag: TokenTag) bool {
    return tag == .left_bracket or
        tag.isIdentifierLike() or
        tag == .string_literal or
        tag.isNumericLiteral();
}

// bad combinations are reported later
inline fn canFollowReadonlyModifier(tag: TokenTag) bool {
    return canFollowAccessorKeyword(tag) or
        tag == .left_brace or
        tag == .star or
        tag == .spread;
}

fn parseCallOrConstructSignature(
    parser: *Parser,
    comptime is_construct: bool,
) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (is_construct) _ = try parser.take(.new) orelse return null;

    const type_parameters = try parseTypeParameters(parser);
    const params = try functions.parseFormalParameters(parser, .signature, false) orelse
        return null;
    const return_type: ast.NodeIndex = if (parser.at(.colon))
        try parseReturnTypeAnnotation(parser) orelse return null
    else
        .null;

    const tag = if (is_construct)
        "ts_construct_signature_declaration"
    else
        "ts_call_signature_declaration";
    return try parser.finishAt(
        start,
        if (return_type != .null) return_type else params,
        @unionInit(ast.NodeData, tag, .{
            .type_parameters = type_parameters,
            .params = params,
            .return_type = return_type,
        }),
    );
}

pub const IndexSignatureModifiers = struct {
    readonly: bool = false,
    static: bool = false,
};

pub fn parseIndexSignature(
    parser: *Parser,
    start: u32,
    modifiers: IndexSignatureModifiers,
) Error!?ast.NodeIndex {
    _ = try parser.take(.left_bracket) orelse return null;

    const name = try literals.parseBindingIdentifier(parser) orelse return null;
    if (!parser.at(.colon)) return parser.fail(
        parser.current_token.span,
        "An index signature parameter must have a type annotation",
        .{ .help = "Add a type after the name, such as '[key: string]'" },
    );
    const param = try parsePatternTypeAnnotation(parser, name) orelse return null;
    if (parser.at(.comma)) return parser.fail(
        parser.current_token.span,
        "An index signature must have exactly one parameter",
        .{ .help = "Write an index signature as '[key: KeyType]: ValueType'" },
    );
    const parameters = try parser.tree.addExtra(&.{param});

    if (!try parser.expect(
        .right_bracket,
        "Expected ']' to close the index signature parameter",
        "Write an index signature as '[key: KeyType]: ValueType'",
    )) return null;
    if (!parser.at(.colon)) return parser.fail(
        parser.current_token.span,
        "An index signature must have a type annotation",
        .{ .help = "Add a value type, such as '[key: string]: number'" },
    );

    const type_annotation = try parseTypeAnnotation(parser) orelse return null;
    return try parser.finishAt(start, type_annotation, .{ .ts_index_signature = .{
        .parameters = parameters,
        .type_annotation = type_annotation,
        .readonly = modifiers.readonly,
        .static = modifiers.static,
    } });
}

fn parsePropertyOrMethodSignature(
    parser: *Parser,
    start: u32,
    is_readonly: bool,
) Error!?ast.NodeIndex {
    var kind: ast.TSMethodSignatureKind = .method;
    if (parser.at(.get) or parser.at(.set)) {
        if (canFollowAccessorKeyword(parser.peekAhead().tag)) {
            kind = if (parser.at(.get)) .get else .set;
            try parser.advance() orelse return null;
        }
    }

    const key, const computed = try object.parsePropertyKey(parser, .signature) orelse
        return null;
    const is_optional = try parser.eat(.question) orelse return null;

    // accessors take the method path so a missing `(` is an error, not a property
    if (kind != .method or parser.at(.left_paren) or parser.at(.less_than)) {
        if (is_readonly) try parser.report(
            .{ .start = start, .end = parser.tree.span(key).end },
            "'readonly' modifier can only appear on a property declaration or index signature",
            .{ .help = "Remove 'readonly'" },
        );

        const type_parameters = try parseTypeParameters(parser);
        const params = try functions.parseFormalParameters(parser, .signature, false) orelse
            return null;
        const return_type: ast.NodeIndex = if (parser.at(.colon))
            try parseReturnTypeAnnotation(parser) orelse return null
        else
            .null;
        if (kind == .set and return_type != .null) try parser.report(
            parser.tree.span(return_type),
            "A 'set' accessor cannot have a return type annotation",
            .{ .help = "Remove the return type annotation" },
        );

        return try parser.finishAt(start, if (return_type != .null) return_type else params, .{
            .ts_method_signature = .{
                .key = key,
                .type_parameters = type_parameters,
                .params = params,
                .return_type = return_type,
                .kind = kind,
                .computed = computed,
                .optional = is_optional,
            },
        });
    }

    var end = parser.prev_token_end;
    var type_annotation: ast.NodeIndex = .null;
    if (parser.at(.colon)) {
        type_annotation = try parseTypeAnnotation(parser) orelse return null;
        end = parser.tree.span(type_annotation).end;
    }
    return try parser.tree.addNode(.{ .ts_property_signature = .{
        .key = key,
        .type_annotation = type_annotation,
        .computed = computed,
        .optional = is_optional,
        .readonly = is_readonly,
    } }, .{ .start = start, .end = end });
}

pub fn parseTypeAnnotation(parser: *Parser) Error!?ast.NodeIndex {
    return parseAnnotation(parser, parseTypeInline);
}

pub fn parseReturnTypeAnnotation(parser: *Parser) Error!?ast.NodeIndex {
    return parseAnnotation(parser, parseTypeOrTypePredicate);
}

inline fn parseAnnotation(
    parser: *Parser,
    comptime parseInner: anytype,
) Error!?ast.NodeIndex {
    const start = try parser.take(.colon) orelse return null;
    const inner = try parseInner(parser) orelse return null;
    return try parser.finishAt(start, inner, .{
        .ts_type_annotation = .{ .type_annotation = inner },
    });
}

// only bare `id is T` needs lookahead, `this is T` and `asserts` go through parseType
fn parseTypeOrTypePredicate(parser: *Parser) Error!?ast.NodeIndex {
    if (!isIdentifierPredicateStart(parser)) return parseTypeInline(parser);

    const parameter_name = try literals.parseIdentifierName(parser) orelse return null;
    return finishTypePredicate(parser, parameter_name, null);
}

// `this is T` is handled by the primary type path
fn isIdentifierPredicateStart(parser: *Parser) bool {
    const current = parser.current_token;
    if (current.isEscaped() or current.tag == .this or !current.tag.isIdentifierLike()) {
        return false;
    }

    if (!parser.nextMayStartWith('i')) return false;
    const next = parser.peekAhead();
    return next.tag == .is and !next.isEscaped() and !next.hasLineTerminatorBefore();
}

fn parseThisTypeOrPredicate(parser: *Parser) Error!?ast.NodeIndex {
    const this_type = try parser.leaf(.{ .ts_this_type = .{} }) orelse return null;

    const next = parser.current_token;
    if (next.tag != .is or next.isEscaped() or next.hasLineTerminatorBefore()) return this_type;
    return finishTypePredicate(parser, this_type, null);
}

fn parseAssertsTypePredicate(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.asserts) orelse return null;
    const parameter_name = try (if (parser.at(.this))
        parser.leaf(.{ .ts_this_type = .{} })
    else
        literals.parseIdentifierName(parser)) orelse return null;
    return finishTypePredicate(parser, parameter_name, start);
}

fn finishTypePredicate(
    parser: *Parser,
    parameter_name: ast.NodeIndex,
    asserts_start: ?u32,
) Error!?ast.NodeIndex {
    var last = parameter_name;
    var type_annotation: ast.NodeIndex = .null;
    if (parser.at(.is) and !parser.current_token.isEscaped()) {
        try parser.advance() orelse return null;
        last = try parseType(parser) orelse return null;
        type_annotation = try parser.tree.addNode(
            .{ .ts_type_annotation = .{ .type_annotation = last } },
            parser.tree.span(unparenthesized(parser, last)),
        );
    }

    const start = asserts_start orelse parser.tree.span(parameter_name).start;
    return try parser.finishAt(start, last, .{ .ts_type_predicate = .{
        .parameter_name = parameter_name,
        .type_annotation = type_annotation,
        .asserts = asserts_start != null,
    } });
}

// spans its type as the paren strip leaves it
fn unparenthesized(parser: *Parser, node: ast.NodeIndex) ast.NodeIndex {
    if (parser.options.preserve_parens) return node;
    var current = node;
    while (true) switch (parser.tree.data(current)) {
        .ts_parenthesized_type => |paren| current = paren.type_annotation,
        else => return current,
    };
}

pub fn parsePatternTypeAnnotation(parser: *Parser, pattern: ast.NodeIndex) Error!?ast.NodeIndex {
    std.debug.assert(@backingInt(pattern) + 1 == parser.tree.nodes.len);

    var data = parser.tree.data(pattern);
    var span = parser.tree.span(pattern);
    parser.tree.nodes.shrinkRetainingCapacity(@backingInt(pattern));

    const annotation = try parseTypeAnnotation(parser) orelse return null;
    switch (data) {
        inline .binding_identifier,
        .object_pattern,
        .array_pattern,
        .assignment_pattern,
        => |*v| {
            v.type_annotation = annotation;
            span.end = @max(span.end, parser.tree.span(annotation).end);
        },
        else => {},
    }
    return try parser.tree.addNode(data, span);
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
        const start = parser.tree.span(parser.tree.extra(decorators)[0]).start;
        const span = parser.tree.span(pattern);
        if (start < span.start) parser.tree.setSpan(pattern, .{ .start = start, .end = span.end });
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
            "Declarations with initializers cannot also have definite assignment assertions",
            .{ .help = "Remove the '!', since the initializer already assigns a value" },
        );
    } else if (!has_type_annotation) {
        try parser.report(
            span,
            "Declarations with definite assignment assertions must also have type annotations",
            .{ .help = "Add a type annotation, such as 'x!: string'" },
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
    const span = parser.tree.span(pattern);
    if (end > span.end) parser.tree.setSpan(pattern, .{ .start = span.start, .end = end });
}
