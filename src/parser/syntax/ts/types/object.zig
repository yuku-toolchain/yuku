const std = @import("std");
const ast = @import("../../../ast.zig");
const Parser = @import("../../../parser.zig").Parser;
const Error = @import("../../../parser.zig").Error;
const TokenTag = @import("../../../token.zig").TokenTag;
const Precedence = @import("../../../token.zig").Precedence;

const literals = @import("../../literals.zig");
const expressions = @import("../../expressions.zig");
const functions = @import("../../functions.zig");

const core = @import("core.zig");
const generics = @import("generics.zig");
const predicate = @import("predicate.zig");

// { x: T; foo(): U; [k: string]: V }
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
pub fn parseTypeLiteral(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);

    const start = parser.current_token.span.start;
    const members = try parseObjectTypeMembers(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_type_literal = .{ .members = members } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

// shared member list for `TSTypeLiteral` and `TSInterfaceBody`
pub fn parseObjectTypeMembers(parser: *Parser) Error!?ast.IndexRange {
    std.debug.assert(parser.current_token.tag == .left_brace);

    try parser.advance() orelse return null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .right_brace and parser.current_token.tag != .eof) {
        const member = try parseTypeMember(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), member);
        if (!try consumeTypeMemberSeparator(parser, member)) return null;
    }

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close an object-type body",
        "Each '{' in a type must be matched by a '}'",
    )) return null;

    return try parser.addExtraFromScratch(&parser.scratch_a, checkpoint);
}

// comma or semicolon folds into span, newline ends member without span stretch, else error
fn consumeTypeMemberSeparator(parser: *Parser, member: ast.NodeIndex) Error!bool {
    switch (parser.current_token.tag) {
        .semicolon, .comma => {
            const sep_end = parser.current_token.span.end;
            const member_span = parser.tree.span(member);
            parser.tree.setSpan(member, .{ .start = member_span.start, .end = sep_end });
            try parser.advance() orelse return false;
        },
        .right_brace => {},
        else => if (!parser.current_token.hasLineTerminatorBefore()) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected ';', ',', or newline between type members",
                .{ .help = "Separate type members with ';', ',', or a newline, or close the block with '}'" },
            );
            return false;
        },
    }
    return true;
}

// `{ [K in T]: V }` vs `{ [expr]: T }`
pub fn isStartOfMappedType(parser: *Parser) bool {
    std.debug.assert(parser.current_token.tag == .left_brace);

    var peek = parser.beginPeek();
    defer peek.end();

    var t = peek.next() orelse return false;
    if (t.tag == .plus or t.tag == .minus) {
        const ro = peek.next() orelse return false;
        if (ro.tag != .readonly) return false;
        t = peek.next() orelse return false;
    } else if (t.tag == .readonly) {
        t = peek.next() orelse return false;
    }
    if (t.tag != .left_bracket) return false;

    const name = peek.next() orelse return false;
    if (!name.tag.isIdentifierLike()) return false;

    const in_tok = peek.next() orelse return false;
    return in_tok.tag == .in;
}

// { [K in T]: V }   { readonly [K in T]?: V }   { -readonly [K in T as U]-?: V }
// ^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
pub fn parseMappedType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const readonly = try parseMappedModifier(parser, .readonly) orelse return null;

    if (!try parser.expect(
        .left_bracket,
        "Expected '[' in a mapped type",
        "A mapped type is written '{ [K in T]: V }'",
    )) return null;

    const key = try literals.parseBindingIdentifier(parser) orelse return null;

    if (!try parser.expect(
        .in,
        "Expected 'in' after the mapped type parameter name",
        "A mapped type iterates its key with '[K in ConstraintType]'",
    )) return null;

    const constraint = try core.parseType(parser) orelse return null;

    var name_type: ast.NodeIndex = .null;
    if (parser.current_token.tag == .as) {
        try parser.advance() orelse return null;
        name_type = try core.parseType(parser) orelse return null;
    }

    if (!try parser.expect(
        .right_bracket,
        "Expected ']' to close a mapped type parameter",
        "A mapped type parameter must be closed with ']'",
    )) return null;

    const optional = try parseMappedModifier(parser, .question) orelse return null;

    var type_annotation: ast.NodeIndex = .null;
    if (parser.current_token.tag == .colon) {
        try parser.advance() orelse return null;
        type_annotation = try core.parseType(parser) orelse return null;
    }

    if (parser.current_token.tag == .semicolon or parser.current_token.tag == .comma) {
        try parser.advance() orelse return null;
    }

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close a mapped type",
        "Each '{' in a mapped type must be matched by a '}'",
    )) return null;

    return try parser.tree.addNode(
        .{ .ts_mapped_type = .{
            .key = key,
            .constraint = constraint,
            .name_type = name_type,
            .type_annotation = type_annotation,
            .optional = optional,
            .readonly = readonly,
        } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

// optional explicit plus or minus before readonly or optional marker in mapped type
fn parseMappedModifier(parser: *Parser, comptime terminator: TokenTag) Error!?ast.TSMappedTypeModifier {
    const tag = parser.current_token.tag;

    if (tag == terminator) {
        try parser.advance() orelse return null;
        return .true;
    }

    if (tag != .plus and tag != .minus) return .none;

    const sign: ast.TSMappedTypeModifier = if (tag == .plus) .plus else .minus;
    try parser.advance() orelse return null;

    if (!try parser.expect(
        terminator,
        if (terminator == .readonly)
            "Expected 'readonly' after '+' or '-' in a mapped type"
        else
            "Expected '?' after '+' or '-' in a mapped type",
        "Mapped type '+' and '-' modifiers can only decorate 'readonly' or '?'",
    )) return null;

    return sign;
}

// one property method getter setter call construct index member
fn parseTypeMember(parser: *Parser) Error!?ast.NodeIndex {
    const tag = parser.current_token.tag;

    if (tag == .left_paren or tag == .less_than) return parseCallOrConstructSignature(parser, false);

    if (tag == .new) {
        const next = parser.peekAhead() orelse return null;
        if (next.tag == .left_paren or next.tag == .less_than) {
            return parseCallOrConstructSignature(parser, true);
        }
    }

    // readonly modifier only when same line token starts a real member
    if (tag == .readonly) {
        const next = parser.peekAhead() orelse return null;
        if (!next.hasLineTerminatorBefore() and canFollowReadonlyModifier(next.tag)) {
            const readonly_start = parser.current_token.span.start;
            try parser.advance() orelse return null;
            if (parser.current_token.tag == .left_bracket and isIndexSignatureStart(parser)) {
                return parseIndexSignature(parser, readonly_start, .{ .readonly = true });
            }
            return parsePropertyOrMethodSignature(parser, readonly_start, true);
        }
    }

    if (tag == .left_bracket and isIndexSignatureStart(parser)) {
        const start = parser.current_token.span.start;
        return parseIndexSignature(parser, start, .{});
    }

    const start = parser.current_token.span.start;
    return parsePropertyOrMethodSignature(parser, start, false);
}

// `[k: T]: V` index vs `[expr]: T` computed prop
pub fn isIndexSignatureStart(parser: *Parser) bool {
    std.debug.assert(parser.current_token.tag == .left_bracket);

    var peek = parser.beginPeek();
    defer peek.end();

    const t1 = peek.next() orelse return false;
    if (!t1.tag.isIdentifierLike()) return false;

    const t2 = peek.next() orelse return false;
    return t2.tag == .colon or t2.tag == .comma;
}

// property name starters
inline fn canFollowAccessorKeyword(tag: TokenTag) bool {
    return tag == .left_bracket or
        tag.isIdentifierLike() or
        tag == .string_literal or
        tag.isNumericLiteral();
}

// like accessor follow set plus `{` `*` `...`, bad combos caught later
inline fn canFollowReadonlyModifier(tag: TokenTag) bool {
    return canFollowAccessorKeyword(tag) or
        tag == .left_brace or
        tag == .star or
        tag == .spread;
}

// (params): R    <T>(params): R    new (params): R    new <T>(params): R
// ^^^^^^^^^^^    ^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^
fn parseCallOrConstructSignature(parser: *Parser, comptime is_construct: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (is_construct) {
        std.debug.assert(parser.current_token.tag == .new);
        try parser.advance() orelse return null;
    }

    const type_parameters = try generics.parseTypeParameters(parser);
    const params = try functions.parseFormalParameters(parser, .signature, false) orelse return null;

    var return_type: ast.NodeIndex = .null;
    var end = parser.prev_token_end;
    if (parser.current_token.tag == .colon) {
        return_type = try predicate.parseReturnTypeAnnotation(parser) orelse return null;
        end = parser.tree.span(return_type).end;
    }

    const data: ast.NodeData = if (is_construct) .{ .ts_construct_signature_declaration = .{
        .type_parameters = type_parameters,
        .params = params,
        .return_type = return_type,
    } } else .{ .ts_call_signature_declaration = .{
        .type_parameters = type_parameters,
        .params = params,
        .return_type = return_type,
    } };

    return try parser.tree.addNode(data, .{ .start = start, .end = end });
}

pub const IndexSignatureModifiers = struct {
    readonly: bool = false,
    static: bool = false,
};

// [k: T]: V    readonly [k: T]: V
// ^^^^^^^^^    ^^^^^^^^^^^^^^^^^^
pub fn parseIndexSignature(parser: *Parser, start: u32, mods: IndexSignatureModifiers) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_bracket);
    try parser.advance() orelse return null;

    // TS1096 diagnostic, exactly one index parameter
    const param = try parseIndexSignatureParameter(parser) orelse return null;
    if (parser.current_token.tag == .comma) {
        try parser.report(
            parser.current_token.span,
            "An index signature must have exactly one parameter",
            .{ .help = "An index signature is written '[name: KeyType]: ValueType'." },
        );
        return null;
    }
    const parameters = try parser.tree.addExtra(&.{param});

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

    const type_annotation = try predicate.parseTypeAnnotation(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_index_signature = .{
            .parameters = parameters,
            .type_annotation = type_annotation,
            .readonly = mods.readonly,
            .static = mods.static,
        } },
        .{ .start = start, .end = parser.tree.span(type_annotation).end },
    );
}

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

    const annotation = try predicate.parseTypeAnnotation(parser) orelse return null;
    predicate.applyTypeAnnotationToPattern(parser, name, annotation);
    return name;
}

// optional accessor keyword, key, optional `?`, callish tail or `: T` field
fn parsePropertyOrMethodSignature(parser: *Parser, start: u32, is_readonly: bool) Error!?ast.NodeIndex {
    var kind: ast.TSMethodSignatureKind = .method;
    const head_tag = parser.current_token.tag;
    if (head_tag == .get or head_tag == .set) {
        const next = parser.peekAhead() orelse return null;
        if (canFollowAccessorKeyword(next.tag)) {
            kind = if (head_tag == .get) .get else .set;
            try parser.advance() orelse return null;
        }
    }

    const key_result = try parsePropertyKey(parser) orelse return null;
    const key = key_result.key;
    const computed = key_result.computed;

    var is_optional = false;
    var tail_end = if (computed) parser.prev_token_end else parser.tree.span(key).end;

    if (parser.current_token.tag == .question) {
        is_optional = true;
        tail_end = parser.current_token.span.end;
        try parser.advance() orelse return null;
    }

    // accessors need method path so bad `(` is an error not a prop
    if (kind != .method or parser.current_token.tag == .left_paren or parser.current_token.tag == .less_than) {
        return parseMethodSignatureBody(parser, start, key, kind, computed, is_optional);
    }

    var type_annotation: ast.NodeIndex = .null;
    if (parser.current_token.tag == .colon) {
        type_annotation = try predicate.parseTypeAnnotation(parser) orelse return null;
        tail_end = parser.tree.span(type_annotation).end;
    }

    return try parser.tree.addNode(
        .{ .ts_property_signature = .{
            .key = key,
            .type_annotation = type_annotation,
            .computed = computed,
            .optional = is_optional,
            .readonly = is_readonly,
        } },
        .{ .start = start, .end = tail_end },
    );
}

// shared `(params): R` part after method get set key
fn parseMethodSignatureBody(
    parser: *Parser,
    start: u32,
    key: ast.NodeIndex,
    kind: ast.TSMethodSignatureKind,
    computed: bool,
    is_optional: bool,
) Error!?ast.NodeIndex {
    const type_parameters = try generics.parseTypeParameters(parser);
    const params = try functions.parseFormalParameters(parser, .signature, false) orelse return null;

    var return_type: ast.NodeIndex = .null;
    var end = parser.prev_token_end;
    if (parser.current_token.tag == .colon) {
        return_type = try predicate.parseReturnTypeAnnotation(parser) orelse return null;
        end = parser.tree.span(return_type).end;

        if (kind == .set) {
            try parser.report(
                parser.tree.span(return_type),
                "A 'set' accessor cannot have a return type annotation",
                .{ .help = "Setters do not return a value; remove the ': T' annotation." },
            );
        }
    }

    return try parser.tree.addNode(
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

// key forms for signatures
fn parsePropertyKey(parser: *Parser) Error!?PropertyKeyResult {
    const tag = parser.current_token.tag;

    if (tag == .left_bracket) {
        try parser.advance() orelse return null;
        const expr = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        if (!try parser.expect(
            .right_bracket,
            "Expected ']' to close a computed property key",
            "A computed property key is written '[expr]'",
        )) return null;
        return .{ .key = expr, .computed = true };
    }

    const key = if (tag.isIdentifierLike())
        try literals.parseIdentifierName(parser) orelse return null
    else if (tag == .string_literal)
        try literals.parseStringLiteral(parser) orelse return null
    else if (tag.isNumericLiteral())
        try literals.parseNumericLiteral(parser) orelse return null
    else {
        try parser.report(
            parser.current_token.span,
            try parser.fmt("Unexpected token '{s}' as signature key", .{parser.describeToken(parser.current_token)}),
            .{ .help = "Signature keys must be identifiers, strings, numbers, or computed expressions [expr]." },
        );
        return null;
    };

    return .{ .key = key, .computed = false };
}
