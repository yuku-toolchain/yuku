const std = @import("std");
const ast = @import("../../../ast.zig");
const Parser = @import("../../../parser.zig").Parser;
const Error = @import("../../../parser.zig").Error;
const TokenTag = @import("../../../token.zig").TokenTag;

const literals = @import("../../literals.zig");
const functions = @import("../../functions.zig");
const object_literal = @import("../../object.zig");

const core = @import("core.zig");
const generics = @import("generics.zig");
const predicate = @import("predicate.zig");

pub fn parseTypeLiteral(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);

    const start = parser.current_token.span.start;
    const members = try parseObjectTypeMembers(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_type_literal = .{ .members = members } },
        .{ .start = start, .end = parser.prev_token_end },
    );
}

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
        "Expected '}' to close the object type",
        "Check that every '{' in the type has a matching '}'",
    )) return null;

    return try parser.flushToExtras(&parser.scratch_a, checkpoint);
}

// the separator joins the member span, matching TS-ESTree
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
                "Expected ';', ',', or a line break between type members",
                .{ .help = "Separate the members with ';', or close the type with '}'" },
            );
            return false;
        },
    }
    return true;
}

pub fn isStartOfMappedType(parser: *Parser) bool {
    std.debug.assert(parser.current_token.tag == .left_brace);

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

    const name = peek.next();
    if (!name.tag.isIdentifierLike()) return false;

    return peek.next().tag == .in;
}

pub fn parseMappedType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);

    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const readonly = try parseMappedModifier(parser, .readonly) orelse return null;

    if (!try parser.expect(
        .left_bracket,
        "Expected '[' in the mapped type",
        "Write a mapped type as '{ [K in T]: V }'",
    )) return null;

    const key = try literals.parseBindingIdentifier(parser) orelse return null;

    if (!try parser.expect(
        .in,
        "Expected 'in' after the mapped type parameter name",
        "Write the key as '[K in Keys]'",
    )) return null;

    const constraint = try core.parseType(parser) orelse return null;

    var name_type: ast.NodeIndex = .null;
    if (parser.current_token.tag == .as) {
        try parser.advance() orelse return null;
        name_type = try core.parseType(parser) orelse return null;
    }

    if (!try parser.expect(
        .right_bracket,
        "Expected ']' to close the mapped type parameter",
        "Check that every '[' in the mapped type has a matching ']'",
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
        "Expected '}' to close the mapped type",
        "Check that every '{' in the mapped type has a matching '}'",
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

fn parseMappedModifier(
    parser: *Parser,
    comptime terminator: TokenTag,
) Error!?ast.TSMappedTypeModifier {
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
            "Expected 'readonly' after '+' or '-' in the mapped type"
        else
            "Expected '?' after '+' or '-' in the mapped type",
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
    if (tag == .readonly) {
        const next = parser.peekAhead();
        if (!next.hasLineTerminatorBefore() and canFollowReadonlyModifier(next.tag)) {
            try parser.advance() orelse return null;
            if (parser.current_token.tag == .left_bracket and isIndexSignatureStart(parser)) {
                return parseIndexSignature(parser, start, .{ .readonly = true });
            }
            return parsePropertyOrMethodSignature(parser, start, true);
        }
    }

    if (tag == .left_bracket and isIndexSignatureStart(parser)) {
        return parseIndexSignature(parser, start, .{});
    }

    return parsePropertyOrMethodSignature(parser, start, false);
}

pub fn isIndexSignatureStart(parser: *Parser) bool {
    std.debug.assert(parser.current_token.tag == .left_bracket);

    var peek = parser.beginPeek();
    defer peek.end();

    const name = peek.next();
    if (!name.tag.isIdentifierLike()) return false;

    const after_name = peek.next();
    return after_name.tag == .colon or after_name.tag == .comma;
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

    if (is_construct) {
        std.debug.assert(parser.current_token.tag == .new);
        try parser.advance() orelse return null;
    }

    const type_parameters = try generics.parseTypeParameters(parser);
    const params = try functions.parseFormalParameters(parser, .signature, false) orelse
        return null;

    var return_type: ast.NodeIndex = .null;
    var end = parser.prev_token_end;
    if (parser.current_token.tag == .colon) {
        return_type = try predicate.parseReturnTypeAnnotation(parser) orelse return null;
        end = parser.tree.span(return_type).end;
    }

    const tag = if (is_construct)
        "ts_construct_signature_declaration"
    else
        "ts_call_signature_declaration";
    return try parser.tree.addNode(@unionInit(ast.NodeData, tag, .{
        .type_parameters = type_parameters,
        .params = params,
        .return_type = return_type,
    }), .{ .start = start, .end = end });
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
    std.debug.assert(parser.current_token.tag == .left_bracket);
    try parser.advance() orelse return null;

    const name = try literals.parseBindingIdentifier(parser) orelse return null;
    if (parser.current_token.tag != .colon) {
        try parser.report(
            parser.current_token.span,
            "An index signature parameter must have a type annotation",
            .{ .help = "Add a type after the name, such as '[key: string]'" },
        );
        return null;
    }
    const param = try predicate.parsePatternTypeAnnotation(parser, name) orelse return null;
    if (parser.current_token.tag == .comma) {
        try parser.report(
            parser.current_token.span,
            "An index signature must have exactly one parameter",
            .{ .help = "Write an index signature as '[key: KeyType]: ValueType'" },
        );
        return null;
    }
    const parameters = try parser.tree.addExtra(&.{param});

    if (!try parser.expect(
        .right_bracket,
        "Expected ']' to close the index signature parameter",
        "Write an index signature as '[key: KeyType]: ValueType'",
    )) return null;

    if (parser.current_token.tag != .colon) {
        try parser.report(
            parser.current_token.span,
            "An index signature must have a type annotation",
            .{ .help = "Add a value type, such as '[key: string]: number'" },
        );
        return null;
    }

    const type_annotation = try predicate.parseTypeAnnotation(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_index_signature = .{
            .parameters = parameters,
            .type_annotation = type_annotation,
            .readonly = modifiers.readonly,
            .static = modifiers.static,
        } },
        .{ .start = start, .end = parser.tree.span(type_annotation).end },
    );
}

fn parsePropertyOrMethodSignature(
    parser: *Parser,
    start: u32,
    is_readonly: bool,
) Error!?ast.NodeIndex {
    var kind: ast.TSMethodSignatureKind = .method;
    const head_tag = parser.current_token.tag;
    if (head_tag == .get or head_tag == .set) {
        const next = parser.peekAhead();
        if (canFollowAccessorKeyword(next.tag)) {
            kind = if (head_tag == .get) .get else .set;
            try parser.advance() orelse return null;
        }
    }

    const key, const computed = try object_literal.parsePropertyKey(parser, .signature) orelse
        return null;

    var is_optional = false;
    var tail_end = if (computed) parser.prev_token_end else parser.tree.span(key).end;

    if (parser.current_token.tag == .question) {
        is_optional = true;
        tail_end = parser.current_token.span.end;
        try parser.advance() orelse return null;
    }

    // accessors take the method path so a missing `(` is an error, not a property
    const enter_method = kind != .method or
        parser.current_token.tag == .left_paren or
        parser.current_token.tag == .less_than;
    if (enter_method) {
        if (is_readonly) {
            try parser.report(
                .{ .start = start, .end = parser.tree.span(key).end },
                "'readonly' modifier can only appear on a property declaration or index" ++
                    " signature",
                .{ .help = "Remove 'readonly'" },
            );
        }
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

fn parseMethodSignatureBody(
    parser: *Parser,
    start: u32,
    key: ast.NodeIndex,
    kind: ast.TSMethodSignatureKind,
    computed: bool,
    is_optional: bool,
) Error!?ast.NodeIndex {
    const type_parameters = try generics.parseTypeParameters(parser);
    const params = try functions.parseFormalParameters(parser, .signature, false) orelse
        return null;

    var return_type: ast.NodeIndex = .null;
    var end = parser.prev_token_end;
    if (parser.current_token.tag == .colon) {
        return_type = try predicate.parseReturnTypeAnnotation(parser) orelse return null;
        end = parser.tree.span(return_type).end;

        if (kind == .set) {
            try parser.report(
                parser.tree.span(return_type),
                "A 'set' accessor cannot have a return type annotation",
                .{ .help = "Remove the return type annotation" },
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
