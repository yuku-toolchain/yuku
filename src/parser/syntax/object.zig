const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const Token = @import("../token.zig").Token;
const TokenTag = @import("../token.zig").TokenTag;

const literals = @import("literals.zig");
const expressions = @import("expressions.zig");
const grammar = @import("../grammar.zig");
const functions = @import("functions.zig");
const ts = @import("ts/types.zig");

pub fn parseObjectExpression(parser: *Parser) Error!?ast.NodeIndex {
    const object = try grammar.parseBracketed(parser, .object) orelse return null;
    return try parser.finish(object.start, .{
        .object_expression = .{ .properties = object.elements },
    });
}

pub fn parseProperty(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var is_async = false;
    var is_generator = false;
    var kind: ast.PropertyKind = .init;
    var computed = false;

    var key: ast.NodeIndex = .null;
    var key_identifier_token: ?Token = null;

    if (parser.at(.async)) {
        const async_token = parser.current_token;
        try parser.advanceWithoutEscapeCheck() orelse return null;

        // async [no LineTerminator here] MethodDefinition
        if (isPropertyKeyStart(parser.current_token.tag) and
            !parser.current_token.hasLineTerminatorBefore())
        {
            try parser.reportIfEscapedKeyword(async_token);
            is_async = true;
        } else {
            key = try literals.addIdentifierName(parser, async_token);
        }
    }

    if (key == .null) is_generator = try parser.eat(.star) orelse return null;

    if (key == .null and !is_async and !is_generator and (parser.at(.get) or parser.at(.set))) {
        const accessor_token = parser.current_token;
        try parser.advanceWithoutEscapeCheck() orelse return null;

        if (isPropertyKeyStart(parser.current_token.tag)) {
            try parser.reportIfEscapedKeyword(accessor_token);
            kind = if (accessor_token.tag == .get) .get else .set;
        } else {
            key = try literals.addIdentifierName(parser, accessor_token);
        }
    }

    if (key == .null) {
        const key_token = parser.current_token;
        key, computed = try parsePropertyKey(parser, .object) orelse return null;
        if (key_token.tag.isIdentifierLike()) key_identifier_token = key_token;
    }

    if (parser.at(.left_paren) or (parser.tree.isTs() and parser.at(.less_than))) {
        return parseMethod(parser, start, key, computed, kind, is_async, is_generator);
    }
    if (is_async or is_generator or kind != .init) {
        return parser.failExpected("Expected '(' to start the method parameters", .{});
    }

    if (try parser.eat(.colon) orelse return null) {
        const value = try expressions.parseAssignment(parser) orelse return null;
        return try parser.finishAt(start, value, .{ .object_property = .{
            .key = key,
            .value = value,
            .kind = .init,
            .method = false,
            .shorthand = false,
            .computed = computed,
        } });
    }

    // a shorthand `a`, or the CoverInitializedName `a = default`
    const key_span = parser.tree.span(key);
    const key_data = parser.tree.data(key);
    const has_default = parser.at(.assign);
    if (computed) return parser.fail(
        key_span,
        if (has_default)
            "A computed property cannot have a default value without ':'"
        else
            "A computed property must have a value",
        .{ .help = if (has_default)
            "Write the default after the value, such as '[key]: value = 1'"
        else
            "Add ':' and a value after the computed name" },
    );
    if (!has_default) if (key_identifier_token) |token| {
        try literals.validateIdentifier(parser, "an identifier", token);
    };
    if (key_data != .identifier_name) return parser.fail(
        key_span,
        if (has_default)
            "Only an identifier property can have a default value without ':'"
        else
            "A shorthand property name must be an identifier",
        .{ .help = if (has_default)
            "Write the default after the value, such as 'key: value = 1'"
        else
            "Add ':' and a value after the name" },
    );

    const default_value: ast.NodeIndex = if (try parser.eat(.assign) orelse return null)
        try expressions.parseAssignment(parser) orelse return null
    else
        .null;
    var value = try parser.tree.addNode(
        .{ .identifier_reference = .{ .name = key_data.identifier_name.name } },
        key_span,
    );
    if (default_value != .null) {
        value = try parser.finishAt(key_span.start, default_value, .{ .assignment_expression = .{
            .left = value,
            .right = default_value,
            .operator = .assign,
        } });
        parser.state.cover_init_names += 1;
    }

    return try parser.finishAt(start, value, .{ .object_property = .{
        .key = key,
        .value = value,
        .kind = .init,
        .method = false,
        .shorthand = true,
        .computed = false,
    } });
}

pub fn parsePropertyKey(
    parser: *Parser,
    comptime owner: enum { object, class, signature },
) Error!?struct { ast.NodeIndex, bool } {
    const token = parser.current_token;
    if (try parser.eat(.left_bracket) orelse return null) {
        // a key is never a pattern, so its strips must not replace the pattern's record
        const outer_stripped_paren = parser.state.stripped_paren;
        const key = try expressions.parseAssignment(parser) orelse return null;
        if (owner == .object) parser.state.stripped_paren = outer_stripped_paren;
        if (!try parser.expect(
            .right_bracket,
            "Expected ']' to close the computed property name",
            null,
        )) return null;
        return .{ key, true };
    }

    const key = if (token.tag.isIdentifierLike())
        try literals.parseIdentifierName(parser) orelse return null
    else if (token.tag == .string_literal)
        try literals.parseStringLiteral(parser) orelse return null
    else if (token.tag.isNumericLiteral())
        try literals.parseNumericLiteral(parser) orelse return null
    else if (owner == .class and token.tag == .private_identifier)
        try literals.parsePrivateIdentifier(parser) orelse return null
    else {
        const message, const help = switch (owner) {
            .object => .{
                "Expected a property name",
                "Use an identifier, a string, a number, or a computed name such as '[key]'",
            },
            .class => .{
                "Expected a class member name",
                "Use an identifier, a string, a number, a private name such as '#name', or a" ++
                    " computed name such as '[key]'",
            },
            .signature => .{
                "Expected a type member name",
                "Use an identifier, a string, a number, or a computed name such as '[key]'",
            },
        };
        try parser.reportExpected(message, .{ .help = help });
        return null;
    };
    return .{ key, false };
}

inline fn isPropertyKeyStart(tag: TokenTag) bool {
    return tag == .star or
        tag == .left_bracket or
        tag.isIdentifierLike() or
        tag == .string_literal or
        tag.isNumericLiteral();
}

fn parseMethod(
    parser: *Parser,
    start: u32,
    key: ast.NodeIndex,
    computed: bool,
    kind: ast.PropertyKind,
    is_async: bool,
    is_generator: bool,
) Error!?ast.NodeIndex {
    // `get` and `set` after `async` or `*` are keys, not accessor markers
    if (is_async or is_generator) std.debug.assert(kind == .init);

    const outer = parser.enterFunction(is_async, is_generator);
    defer parser.leaveFunction(outer);

    const is_ts = parser.tree.isTs();
    const function_start = parser.current_token.span.start;
    const type_parameters: ast.NodeIndex = if (is_ts) try ts.parseTypeParameters(parser) else .null;
    const params = try functions.parseFormalParameters(
        parser,
        .unique_formal_parameters,
        false,
    ) orelse return null;
    if (!try functions.checkAccessorArity(parser, kind, params)) return null;
    const return_type: ast.NodeIndex = if (is_ts and parser.at(.colon))
        try ts.parseReturnTypeAnnotation(parser) orelse return null
    else
        .null;
    const body = try functions.parseFunctionBody(parser, .required) orelse return null;

    const function = try parser.finish(function_start, .{ .function = .{
        .type = .function_expression,
        .id = .null,
        .generator = is_generator,
        .async = is_async,
        .params = params,
        .body = body,
        .type_parameters = type_parameters,
        .return_type = return_type,
    } });
    return try parser.finish(start, .{ .object_property = .{
        .key = key,
        .value = function,
        .kind = kind,
        .method = kind == .init,
        .shorthand = false,
        .computed = computed,
    } });
}
