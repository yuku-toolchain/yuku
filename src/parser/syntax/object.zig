const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const Token = @import("../token.zig").Token;
const TokenTag = @import("../token.zig").TokenTag;
const Precedence = @import("../token.zig").Precedence;

const literals = @import("literals.zig");
const expressions = @import("expressions.zig");
const grammar = @import("../grammar.zig");
const functions = @import("functions.zig");
const ts = @import("ts/types.zig");

/// Properties parsed by the object cover grammar, before they become an expression or a pattern.
pub const ObjectCover = struct {
    properties: ast.IndexRange,
    start: u32,
    end: u32,
};

/// Parses an object literal permissively, covering ObjectAssignmentPattern as well.
/// https://tc39.es/ecma262/#sec-object-initializer
pub fn parseCover(parser: *Parser) Error!?ObjectCover {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const checkpoint = parser.scratch_cover.begin();
    defer parser.scratch_cover.reset(checkpoint);

    var end = start + 1;

    while (parser.current_token.tag != .right_brace and parser.current_token.tag != .eof) {
        const property = if (parser.current_token.tag == .spread)
            try expressions.parseSpreadElement(parser) orelse return null
        else
            try parseCoverProperty(parser) orelse return null;
        try parser.scratch_cover.append(parser.allocator(), property);
        end = parser.tree.span(property).end;

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
        } else if (parser.current_token.tag != .right_brace) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected ',' or '}' in object",
                .{ .help = "Add a comma between properties or close the object with '}'." },
            );
            return null;
        }
    }

    if (parser.current_token.tag != .right_brace) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated object",
            .{
                .help = "Add a closing '}' to complete the object.",
                .labels = try parser.labels(&.{
                    parser.label(.{ .start = start, .end = start + 1 }, "Opened here"),
                }),
            },
        );
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance() orelse return null;

    return .{
        .properties = try parser.flushToExtras(&parser.scratch_cover, checkpoint),
        .start = start,
        .end = end,
    };
}

fn parseCoverProperty(parser: *Parser) Error!?ast.NodeIndex {
    const prop_start = parser.current_token.span.start;
    var is_async = false;
    var is_generator = false;
    var kind: ast.PropertyKind = .init;
    var computed = false;

    var key: ast.NodeIndex = .null;
    var key_identifier_token: ?Token = null;

    if (parser.current_token.tag == .async) {
        const async_token = parser.current_token;
        try parser.advanceWithoutEscapeCheck() orelse return null;

        // async [no LineTerminator here] MethodDefinition
        const starts_method = isPropertyKeyStart(parser.current_token.tag) and
            !parser.current_token.hasLineTerminatorBefore();
        if (starts_method) {
            try parser.reportIfEscapedKeyword(async_token);
            is_async = true;
        } else {
            key = try literals.addIdentifierName(parser, async_token);
        }
    }

    if (key == .null and parser.current_token.tag == .star) {
        is_generator = true;
        try parser.advance() orelse return null;
    }

    if (key == .null and !is_async and !is_generator) {
        const get_set_token = parser.current_token;
        if (get_set_token.tag == .get or get_set_token.tag == .set) {
            try parser.advanceWithoutEscapeCheck() orelse return null;

            if (isPropertyKeyStart(parser.current_token.tag)) {
                try parser.reportIfEscapedKeyword(get_set_token);
                kind = if (get_set_token.tag == .get) .get else .set;
            } else {
                key = try literals.addIdentifierName(parser, get_set_token);
            }
        }
    }

    if (key == .null) {
        const key_token = parser.current_token;
        key, computed = try parsePropertyKey(parser, .object) orelse return null;
        if (key_token.tag.isIdentifierLike()) key_identifier_token = key_token;
    }

    const key_span = parser.tree.span(key);

    const is_method_start = parser.current_token.tag == .left_paren or
        (parser.tree.isTs() and parser.current_token.tag == .less_than);

    if (is_method_start) {
        return parseObjectMethodProperty(
            parser,
            prop_start,
            key,
            computed,
            kind,
            is_async,
            is_generator,
        );
    }

    if (is_async or is_generator or kind != .init) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '(' for method definition",
            .{ .help = "Method definitions require a parameter list. Use 'method() {}' syntax." },
        );
        return null;
    }

    if (parser.current_token.tag == .colon) {
        try parser.advance() orelse return null;
        const value = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
            return null;
        return try parser.tree.addNode(
            .{ .object_property = .{
                .key = key,
                .value = value,
                .kind = .init,
                .method = false,
                .shorthand = false,
                .computed = computed,
            } },
            .{ .start = prop_start, .end = parser.tree.span(value).end },
        );
    }

    const key_data = parser.tree.data(key);
    var value: ast.NodeIndex = undefined;
    // CoverInitializedName `a = default`
    if (parser.current_token.tag == .assign) {
        if (computed) {
            try parser.report(
                key_span,
                "Computed property cannot have a default value without ':'",
                .{ .help = "Use '[key]: value = default' syntax instead." },
            );
            return null;
        }
        if (key_data != .identifier_name) {
            try parser.report(
                key_span,
                "Invalid shorthand property initializer",
                .{ .help = "Only identifier keys can have default values." ++
                    " Use 'key: value = default' syntax." },
            );
            return null;
        }

        try parser.advance() orelse return null;
        const default_value = try expressions.parseExpression(
            parser,
            Precedence.Assignment,
            .{},
        ) orelse return null;
        const reference = try parser.tree.addNode(
            .{ .identifier_reference = .{ .name = key_data.identifier_name.name } },
            key_span,
        );
        value = try parser.tree.addNode(
            .{ .assignment_expression = .{
                .left = reference,
                .right = default_value,
                .operator = .assign,
            } },
            .{ .start = key_span.start, .end = parser.tree.span(default_value).end },
        );
        parser.state.cover_init_names += 1;
    } else {
        if (computed) {
            try parser.report(
                key_span,
                "Computed property must have a value",
                .{ .help = "Add ': value' after the computed key." },
            );
            return null;
        }
        if (key_identifier_token) |key_token| {
            try literals.validateIdentifier(parser, "an identifier", key_token);
        }
        if (key_data != .identifier_name) {
            try parser.report(
                key_span,
                "Shorthand property must be an identifier",
                .{ .help = "String and numeric keys require explicit ': value' syntax." },
            );
            return null;
        }
        value = try parser.tree.addNode(
            .{ .identifier_reference = .{ .name = key_data.identifier_name.name } },
            key_span,
        );
    }

    return try parser.tree.addNode(
        .{ .object_property = .{
            .key = key,
            .value = value,
            .kind = .init,
            .method = false,
            .shorthand = true,
            .computed = false,
        } },
        .{ .start = prop_start, .end = parser.tree.span(value).end },
    );
}

pub fn parsePropertyKey(
    parser: *Parser,
    comptime owner: enum { object, class, signature },
) Error!?struct { ast.NodeIndex, bool } {
    const token = parser.current_token;
    if (token.tag == .left_bracket) {
        try parser.advance() orelse return null;
        // a key is never a pattern, so its strips must not replace the pattern's record
        const outer_stripped_paren = parser.state.stripped_paren;
        const key = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
            return null;
        if (owner == .object) parser.state.stripped_paren = outer_stripped_paren;
        if (!try parser.expect(
            .right_bracket,
            if (owner == .signature)
                "Expected ']' to close a computed property key"
            else
                "Expected ']' after computed property key",
            if (owner == .signature) "A computed property key is written '[expr]'" else null,
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
        const name, const help = switch (owner) {
            .object => .{
                "property key",
                "Property keys must be identifiers, strings, numbers, or" ++
                    " computed expressions [expr].",
            },
            .class => .{
                "class element key",
                "Class element keys must be identifiers, strings, numbers," ++
                    " private identifiers (#name), or computed expressions [expr].",
            },
            .signature => .{
                "signature key",
                "Signature keys must be identifiers, strings, numbers," ++
                    " or computed expressions [expr].",
            },
        };
        try parser.report(
            token.span,
            try parser.fmt("Unexpected token '{s}' as " ++ name, .{parser.describeToken(token)}),
            .{ .help = help },
        );
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

fn parseObjectMethodProperty(
    parser: *Parser,
    prop_start: u32,
    key: ast.NodeIndex,
    computed: bool,
    kind: ast.PropertyKind,
    is_async: bool,
    is_generator: bool,
) Error!?ast.NodeIndex {
    // `get` and `set` after `async` or `*` are keys, not accessor markers
    if (is_async) std.debug.assert(kind == .init);
    if (is_generator) std.debug.assert(kind == .init);

    const saved_await = parser.context.await;
    const saved_yield = parser.context.yield;
    parser.context.await = is_async;
    parser.context.yield = is_generator;
    defer parser.context.await = saved_await;
    defer parser.context.yield = saved_yield;

    const is_ts = parser.tree.isTs();
    const func_start = parser.current_token.span.start;

    const type_parameters: ast.NodeIndex = if (is_ts)
        try ts.parseTypeParameters(parser)
    else
        .null;

    const params = try functions.parseFormalParameters(
        parser,
        .unique_formal_parameters,
        false,
    ) orelse return null;
    if (!try functions.checkAccessorArity(parser, kind, params)) return null;

    var return_type: ast.NodeIndex = .null;
    if (is_ts and parser.current_token.tag == .colon) {
        return_type = try ts.parseReturnTypeAnnotation(parser) orelse return null;
    }

    const body = try functions.parseFunctionBody(parser, .required) orelse return null;
    const body_end = parser.tree.span(body).end;

    const func = try parser.tree.addNode(
        .{ .function = .{
            .type = .function_expression,
            .id = .null,
            .generator = is_generator,
            .async = is_async,
            .params = params,
            .body = body,
            .type_parameters = type_parameters,
            .return_type = return_type,
        } },
        .{ .start = func_start, .end = body_end },
    );

    return try parser.tree.addNode(
        .{ .object_property = .{
            .key = key,
            .value = func,
            .kind = kind,
            .method = kind == .init,
            .shorthand = false,
            .computed = computed,
        } },
        .{ .start = prop_start, .end = body_end },
    );
}

/// Converts an object cover to an ObjectExpression.
pub fn coverToExpression(parser: *Parser, cover: ObjectCover) Error!ast.NodeIndex {
    return parser.tree.addNode(
        .{ .object_expression = .{ .properties = cover.properties } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// Converts an object cover to an ObjectPattern.
pub fn coverToPattern(
    parser: *Parser,
    cover: ObjectCover,
    comptime context: grammar.PatternContext,
) Error!ast.NodeIndex {
    return toObjectPatternImpl(
        parser,
        null,
        cover.properties,
        .{ .start = cover.start, .end = cover.end },
        context,
    );
}

/// Converts an ObjectExpression node to an ObjectPattern in place.
pub fn toObjectPattern(
    parser: *Parser,
    expr_node: ast.NodeIndex,
    properties_range: ast.IndexRange,
    span: ast.Span,
    comptime context: grammar.PatternContext,
) Error!void {
    _ = try toObjectPatternImpl(parser, expr_node, properties_range, span, context);
}

fn toObjectPatternImpl(
    parser: *Parser,
    mutate_node: ?ast.NodeIndex,
    properties_range: ast.IndexRange,
    span: ast.Span,
    comptime context: grammar.PatternContext,
) Error!ast.NodeIndex {
    const properties = parser.tree.extra(properties_range);

    var rest: ast.NodeIndex = .null;
    var properties_len = properties_range.len;

    for (properties, 0..) |prop, i| {
        const prop_data = parser.tree.data(prop);

        if (prop_data == .spread_element) {
            if (i == properties.len - 1) {
                if (grammar.isFollowedByComma(parser, prop)) {
                    try parser.report(
                        span,
                        "Rest element cannot have a trailing comma in object destructuring.",
                        .{ .help = "Remove the trailing comma after the rest element" },
                    );
                }
            } else {
                try parser.report(
                    parser.tree.span(prop),
                    "Rest element must be the last property",
                    .{ .help = "No properties can follow the rest element in a" ++
                        " destructuring pattern." },
                );
            }

            try grammar.expressionToPattern(parser, prop, context);

            // BindingRestProperty only allows a BindingIdentifier
            const argument = parser.tree.data(prop).binding_rest_element.argument;
            switch (parser.tree.data(argument)) {
                .array_pattern, .object_pattern => try parser.report(
                    parser.tree.span(argument),
                    "Object rest argument cannot be a destructuring pattern",
                    .{ .help = "Collect the remaining properties into an identifier" ++
                        " and destructure it separately." },
                ),
                else => {},
            }

            rest = prop;
            properties_len = @intCast(i);
            break;
        }

        if (prop_data != .object_property) {
            try parser.report(parser.tree.span(prop), "Invalid property in object pattern", .{});
            continue;
        }

        const obj_prop = prop_data.object_property;
        if (obj_prop.method) {
            try parser.report(
                parser.tree.span(prop),
                "Method cannot appear in destructuring pattern",
                .{ .help = "Use a regular property instead of a method definition." },
            );
            continue;
        }

        if (obj_prop.kind != .init) {
            try parser.report(
                parser.tree.span(prop),
                "Getter/setter cannot appear in destructuring pattern",
                .{ .help = "Use a regular property instead of a getter or setter." },
            );
            continue;
        }

        if (obj_prop.shorthand and grammar.isCoverInitializedName(parser, obj_prop.value)) {
            std.debug.assert(parser.state.cover_init_names > 0);
            parser.state.cover_init_names -= 1;
        }
        try grammar.expressionToPattern(parser, obj_prop.value, context);

        parser.tree.setData(prop, .{ .binding_property = .{
            .key = obj_prop.key,
            .value = obj_prop.value,
            .shorthand = obj_prop.shorthand,
            .computed = obj_prop.computed,
        } });
    }

    const pattern_data: ast.NodeData = .{ .object_pattern = .{
        .properties = .{ .start = properties_range.start, .len = properties_len },
        .rest = rest,
    } };

    if (mutate_node) |node| {
        parser.tree.setData(node, pattern_data);
        return node;
    }

    return try parser.tree.addNode(pattern_data, span);
}
