const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const Token = @import("../token.zig").Token;
const TokenTag = @import("../token.zig").TokenTag;
const Precedence = @import("../token.zig").Precedence;

const literals = @import("literals.zig");
const grammar = @import("../grammar.zig");
const functions = @import("functions.zig");

/// result from parsing object cover grammar: {a, b: c, ...d}
pub const ObjectCover = struct {
    properties: ast.IndexRange,
    start: u32,
    end: u32,
};

/// parse object literal permissively using cover grammar: {a, b: c, ...d}
/// returns raw properties for later conversion to ObjectExpression or ObjectPattern.
/// https://tc39.es/ecma262/#sec-object-initializer (covers ObjectAssignmentPattern)
pub fn parseCover(parser: *Parser) Error!?ObjectCover {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume {

    const checkpoint = parser.scratch_cover.begin();
    defer parser.scratch_cover.reset(checkpoint);

    var end = start + 1;

    while (parser.current_token.tag != .right_brace and parser.current_token.tag != .eof) {
        // spread: {...x}
        if (parser.current_token.tag == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance() orelse return null;
            const argument = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse return null;
            const spread_end = parser.getSpan(argument).end;
            const spread = try parser.addNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );
            try parser.scratch_cover.append(parser.allocator(), spread);
            end = spread_end;
        } else {
            // property
            const prop = try parseCoverProperty(parser) orelse return null;
            try parser.scratch_cover.append(parser.allocator(), prop);
            end = parser.getSpan(prop).end;
        }

        // comma or end
        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
            // then it's a trailing comma
            if (parser.current_token.tag == .right_brace) {
                parser.state.cover_has_trailing_comma = start;
            }
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
                .labels = try parser.makeLabels(&.{parser.label(.{ .start = start, .end = start + 1 }, "Opened here")}),
            },
        );
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance() orelse return null; // consume }

    const properties = try parser.addExtraFromScratch(&parser.scratch_cover, checkpoint);

    return .{
        .properties = properties,
        .start = start,
        .end = end,
    };
}

/// parse a single property in object cover grammar.
fn parseCoverProperty(parser: *Parser) Error!?ast.NodeIndex {
    const prop_start = parser.current_token.span.start;
    var is_async = false;
    var is_generator = false;
    var kind: ast.PropertyKind = .init;
    var computed = false;

    var key: ast.NodeIndex = ast.null_node;
    var key_identifier_token: ?Token = null;

    // check for async, consume it, then decide if it's a modifier or key based on what follows
    if (parser.current_token.tag == .async) {
        const async_token = parser.current_token;

        try parser.advanceWithoutEscapeCheck() orelse return null;

        // async [no LineTerminator here] MethodDefinition
        if (isPropertyKeyStart(parser.current_token.tag) and !parser.current_token.hasLineTerminatorBefore()) {
            try parser.reportIfEscapedKeyword(async_token);
            is_async = true;
        } else {
            // it's a key named "async"
            key = try parser.addNode(
                .{ .identifier_name = .{ .name_start = async_token.span.start, .name_len = @intCast(async_token.len()) } },
                async_token.span,
            );
        }
    }

    // check for generator, only if we don't already have a key
    if (ast.isNull(key) and parser.current_token.tag == .star) {
        is_generator = true;
        try parser.advance() orelse return null;
    }

    // check for get/set, only if no async/generator modifiers and no key yet
    if (ast.isNull(key) and !is_async and !is_generator) {
        const cur_tag = parser.current_token.tag;
        if (cur_tag == .get or cur_tag == .set) {
            const get_set_token = parser.current_token;

            try parser.advanceWithoutEscapeCheck() orelse return null;

            if (isPropertyKeyStart(parser.current_token.tag)) {
                try parser.reportIfEscapedKeyword(get_set_token);
                kind = if (cur_tag == .get) .get else .set;
            } else {
                key = try parser.addNode(
                    .{ .identifier_name = .{ .name_start = get_set_token.span.start, .name_len = @intCast(get_set_token.len()) } },
                    get_set_token.span,
                );
            }
        }
    }

    // parse property key if not already determined
    if (ast.isNull(key)) {
        if (parser.current_token.tag == .left_bracket) {
            computed = true;
            try parser.advance() orelse return null;
            key = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse return null;
            if (!try parser.expect(.right_bracket, "Expected ']' after computed property key", null)) {
                return null;
            }
        } else if (parser.current_token.tag.isIdentifierLike()) {
            key_identifier_token = parser.current_token;
            key = try literals.parseIdentifierName(parser) orelse return null;
        } else if (parser.current_token.tag == .string_literal) {
            key = try literals.parseStringLiteral(parser) orelse return null;
        } else if (parser.current_token.tag.isNumericLiteral()) {
            key = try literals.parseNumericLiteral(parser) orelse return null;
        } else {
            try parser.reportFmt(
                parser.current_token.span,
                "Unexpected token '{s}' as property key",
                .{parser.describeToken(parser.current_token)},
                .{ .help = "Property keys must be identifiers, strings, numbers, or computed expressions [expr]." },
            );
            return null;
        }
    }

    const key_span = parser.getSpan(key);

    // method definition, key followed by (
    if (parser.current_token.tag == .left_paren) {
        return parseObjectMethodProperty(parser, prop_start, key, computed, kind, is_async, is_generator);
    }

    // if we had async, generator, or get/set prefix but no (, it's an error
    if (is_async or is_generator or kind != .init) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '(' for method definition",
            .{ .help = "Method definitions require a parameter list. Use 'method() {}' syntax." },
        );
        return null;
    }

    // regular property: key: value
    if (parser.current_token.tag == .colon) {
        try parser.advance() orelse return null;
        const value = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse return null;
        return try parser.addNode(
            .{ .object_property = .{ .key = key, .value = value, .kind = .init, .method = false, .shorthand = false, .computed = computed } },
            .{ .start = prop_start, .end = parser.getSpan(value).end },
        );
    }

    // CoverInitializedName: a = default
    if (parser.current_token.tag == .assign) {
        if (computed) {
            try parser.report(
                key_span,
                "Computed property cannot have a default value without ':'",
                .{ .help = "Use '[key]: value = default' syntax instead." },
            );
            return null;
        }

        const key_data = parser.getData(key);
        if (key_data != .identifier_name) {
            try parser.report(
                key_span,
                "Invalid shorthand property initializer",
                .{ .help = "Only identifier keys can have default values. Use 'key: value = default' syntax." },
            );
            return null;
        }

        try parser.advance() orelse return null;
        const default_value = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse return null;

        const id_ref = try parser.addNode(
            .{ .identifier_reference = .{ .name_start = key_data.identifier_name.name_start, .name_len = key_data.identifier_name.name_len } },
            key_span,
        );

        const assign_expr = try parser.addNode(
            .{ .assignment_expression = .{ .left = id_ref, .right = default_value, .operator = .assign } },
            .{ .start = key_span.start, .end = parser.getSpan(default_value).end },
        );

        parser.state.cover_has_init_name = true;

        return try parser.addNode(
            .{ .object_property = .{ .key = key, .value = assign_expr, .kind = .init, .method = false, .shorthand = true, .computed = false } },
            .{ .start = prop_start, .end = parser.getSpan(default_value).end },
        );
    }

    if (computed) {
        try parser.report(
            key_span,
            "Computed property must have a value",
            .{ .help = "Add ': value' after the computed key." },
        );
        return null;
    }

    // shorthand property: { a }

    if (key_identifier_token) |key_token| {
        if (!try literals.validateIdentifier(parser, "an identifier", key_token)) {
            return null;
        }
    }

    const key_data = parser.getData(key);

    if (key_data != .identifier_name) {
        try parser.report(
            key_span,
            "Shorthand property must be an identifier",
            .{ .help = "String and numeric keys require explicit ': value' syntax." },
        );
        return null;
    }

    const value = try parser.addNode(
        .{ .identifier_reference = .{ .name_start = key_data.identifier_name.name_start, .name_len = key_data.identifier_name.name_len } },
        key_span,
    );

    return try parser.addNode(
        .{ .object_property = .{ .key = key, .value = value, .kind = .init, .method = false, .shorthand = true, .computed = false } },
        .{ .start = prop_start, .end = key_span.end },
    );
}

inline fn isPropertyKeyStart(tag: TokenTag) bool {
    return tag == .star or
        tag == .left_bracket or
        tag.isIdentifierLike() or
        tag == .string_literal or
        tag.isNumericLiteral();
}

/// parse a method definition: key(...) { ... }
fn parseObjectMethodProperty(
    parser: *Parser,
    prop_start: u32,
    key: ast.NodeIndex,
    computed: bool,
    kind: ast.PropertyKind,
    is_async: bool,
    is_generator: bool,
) Error!?ast.NodeIndex {
    if (is_generator) {
        if (kind == .get) {
            try parser.report(
                .{ .start = prop_start, .end = parser.current_token.span.end },
                "Getter cannot be a generator",
                .{ .help = "Remove the '*' from the getter definition." },
            );
            return null;
        } else if (kind == .set) {
            try parser.report(
                .{ .start = prop_start, .end = parser.current_token.span.end },
                "Setter cannot be a generator",
                .{ .help = "Remove the '*' from the setter definition." },
            );
            return null;
        }
    }

    const saved_await_is_keyword = parser.context.await_is_keyword;
    const saved_yield_is_keyword = parser.context.yield_is_keyword;

    parser.context.await_is_keyword = is_async;
    parser.context.yield_is_keyword = is_generator;

    defer {
        parser.context.await_is_keyword = saved_await_is_keyword;
        parser.context.yield_is_keyword = saved_yield_is_keyword;
    }

    const func_start = parser.current_token.span.start;
    if (!try parser.expect(.left_paren, "Expected '(' to start method parameters", null)) return null;

    const params = try functions.parseFormalParamaters(parser, .unique_formal_parameters) orelse return null;
    const params_data = parser.getData(params).formal_parameters;

    // validate getter has no parameters
    if (kind == .get) {
        if (params_data.items.len != 0 or !ast.isNull(params_data.rest)) {
            try parser.report(
                parser.getSpan(params),
                "Getter must have no parameters",
                .{ .help = "Remove all parameters from the getter." },
            );
            return null;
        }
    }

    // validate setter has exactly one parameter
    if (kind == .set) {
        if (params_data.items.len != 1 or !ast.isNull(params_data.rest)) {
            try parser.report(
                parser.getSpan(params),
                "Setter must have exactly one parameter",
                .{ .help = "Setters accept exactly one argument." },
            );
            return null;
        }
    }

    if (!try parser.expect(.right_paren, "Expected ')' after method parameters", null)) return null;

    // parse body
    const body = try functions.parseFunctionBody(parser) orelse return null;
    const body_end = parser.getSpan(body).end;

    // create function expression for the method value
    const func = try parser.addNode(
        .{ .function = .{
            .type = .function_expression,
            .id = ast.null_node,
            .generator = is_generator,
            .async = is_async,
            .params = params,
            .body = body,
        } },
        .{ .start = func_start, .end = body_end },
    );

    const is_method = kind == .init;

    return try parser.addNode(
        .{ .object_property = .{
            .key = key,
            .value = func,
            .kind = kind,
            .method = is_method,
            .shorthand = false,
            .computed = computed,
        } },
        .{ .start = prop_start, .end = body_end },
    );
}

/// convert object cover to ObjectExpression.
/// validates that the expression does not contain CoverInitializedName when validate=true.
pub fn coverToExpression(parser: *Parser, cover: ObjectCover, validate: bool) Error!?ast.NodeIndex {
    const object_expression = try parser.addNode(
        .{ .object_expression = .{ .properties = cover.properties } },
        .{ .start = cover.start, .end = cover.end },
    );

    if (validate) try grammar.validateNoCoverInitializedSyntax(parser, object_expression);

    return object_expression;
}

/// convert object cover to ObjectPattern.
pub fn coverToPattern(parser: *Parser, cover: ObjectCover, comptime context: grammar.PatternContext) Error!ast.NodeIndex {
    return toObjectPatternImpl(parser, null, cover.properties, .{ .start = cover.start, .end = cover.end }, context);
}

/// convert ObjectExpression to ObjectPattern (mutates in-place).
pub fn toObjectPattern(parser: *Parser, expr_node: ast.NodeIndex, properties_range: ast.IndexRange, span: ast.Span, comptime context: grammar.PatternContext) Error!void {
    _ = try toObjectPatternImpl(parser, expr_node, properties_range, span, context);
}

fn toObjectPatternImpl(parser: *Parser, mutate_node: ?ast.NodeIndex, properties_range: ast.IndexRange, span: ast.Span, comptime context: grammar.PatternContext) Error!ast.NodeIndex {
    const properties = parser.getExtra(properties_range);

    var rest: ast.NodeIndex = ast.null_node;
    var properties_len = properties_range.len;

    for (properties, 0..) |prop, i| {
        const prop_data = parser.getData(prop);

        if (prop_data == .spread_element) {
            if (parser.state.cover_has_trailing_comma == span.start) {
                try parser.report(span, "Rest element cannot have a trailing comma in object destructuring.", .{
                    .help = "Remove the trailing comma after the rest element",
                });

                parser.state.cover_has_trailing_comma = null;
            }

            if (i != properties.len - 1) {
                try parser.report(parser.getSpan(prop), "Rest element must be the last property", .{
                    .help = "No properties can follow the rest element in a destructuring pattern.",
                });
            }

            // spread_element to binding_rest_element
            try grammar.expressionToPattern(parser, prop, context);

            rest = prop;
            properties_len = @intCast(i);
            break;
        }

        if (prop_data != .object_property) {
            try parser.report(parser.getSpan(prop), "Invalid property in object pattern", .{});
            continue;
        }

        const obj_prop = prop_data.object_property;

        if (obj_prop.method) {
            try parser.report(parser.getSpan(prop), "Method cannot appear in destructuring pattern", .{
                .help = "Use a regular property instead of a method definition.",
            });
            continue;
        }

        if (obj_prop.kind != .init) {
            try parser.report(parser.getSpan(prop), "Getter/setter cannot appear in destructuring pattern", .{
                .help = "Use a regular property instead of a getter or setter.",
            });
            continue;
        }

        try grammar.expressionToPattern(parser, obj_prop.value, context);

        parser.setData(prop, .{ .binding_property = .{
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
        parser.setData(node, pattern_data);
        return node;
    }

    return try parser.addNode(pattern_data, span);
}
