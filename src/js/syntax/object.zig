const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");

const literals = @import("literals.zig");
const grammar = @import("../grammar.zig");

/// result from parsing object cover grammar: {a, b: c, ...d}
pub const ObjectCover = struct {
    properties: []const ast.NodeIndex,
    start: u32,
    end: u32,
};

/// parse object literal permissively using cover grammar: {a, b: c, ...d}
/// returns raw properties for later conversion to ObjectExpression or ObjectPattern.
/// https://tc39.es/ecma262/#sec-object-initializer (covers ObjectAssignmentPattern)
pub fn parseCover(parser: *Parser) Error!?ObjectCover {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume {

    const checkpoint = parser.scratch_a.begin();
    errdefer parser.scratch_a.reset(checkpoint);

    var end = start + 1;

    while (parser.current_token.type != .right_brace and parser.current_token.type != .eof) {
        // spread: {...x}
        if (parser.current_token.type == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance();
            const argument = try grammar.parseCoverElement(parser) orelse {
                parser.scratch_a.reset(checkpoint);
                return null;
            };
            const spread_end = parser.getSpan(argument).end;
            const spread = try parser.addNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );
            try parser.scratch_a.append(parser.allocator(), spread);
            end = spread_end;
        } else {
            // property
            const prop = try parseCoverProperty(parser) orelse {
                parser.scratch_a.reset(checkpoint);
                return null;
            };
            try parser.scratch_a.append(parser.allocator(), prop);
            end = parser.getSpan(prop).end;
        }

        // comma or end
        if (parser.current_token.type == .comma) {
            try parser.advance();
        } else if (parser.current_token.type != .right_brace) {
            try parser.report(
                parser.current_token.span,
                "Expected ',' or '}' in object",
                .{ .help = "Add a comma between properties or close the object with '}'." },
            );
            parser.scratch_a.reset(checkpoint);
            return null;
        }
    }

    if (parser.current_token.type != .right_brace) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated object",
            .{ .help = "Add a closing '}' to complete the object." },
        );
        parser.scratch_a.reset(checkpoint);
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance(); // consume }

    return .{
        .properties = parser.scratch_a.take(checkpoint),
        .start = start,
        .end = end,
    };
}

/// parse a single property in object cover grammar.
fn parseCoverProperty(parser: *Parser) Error!?ast.NodeIndex {
    const prop_start = parser.current_token.span.start;
    var computed = false;

    // key
    var key: ast.NodeIndex = undefined;

    if (parser.current_token.type == .left_bracket) {
        // computed property: [expr]: value
        computed = true;
        try parser.advance();
        key = try grammar.parseCoverElement(parser) orelse return null;
        if (!try parser.expect(.right_bracket, "Expected ']' after computed property key", null)) {
            return null;
        }
    } else if (parser.current_token.type.isIdentifierLike()) {
        // identifier key
        const key_token = parser.current_token;
        try parser.advance();
        key = try parser.addNode(
            .{ .identifier_name = .{ .name_start = key_token.span.start, .name_len = @intCast(key_token.lexeme.len) } },
            key_token.span,
        );
    } else if (parser.current_token.type == .string_literal) {
        key = try literals.parseStringLiteral(parser) orelse return null;
    } else if (parser.current_token.type.isNumericLiteral()) {
        key = try literals.parseNumericLiteral(parser) orelse return null;
    } else {
        try parser.reportFmt(
            parser.current_token.span,
            "Unexpected token '{s}' as property key",
            .{parser.current_token.lexeme},
            .{ .help = "Property keys must be identifiers, strings, numbers, or computed expressions [expr]." },
        );
        return null;
    }

    // check what follows the key
    const key_span = parser.getSpan(key);

    if (parser.current_token.type == .colon) {
        // key: value
        try parser.advance();
        const value = try grammar.parseCoverElement(parser) orelse return null;
        return try parser.addNode(
            .{ .object_property = .{ .key = key, .value = value, .kind = .init, .shorthand = false, .computed = computed } },
            .{ .start = prop_start, .end = parser.getSpan(value).end },
        );
    }

    if (parser.current_token.type == .assign) {
        // CoverInitializedName: a = default (for destructuring patterns, we are permissive here)
        // store as object_property with shorthand + assignment expression for the value
        if (computed) {
            try parser.report(
                key_span,
                "Computed property cannot have a default value without ':'",
                .{ .help = "Use '[key]: value = default' syntax instead." },
            );
            return null;
        }

        // CoverInitializedName only allows identifier keys
        const key_data = parser.getData(key);
        if (key_data != .identifier_name) {
            try parser.report(
                key_span,
                "Invalid shorthand property initializer",
                .{ .help = "Only identifier keys can have default values. Use 'key: value = default' syntax." },
            );
            return null;
        }

        try parser.advance();
        const default_value = try grammar.parseCoverElement(parser) orelse return null;

        // create identifier reference from the key for the left side
        const id_ref = try parser.addNode(
            .{ .identifier_reference = .{ .name_start = key_data.identifier_name.name_start, .name_len = key_data.identifier_name.name_len } },
            key_span,
        );

        // create assignment expression as the value (will be converted to assignment_pattern for patterns)
        const assign_expr = try parser.addNode(
            .{ .assignment_expression = .{ .left = id_ref, .right = default_value, .operator = .assign } },
            .{ .start = key_span.start, .end = parser.getSpan(default_value).end },
        );

        return try parser.addNode(
            .{ .object_property = .{ .key = key, .value = assign_expr, .kind = .init, .shorthand = true, .computed = false } },
            .{ .start = prop_start, .end = parser.getSpan(default_value).end },
        );
    }

    // shorthand: { a } equivalent to { a: a }
    if (computed) {
        try parser.report(
            key_span,
            "Computed property must have a value",
            .{ .help = "Add ': value' after the computed key." },
        );
        return null;
    }

    // shorthand only works with identifier keys
    const key_data = parser.getData(key);
    if (key_data != .identifier_name) {
        try parser.report(
            key_span,
            "Shorthand property must be an identifier",
            .{ .help = "String and numeric keys require explicit ': value' syntax." },
        );
        return null;
    }

    // for shorthand, create identifier reference as value
    const value = try parser.addNode(
        .{ .identifier_reference = .{ .name_start = key_data.identifier_name.name_start, .name_len = key_data.identifier_name.name_len } },
        key_span,
    );

    return try parser.addNode(
        .{ .object_property = .{ .key = key, .value = value, .kind = .init, .shorthand = true, .computed = false } },
        .{ .start = prop_start, .end = key_span.end },
    );
}

/// convert object cover to ObjectExpression.
/// currently there are no checks, we can add in future if needed
/// for no checks, use coverToExpressionUnchecked
pub fn coverToExpression(parser: *Parser, cover: ObjectCover) Error!?ast.NodeIndex {
    return try parser.addNode(
        .{ .object_expression = .{ .properties = try parser.addExtra(cover.properties) } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// convert object cover to ObjectExpression without validation (nested use).
/// used for nested objects that might later become patterns when parent converts.
pub fn coverToExpressionUnchecked(parser: *Parser, cover: ObjectCover) Error!?ast.NodeIndex {
    return try parser.addNode(
        .{ .object_expression = .{ .properties = try parser.addExtra(cover.properties) } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// validate that object cover doesn't contain CoverInitializedName.
pub fn validateCoverForExpression(parser: *Parser, cover: ObjectCover) Error!bool {
    for (cover.properties) |prop| {
        if (ast.isNull(prop)) continue;

        const prop_data = parser.getData(prop);
        switch (prop_data) {
            .object_property => |obj_prop| {
                // check for CoverInitializedName: { a = 1 }
                if (obj_prop.shorthand and grammar.isCoverInitializedName(parser, obj_prop.value)) {
                    try grammar.reportCoverInitializedNameError(parser, prop);
                    return false;
                }
                // recursively validate nested structures
                if (!try grammar.validateNoInvalidCoverSyntax(parser, obj_prop.value)) {
                    return false;
                }
            },
            .spread_element => |spread| {
                if (!try grammar.validateNoInvalidCoverSyntax(parser, spread.argument)) {
                    return false;
                }
            },
            else => {},
        }
    }
    return true;
}

/// convert object cover to ObjectPattern.
/// CoverInitializedName ({ a = 1 }) becomes valid AssignmentPattern here.
pub fn coverToPattern(parser: *Parser, cover: ObjectCover) Error!?ast.NodeIndex {
    return toObjectPattern(parser, cover.properties, .{ .start = cover.start, .end = cover.end });
}

/// for converting object properties to ObjectPattern.
pub fn toObjectPattern(parser: *Parser, properties: []const ast.NodeIndex, span: ast.Span) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_b.begin();
    errdefer parser.scratch_b.reset(checkpoint);

    var rest: ast.NodeIndex = ast.null_node;

    for (properties, 0..) |prop, i| {
        const prop_data = parser.getData(prop);
        const prop_span = parser.getSpan(prop);

        if (prop_data == .spread_element) {
            if (i != properties.len - 1) {
                try parser.report(prop_span, "Rest element must be the last property", .{
                    .help = "No properties can follow the rest element in a destructuring pattern.",
                });
                parser.scratch_b.reset(checkpoint);
                return null;
            }

            // rest argument must be a simple identifier
            const arg = prop_data.spread_element.argument;
            const arg_data = parser.getData(arg);
            if (arg_data != .identifier_reference) {
                try parser.report(parser.getSpan(arg), "Rest element argument must be an identifier", .{
                    .help = "Object rest patterns only accept simple identifiers, not nested patterns.",
                });
                parser.scratch_b.reset(checkpoint);
                return null;
            }

            const binding_id = try parser.addNode(
                .{ .binding_identifier = .{ .name_start = arg_data.identifier_reference.name_start, .name_len = arg_data.identifier_reference.name_len } },
                parser.getSpan(arg),
            );

            rest = try parser.addNode(.{ .binding_rest_element = .{ .argument = binding_id } }, prop_span);
            continue;
        }

        if (prop_data != .object_property) {
            try parser.report(prop_span, "Invalid property in object pattern", .{});
            parser.scratch_b.reset(checkpoint);
            return null;
        }

        const obj_prop = prop_data.object_property;

        // TODO: validate: obj_prop.method=true and non .init kind are not allowed
        // validate it after implmenting property key method

        const value_pattern = try grammar.expressionToPattern(parser, obj_prop.value) orelse {
            parser.scratch_b.reset(checkpoint);
            return null;
        };

        const binding_prop = try parser.addNode(
            .{ .binding_property = .{ .key = obj_prop.key, .value = value_pattern, .shorthand = obj_prop.shorthand, .computed = obj_prop.computed } },
            prop_span,
        );
        try parser.scratch_b.append(parser.allocator(), binding_prop);
    }

    return try parser.addNode(
        .{ .object_pattern = .{ .properties = try parser.addExtra(parser.scratch_b.take(checkpoint)), .rest = rest } },
        span,
    );
}
