const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;
const ast = @import("ast.zig");
const token = @import("token.zig");

const expressions = @import("syntax/expressions.zig");
const literals = @import("syntax/literals.zig");

/// result from parsing array cover grammar [a, b, ...c]
/// stores in scratch
pub const ArrayCover = struct {
    elements: []const ast.NodeIndex,
    start: u32,
    end: u32,
};

/// result from parsing object cover grammar {a, b: c, ...d}
/// stores in scratch
pub const ObjectCover = struct {
    properties: []const ast.NodeIndex,
    start: u32,
    end: u32,
};

/// parses array literal permissively (cover grammar).
/// elements are parsed as expressions, holes allowed, spread allowed.
/// returns the raw elements for later conversion to expression or pattern.
pub fn parseArrayCover(parser: *Parser) Error!?ArrayCover {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume [

    const checkpoint = parser.scratch_a.begin();
    errdefer parser.scratch_a.reset(checkpoint);

    var end = start + 1;

    while (parser.current_token.type != .right_bracket and parser.current_token.type != .eof) {
        // Handle elision (holes): [,,,]
        if (parser.current_token.type == .comma) {
            try parser.scratch_a.append(parser.allocator(), ast.null_node);
            try parser.advance();
            continue;
        }

        // Handle spread: [...x]
        if (parser.current_token.type == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance();
            const argument = try expressions.parseExpression(parser, 2) orelse {
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
            // Regular element - parse as assignment expression (precedence 2)
            const element = try expressions.parseExpression(parser, 2) orelse {
                parser.scratch_a.reset(checkpoint);
                return null;
            };
            try parser.scratch_a.append(parser.allocator(), element);
            end = parser.getSpan(element).end;
        }

        // Handle comma or end
        if (parser.current_token.type == .comma) {
            try parser.advance();
        } else if (parser.current_token.type != .right_bracket) {
            try parser.report(
                parser.current_token.span,
                "Expected ',' or ']' in array",
                .{ .help = "Add a comma between elements or close the array with ']'." },
            );
            parser.scratch_a.reset(checkpoint);
            return null;
        }
    }

    if (parser.current_token.type != .right_bracket) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated array literal",
            .{ .help = "Add a closing ']' to complete the array." },
        );
        parser.scratch_a.reset(checkpoint);
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance(); // consume ]

    return .{
        .elements = parser.scratch_a.take(checkpoint),
        .start = start,
        .end = end,
    };
}

/// parses object literal permissively (cover grammar).
/// properties are parsed allowing shorthand, computed keys, spread, and CoverInitializedName.
pub fn parseObjectCover(parser: *Parser) Error!?ObjectCover {
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
            const argument = try expressions.parseExpression(parser, 2) orelse {
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
            const prop = try parseObjectCoverProperty(parser) orelse {
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
            "Unterminated object literal",
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
/// handles: shorthand, key: value, computed [key]: value, and CoverInitializedName (a = 1).
fn parseObjectCoverProperty(parser: *Parser) Error!?ast.NodeIndex {
    const prop_start = parser.current_token.span.start;
    var computed = false;

    // Parse key
    var key: ast.NodeIndex = undefined;

    if (parser.current_token.type == .left_bracket) {
        // computed property: [expr]: value
        computed = true;
        try parser.advance();
        key = try expressions.parseExpression(parser, 2) orelse return null;
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
        const value = try expressions.parseExpression(parser, 2) orelse return null;
        return try parser.addNode(
            .{ .object_property = .{ .key = key, .value = value, .kind = .init, .shorthand = false, .computed = computed } },
            .{ .start = prop_start, .end = parser.getSpan(value).end },
        );
    }

    if (parser.current_token.type == .assign) {
        // CoverInitializedName: a = default (for destructuring patterns)
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
        const default_value = try expressions.parseExpression(parser, 2) orelse return null;

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

/// convert array cover to ArrayExpression.
/// array cover elements are already valid expressions, just wrap them.
pub fn arrayCoverToExpression(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    return try parser.addNode(
        .{ .array_expression = .{ .elements = try parser.addExtra(cover.elements) } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// convert object cover to ObjectExpression.
/// validates that no CoverInitializedName syntax is used ({a = 1} is invalid in expression).
pub fn objectCoverToExpression(parser: *Parser, cover: ObjectCover) Error!?ast.NodeIndex {
    // validate: CoverInitializedName ({a = 1}) is only valid in patterns, not expressions
    for (cover.properties) |prop| {
        const data = parser.getData(prop);
        if (data == .object_property and data.object_property.shorthand) {
            const value_data = parser.getData(data.object_property.value);
            if (value_data == .assignment_expression and value_data.assignment_expression.operator == .assign) {
                try parser.report(
                    parser.getSpan(prop),
                    "Shorthand property cannot have a default value in object expression",
                    .{ .help = "Use '{ a: a = 1 }' syntax or this is only valid in destructuring patterns." },
                );
                return null;
            }
        }
    }

    return try parser.addNode(
        .{ .object_expression = .{ .properties = try parser.addExtra(cover.properties) } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// convert array cover to ArrayPattern.
pub fn arrayCoverToPattern(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    return elementsToArrayPattern(parser, cover.elements, .{ .start = cover.start, .end = cover.end });
}

/// convert object cover to ObjectPattern.
pub fn objectCoverToPattern(parser: *Parser, cover: ObjectCover) Error!?ast.NodeIndex {
    return propertiesToObjectPattern(parser, cover.properties, .{ .start = cover.start, .end = cover.end });
}

/// convert an expression node to a binding pattern.
/// handles: identifiers, arrays, objects, assignment expressions.
pub fn expressionToPattern(parser: *Parser, expr: ast.NodeIndex) Error!?ast.NodeIndex {
    const data = parser.getData(expr);
    const span = parser.getSpan(expr);

    switch (data) {
        // Identifier → BindingIdentifier
        .identifier_reference => |id| {
            return try parser.addNode(
                .{ .binding_identifier = .{ .name_start = id.name_start, .name_len = id.name_len } },
                span,
            );
        },

        // AssignmentExpression → AssignmentPattern
        .assignment_expression => |assign| {
            if (assign.operator != .assign) {
                try parser.report(
                    span,
                    "Invalid assignment operator in destructuring pattern",
                    .{ .help = "Only '=' is allowed in destructuring defaults, not compound operators like '+='." },
                );
                return null;
            }

            const left_pattern = try expressionToPattern(parser, assign.left) orelse return null;
            return try parser.addNode(
                .{ .assignment_pattern = .{ .left = left_pattern, .right = assign.right } },
                span,
            );
        },

        // ArrayExpression → ArrayPattern (recursive)
        .array_expression => |arr| {
            return try arrayExpressionToPattern(parser, arr, span);
        },

        // ObjectExpression → ObjectPattern (recursive)
        .object_expression => |obj| {
            return try objectExpressionToPattern(parser, obj, span);
        },

        // Already a pattern (can happen with nested patterns)
        .binding_identifier, .array_pattern, .object_pattern, .assignment_pattern => {
            return expr;
        },

        else => {
            try parser.report(
                span,
                "Invalid element in destructuring pattern",
                .{ .help = "Expected an identifier, array pattern, object pattern, or assignment pattern." },
            );
            return null;
        },
    }
}

/// convert ArrayExpression data to ArrayPattern.
pub fn arrayExpressionToPattern(parser: *Parser, arr: ast.ArrayExpression, span: ast.Span) Error!?ast.NodeIndex {
    return elementsToArrayPattern(parser, parser.getExtra(arr.elements), span);
}

/// core logic for converting elements to ArrayPattern.
fn elementsToArrayPattern(parser: *Parser, elements: []const ast.NodeIndex, span: ast.Span) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_b.begin();
    errdefer parser.scratch_b.reset(checkpoint);

    var rest: ast.NodeIndex = ast.null_node;
    var found_rest = false;

    for (elements, 0..) |elem, i| {
        if (ast.isNull(elem)) {
            if (found_rest) {
                try parser.report(span, "Rest element must be the last element", .{
                    .help = "No elements (including holes) can follow the rest element.",
                });
                parser.scratch_b.reset(checkpoint);
                return null;
            }
            try parser.scratch_b.append(parser.allocator(), ast.null_node);
            continue;
        }

        const data = parser.getData(elem);
        const elem_span = parser.getSpan(elem);

        if (data == .spread_element) {
            if (found_rest) {
                try parser.report(elem_span, "Only one rest element is allowed in array pattern", .{});
                parser.scratch_b.reset(checkpoint);
                return null;
            }

            // rest must be last (no non-hole elements after)
            const has_more = for (elements[i + 1 ..]) |next| {
                if (!ast.isNull(next)) break true;
            } else false;

            if (has_more) {
                try parser.report(elem_span, "Rest element must be the last element", .{
                    .help = "No elements can follow the rest element in a destructuring pattern.",
                });
                parser.scratch_b.reset(checkpoint);
                return null;
            }

            const pattern = try expressionToPattern(parser, data.spread_element.argument) orelse {
                parser.scratch_b.reset(checkpoint);
                return null;
            };

            rest = try parser.addNode(.{ .binding_rest_element = .{ .argument = pattern } }, elem_span);
            found_rest = true;
            continue;
        }

        if (found_rest) {
            try parser.report(elem_span, "Rest element must be the last element", .{
                .help = "No elements can follow the rest element.",
            });
            parser.scratch_b.reset(checkpoint);
            return null;
        }

        const pattern = try expressionToPattern(parser, elem) orelse {
            parser.scratch_b.reset(checkpoint);
            return null;
        };
        try parser.scratch_b.append(parser.allocator(), pattern);
    }

    return try parser.addNode(
        .{ .array_pattern = .{ .elements = try parser.addExtra(parser.scratch_b.take(checkpoint)), .rest = rest } },
        span,
    );
}

/// convert ObjectExpression data to ObjectPattern.
pub fn objectExpressionToPattern(parser: *Parser, obj: ast.ObjectExpression, span: ast.Span) Error!?ast.NodeIndex {
    return propertiesToObjectPattern(parser, parser.getExtra(obj.properties), span);
}

/// core logic for converting properties to ObjectPattern.
fn propertiesToObjectPattern(parser: *Parser, properties: []const ast.NodeIndex, span: ast.Span) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_b.begin();
    errdefer parser.scratch_b.reset(checkpoint);

    var rest: ast.NodeIndex = ast.null_node;
    var found_rest = false;

    for (properties, 0..) |prop, i| {
        const data = parser.getData(prop);
        const prop_span = parser.getSpan(prop);

        if (data == .spread_element) {
            if (found_rest) {
                try parser.report(prop_span, "Only one rest element is allowed in object pattern", .{});
                parser.scratch_b.reset(checkpoint);
                return null;
            }

            if (i != properties.len - 1) {
                try parser.report(prop_span, "Rest element must be the last property", .{
                    .help = "No properties can follow the rest element in a destructuring pattern.",
                });
                parser.scratch_b.reset(checkpoint);
                return null;
            }

            // rest argument must be a simple identifier
            const arg = data.spread_element.argument;
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
            found_rest = true;
            continue;
        }

        if (found_rest) {
            try parser.report(prop_span, "Rest element must be the last property", .{});
            parser.scratch_b.reset(checkpoint);
            return null;
        }

        if (data != .object_property) {
            try parser.report(prop_span, "Invalid property in object pattern", .{});
            parser.scratch_b.reset(checkpoint);
            return null;
        }

        const obj_prop = data.object_property;

        const value_pattern = try expressionToPattern(parser, obj_prop.value) orelse {
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
