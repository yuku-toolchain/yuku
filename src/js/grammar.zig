const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;
const ast = @import("ast.zig");
const token = @import("token.zig");

const expressions = @import("syntax/expressions.zig");
const literals = @import("syntax/literals.zig");

/// result from parsing array cover grammar: [a, b, ...c]
///
/// elements are stored in parser scratch and can later be converted to:
/// - ArrayExpression (for regular array literals)
/// - ArrayPattern (for destructuring)
pub const ArrayCover = struct {
    elements: []const ast.NodeIndex,
    start: u32,
    end: u32,
};

/// result from parsing object cover grammar: {a, b: c, ...d}
///
/// properties are stored in parser scratch and can later be converted to:
/// - ObjectExpression (for regular object literals)
/// - ObjectPattern (for destructuring)
pub const ObjectCover = struct {
    properties: []const ast.NodeIndex,
    start: u32,
    end: u32,
};

/// parse an element within a cover grammar context (used for nested arrays/objects).
///
/// keeps nested arrays/objects as "covers" by converting them to expressions
/// without validation. validation is deferred until the top-level context is known:
/// - if parent becomes an expression -> validate at top level
/// - if parent becomes a pattern -> no validation needed
fn parseCoverElement(parser: *Parser) Error!?ast.NodeIndex {
    return switch (parser.current_token.type) {
        .left_bracket => blk: {
            const cover = try parseArrayCover(parser) orelse return null;
            break :blk arrayCoverToExpressionUnchecked(parser, cover);
        },
        .left_brace => blk: {
            const cover = try parseObjectCover(parser) orelse return null;
            break :blk objectCoverToExpressionUnchecked(parser, cover);
        },
        else => expressions.parseExpression(parser, 2),
    };
}

/// parse array literal permissively using cover grammar: [a, b, ...c]
/// returns raw elements for later conversion to ArrayExpression or ArrayPattern.
pub fn parseArrayCover(parser: *Parser) Error!?ArrayCover {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume [

    const checkpoint = parser.scratch_a.begin();
    errdefer parser.scratch_a.reset(checkpoint);

    var end = start + 1;

    while (parser.current_token.type != .right_bracket and parser.current_token.type != .eof) {
        // elision (holes): [,,,]
        if (parser.current_token.type == .comma) {
            try parser.scratch_a.append(parser.allocator(), ast.null_node);
            try parser.advance();
            continue;
        }

        // spread: [...x]
        if (parser.current_token.type == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance();
            const argument = try parseCoverElement(parser) orelse {
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
            // regular element - parse as cover element
            const element = try parseCoverElement(parser) orelse {
                parser.scratch_a.reset(checkpoint);
                return null;
            };
            try parser.scratch_a.append(parser.allocator(), element);
            end = parser.getSpan(element).end;
        }

        // comma or end
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
            "Unterminated array",
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

/// parse object literal permissively using cover grammar: {a, b: c, ...d}
/// returns raw properties for later conversion to ObjectExpression or ObjectPattern.
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
            const argument = try parseCoverElement(parser) orelse {
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
fn parseObjectCoverProperty(parser: *Parser) Error!?ast.NodeIndex {
    const prop_start = parser.current_token.span.start;
    var computed = false;

    // key
    var key: ast.NodeIndex = undefined;

    if (parser.current_token.type == .left_bracket) {
        // computed property: [expr]: value
        computed = true;
        try parser.advance();
        key = try parseCoverElement(parser) orelse return null;
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
        const value = try parseCoverElement(parser) orelse return null;
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
        const default_value = try parseCoverElement(parser) orelse return null;

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

/// convert array cover to ArrayExpression with validation (top-level use).
///
/// validates recursively that no nested objects contain CoverInitializedName.
/// call this when you know the array is definitely an expression, not a pattern.
pub fn arrayCoverToExpression(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    for (cover.elements) |elem| {
        if (ast.isNull(elem)) continue;
        if (!try validateNoInvalidCoverSyntax(parser, elem)) return null;
    }
    return arrayCoverToExpressionUnchecked(parser, cover);
}

/// convert array cover to ArrayExpression without validation (nested use).
/// used for nested arrays that might later become patterns when parent converts.
fn arrayCoverToExpressionUnchecked(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    return try parser.addNode(
        .{ .array_expression = .{ .elements = try parser.addExtra(cover.elements) } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// convert object cover to ObjectExpression with validation (top-level use).
/// validation is done separately via validateObjectCoverForExpression.
/// call this when you know the object is definitely an expression, not a pattern.
pub fn objectCoverToExpression(parser: *Parser, cover: ObjectCover) Error!?ast.NodeIndex {
    return objectCoverToExpressionUnchecked(parser, cover);
}

/// convert object cover to ObjectExpression without validation (nested use).
///
/// used for nested objects that might later become patterns when parent converts.
fn objectCoverToExpressionUnchecked(parser: *Parser, cover: ObjectCover) Error!?ast.NodeIndex {
    return try parser.addNode(
        .{ .object_expression = .{ .properties = try parser.addExtra(cover.properties) } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// validate that object cover doesn't contain CoverInitializedName.
pub fn validateObjectCoverForExpression(parser: *Parser, cover: ObjectCover) Error!bool {
    for (cover.properties) |prop| {
        if (ast.isNull(prop)) continue;

        const prop_data = parser.getData(prop);
        switch (prop_data) {
            .object_property => |obj_prop| {
                // check for CoverInitializedName: { a = 1 }
                if (obj_prop.shorthand and isCoverInitializedName(parser, obj_prop.value)) {
                    try reportCoverInitializedNameError(parser, prop);
                    return false;
                }
                // recursively validate nested structures
                if (!try validateNoInvalidCoverSyntax(parser, obj_prop.value)) {
                    return false;
                }
            },
            .spread_element => |spread| {
                if (!try validateNoInvalidCoverSyntax(parser, spread.argument)) {
                    return false;
                }
            },
            else => {},
        }
    }
    return true;
}

/// recursively validate that an expression tree doesn't contain CoverInitializedName.
///
/// this walks the entire expression tree looking for object expressions with
/// the invalid shorthand property syntax ({ a = 1 }).
fn validateNoInvalidCoverSyntax(parser: *Parser, expr: ast.NodeIndex) Error!bool {
    const data = parser.getData(expr);

    switch (data) {
        .object_expression => |obj| {
            const properties = parser.getExtra(obj.properties);
            for (properties) |prop| {
                if (ast.isNull(prop)) continue;

                const prop_data = parser.getData(prop);
                switch (prop_data) {
                    .object_property => |obj_prop| {
                        // check for CoverInitializedName
                        if (obj_prop.shorthand and isCoverInitializedName(parser, obj_prop.value)) {
                            try reportCoverInitializedNameError(parser, prop);
                            return false;
                        }
                        // recurse into property value
                        if (!try validateNoInvalidCoverSyntax(parser, obj_prop.value)) {
                            return false;
                        }
                    },
                    .spread_element => |spread| {
                        if (!try validateNoInvalidCoverSyntax(parser, spread.argument)) {
                            return false;
                        }
                    },
                    else => {},
                }
            }
        },
        .array_expression => |arr| {
            const elements = parser.getExtra(arr.elements);
            for (elements) |elem| {
                if (ast.isNull(elem)) continue;
                if (!try validateNoInvalidCoverSyntax(parser, elem)) {
                    return false;
                }
            }
        },
        .spread_element => |spread| {
            return validateNoInvalidCoverSyntax(parser, spread.argument);
        },
        else => {},
    }

    return true;
}

/// check if a node is a CoverInitializedName (assignment expression with = operator).
/// CoverInitializedName: { a = 1 } where the value is AssignmentExpression
inline fn isCoverInitializedName(parser: *Parser, node: ast.NodeIndex) bool {
    const data = parser.getData(node);
    return data == .assignment_expression and data.assignment_expression.operator == .assign;
}

/// report error for CoverInitializedName in expression context.
inline fn reportCoverInitializedNameError(parser: *Parser, node: ast.NodeIndex) Error!void {
    try parser.report(
        parser.getSpan(node),
        "Shorthand property cannot have a default value in object expression",
        .{ .help = "Use '{ a: a = 1 }' syntax or this is only valid in destructuring patterns." },
    );
}

/// convert array cover to ArrayPattern.
///
/// recursively converts all elements from expressions to patterns.
/// CoverInitializedName ({ a = 1 }) becomes valid AssignmentPattern here.
pub fn arrayCoverToPattern(parser: *Parser, cover: ArrayCover) Error!?ast.NodeIndex {
    return toArrayPattern(parser, cover.elements, .{ .start = cover.start, .end = cover.end });
}

/// convert object cover to ObjectPattern.
///
/// recursively converts all properties from expressions to patterns.
/// CoverInitializedName ({ a = 1 }) becomes valid AssignmentPattern here.
pub fn objectCoverToPattern(parser: *Parser, cover: ObjectCover) Error!?ast.NodeIndex {
    return toObjectPattern(parser, cover.properties, .{ .start = cover.start, .end = cover.end });
}

/// Convert an expression node to a binding pattern.
///
/// transformations:
/// - IdentifierReference -> BindingIdentifier
/// - ArrayExpression -> ArrayPattern (recursive)
/// - ObjectExpression -> ObjectPattern (recursive)
/// - AssignmentExpression -> AssignmentPattern (for defaults)
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
            return toArrayPattern(parser, parser.getExtra(arr.elements), span);
        },

        // ObjectExpression → ObjectPattern (recursive)
        .object_expression => |obj| {
            return toObjectPattern(parser, parser.getExtra(obj.properties), span);
        },

        // already a pattern (can happen with nested patterns)
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

/// for converting array elements to ArrayPattern.
fn toArrayPattern(parser: *Parser, elements: []const ast.NodeIndex, span: ast.Span) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_b.begin();
    errdefer parser.scratch_b.reset(checkpoint);

    var rest: ast.NodeIndex = ast.null_node;

    for (elements, 0..) |elem, i| {
        if (ast.isNull(elem)) {
            try parser.scratch_b.append(parser.allocator(), ast.null_node);
            continue;
        }

        const elem_data = parser.getData(elem);
        const elem_span = parser.getSpan(elem);

        if (elem_data == .spread_element) {
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

            const pattern = try expressionToPattern(parser, elem_data.spread_element.argument) orelse {
                parser.scratch_b.reset(checkpoint);
                return null;
            };

            rest = try parser.addNode(.{ .binding_rest_element = .{ .argument = pattern } }, elem_span);
            continue;
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

/// for converting object properties to ObjectPattern.
fn toObjectPattern(parser: *Parser, properties: []const ast.NodeIndex, span: ast.Span) Error!?ast.NodeIndex {
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
