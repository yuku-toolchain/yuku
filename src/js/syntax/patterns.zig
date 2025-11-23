const std = @import("std");
const token = @import("../token.zig");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;

const literals = @import("literals.zig");
const expressions = @import("expressions.zig");

pub fn parseBindingPattern(parser: *Parser) ?*ast.BindingPattern {
    if (parser.current_token.type.isIdentifierLike()) {
        return parseBindingIdentifierPattern(parser);
    }

    return switch (parser.current_token.type) {
        .LeftBracket => parseArrayPattern(parser),
        .LeftBrace => parseObjectPattern(parser),
        else => {
            const bad_token = parser.current_token;
            parser.err(
                bad_token.span.start,
                bad_token.span.end,
                "Expected binding pattern",
                "Use an identifier, array pattern [...], or object pattern {...}",
            );
            return null;
        },
    };
}

fn parseBindingIdentifierPattern(parser: *Parser) ?*ast.BindingPattern {
    if (!parser.current_token.type.isIdentifierLike()) {
        const bad_token = parser.current_token;
        parser.err(
            bad_token.span.start,
            bad_token.span.end,
            "Expected identifier for variable name",
            "Use a valid identifier (letters, digits, _, or $ - must start with letter, _ or $)",
        );
        return null;
    }

    const name = parser.current_token.lexeme;
    const span = parser.current_token.span;

    if (!parser.ensureValidIdentifier(parser.current_token, "as an identifier", "Choose a different identifier name")) {
        return null;
    }

    parser.advance();

    const binding_id = ast.BindingIdentifier{
        .name = name,
        .span = span,
    };

    return parser.createNode(ast.BindingPattern, .{ .binding_identifier = binding_id });
}

fn parseArrayPattern(parser: *Parser) ?*ast.BindingPattern {
    const opening_bracket = parser.current_token;
    const start = opening_bracket.span.start;

    if (!parser.expect(.LeftBracket, "Expected '[' to start array pattern", "Add '[' here to begin the array destructuring pattern")) {
        return null;
    }

    parser.clear(&parser.scratch_array_pattern_elements);
    parser.ensureCapacity(&parser.scratch_array_pattern_elements, 4);

    var last_end = parser.current_token.span.start;

    // parse array elements
    while (parser.current_token.type != .RightBracket and parser.current_token.type != .EOF) {
        // check for rest element
        if (parser.current_token.type == .Spread) {
            const rest_elem = parseArrayRestElement(parser) orelse return null;
            parser.append(&parser.scratch_array_pattern_elements, rest_elem);
            last_end = rest_elem.getSpan().end;

            // rest element must be last
            if (parser.current_token.type == .Comma) {
                const comma_token = parser.current_token;
                parser.err(
                    rest_elem.getSpan().start,
                    comma_token.span.end,
                    "Rest element must be last in array pattern",
                    "Move the rest element to the end. Example: { a, b, ...rest } instead of { ...rest, a, b }",
                );
                parser.advance();
                return null;
            }
            break;
        }

        // parse regular element or empty slot
        if (parser.current_token.type == .Comma) {
            // empty slot: [a, , b]
            parser.append(&parser.scratch_array_pattern_elements, null);
            last_end = parser.current_token.span.end;
            parser.advance();
        } else {
            const elem = parseArrayPatternElement(parser) orelse return null;
            parser.append(&parser.scratch_array_pattern_elements, elem);
            last_end = elem.getSpan().end;

            if (parser.current_token.type == .Comma) {
                last_end = parser.current_token.span.end;
                parser.advance();
            } else {
                break;
            }
        }
    }

    const end = if (parser.current_token.type == .RightBracket) blk: {
        const right_bracket_end = parser.current_token.span.end;
        parser.advance();
        break :blk right_bracket_end;
    } else blk: {
        parser.err(
            start,
            last_end,
            "Expected ']' to close array pattern",
            "Add ']' to close the array destructuring pattern",
        );
        break :blk last_end;
    };

    const array_pattern = ast.ArrayPattern{
        .elements = parser.dupe(?*ast.ArrayPatternElement, parser.scratch_array_pattern_elements.items),
        .span = .{ .start = start, .end = end },
    };

    return parser.createNode(ast.BindingPattern, .{ .array_pattern = array_pattern });
}

fn parseArrayPatternElement(parser: *Parser) ?*ast.ArrayPatternElement {
    const pattern = parseBindingPattern(parser) orelse return null;

    // check for default value
    const final_pattern = if (parser.current_token.type == .Assign)
        parseAssignmentPatternDefault(parser, pattern) orelse return null
    else
        pattern;

    const elem = ast.ArrayPatternElement{ .binding_pattern = final_pattern };
    return parser.createNode(ast.ArrayPatternElement, elem);
}

fn parseArrayRestElement(parser: *Parser) ?*ast.ArrayPatternElement {
    const spread_token = parser.current_token;
    const start = spread_token.span.start;

    if (!parser.expect(.Spread, "Expected '...' for rest element", "Add '...' here for the rest element")) {
        return null;
    }

    const argument = parseBindingPattern(parser) orelse return null;
    const end = argument.getSpan().end;

    const rest_elem = ast.BindingRestElement{
        .argument = argument,
        .span = .{ .start = start, .end = end },
    };

    const rest_elem_ptr = parser.createNode(ast.BindingRestElement, rest_elem);
    const elem = ast.ArrayPatternElement{ .rest_element = rest_elem_ptr };
    return parser.createNode(ast.ArrayPatternElement, elem);
}

fn parseObjectPattern(parser: *Parser) ?*ast.BindingPattern {
    const opening_brace = parser.current_token;
    const start = opening_brace.span.start;

    if (!parser.expect(.LeftBrace, "Expected '{' to start object pattern", "Add '{' here to begin the object destructuring pattern")) {
        return null;
    }

    parser.clear(&parser.scratch_object_pattern_properties);
    parser.ensureCapacity(&parser.scratch_object_pattern_properties, 4);

    var last_end = parser.current_token.span.start;

    // parse object properties
    while (parser.current_token.type != .RightBrace and parser.current_token.type != .EOF) {
        // check for rest element
        if (parser.current_token.type == .Spread) {
            const rest_prop = parseObjectRestElement(parser) orelse return null;
            parser.append(&parser.scratch_object_pattern_properties, rest_prop);
            last_end = rest_prop.getSpan().end;

            // rest element must be last
            if (parser.current_token.type == .Comma) {
                const comma_token = parser.current_token;
                parser.err(
                    rest_prop.getSpan().start,
                    comma_token.span.end,
                    "Rest element must be last in object pattern",
                    "Move the rest element to the end. Example: { a, b, ...rest } instead of { ...rest, a, b }",
                );
                parser.advance();
                return null;
            }
            break;
        }

        // parse regular property
        const prop = parseObjectPatternProperty(parser) orelse return null;
        parser.append(&parser.scratch_object_pattern_properties, prop);
        last_end = prop.getSpan().end;

        if (parser.current_token.type == .Comma) {
            last_end = parser.current_token.span.end;
            parser.advance();
        } else {
            break;
        }
    }

    const end = if (parser.current_token.type == .RightBrace) blk: {
        const right_brace_end = parser.current_token.span.end;
        parser.advance();
        break :blk right_brace_end;
    } else blk: {
        parser.err(
            start,
            last_end,
            "Expected '}' to close object pattern",
            "Add '}' here to close the object destructuring pattern",
        );
        break :blk last_end;
    };

    const object_pattern = ast.ObjectPattern{
        .properties = parser.dupe(*ast.ObjectPatternProperty, parser.scratch_object_pattern_properties.items),
        .span = .{ .start = start, .end = end },
    };

    return parser.createNode(ast.BindingPattern, .{ .object_pattern = object_pattern });
}

fn parseObjectPatternProperty(parser: *Parser) ?*ast.ObjectPatternProperty {
    const start = parser.current_token.span.start;

    var computed = false;
    var key: *ast.PropertyKey = undefined;
    var key_span: token.Span = undefined;

    // token of the key if it's an identifier, otherwise undefined. used for reserved word check
    var identifier_key_token: token.Token = undefined;

    // check for computed property: [expression]
    if (parser.current_token.type == .LeftBracket) {
        computed = true;
        const bracket_start = parser.current_token.span.start;
        parser.advance();

        const key_expr = expressions.parseExpression(parser, 0) orelse return null;
        key = parser.createNode(ast.PropertyKey, .{ .expression = key_expr });
        key_span = .{ .start = bracket_start, .end = key_expr.getSpan().end };

        if (parser.current_token.type != .RightBracket) {
            parser.err(
                bracket_start,
                parser.current_token.span.start,
                "Expected ']' to close computed property key",
                "Add ']' here to close the computed property name",
            );
            return null;
        }

        key_span.end = parser.current_token.span.end;
        parser.advance();
    } else if (parser.current_token.type.isIdentifierLike()) {
        identifier_key_token = parser.current_token;

        const span = parser.current_token.span;
        const name = parser.current_token.lexeme;

        parser.advance();

        const identifier_name = ast.IdentifierName{
            .name = name,
            .span = span,
        };

        key_span = span;

        key = parser.createNode(ast.PropertyKey, .{ .identifier_name = identifier_name });
    } else if (parser.current_token.type.isNumericLiteral()) {
        const numeric_literal = literals.parseNumericLiteral(parser) orelse return null;

        key_span = numeric_literal.getSpan();

        key = parser.createNode(ast.PropertyKey, .{ .expression = numeric_literal });
    } else if (parser.current_token.type == .StringLiteral) {
        const string_literal = literals.parseStringLiteral(parser) orelse return null;

        key_span = string_literal.getSpan();

        key = parser.createNode(ast.PropertyKey, .{ .expression = string_literal });
    } else {
        parser.err(
            parser.current_token.span.start,
            parser.current_token.span.end,
            "Expected property key",
            "Property key must be an identifier, string, number, or computed property ([expression])",
        );
        return null;
    }

    const is_shorthand = parser.current_token.type == .Comma or parser.current_token.type == .RightBrace or parser.current_token.type == .Assign;
    var value: *ast.BindingPattern = undefined;

    if (is_shorthand) {
        // shorthand: only allowed with identifier_name keys
        const identifier_name = switch (key.*) {
            .identifier_name => |id| id,
            .expression => {
                const key_start = key_span.start;
                const key_end = key_span.end;
                parser.err(
                    key_start,
                    key_end,
                    "Cannot use computed property as shorthand property",
                    "Computed properties require explicit binding. Use ': <pattern>' after the key",
                );
                return null;
            },
            else => return null,
        };

        if (!parser.ensureValidIdentifier(identifier_key_token, "in shorthand property", parser.formatMessage("Use the full form: {{ {s}: identifier }}", .{identifier_name.name}))) {
            return null;
        }

        const binding_id = ast.BindingIdentifier{
            .name = identifier_name.name,
            .span = key_span,
        };

        value = parser.createNode(ast.BindingPattern, .{ .binding_identifier = binding_id });

        // check for default value in shorthand: { x = 5 }
        if (parser.current_token.type == .Assign) {
            value = parseAssignmentPatternDefault(parser, value) orelse return null;
        }
    } else {
        // regular property: { x: y }
        if (parser.current_token.type != .Colon) {
            parser.err(
                key_span.start,
                parser.current_token.span.start,
                "Expected ':' after property key",
                "Add ':' here to separate the key from the value pattern",
            );
            return null;
        }

        parser.advance();
        value = parseBindingPattern(parser) orelse return null;

        // check for default value: { x: y = 5 }
        if (parser.current_token.type == .Assign) {
            value = parseAssignmentPatternDefault(parser, value) orelse return null;
        }
    }

    const end = value.getSpan().end;

    const prop = ast.BindingProperty{
        .key = key,
        .value = value,
        .shorthand = is_shorthand,
        .computed = computed,
        .span = .{ .start = start, .end = end },
    };

    const prop_ptr = parser.createNode(ast.BindingProperty, prop);
    return parser.createNode(ast.ObjectPatternProperty, .{ .binding_property = prop_ptr });
}

fn parseObjectRestElement(parser: *Parser) ?*ast.ObjectPatternProperty {
    const spread_token = parser.current_token;
    const start = spread_token.span.start;

    parser.advance(); // consume '...'

    const argument = parseBindingPattern(parser) orelse return null;

    if (argument.* != .binding_identifier) {
        const arg_span = argument.getSpan();
        parser.err(
            arg_span.start,
            arg_span.end,
            "Object rest property must be an identifier",
            "Object rest properties can only be simple identifiers, not array or object patterns",
        );
        return null;
    }

    const end = argument.getSpan().end;

    const rest_elem = ast.BindingRestElement{
        .argument = argument,
        .span = .{ .start = start, .end = end },
    };

    const rest_elem_ptr = parser.createNode(ast.BindingRestElement, rest_elem);
    return parser.createNode(ast.ObjectPatternProperty, .{ .rest_element = rest_elem_ptr });
}

fn parseAssignmentPatternDefault(parser: *Parser, left: *ast.BindingPattern) ?*ast.BindingPattern {
    const start = left.getSpan().start;

    // consume '='
    if (parser.current_token.type != .Assign) {
        return left;
    }

    parser.advance();

    // parse the default expression
    const right = expressions.parseExpression(parser, 0) orelse return null;

    const end = right.getSpan().end;

    const assignment_pattern = ast.AssignmentPattern{
        .left = left,
        .right = right,
        .span = .{ .start = start, .end = end },
    };

    return parser.createNode(ast.BindingPattern, .{ .assignment_pattern = assignment_pattern });
}

pub fn isDestructuringPattern(parser: *Parser, pattern: *ast.BindingPattern) bool {
    return switch (pattern.*) {
        .array_pattern, .object_pattern => true,
        .assignment_pattern => |ap| isDestructuringPattern(parser, ap.left),
        else => false,
    };
}
