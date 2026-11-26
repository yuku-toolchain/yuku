const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;
const ast = @import("ast.zig");
const TokenTag = @import("token.zig").TokenTag;

const object = @import("syntax/object.zig");
const expressions = @import("syntax/expressions.zig");

pub const Bracketed = struct {
    elements: ast.IndexRange,
    start: u32,
    end: u32,
    has_trailing_comma: bool,

    pub fn span(self: Bracketed) ast.Span {
        return .{ .start = self.start, .end = self.end };
    }
};

const Bracket = struct {
    open: TokenTag,
    close: TokenTag,
    separator_message: []const u8,
    separator_help: []const u8,
    unterminated_message: []const u8,
    unterminated_help: []const u8,
};

const BracketKind = enum {
    array,
    object,
    parenthesized,

    fn bracket(comptime kind: BracketKind) Bracket {
        return switch (kind) {
            .array => .{
                .open = .left_bracket,
                .close = .right_bracket,
                .separator_message = "Expected ',' or ']' in the array",
                .separator_help = "Separate the elements with ','",
                .unterminated_message = "Unterminated array literal",
                .unterminated_help = "Add ']' to close the array",
            },
            .object => .{
                .open = .left_brace,
                .close = .right_brace,
                .separator_message = "Expected ',' or '}' in the object",
                .separator_help = "Separate the properties with ','",
                .unterminated_message = "Unterminated object literal",
                .unterminated_help = "Add '}' to close the object",
            },
            .parenthesized => .{
                .open = .left_paren,
                .close = .right_paren,
                .separator_message = "Expected ',' or ')' in the parenthesized expression",
                .separator_help = "Separate the expressions with ','",
                .unterminated_message = "Unterminated parenthesized expression",
                .unterminated_help = "Add ')' to close the expression",
            },
        };
    }
};

/// https://tc39.es/ecma262/#sec-array-initializer
/// https://tc39.es/ecma262/#sec-object-initializer
/// https://tc39.es/ecma262/#prod-CoverParenthesizedExpressionAndArrowParameterList
pub fn parseBracketed(parser: *Parser, comptime kind: BracketKind) Error!?Bracketed {
    const bracket = comptime kind.bracket();
    const start = try parser.take(bracket.open) orelse return null;

    const elements = parser.list();
    defer elements.deinit();
    var end = start + 1;
    var has_trailing_comma = false;

    while (!parser.at(bracket.close) and !parser.at(.eof)) {
        if (kind == .array and parser.at(.comma)) {
            try elements.append(.null);
            try parser.advance() orelse return null;
            continue;
        }

        const is_spread = parser.at(.spread);
        const element = if (kind == .object and !is_spread)
            try object.parseProperty(parser) orelse return null
        else
            try expressions.parseSpreadOrAssignment(parser) orelse return null;
        try elements.append(element);
        end = parser.tree.span(element).end;

        if (try parser.eat(.comma) orelse return null) {
            if (kind == .parenthesized) has_trailing_comma = is_spread or parser.at(.right_paren);
        } else if (!parser.at(bracket.close) and !(kind == .parenthesized and is_spread)) {
            try parser.reportExpected(bracket.separator_message, .{
                .help = bracket.separator_help,
            });
            return null;
        }
    }

    if (!parser.at(bracket.close)) {
        try parser.report(.{ .start = start, .end = end }, bracket.unterminated_message, .{
            .help = bracket.unterminated_help,
            .label = .{ .span = .{ .start = start, .end = start + 1 }, .message = "opened here" },
        });
        return null;
    }
    try parser.advance() orelse return null;

    return .{
        .elements = try elements.finish(),
        .start = start,
        .end = parser.prev_token_end,
        .has_trailing_comma = has_trailing_comma,
    };
}

pub fn reportCoverInitializedNames(parser: *Parser) Error!void {
    for (parser.tree.nodes.items(.data), 0..) |data, i| {
        if (data != .object_property) continue;
        const prop = data.object_property;
        if (!prop.shorthand or !isCoverInitializedName(parser, prop.value)) continue;
        try parser.report(
            parser.tree.span(@fromBackingInt(@intCast(i))),
            "An '=' can only follow a property name when the containing object literal is" ++
                " part of a destructuring pattern",
            .{ .help = "Use ':' to give the property a value, or destructure into the object" },
        );
    }
}

inline fn isCoverInitializedName(parser: *Parser, node: ast.NodeIndex) bool {
    const data = parser.tree.data(node);
    return data == .assignment_expression and data.assignment_expression.operator == .assign;
}

fn isFollowedByComma(parser: *Parser, node: ast.NodeIndex) bool {
    var peek = parser.beginPeek();
    defer peek.end();
    parser.lexer.rewindTo(parser.tree.span(node).end);
    return peek.next().tag == .comma;
}

pub const PatternContext = enum {
    binding,
    assignable,
};

pub fn expressionToPattern(
    parser: *Parser,
    expr: ast.NodeIndex,
    comptime context: PatternContext,
) Error!void {
    const data = parser.tree.data(expr);

    // including a paren the `.assignable` pass stripped from a legal `(a) = b`
    if (context == .binding and
        (data == .parenthesized_expression or parser.state.stripped_paren == expr))
    {
        return parser.report(
            parser.tree.span(expr),
            "Parentheses are not allowed in binding patterns",
            .{ .help = "Remove the parentheses" },
        );
    }

    switch (data) {
        .identifier_reference => |id| if (context == .binding) {
            parser.tree.setData(expr, .{ .binding_identifier = .{ .name = id.name } });
        },

        .assignment_expression => |assign| {
            if (assign.operator != .assign) return parser.report(
                parser.tree.span(expr),
                "Only '=' can assign a default value in a destructuring pattern",
                .{ .help = "Replace the operator with '='" },
            );
            try expressionToPattern(parser, assign.left, context);
            parser.tree.setData(expr, .{ .assignment_pattern = .{
                .left = assign.left,
                .right = assign.right,
            } });
        },

        .array_expression => |literal| {
            const span = parser.tree.span(expr);
            const pattern = try arrayToPattern(parser, literal.elements, span, context);
            parser.tree.setData(expr, .{ .array_pattern = pattern });
        },

        .object_expression => |literal| {
            const span = parser.tree.span(expr);
            const pattern = try objectToPattern(parser, literal.properties, span, context);
            parser.tree.setData(expr, .{ .object_pattern = pattern });
        },

        .spread_element => |spread| {
            try expressionToPattern(parser, spread.argument, context);
            if (parser.tree.data(spread.argument) == .assignment_pattern) try parser.report(
                parser.tree.span(expr),
                "A rest element cannot have an initializer",
                .{ .help = "Remove the initializer" },
            );
            parser.tree.setData(expr, .{
                .binding_rest_element = .{ .argument = spread.argument },
            });
        },

        .chain_expression => try parser.report(
            parser.tree.span(expr),
            if (context == .binding)
                "Optional chaining is not allowed in binding patterns"
            else
                "The left-hand side of an assignment expression may not be an optional" ++
                    " property access",
            .{ .help = if (context == .binding)
                "Use an identifier instead"
            else
                "Use '.' instead of '?.'" },
        ),

        .member_expression => if (context == .binding) try parser.report(
            parser.tree.span(expr),
            "Member expressions are not allowed in binding patterns",
            .{ .help = "Use an identifier instead" },
        ),

        .ts_non_null_expression,
        .ts_as_expression,
        .ts_satisfies_expression,
        .ts_type_assertion,
        => if (context == .binding) {
            try parser.report(
                parser.tree.span(expr),
                "Non-null and type assertions are not allowed in binding patterns",
                .{ .help = "Remove the assertion" },
            );
        } else if (!expressions.isSimpleAssignmentTarget(parser, expr)) {
            try parser.report(
                parser.tree.span(expr),
                "The left-hand side of an assignment expression must be a variable or a" ++
                    " property access",
                .{ .help = "Only a variable or a property access can be asserted here" },
            );
        },

        .parenthesized_expression => |paren| {
            std.debug.assert(context == .assignable);

            if (!expressions.isSimpleAssignmentTarget(parser, paren.expression)) {
                return parser.report(
                    parser.tree.span(paren.expression),
                    "A parenthesized assignment target must be a variable or a property access",
                    .{ .help = "Optional chains cannot be assigned to, even in parentheses" },
                );
            }

            switch (parser.tree.data(paren.expression)) {
                .ts_as_expression,
                .ts_satisfies_expression,
                .ts_type_assertion,
                .ts_non_null_expression,
                => {},
                else => try expressionToPattern(parser, paren.expression, context),
            }

            // assignment targets drop outer parens regardless of `preserve_parens`
            parser.state.stripped_paren = expr;
            parser.tree.setData(expr, parser.tree.data(paren.expression));
            parser.tree.setSpan(expr, parser.tree.span(paren.expression));
        },

        .binding_identifier => {},

        // `.binding` re-descends what `.assignable` converted, as in `([a.b] = []) => {}`
        .assignment_pattern => |pattern| if (context == .binding) {
            try expressionToPattern(parser, pattern.left, context);
        },

        .array_pattern => |pattern| if (context == .binding) {
            for (parser.tree.extra(pattern.elements)) |element| {
                if (element == .null) continue;
                try expressionToPattern(parser, element, context);
            }
            if (pattern.rest != .null) try expressionToPattern(parser, pattern.rest, context);
        },

        .object_pattern => |pattern| if (context == .binding) {
            for (parser.tree.extra(pattern.properties)) |property| {
                const property_data = parser.tree.data(property);
                // a method or accessor keeps its object_property shape and is already diagnosed
                if (property_data != .binding_property) continue;
                try expressionToPattern(parser, property_data.binding_property.value, context);
            }
            if (pattern.rest != .null) try expressionToPattern(parser, pattern.rest, context);
        },

        .binding_rest_element => |rest| if (context == .binding) {
            try expressionToPattern(parser, rest.argument, context);
        },

        else => try parser.report(
            parser.tree.span(expr),
            if (context == .binding)
                "Binding patterns can only contain identifiers and nested patterns"
            else
                "The left-hand side of an assignment expression must be a variable or a" ++
                    " property access",
            .{},
        ),
    }
}

pub fn arrayToPattern(
    parser: *Parser,
    elements: ast.IndexRange,
    literal_span: ast.Span,
    comptime context: PatternContext,
) Error!ast.ArrayPattern {
    for (parser.tree.extra(elements), 0..) |element, i| {
        if (element == .null) continue;
        if (parser.tree.data(element) != .spread_element) {
            try expressionToPattern(parser, element, context);
            continue;
        }
        try restToPattern(parser, literal_span, element, i == elements.len - 1, context);
        return .{ .elements = .{ .start = elements.start, .len = @intCast(i) }, .rest = element };
    }
    return .{ .elements = elements, .rest = .null };
}

fn restToPattern(
    parser: *Parser,
    literal_span: ast.Span,
    rest: ast.NodeIndex,
    is_last: bool,
    comptime context: PatternContext,
) Error!void {
    if (!is_last) {
        try parser.report(
            parser.tree.span(rest),
            "A rest element must be last in a destructuring pattern",
            .{ .help = "Move the rest element to the end" },
        );
    } else if (isFollowedByComma(parser, rest)) {
        try parser.report(
            literal_span,
            "A rest element cannot have a trailing comma",
            .{ .help = "Remove the trailing comma" },
        );
    }
    try expressionToPattern(parser, rest, context);
}

pub fn objectToPattern(
    parser: *Parser,
    properties: ast.IndexRange,
    literal_span: ast.Span,
    comptime context: PatternContext,
) Error!ast.ObjectPattern {
    for (parser.tree.extra(properties), 0..) |property, i| {
        const data = parser.tree.data(property);

        if (data == .spread_element) {
            try restToPattern(parser, literal_span, property, i == properties.len - 1, context);

            // BindingRestProperty only allows a BindingIdentifier
            const argument = parser.tree.data(property).binding_rest_element.argument;
            switch (parser.tree.data(argument)) {
                .array_pattern, .object_pattern => try parser.report(
                    parser.tree.span(argument),
                    "An object rest element cannot contain a binding pattern",
                    .{ .help = "Collect the rest into an identifier, then destructure it" },
                ),
                else => {},
            }

            return .{
                .properties = .{ .start = properties.start, .len = @intCast(i) },
                .rest = property,
            };
        }

        if (data != .object_property) {
            try parser.report(
                parser.tree.span(property),
                "Invalid property in a destructuring pattern",
                .{},
            );
            continue;
        }

        const prop = data.object_property;
        if (prop.method or prop.kind != .init) {
            try parser.report(
                parser.tree.span(property),
                if (prop.method)
                    "Methods are not allowed in destructuring patterns"
                else
                    "Getters and setters are not allowed in destructuring patterns",
                .{ .help = "Use a property instead" },
            );
            continue;
        }

        if (prop.shorthand and isCoverInitializedName(parser, prop.value)) {
            std.debug.assert(parser.state.cover_init_names > 0);
            parser.state.cover_init_names -= 1;
        }
        try expressionToPattern(parser, prop.value, context);

        parser.tree.setData(property, .{ .binding_property = .{
            .key = prop.key,
            .value = prop.value,
            .shorthand = prop.shorthand,
            .computed = prop.computed,
        } });
    }

    return .{ .properties = properties, .rest = .null };
}
