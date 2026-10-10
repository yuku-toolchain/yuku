const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;
const ast = @import("ast.zig");

const object = @import("syntax/object.zig");
const expressions = @import("syntax/expressions.zig");
const array = @import("syntax/array.zig");

/// Reports every `{ a = 1 }` shorthand default whose literal never became a pattern.
pub fn reportCoverInitializedNames(parser: *Parser) Error!void {
    for (parser.tree.nodes.items(.data), 0..) |data, i| {
        if (data != .object_property) continue;
        const prop = data.object_property;
        if (!prop.shorthand or !isCoverInitializedName(parser, prop.value)) continue;
        try parser.report(
            parser.tree.span(@fromBackingInt(@intCast(i))),
            "Shorthand property cannot have a default value in object expression",
            .{ .help = "Use '{ a: a = 1 }' syntax or this is only valid in destructuring" ++
                " patterns." },
        );
    }
}

/// Whether a shorthand property value is a CoverInitializedName (`{ a = 1 }`).
pub inline fn isCoverInitializedName(parser: *Parser, node: ast.NodeIndex) bool {
    const data = parser.tree.data(node);
    return data == .assignment_expression and data.assignment_expression.operator == .assign;
}

/// Whether a comma directly follows `node` in the source.
pub fn isFollowedByComma(parser: *Parser, node: ast.NodeIndex) bool {
    var peek = parser.beginPeek();
    defer peek.end();
    parser.lexer.rewindTo(parser.tree.span(node).end);
    return peek.next().tag == .comma;
}

pub const PatternContext = enum {
    /// Binding patterns, as in parameters and variable declarations.
    binding,
    /// Assignment targets.
    assignable,
};

/// Converts an expression node in place into a destructuring pattern allowed by `context`.
pub fn expressionToPattern(
    parser: *Parser,
    expr: ast.NodeIndex,
    comptime context: PatternContext,
) Error!void {
    const data = parser.tree.data(expr);

    if (context == .binding) {
        // including a paren the `.assignable` pass stripped from a legal `(a) = b`
        if (data == .parenthesized_expression or parser.state.stripped_paren == expr) {
            try parser.report(
                parser.tree.span(expr),
                "Parentheses are not allowed in this binding pattern",
                .{ .help = "Remove the extra parentheses. Binding patterns can only be" ++
                    " identifiers, destructuring patterns, or assignment patterns, not" ++
                    " parenthesized expressions." },
            );
            return;
        }
    }

    switch (data) {
        .identifier_reference => |id| {
            if (context == .binding) {
                parser.tree.setData(expr, .{ .binding_identifier = .{
                    .name = id.name,
                } });
            }
        },

        .assignment_expression => |assign| {
            if (assign.operator != .assign) {
                try parser.report(
                    parser.tree.span(expr),
                    if (context == .binding)
                        "Invalid assignment operator in binding pattern"
                    else
                        "Invalid assignment operator in assignment pattern",
                    .{ .help = "Only '=' is allowed in destructuring defaults, not" ++
                        " compound operators like '+='." },
                );
                return;
            }

            try expressionToPattern(parser, assign.left, context);

            parser.tree.setData(expr, .{ .assignment_pattern = .{
                .left = assign.left,
                .right = assign.right,
            } });
        },

        .array_expression => |arr| {
            try array.toArrayPattern(parser, expr, arr.elements, parser.tree.span(expr), context);
        },

        .object_expression => |obj| {
            try object.toObjectPattern(
                parser,
                expr,
                obj.properties,
                parser.tree.span(expr),
                context,
            );
        },

        .spread_element => |spread| {
            const arg = spread.argument;
            try expressionToPattern(parser, arg, context);

            if (parser.tree.data(arg) == .assignment_pattern) {
                try parser.report(
                    parser.tree.span(expr),
                    "A rest element cannot have an initializer",
                    .{ .help = "Remove the '= ...' from the rest element." },
                );
            }

            parser.tree.setData(expr, .{ .binding_rest_element = .{ .argument = arg } });
        },

        .chain_expression => {
            try parser.report(
                parser.tree.span(expr),
                if (context == .binding)
                    "Optional chaining is not allowed in binding pattern"
                else
                    "Optional chaining is not allowed in assignment pattern",
                .{ .help = "Optional chaining ('?.') cannot be used as an assignment target" ++
                    " in destructuring patterns." },
            );
        },

        .member_expression => {
            if (context != .assignable) {
                try parser.report(
                    parser.tree.span(expr),
                    "Member expression is not allowed in binding pattern",
                    .{ .help = "Function parameters and variable declarations can only bind" ++
                        " to identifiers, not member expressions like 'obj.prop' or" ++
                        " 'obj[key]'. Use a simple identifier instead." },
                );
            }
        },

        .ts_non_null_expression,
        .ts_as_expression,
        .ts_satisfies_expression,
        .ts_type_assertion,
        => {
            if (context != .assignable) {
                try parser.report(
                    parser.tree.span(expr),
                    "TypeScript assertion is not allowed in binding pattern",
                    .{ .help = "Non-null ('!') and type ('as', 'satisfies', '<T>')" ++
                        " assertions can only appear in assignment targets." },
                );
                return;
            }

            if (!expressions.isSimpleAssignmentTarget(parser, expr)) {
                try parser.report(
                    parser.tree.span(expr),
                    "Invalid assignment target",
                    .{ .help = "The expression behind a non-null or type assertion must" ++
                        " itself be a simple assignment target, like an identifier or" ++
                        " member access, not a call or other expression." },
                );
            }
        },

        .parenthesized_expression => |paren| {
            std.debug.assert(context == .assignable);

            if (!expressions.isSimpleAssignmentTarget(parser, paren.expression)) {
                try parser.report(
                    parser.tree.span(paren.expression),
                    "Parenthesized expression in assignment pattern must be a simple" ++
                        " assignment target",
                    .{ .help = "Only identifiers or member expressions (without optional" ++
                        " chaining) are allowed inside parentheses in assignment patterns." },
                );

                return;
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

        // `.binding` re-descends every target already converted at `=` under `.assignable`
        // rules. in `([a.b] = []) => {}` the array becomes an array_pattern at `=`, then at
        // `=>` it is a parameter where `a.b` and a stripped `(a)` are illegal and `a` must
        // become a declaring binding_identifier
        .assignment_pattern => |pattern| if (context == .binding) {
            try expressionToPattern(parser, pattern.left, context);
        },

        .array_pattern => |pattern| if (context == .binding) {
            for (parser.tree.extra(pattern.elements)) |element| {
                if (element == .null) continue;
                try expressionToPattern(parser, element, context);
            }
            if (pattern.rest != .null) {
                try expressionToPattern(parser, pattern.rest, context);
            }
        },

        .object_pattern => |pattern| if (context == .binding) {
            for (parser.tree.extra(pattern.properties)) |property| {
                const property_data = parser.tree.data(property);
                // a method or accessor keeps its object_property shape and is already diagnosed
                if (property_data != .binding_property) continue;
                try expressionToPattern(parser, property_data.binding_property.value, context);
            }
            if (pattern.rest != .null) {
                try expressionToPattern(parser, pattern.rest, context);
            }
        },

        .binding_rest_element => |rest| if (context == .binding) {
            try expressionToPattern(parser, rest.argument, context);
        },

        else => {
            try parser.report(
                parser.tree.span(expr),
                if (context == .binding)
                    "Invalid element in binding pattern"
                else
                    "Invalid element in assignment pattern",
                .{ .help = "Expected an identifier, array pattern, object pattern," ++
                    " or assignment pattern." },
            );
        },
    }
}
