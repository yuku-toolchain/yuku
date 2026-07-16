const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;
const ast = @import("ast.zig");

const object = @import("syntax/object.zig");
const expressions = @import("syntax/expressions.zig");
const array = @import("syntax/array.zig");

/// parse an expression within a cover grammar context without validation.
/// validation is deferred until the top-level context is known.
pub inline fn parseExpressionInCover(parser: *Parser, precedence: u8) Error!?ast.NodeIndex {
    return expressions.parseExpression(parser, precedence, .{ .in_cover = true });
}

/// validate that an expression doesn't contain CoverInitializedName.
/// only called when `state.cover_has_init_name` is set during parsing,
/// so the recursive walk here is bounded to cases that actually need it.
pub fn validateNoCoverInitializedSyntax(parser: *Parser, expr: ast.NodeIndex) Error!void {
    const data = parser.tree.data(expr);

    switch (data) {
        .object_expression => |obj| {
            const properties = parser.tree.extra(obj.properties);
            for (properties) |prop| {
                if (prop == .null) continue;

                const prop_data = parser.tree.data(prop);

                switch (prop_data) {
                    .object_property => |obj_prop| {
                        if (obj_prop.shorthand and isCoverInitializedName(parser, obj_prop.value)) {
                            try reportCoverInitializedNameError(parser, prop);
                        }

                        try validateNoCoverInitializedSyntax(parser, obj_prop.value);
                    },
                    .spread_element => |spread| {
                        try validateNoCoverInitializedSyntax(parser, spread.argument);
                    },
                    else => {},
                }
            }
        },
        .array_expression => |arr| {
            const elements = parser.tree.extra(arr.elements);
            for (elements) |elem| {
                if (elem == .null) continue;
                try validateNoCoverInitializedSyntax(parser, elem);
            }
        },
        .object_property => |obj_prop| {
            if (obj_prop.shorthand and isCoverInitializedName(parser, obj_prop.value)) {
                try reportCoverInitializedNameError(parser, expr);
            }
        },
        .spread_element => |spread| {
            return validateNoCoverInitializedSyntax(parser, spread.argument);
        },
        .parenthesized_expression => |paren| {
            return validateNoCoverInitializedSyntax(parser, paren.expression);
        },
        .sequence_expression => |seq| {
            for (parser.tree.extra(seq.expressions)) |e| {
                try validateNoCoverInitializedSyntax(parser, e);
            }
        },
        else => {},
    }
}

/// check if a node is a CoverInitializedName (assignment expression with = operator).
/// CoverInitializedName: { a = 1 } where the value is AssignmentExpression
pub inline fn isCoverInitializedName(parser: *Parser, node: ast.NodeIndex) bool {
    const data = parser.tree.data(node);
    return data == .assignment_expression and data.assignment_expression.operator == .assign;
}

pub inline fn reportCoverInitializedNameError(parser: *Parser, node: ast.NodeIndex) Error!void {
    try parser.report(
        parser.tree.span(node),
        "Shorthand property cannot have a default value in object expression",
        .{ .help = "Use '{ a: a = 1 }' syntax or this is only valid in destructuring patterns." },
    );
}

pub const PatternContext = enum {
    /// binding patterns for function parameters, variable declarations, etc.
    binding,
    /// assignable patterns for assignment expressions.
    assignable,
};

/// convert an expression node to a destructuring pattern (mutates in-place).
/// the context determines what syntax is allowed.
pub fn expressionToPattern(
    parser: *Parser,
    expr: ast.NodeIndex,
    comptime context: PatternContext,
) Error!void {
    // `(a) = b` is a valid assignment, so the `.assignable` pass stripped
    // the paren in-place. in binding position that paren is a syntax error,
    // visible only through the recorded evidence.
    if (context == .binding and parser.state.stripped_paren == expr) {
        try parser.report(
            parser.tree.span(expr),
            "Parentheses are not allowed in this binding pattern",
            .{ .help = "Remove the extra parentheses. Binding patterns can only be" ++
                " identifiers, destructuring patterns, or assignment patterns, not" ++
                " parenthesized expressions." },
        );
        return;
    }

    const data = parser.tree.data(expr);

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
            if (context != .assignable) {
                try parser.report(
                    parser.tree.span(expr),
                    "Parentheses are not allowed in this binding pattern",
                    .{ .help = "Remove the extra parentheses. Binding patterns can only be" ++
                        " identifiers, destructuring patterns, or assignment patterns, not" ++
                        " parenthesized expressions." },
                );
                return;
            }

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

            // recurse for nested patterns (`(({a = 1}))`)
            switch (parser.tree.data(paren.expression)) {
                .ts_as_expression,
                .ts_satisfies_expression,
                .ts_type_assertion,
                .ts_non_null_expression,
                => {},
                else => try expressionToPattern(parser, paren.expression, context),
            }

            // strip the parenthesized wrapper, assignment targets don't
            // preserve outer parens regardless of `preserveParens`. record
            // the node so a `.binding` pass can still reject the paren
            // the tree no longer shows.
            parser.state.stripped_paren = expr;
            parser.tree.setData(expr, parser.tree.data(paren.expression));
            parser.tree.setSpan(expr, parser.tree.span(paren.expression));
        },

        .binding_identifier => {},

        // this pattern was already converted at its `=` under the looser
        // `.assignable` rules, while "assignment or arrow head?" was
        // still unknown:
        //
        //   ([a.b] = []) => {}
        //   at `=`  : valid as an assignment, array becomes array_pattern
        //   at `=>` : the same node is now a parameter, where `a.b` is
        //             illegal, a stripped `(a)` is illegal, and `a` must
        //             become a declaring binding_identifier
        //
        // so `.binding` re-descends every target and judges it again.
        // defaults and computed keys stay expressions and are skipped.
        // `.assignable` again has nothing stricter to add: no-op.
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
                // a property the `.assignable` pass could not convert (a
                // method or getter/setter, e.g. `({ get x() {} } = ...)`)
                // was reported back then and kept its object_property
                // shape. skip it, only binding_property has a converted
                // value, and its error is already on record.
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
