const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;

const TsrxNodeWalkDepthMax: u32 = 64;

pub fn validateExpression(
    parser: *Parser,
    container: ast.NodeIndex,
) Error!void {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(container != .null);
    std.debug.assert(parser.tree.data(container) == .jsx_expression_container);

    if (isValidTsrxDynamicTagExpression(parser, container)) return;

    try parser.report(
        parser.tree.span(container),
        "TSRX dynamic tag expression must resolve to an element name",
        .{ .help = "Use an identifier, member expression, string literal, or conditional." },
    );
}

fn isValidTsrxDynamicTagExpression(
    parser: *const Parser,
    container: ast.NodeIndex,
) bool {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(container != .null);
    std.debug.assert(parser.tree.data(container) == .jsx_expression_container);

    var expression = parser.tree.data(container).jsx_expression_container.expression;
    var depth: u32 = 0;
    while (depth < TsrxNodeWalkDepthMax) : (depth += 1) {
        if (expression == .null) return false;
        switch (parser.tree.data(expression)) {
            .ts_as_expression => |node| expression = node.expression,
            .ts_type_assertion => |node| expression = node.expression,
            .ts_non_null_expression => |node| expression = node.expression,
            .parenthesized_expression => |node| expression = node.expression,
            .chain_expression => |node| expression = node.expression,
            else => break,
        }
    }

    if (depth == TsrxNodeWalkDepthMax) return false;
    std.debug.assert(expression != .null);

    return switch (parser.tree.data(expression)) {
        .identifier_reference => |node| !std.mem.eql(
            u8,
            parser.tree.string(node.name),
            "undefined",
        ),
        .string_literal => true,
        .template_literal => |node| node.expressions.len == 0,
        .numeric_literal,
        .bigint_literal,
        .boolean_literal,
        .null_literal,
        .regexp_literal,
        .jsx_element,
        .jsx_fragment,
        .jsx_style_element,
        .jsx_code_block,
        .jsx_if_expression,
        .jsx_for_expression,
        .jsx_switch_expression,
        .jsx_try_expression,
        .jsx_expression_container,
        .jsx_empty_expression,
        .jsx_text,
        .jsx_spread_child,
        => false,
        .unary_expression => |node| if (node.operator == .void)
            false
        else
            !tsrxDynamicTagContainsDisallowedSyntax(parser, expression, 0),
        else => !tsrxDynamicTagContainsDisallowedSyntax(parser, expression, 0),
    };
}

fn tsrxDynamicTagContainsDisallowedSyntax(
    parser: *const Parser,
    node: ast.NodeIndex,
    depth: u32,
) bool {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(depth <= TsrxNodeWalkDepthMax);

    if (node == .null) return false;
    if (depth == TsrxNodeWalkDepthMax) return true;

    const next_depth = depth + 1;
    return switch (parser.tree.data(node)) {
        .spread_element,
        .object_expression,
        .array_expression,
        .call_expression,
        .new_expression,
        .tagged_template_expression,
        .import_expression,
        .jsx_element,
        .jsx_fragment,
        .jsx_style_element,
        .jsx_code_block,
        .jsx_if_expression,
        .jsx_for_expression,
        .jsx_switch_expression,
        .jsx_try_expression,
        .jsx_expression_container,
        .jsx_empty_expression,
        .jsx_text,
        .jsx_spread_child,
        => true,
        .template_literal => |template| template.expressions.len > 0,
        .binary_expression => |expr| if (expr.operator == .add)
            true
        else
            tsrxDynamicTagContainsDisallowedSyntax(parser, expr.left, next_depth) or
                tsrxDynamicTagContainsDisallowedSyntax(parser, expr.right, next_depth),
        .logical_expression => |expr| tsrxDynamicTagContainsDisallowedSyntax(
            parser,
            expr.left,
            next_depth,
        ) or tsrxDynamicTagContainsDisallowedSyntax(parser, expr.right, next_depth),
        .conditional_expression => |expr| tsrxDynamicTagContainsDisallowedSyntax(
            parser,
            expr.@"test",
            next_depth,
        ) or tsrxDynamicTagContainsDisallowedSyntax(
            parser,
            expr.consequent,
            next_depth,
        ) or tsrxDynamicTagContainsDisallowedSyntax(parser, expr.alternate, next_depth),
        .unary_expression => |expr| if (expr.operator == .void)
            true
        else
            tsrxDynamicTagContainsDisallowedSyntax(parser, expr.argument, next_depth),
        .update_expression => |expr| tsrxDynamicTagContainsDisallowedSyntax(
            parser,
            expr.argument,
            next_depth,
        ),
        .assignment_expression => |expr| tsrxDynamicTagContainsDisallowedSyntax(
            parser,
            expr.left,
            next_depth,
        ) or tsrxDynamicTagContainsDisallowedSyntax(parser, expr.right, next_depth),
        .sequence_expression => |expr| tsrxDynamicTagRangeContainsDisallowedSyntax(
            parser,
            expr.expressions,
            next_depth,
        ),
        .member_expression => |expr| tsrxDynamicTagContainsDisallowedSyntax(
            parser,
            expr.object,
            next_depth,
        ) or tsrxDynamicTagContainsDisallowedSyntax(parser, expr.property, next_depth),
        .chain_expression => |expr| tsrxDynamicTagContainsDisallowedSyntax(
            parser,
            expr.expression,
            next_depth,
        ),
        .await_expression => |expr| tsrxDynamicTagContainsDisallowedSyntax(
            parser,
            expr.argument,
            next_depth,
        ),
        .yield_expression => |expr| tsrxDynamicTagContainsDisallowedSyntax(
            parser,
            expr.argument,
            next_depth,
        ),
        .parenthesized_expression => |expr| tsrxDynamicTagContainsDisallowedSyntax(
            parser,
            expr.expression,
            next_depth,
        ),
        inline .ts_as_expression,
        .ts_satisfies_expression,
        .ts_type_assertion,
        .ts_non_null_expression,
        .ts_instantiation_expression,
        => |expr| tsrxDynamicTagContainsDisallowedSyntax(
            parser,
            expr.expression,
            next_depth,
        ),
        else => false,
    };
}

fn tsrxDynamicTagRangeContainsDisallowedSyntax(
    parser: *const Parser,
    nodes: ast.IndexRange,
    depth: u32,
) bool {
    std.debug.assert(parser.tree.isTsrx());
    std.debug.assert(depth <= TsrxNodeWalkDepthMax);
    std.debug.assert(nodes.start + nodes.len <= parser.tree.extras.items.len);

    for (parser.tree.extra(nodes)) |node| {
        if (tsrxDynamicTagContainsDisallowedSyntax(parser, node, depth)) return true;
    }
    return false;
}
