const ast = @import("../../../ast.zig");
const Parser = @import("../../../parser.zig").Parser;
const Error = @import("../../../parser.zig").Error;

const literals = @import("../../literals.zig");
const core = @import("core.zig");

pub fn parseLiteralType(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    const literal: ast.NodeIndex = switch (token.tag) {
        .true, .false => try literals.parseBooleanLiteral(parser) orelse return null,
        .string_literal => try literals.parseStringLiteral(parser) orelse return null,
        .numeric_literal,
        .hex_literal,
        .octal_literal,
        .binary_literal,
        .bigint_literal,
        => try literals.parseNumericLiteral(parser) orelse return null,
        .no_substitution_template => try literals.parseNoSubstitutionTemplate(parser, false) orelse
            return null,
        .minus, .plus => try parseSignedNumericLiteralType(parser) orelse return null,
        else => unreachable,
    };

    return try parser.tree.addNode(
        .{ .ts_literal_type = .{ .literal = literal } },
        .{ .start = token.span.start, .end = parser.tree.span(literal).end },
    );
}

fn parseSignedNumericLiteralType(parser: *Parser) Error!?ast.NodeIndex {
    const sign_token = parser.current_token;
    if (!parser.peekAhead().tag.isNumericLiteral()) return null;

    try parser.advance() orelse return null;
    const arg = try literals.parseNumericLiteral(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .unary_expression = .{
            .argument = arg,
            .operator = if (sign_token.tag == .minus) .negate else .positive,
        } },
        .{ .start = sign_token.span.start, .end = parser.tree.span(arg).end },
    );
}

pub fn parseTemplateLiteralType(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const parts = try literals.parseTemplateParts(
        parser,
        false,
        "type interpolation",
        core.parseType,
    ) orelse return null;

    return try parser.tree.addNode(
        .{ .ts_template_literal_type = .{ .quasis = parts.quasis, .types = parts.substitutions } },
        .{ .start = start, .end = parts.end },
    );
}
