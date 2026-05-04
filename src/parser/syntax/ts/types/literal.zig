const std = @import("std");
const ast = @import("../../../ast.zig");
const lexer = @import("../../../lexer.zig");
const Parser = @import("../../../parser.zig").Parser;
const Error = @import("../../../parser.zig").Error;

const literals = @import("../../literals.zig");
const core = @import("core.zig");

// 42   "foo"   true   -1
// ^^   ^^^^^   ^^^^   ^^
pub fn parseLiteralType(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    const start = token.span.start;

    const literal: ast.NodeIndex = switch (token.tag) {
        .true, .false => try literals.parseBooleanLiteral(parser) orelse return null,
        .string_literal => try literals.parseStringLiteral(parser) orelse return null,
        .numeric_literal, .hex_literal, .octal_literal, .binary_literal, .bigint_literal => try literals.parseNumericLiteral(parser) orelse return null,
        .no_substitution_template => try literals.parseNoSubstitutionTemplate(parser, false) orelse return null,
        .minus, .plus => try parseSignedNumericLiteralType(parser) orelse return null,
        else => unreachable,
    };

    return try parser.tree.addNode(
        .{ .ts_literal_type = .{ .literal = literal } },
        .{ .start = start, .end = parser.tree.span(literal).end },
    );
}

// unary plus or minus then numeric literal in a `UnaryExpression`
fn parseSignedNumericLiteralType(parser: *Parser) Error!?ast.NodeIndex {
    const sign_token = parser.current_token;
    const next = parser.peekAhead() orelse return null;
    if (!next.tag.isNumericLiteral()) return null;

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

// `Hello, ${World}!`
// ^^^^^^^^^^^^^^^^^^
pub fn parseTemplateLiteralType(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .template_head);

    const start = parser.current_token.span.start;

    const quasis_checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(quasis_checkpoint);
    const types_checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(types_checkpoint);

    const head = parser.current_token;
    try parser.scratch_a.append(
        parser.allocator(),
        try literals.addTemplateElement(parser, head, false, false),
    );
    try parser.advance() orelse return null;

    var end = head.span.end;

    while (true) {
        const ty = try core.parseType(parser) orelse return null;
        try parser.scratch_b.append(parser.allocator(), ty);

        // `}` rescanned so tail stays template not stray text
        if (parser.current_token.tag != .right_brace) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected '}' to close template type interpolation",
                .{ .help = "Template type interpolations must be closed with '}'" },
            );
            return null;
        }

        const right_brace = parser.current_token;
        const template_token = parser.lexer.reScanTemplateContinuation(right_brace.span.start) catch |e| {
            try parser.report(
                right_brace.span,
                lexer.getLexicalErrorMessage(e),
                .{ .help = lexer.getLexicalErrorHelp(e) },
            );
            return null;
        };

        const is_tail = template_token.tag == .template_tail;

        try parser.scratch_a.append(
            parser.allocator(),
            try literals.addTemplateElement(parser, template_token, is_tail, false),
        );

        end = template_token.span.end;
        try parser.advanceWithRescannedToken(template_token) orelse return null;

        if (is_tail) break;
    }

    return try parser.tree.addNode(
        .{ .ts_template_literal_type = .{
            .quasis = try parser.addExtraFromScratch(&parser.scratch_a, quasis_checkpoint),
            .types = try parser.addExtraFromScratch(&parser.scratch_b, types_checkpoint),
        } },
        .{ .start = start, .end = end },
    );
}
