const ast = @import("../ast.zig");
const lexer = @import("../lexer.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const expressions = @import("expressions.zig");

pub inline fn parseStringLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .string_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub inline fn parseBooleanLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .boolean_literal = .{ .value = token.type == .true },
    }, token.span);
}

pub inline fn parseNullLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.null_literal, token.span);
}

pub inline fn parseNumericLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .numeric_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub inline fn parseBigIntLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .bigint_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub fn parseRegExpLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    const regex = parser.lexer.reScanAsRegex(token) catch |e| {
        try parser.report(token.span, lexer.getLexicalErrorMessage(e), .{ .help = lexer.getLexicalErrorHelp(e) });
        return null;
    };
    try parser.replaceTokenAndAdvance(parser.lexer.createToken(
        .regex_literal,
        parser.source[regex.span.start..regex.span.end],
        regex.span.start,
        regex.span.end,
    ));
    return try parser.addNode(.{
        .regexp_literal = .{
            .pattern_start = @intCast(regex.span.start + 1),
            .pattern_len = @intCast(regex.pattern.len),
            .flags_start = @intCast(regex.span.end - regex.flags.len),
            .flags_len = @intCast(regex.flags.len),
        },
    }, regex.span);
}

pub fn parseNoSubstitutionTemplate(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    const element_span = getTemplateElementSpan(token);
    const element = try parser.addNode(.{
        .template_element = .{
            .raw_start = element_span.start,
            .raw_len = @intCast(element_span.end - element_span.start),
            .tail = true,
        },
    }, element_span);
    return try parser.addNode(.{
        .template_literal = .{
            .quasis = try parser.addExtra(&[_]ast.NodeIndex{element}),
            .expressions = ast.IndexRange.empty,
        },
    }, token.span);
}

pub fn parseTemplateLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const quasis_checkpoint = parser.scratch_a.begin();
    const exprs_checkpoint = parser.scratch_b.begin();

    const head = parser.current_token;
    const head_span = getTemplateElementSpan(head);

    try parser.scratch_a.append(parser.allocator(), try parser.addNode(.{
        .template_element = .{
            .raw_start = head_span.start,
            .raw_len = @intCast(head_span.end - head_span.start),
            .tail = false,
        },
    }, head_span));

    try parser.advance();

    var end: u32 = undefined;
    while (true) {
        const expr = try expressions.parseExpression(parser, 0, .{}) orelse return null;
        try parser.scratch_b.append(parser.allocator(), expr);

        const token = parser.current_token;
        const is_tail = token.type == .template_tail;

        switch (token.type) {
            .template_middle, .template_tail => {
                const span = getTemplateElementSpan(token);
                try parser.scratch_a.append(parser.allocator(), try parser.addNode(.{
                    .template_element = .{
                        .raw_start = span.start,
                        .raw_len = @intCast(span.end - span.start),
                        .tail = is_tail,
                    },
                }, span));

                if (is_tail) {
                    end = token.span.end;
                    try parser.advance();
                    break;
                }
                try parser.advance();
            },
            else => {
                try parser.report(
                    token.span,
                    "Unexpected token in template literal expression",
                    .{ .help = "Template expressions must be followed by '}' to continue the template string. Check for unmatched braces." },
                );
                parser.scratch_a.reset(quasis_checkpoint);
                parser.scratch_b.reset(exprs_checkpoint);
                return null;
            },
        }
    }

    return try parser.addNode(.{
        .template_literal = .{
            .quasis = try parser.addExtra(parser.scratch_a.take(quasis_checkpoint)),
            .expressions = try parser.addExtra(parser.scratch_b.take(exprs_checkpoint)),
        },
    }, .{ .start = start, .end = end });
}

inline fn getTemplateElementSpan(token: @import("../token.zig").Token) ast.Span {
    return switch (token.type) {
        .template_head, .template_middle => .{
            .start = token.span.start + 1,
            .end = token.span.end - 2,
        },
        .template_tail, .no_substitution_template => .{
            .start = token.span.start + 1,
            .end = token.span.end - 1,
        },
        else => unreachable,
    };
}

pub inline fn parseIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .identifier_reference = .{
            .name_start = token.span.start,
            .name_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub inline fn parsePrivateIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .private_identifier = .{
            .name_start = token.span.start,
            .name_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}
