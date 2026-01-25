const ast = @import("../ast.zig");
const lexer = @import("../lexer.zig");
const Token = @import("../token.zig").Token;
const Precedence = @import("../token.zig").Precedence;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const expressions = @import("expressions.zig");

pub fn parseStringLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.addNode(.{
        .string_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub fn parseBooleanLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.addNode(.{
        .boolean_literal = .{ .value = token.type == .true },
    }, token.span);
}

pub fn parseNullLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.addNode(.null_literal, token.span);
}

pub fn parseNumericLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.addNode(.{
        .numeric_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub fn parseBigIntLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.addNode(.{
        .bigint_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub fn parseRegExpLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    const regex = parser.lexer.reScanAsRegex(token.span.start) catch |e| {
        try parser.report(token.span, lexer.getLexicalErrorMessage(e), .{ .help = lexer.getLexicalErrorHelp(e) });
        return null;
    };

    try parser.advanceWithRescannedToken(parser.lexer.createToken(
        .regex_literal,
        parser.source[regex.span.start..regex.span.end],
        regex.span.start,
        regex.span.end,
    )) orelse return null;

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
    try parser.advance() orelse return null;
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
    defer parser.scratch_a.reset(quasis_checkpoint);
    const exprs_checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(exprs_checkpoint);

    const head = parser.current_token;
    const head_span = getTemplateElementSpan(head);

    try parser.scratch_a.append(parser.allocator(), try parser.addNode(.{
        .template_element = .{
            .raw_start = head_span.start,
            .raw_len = @intCast(head_span.end - head_span.start),
            .tail = false,
        },
    }, head_span));

    try parser.advance() orelse return null;

    var end: u32 = undefined;
    while (true) {
        const expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
        try parser.scratch_b.append(parser.allocator(), expr);

        // after parsing the expression, we expect '}' which closes the ${} substitution.
        // we need to explicitly scan for template continuation (middle or tail).
        if (parser.current_token.type != .right_brace) {
            try parser.report(
                parser.current_token.span,
                "Expected '}' to close template expression",
                .{ .help = "Template expressions must be closed with '}'" },
            );
            return null;
        }

        // now scan template continuation from right after right_brace

        const right_brace = parser.current_token;

        const template_token = parser.lexer.reScanTemplateContinuation(right_brace.span.start) catch |e| {
            try parser.report(right_brace.span, lexer.getLexicalErrorMessage(e), .{ .help = lexer.getLexicalErrorHelp(e) });
            return null;
        };

        const is_tail = template_token.type == .template_tail;
        const span = getTemplateElementSpan(template_token);

        try parser.scratch_a.append(parser.allocator(), try parser.addNode(.{
            .template_element = .{
                .raw_start = span.start,
                .raw_len = @intCast(span.end - span.start),
                .tail = is_tail,
            },
        }, span));

        if (is_tail) {
            end = template_token.span.end;
            try parser.advanceWithRescannedToken(template_token) orelse return null;
            break;
        }

        try parser.advanceWithRescannedToken(template_token) orelse return null;
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
    if (!try validateIdentifier(parser, "an identifier", parser.current_token)) return null;

    const tok = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.addNode(.{
        .identifier_reference = .{
            .name_start = tok.span.start,
            .name_len = @intCast(tok.lexeme.len),
        },
    }, tok.span);
}

pub inline fn parsePrivateIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.addNode(.{
        .private_identifier = .{
            .name_start = token.span.start,
            .name_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub fn parseIdentifierName(parser: *Parser) Error!?ast.NodeIndex {
    const tok = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.addNode(.{
        .identifier_name = .{
            .name_start = tok.span.start,
            .name_len = @intCast(tok.lexeme.len),
        },
    }, tok.span);
}

pub fn parseLabelIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    if (!try validateIdentifier(parser, "a label", parser.current_token)) return null;

    const current = parser.current_token;
    try parser.advance() orelse return null;

    return try parser.addNode(.{
        .label_identifier = .{
            .name_start = current.span.start,
            .name_len = @intCast(current.lexeme.len),
        },
    }, current.span);
}

pub inline fn validateIdentifier(parser: *Parser, comptime as_what: []const u8, token: Token) Error!bool {
    if (!token.type.isIdentifierLike()) {
        try parser.reportFmt(
            token.span,
            "Expected identifier {s}, found '{s}'",
            .{ as_what, parser.describeToken(token) },
            .{ .help = "Identifiers must start with a letter, underscore (_), or dollar sign ($)" },
        );

        return false;
    }

    if (token.type.isReserved()) {
        try parser.reportFmt(
            token.span,
            "'{s}' is a reserved word and cannot be used as {s}",
            .{ token.lexeme, as_what },
            .{},
        );

        return false;
    }

    if (token.type == .yield and parser.context.in_generator) {
        try parser.reportFmt(
            token.span,
            "Cannot use 'yield' as {s} in a generator context",
            .{as_what},
            .{},
        );

        return false;
    }

    return true;
}
