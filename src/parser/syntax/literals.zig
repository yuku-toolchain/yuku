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
            .raw_len = @intCast(token.len()),
        },
    }, token.span);
}

pub fn parseBooleanLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.addNode(.{
        .boolean_literal = .{ .value = token.tag == .true },
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

    // bigint literal is a separate node
    if (token.tag == .bigint_literal) {
        return try parser.addNode(.{
            .bigint_literal = .{
                .raw_start = token.span.start,
                .raw_len = @intCast(token.len()),
            },
        }, token.span);
    }

    return try parser.addNode(.{
        .numeric_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.len()),
            .kind = ast.NumericLiteral.Kind.fromToken(token.tag),
        },
    }, token.span);
}

pub fn parseRegExpLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    const regex = parser.lexer.reScanAsRegex(token.span.start) catch |e| {
        try parser.report(token.span, lexer.getLexicalErrorMessage(e), .{ .help = lexer.getLexicalErrorHelp(e) });
        return null;
    };

    try parser.advanceWithRescannedToken(parser.lexer.createToken(.regex_literal, regex.span.start, regex.span.end)) orelse return null;

    return try parser.addNode(.{
        .regexp_literal = .{
            .pattern_start = @intCast(regex.span.start + 1),
            .pattern_len = @intCast(regex.pattern.len),
            .flags_start = @intCast(regex.span.end - regex.flags.len),
            .flags_len = @intCast(regex.flags.len),
        },
    }, regex.span);
}

pub fn parseNoSubstitutionTemplate(parser: *Parser, tagged: bool) Error!?ast.NodeIndex {
    const tok = parser.current_token;

    const element = try addTemplateElement(parser, tok, true, tagged);

    try parser.advance() orelse return null;

    return try parser.addNode(.{
        .template_literal = .{
            .quasis = try parser.addExtra(&[_]ast.NodeIndex{element}),
            .expressions = ast.IndexRange.empty,
        },
    }, tok.span);
}

pub fn parseTemplateLiteral(parser: *Parser, tagged: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const quasis_checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(quasis_checkpoint);
    const exprs_checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(exprs_checkpoint);

    const head = parser.current_token;

    try parser.scratch_a.append(parser.allocator(), try addTemplateElement(parser, head, false, tagged));

    try parser.advance() orelse return null;

    var end = head.span.end;

    while (true) {
        const expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
        try parser.scratch_b.append(parser.allocator(), expr);

        // after parsing the expression, we expect '}' which closes the ${} substitution.
        // we need to explicitly scan for template continuation (middle or tail).
        if (parser.current_token.tag != .right_brace) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected '}' to close template expression",
                .{ .help = "Template expressions must be closed with '}'" },
            );
            return null;
        }

        const right_brace = parser.current_token;
        const template_token = parser.lexer.reScanTemplateContinuation(right_brace.span.start) catch |e| {
            try parser.report(right_brace.span, lexer.getLexicalErrorMessage(e), .{ .help = lexer.getLexicalErrorHelp(e) });
            return null;
        };

        const is_tail = template_token.tag == .template_tail;

        try parser.scratch_a.append(parser.allocator(), try addTemplateElement(
            parser,
            template_token,
            is_tail,
            tagged,
        ));

        end = template_token.span.end;

        try parser.advanceWithRescannedToken(template_token) orelse return null;

        if (is_tail) break;
    }

    return try parser.addNode(.{
        .template_literal = .{
            .quasis = try parser.addExtraFromScratch(&parser.scratch_a, quasis_checkpoint),
            .expressions = try parser.addExtraFromScratch(&parser.scratch_b, exprs_checkpoint),
        },
    }, .{ .start = start, .end = end });
}

inline fn addTemplateElement(parser: *Parser, tok: Token, tail: bool, tagged: bool) Error!ast.NodeIndex {
    const span = getTemplateElementSpan(tok);

    const is_cooked_undefined = tok.hasInvalidEscape();

    if (!tagged and is_cooked_undefined) {
        try parser.report(span, "Bad escape sequence in untagged template literal", .{});
    }

    return parser.addNode(.{
        .template_element = .{
            .raw_start = span.start,
            .raw_len = @intCast(span.end - span.start),
            .tail = tail,
            .is_cooked_undefined = is_cooked_undefined,
        },
    }, span);
}

inline fn getTemplateElementSpan(token: @import("../token.zig").Token) ast.Span {
    return switch (token.tag) {
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
            .name_len = @intCast(tok.len()),
        },
    }, tok.span);
}

pub inline fn parsePrivateIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.addNode(.{
        .private_identifier = .{
            .name_start = token.span.start,
            .name_len = @intCast(token.len()),
        },
    }, token.span);
}

pub fn parseIdentifierName(parser: *Parser) Error!?ast.NodeIndex {
    const tok = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.addNode(.{
        .identifier_name = .{
            .name_start = tok.span.start,
            .name_len = @intCast(tok.len()),
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
            .name_len = @intCast(current.len()),
        },
    }, current.span);
}

pub inline fn validateIdentifier(parser: *Parser, comptime as_what: []const u8, token: Token) Error!bool {
    if (!token.tag.isIdentifierLike()) {
        try parser.reportExpected(
            token.span,
            "Expected an identifier",
            .{ .help = "Identifiers must start with a letter, underscore (_), or dollar sign ($)" },
        );

        return false;
    }

    if (token.tag.isUnconditionallyReserved()) {
        try parser.reportFmt(
            token.span,
            "'{s}' is a reserved word and cannot be used as {s}",
            .{ parser.getTokenText(token), as_what },
            .{},
        );

        return false;
    }

    if (token.tag == .yield and parser.context.yield_is_keyword) {
        try parser.reportFmt(
            token.span,
            "Cannot use 'yield' as {s} in a generator context",
            .{as_what},
            .{},
        );

        return false;
    }

    if (token.tag == .await and (parser.context.in_async or parser.isModule())) {
        try parser.reportFmt(
            token.span,
            "Cannot use `await` as {s} in an async or module context",
            .{as_what},
            .{},
        );

        return false;
    }

    return true;
}
