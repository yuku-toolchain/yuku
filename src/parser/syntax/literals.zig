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
    return try parser.createNode(.{
        .string_literal = .{
            .raw = try parser.builder.internString(parser.getTokenText(token)),
        },
    }, token.span);
}

pub fn parseBooleanLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.createNode(.{
        .boolean_literal = .{ .value = token.tag == .true },
    }, token.span);
}

pub fn parseNullLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.createNode(.{ .null_literal = .{} }, token.span);
}

pub fn parseNumericLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;

    // bigint literal is a separate node
    if (token.tag == .bigint_literal) {
        return try parser.createNode(.{
            .bigint_literal = .{
                .raw = try parser.builder.internString(parser.getTokenText(token)),
            },
        }, token.span);
    }

    return try parser.createNode(.{
        .numeric_literal = .{
            .raw = try parser.builder.internString(parser.getTokenText(token)),
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

    return try parser.createNode(.{
        .regexp_literal = .{
            .pattern = try parser.builder.internString(parser.source[regex.span.start + 1 ..][0..regex.pattern.len]),
            .flags = try parser.builder.internString(parser.source[regex.span.end - regex.flags.len ..][0..regex.flags.len]),
        },
    }, regex.span);
}

pub fn parseNoSubstitutionTemplate(parser: *Parser, tagged: bool) Error!?ast.NodeIndex {
    const token = parser.current_token;

    const element = try addTemplateElement(parser, token, true, tagged);

    try parser.advance() orelse return null;

    return try parser.createNode(.{
        .template_literal = .{
            .quasis = try parser.createExtra(&[_]ast.NodeIndex{element}),
            .expressions = ast.IndexRange.empty,
        },
    }, token.span);
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

    return try parser.createNode(.{
        .template_literal = .{
            .quasis = try parser.createExtraFromScratch(&parser.scratch_a, quasis_checkpoint),
            .expressions = try parser.createExtraFromScratch(&parser.scratch_b, exprs_checkpoint),
        },
    }, .{ .start = start, .end = end });
}

inline fn addTemplateElement(parser: *Parser, token: Token, tail: bool, tagged: bool) Error!ast.NodeIndex {
    const span = getTemplateElementSpan(token);

    const is_cooked_undefined = token.hasInvalidEscape();

    if (!tagged and is_cooked_undefined) {
        try parser.report(span, "Bad escape sequence in untagged template literal", .{});
    }

    return parser.createNode(.{
        .template_element = .{
            .raw = try parser.builder.internString(parser.source[span.start..span.end]),
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

    const token = parser.current_token;

    try parser.advanceWithoutEscapeCheck() orelse return null;

    return try parser.createNode(.{
        .identifier_reference = .{
            .name = try parser.builder.internString(parser.getTokenText(token)),
        },
    }, token.span);
}

pub inline fn parsePrivateIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    try parser.advance() orelse return null;

    return try parser.createNode(.{
        .private_identifier = .{
            .name = try parser.builder.internString(parser.source[token.span.start + 1 .. token.span.end]),
        },
    }, token.span);
}

pub fn parseIdentifierName(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    try parser.advanceWithoutEscapeCheck() orelse return null;

    return try parser.createNode(.{
        .identifier_name = .{
            .name = try parser.builder.internString(parser.getTokenText(token)),
        },
    }, token.span);
}

pub fn parseLabelIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    if (!try validateIdentifier(parser, "a label", parser.current_token)) return null;

    const current = parser.current_token;
    try parser.advance() orelse return null;

    return try parser.createNode(.{
        .label_identifier = .{
            .name = try parser.builder.internString(parser.getTokenText(current)),
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
            .{ parser.describeToken(token), as_what },
            .{},
        );

        return false;
    }

    if (token.tag.isStrictModeReserved() and parser.isStrictMode()) {
        try parser.reportFmt(
            token.span,
            "'{s}' is reserved in strict mode and cannot be used as {s}",
            .{ parser.describeToken(token), as_what },
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

    if (token.tag == .await and parser.context.await_is_keyword) {
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
