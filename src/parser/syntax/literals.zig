const std = @import("std");
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
    return try parser.tree.createNode(.{
        .string_literal = .{
            .value = try parser.stringValue(token),
        },
    }, token.span);
}

pub fn parseBooleanLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.tree.createNode(.{
        .boolean_literal = .{ .value = token.tag == .true },
    }, token.span);
}

pub fn parseNullLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.tree.createNode(.{ .null_literal = .{} }, token.span);
}

pub fn parseNumericLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;

    // bigint literal is a separate node
    if (token.tag == .bigint_literal) {
        return try parser.tree.createNode(.{
            .bigint_literal = .{
                .value = parser.tree.sourceSlice(token.span.start, token.span.end - 1),
            },
        }, token.span);
    }

    const kind = ast.NumericLiteral.Kind.fromToken(token.tag);
    const raw = parser.source[token.span.start..token.span.end];
    return try parser.tree.createNode(.{
        .numeric_literal = .{
            .kind = kind,
            .value = parseNumericValue(raw, kind),
        },
    }, token.span);
}

fn parseNumericValue(raw: []const u8, kind: ast.NumericLiteral.Kind) f64 {
    // strip numeric separators
    var buf: [128]u8 = undefined;
    var len: usize = 0;
    for (raw) |c| {
        if (c != '_') {
            if (len >= buf.len) return 0;
            buf[len] = c; len += 1;
        }
    }
    const s = buf[0..len];
    if (s.len == 0) return 0;
    return switch (kind) {
        .decimal => std.fmt.parseFloat(f64, s) catch 0,
        .hex => parseIntOrFloat(s[2..], 16),
        .octal => blk: {
            // modern: 0o/0O prefix; legacy: bare 0 prefix
            const digits = if (s.len >= 2 and (s[1] == 'o' or s[1] == 'O')) s[2..] else s[1..];
            break :blk parseIntOrFloat(digits, 8);
        },
        .binary => parseIntOrFloat(s[2..], 2),
    };
}

fn parseIntOrFloat(digits: []const u8, base: u8) f64 {
    const v = std.fmt.parseInt(u64, digits, base) catch {
        // overflow: accumulate into f64 for correct IEEE 754 approximation
        var val: f64 = 0;
        const fbase: f64 = @floatFromInt(base);
        for (digits) |d| {
            val = val * fbase + @as(f64, @floatFromInt(std.fmt.charToDigit(d, base) catch unreachable));
        }
        return val;
    };
    return @floatFromInt(v);
}

pub fn parseRegExpLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;

    const regex = parser.lexer.reScanAsRegex(token.span.start) catch |e| {
        try parser.report(token.span, lexer.getLexicalErrorMessage(e), .{ .help = lexer.getLexicalErrorHelp(e) });
        return null;
    };

    try parser.advanceWithRescannedToken(parser.lexer.createToken(.regex_literal, regex.span.start, regex.span.end)) orelse return null;

    const pattern_start = regex.span.start + 1;
    const pattern_end = pattern_start + @as(u32, @intCast(regex.pattern.len));
    const flags_start = regex.span.end - @as(u32, @intCast(regex.flags.len));
    const flags_end = regex.span.end;

    return try parser.tree.createNode(.{
        .regexp_literal = .{
            .pattern = parser.tree.sourceSlice(pattern_start, pattern_end),
            .flags = parser.tree.sourceSlice(flags_start, flags_end),
        },
    }, regex.span);
}

pub fn parseNoSubstitutionTemplate(parser: *Parser, tagged: bool) Error!?ast.NodeIndex {
    const token = parser.current_token;

    const element = try addTemplateElement(parser, token, true, tagged);

    try parser.advance() orelse return null;

    return try parser.tree.createNode(.{
        .template_literal = .{
            .quasis = try parser.tree.createExtra(&[_]ast.NodeIndex{element}),
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

    return try parser.tree.createNode(.{
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

    const cooked: ast.String = if (is_cooked_undefined)
        .empty
    else
        try parser.templateElementValue(token, span);

    return parser.tree.createNode(.{
        .template_element = .{
            .cooked = cooked,
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
    try validateIdentifier(parser, "an identifier", parser.current_token);

    const token = parser.current_token;

    try parser.advanceWithoutEscapeCheck() orelse return null;

    return try parser.tree.createNode(.{
        .identifier_reference = .{ .name = try parser.identifierName(token) },
    }, token.span);
}

pub inline fn parseBindingIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    try validateIdentifier(parser, "a binding identifier", parser.current_token);

    const current = parser.current_token;

    try parser.advanceWithoutEscapeCheck() orelse return null;

    return try parser.tree.createNode(
        .{ .binding_identifier = .{ .name = try parser.identifierName(current) } },
        current.span,
    );
}

pub inline fn parsePrivateIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance() orelse return null;

    return try parser.tree.createNode(.{
        .private_identifier = .{ .name = try parser.identifierName(token) },
    }, token.span);
}

pub fn parseIdentifierName(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advanceWithoutEscapeCheck() orelse return null;

    return try parser.tree.createNode(.{
        .identifier_name = .{ .name = try parser.identifierName(token) },
    }, token.span);
}

pub fn parseLabelIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    try validateIdentifier(parser, "a label", parser.current_token);

    const current = parser.current_token;
    try parser.advance() orelse return null;

    return try parser.tree.createNode(.{
        .label_identifier = .{ .name = try parser.identifierName(current) },
    }, current.span);
}

pub inline fn validateIdentifier(parser: *Parser, comptime as_what: []const u8, token: Token) Error!void {
    if (!token.tag.isIdentifierLike()) {
        try parser.reportExpected(
            token.span,
            "Expected an identifier",
            .{ .help = "Identifiers must start with a letter, underscore (_), or dollar sign ($)" },
        );
    }

    if (token.tag.isUnconditionallyReserved()) {
        try parser.report(
            token.span,
            try parser.fmt("'{s}' is reserved and cannot be used as " ++ as_what, .{parser.describeToken(token)}),
            .{},
        );
    }

    if (token.tag == .yield and parser.context.yield_is_keyword) {
        try parser.report(
            token.span,
            "'yield' is reserved in a generator context and cannot be used as " ++ as_what,
            .{},
        );
    }

    if (token.tag == .await and (parser.context.await_is_keyword or parser.tree.isModule())) {
        try parser.report(
            token.span,
            "'await' is reserved in an async/module context and cannot be used as " ++ as_what,
            .{},
        );
    }
}
