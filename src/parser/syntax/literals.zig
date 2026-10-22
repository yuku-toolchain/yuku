const std = @import("std");
const ast = @import("../ast.zig");
const Token = @import("../token.zig").Token;
const Precedence = @import("../token.zig").Precedence;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const expressions = @import("expressions.zig");

pub fn parseStringLiteral(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .string_literal) {
        try parser.reportExpected(parser.current_token.span, "Expected a string literal", .{});
        return null;
    }
    const token = parser.current_token;
    try parser.advance() orelse return null;
    const value_span: ast.Span = .{ .start = token.span.start + 1, .end = token.span.end - 1 };
    return try parser.tree.addNode(.{ .string_literal = .{
        .value = try parser.stringValue(token, value_span),
        .raw = parser.tree.sourceSlice(token.span.start, token.span.end),
    } }, token.span);
}

pub fn parseBooleanLiteral(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .true or parser.current_token.tag == .false);
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.tree.addNode(.{
        .boolean_literal = .{ .value = token.tag == .true },
    }, token.span);
}

pub fn parseNullLiteral(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .null_literal);
    const token = parser.current_token;
    try parser.advance() orelse return null;
    return try parser.tree.addNode(.{ .null_literal = .{} }, token.span);
}

pub fn parseNumericLiteral(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag.isNumericLiteral());
    const token = parser.current_token;
    try parser.advance() orelse return null;

    if (token.tag == .bigint_literal) {
        return try parser.tree.addNode(.{ .bigint_literal = .{
            .raw = parser.tree.sourceSlice(token.span.start, token.span.end - 1),
        } }, token.span);
    }

    return try parser.tree.addNode(.{ .numeric_literal = .{
        .kind = ast.NumericLiteral.Kind.fromToken(token.tag),
        .raw = parser.tree.sourceSlice(token.span.start, token.span.end),
    } }, token.span);
}

pub fn parseRegExpLiteral(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .slash or
        parser.current_token.tag == .slash_assign);
    const token = parser.current_token;

    const regex = parser.lexer.reScanAsRegex(token.span.start) catch |e| {
        try parser.reportLexicalError(e);
        return null;
    };

    const regex_token = parser.lexer.createToken(.regex_literal, regex.span.start, regex.span.end);
    try parser.advanceWithRescannedToken(regex_token) orelse return null;

    const pattern_start = regex.span.start + 1;
    const pattern_end = pattern_start + @as(u32, @intCast(regex.pattern.len));
    const flags_start = regex.span.end - @as(u32, @intCast(regex.flags.len));
    try parseRegExpLiteralFlags(parser, flags_start, regex.flags);

    return try parser.tree.addNode(.{ .regexp_literal = .{
        .pattern = parser.tree.sourceSlice(pattern_start, pattern_end),
        .flags = parser.tree.sourceSlice(flags_start, regex.span.end),
    } }, regex.span);
}

fn parseRegExpLiteralFlags(parser: *Parser, flags_start: u32, flags: []const u8) Error!void {
    std.debug.assert(flags_start + flags.len <= parser.source.len);
    const unicode_modes = regExpFlagBit('u') | regExpFlagBit('v');
    var seen: u32 = 0;
    var index: u32 = 0;
    while (index < flags.len) {
        const len: u32 = std.unicode.utf8ByteSequenceLength(flags[index]) catch unreachable;
        const span: ast.Span = .{ .start = flags_start + index, .end = flags_start + index + len };
        const bit = regExpFlagBit(flags[index]);
        index += len;

        if (bit == 0) {
            try parser.report(span, "Invalid regular expression flag", .{
                .help = "Valid flags are `d`, `g`, `i`, `m`, `s`, `u`, `v`, and `y`.",
            });
        } else if (seen & bit != 0) {
            try parser.report(span, "Duplicate regular expression flag", .{
                .help = "Each flag can appear only once.",
            });
        } else if ((seen | bit) & unicode_modes == unicode_modes) {
            try parser.report(
                span,
                "The 'u' and 'v' regular expression flags cannot be used together",
                .{ .help = "Use `v`, which extends `u`, or `u` alone." },
            );
        }
        seen |= bit;
    }
    std.debug.assert(index == flags.len);
}

fn regExpFlagBit(flag: u8) u32 {
    return switch (flag) {
        'd', 'g', 'i', 'm', 's', 'u', 'v', 'y' => @as(u32, 1) << @intCast(flag - 'a'),
        else => 0,
    };
}

pub fn parseNoSubstitutionTemplate(parser: *Parser, tagged: bool) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .no_substitution_template);
    const token = parser.current_token;

    const element = try addTemplateElement(parser, token, true, tagged);
    try parser.advance() orelse return null;

    return try parser.tree.addNode(.{ .template_literal = .{
        .quasis = try parser.tree.addExtra(&.{element}),
        .expressions = ast.IndexRange.empty,
    } }, token.span);
}

pub fn parseTemplateLiteral(parser: *Parser, tagged: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const parts = try parseTemplateParts(
        parser,
        tagged,
        "expression",
        parseTemplateSubstitution,
    ) orelse return null;

    return try parser.tree.addNode(.{
        .template_literal = .{ .quasis = parts.quasis, .expressions = parts.substitutions },
    }, .{ .start = start, .end = parts.end });
}

fn parseTemplateSubstitution(parser: *Parser) Error!?ast.NodeIndex {
    return expressions.parseExpression(parser, Precedence.Lowest, .{});
}

pub const TemplateParts = struct {
    quasis: ast.IndexRange,
    substitutions: ast.IndexRange,
    end: u32,
};

pub fn parseTemplateParts(
    parser: *Parser,
    tagged: bool,
    comptime noun: []const u8,
    comptime parseSubstitution: fn (*Parser) Error!?ast.NodeIndex,
) Error!?TemplateParts {
    std.debug.assert(parser.current_token.tag == .template_head);

    const quasis_checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(quasis_checkpoint);
    const substitutions_checkpoint = parser.scratch_b.begin();
    defer parser.scratch_b.reset(substitutions_checkpoint);

    const head = parser.current_token;
    try parser.scratch_a.append(
        parser.allocator(),
        try addTemplateElement(parser, head, false, tagged),
    );
    try parser.advance() orelse return null;

    var end = head.span.end;
    while (true) {
        const substitution = try parseSubstitution(parser) orelse return null;
        try parser.scratch_b.append(parser.allocator(), substitution);

        // the lexer emits `}` as a punctuator, rescan it as template continuation
        if (parser.current_token.tag != .right_brace) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected '}' to close template " ++ noun,
                .{ .help = "Template " ++ noun ++ "s must be closed with '}'" },
            );
            return null;
        }

        const template_token = parser.lexer.reScanTemplateContinuation(
            parser.current_token.span.start,
        ) catch |e| {
            try parser.reportLexicalError(e);
            return null;
        };
        const is_tail = template_token.tag == .template_tail;

        try parser.scratch_a.append(
            parser.allocator(),
            try addTemplateElement(parser, template_token, is_tail, tagged),
        );
        end = template_token.span.end;
        try parser.advanceWithRescannedToken(template_token) orelse return null;

        if (is_tail) break;
    }

    return .{
        .quasis = try parser.flushToExtras(&parser.scratch_a, quasis_checkpoint),
        .substitutions = try parser.flushToExtras(&parser.scratch_b, substitutions_checkpoint),
        .end = end,
    };
}

inline fn addTemplateElement(
    parser: *Parser,
    token: Token,
    tail: bool,
    tagged: bool,
) Error!ast.NodeIndex {
    const span = templateElementSpan(token);
    const is_cooked_undefined = token.hasInvalidEscape();

    if (!tagged and is_cooked_undefined) {
        try parser.report(span, "Bad escape sequence in untagged template literal", .{});
    }

    const cooked: ast.String = if (is_cooked_undefined)
        .empty
    else
        try parser.stringValue(token, span);

    return parser.tree.addNode(.{ .template_element = .{
        .cooked = cooked,
        .raw = parser.tree.sourceSlice(span.start, span.end),
        .tail = tail,
        .is_cooked_undefined = is_cooked_undefined,
    } }, span);
}

inline fn templateElementSpan(token: Token) ast.Span {
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
    const token = parser.current_token;
    try validateIdentifier(parser, "an identifier", token);
    try parser.advanceWithoutEscapeCheck() orelse return null;

    return try parser.tree.addNode(.{
        .identifier_reference = .{ .name = try parser.identifierName(token) },
    }, token.span);
}

pub inline fn parseBindingIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try validateIdentifier(parser, "a binding identifier", token);
    try parser.advanceWithoutEscapeCheck() orelse return null;

    return try parser.tree.addNode(
        .{ .binding_identifier = .{ .name = try parser.identifierName(token) } },
        token.span,
    );
}

pub inline fn parsePrivateIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .private_identifier);
    const token = parser.current_token;
    try parser.advance() orelse return null;

    return try parser.tree.addNode(.{
        .private_identifier = .{ .name = try parser.identifierName(token) },
    }, token.span);
}

pub fn parseIdentifierName(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag.isIdentifierLike());
    const token = parser.current_token;
    try parser.advanceWithoutEscapeCheck() orelse return null;
    return try addIdentifierName(parser, token);
}

pub fn addIdentifierName(parser: *Parser, token: Token) Error!ast.NodeIndex {
    std.debug.assert(token.tag.isIdentifierLike());
    return parser.tree.addNode(
        .{ .identifier_name = .{ .name = try parser.identifierName(token) } },
        token.span,
    );
}

pub fn parseLabelIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try validateIdentifier(parser, "a label", token);
    try parser.advance() orelse return null;

    return try parser.tree.addNode(.{
        .label_identifier = .{ .name = try parser.identifierName(token) },
    }, token.span);
}

pub inline fn validateIdentifier(
    parser: *Parser,
    comptime as_what: []const u8,
    token: Token,
) Error!void {
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
            try parser.fmt(
                "'{s}' is reserved and cannot be used as " ++ as_what,
                .{parser.describeToken(token)},
            ),
            .{},
        );
    }

    if (token.tag == .yield and parser.context.yield) {
        try parser.report(
            token.span,
            "'yield' is reserved in a generator context and cannot be used as " ++ as_what,
            .{},
        );
    }

    if (token.tag == .await and (parser.context.await or parser.tree.isModule())) {
        try parser.report(
            token.span,
            "'await' is reserved in an async/module context and cannot be used as " ++ as_what,
            .{},
        );
    }
}
