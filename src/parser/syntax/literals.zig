const std = @import("std");
const ast = @import("../ast.zig");
const Token = @import("../token.zig").Token;
const TokenTag = @import("../token.zig").TokenTag;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const expressions = @import("expressions.zig");

pub fn parseStringLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    if (token.tag != .string_literal) return parser.failExpected("Expected a string literal", .{});
    try parser.advance() orelse return null;

    const value_span: ast.Span = .{ .start = token.span.start + 1, .end = token.span.end - 1 };
    return try parser.tree.addNode(.{ .string_literal = .{
        .value = try parser.stringValue(token, value_span),
        .raw = parser.tree.sourceSlice(token.span.start, token.span.end),
    } }, token.span);
}

pub fn parseBooleanLiteral(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.at(.true) or parser.at(.false));
    return parser.leaf(.{ .boolean_literal = .{ .value = parser.at(.true) } });
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
    std.debug.assert(parser.at(.slash) or parser.at(.slash_assign));

    const regex = parser.lexer.reScanAsRegex(parser.current_token.span.start) catch |e| {
        try parser.reportLexicalError(e);
        return null;
    };

    const regex_token = parser.lexer.createToken(.regex_literal, regex.span.start, regex.span.end);
    try parser.advanceWithRescannedToken(regex_token) orelse return null;

    const pattern_start = regex.span.start + 1;
    const pattern_end = pattern_start + @as(u32, @intCast(regex.pattern.len));
    const flags_start = regex.span.end - @as(u32, @intCast(regex.flags.len));
    try checkRegExpFlags(parser, flags_start, regex.flags);

    return try parser.tree.addNode(.{ .regexp_literal = .{
        .pattern = parser.tree.sourceSlice(pattern_start, pattern_end),
        .flags = parser.tree.sourceSlice(flags_start, regex.span.end),
    } }, regex.span);
}

fn checkRegExpFlags(parser: *Parser, flags_start: u32, flags: []const u8) Error!void {
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
            try parser.report(
                span,
                try parser.fmt("Unknown regular expression flag '{s}'", .{parser.spanText(span)}),
                .{ .help = "Valid flags are 'd', 'g', 'i', 'm', 's', 'u', 'v', and 'y'" },
            );
        } else if (seen & bit != 0) {
            try parser.report(
                span,
                try parser.fmt("Duplicate regular expression flag '{s}'", .{parser.spanText(span)}),
                .{ .help = "Remove the repeated flag" },
            );
        } else if ((seen | bit) & unicode_modes == unicode_modes) {
            try parser.report(
                span,
                "The Unicode (u) flag and the Unicode Sets (v) flag cannot be set simultaneously",
                .{ .help = "Keep only one of the two flags" },
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
    std.debug.assert(parser.at(.no_substitution_template));
    const token = parser.current_token;

    const element = try addTemplateElement(parser, token, true, tagged);
    try parser.advance() orelse return null;

    return try parser.tree.addNode(.{ .template_literal = .{
        .quasis = try parser.tree.addExtra(&.{element}),
        .expressions = .empty,
    } }, token.span);
}

pub fn parseTemplateLiteral(parser: *Parser, tagged: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const parts = try parseTemplateParts(parser, tagged, expressions.parseFull) orelse return null;
    return try parser.finish(start, .{
        .template_literal = .{ .quasis = parts.quasis, .expressions = parts.substitutions },
    });
}

pub const TemplateParts = struct {
    quasis: ast.IndexRange,
    substitutions: ast.IndexRange,
};

pub fn parseTemplateParts(
    parser: *Parser,
    tagged: bool,
    comptime parseSubstitution: anytype,
) Error!?TemplateParts {
    std.debug.assert(parser.at(.template_head));

    const quasis = parser.list();
    defer quasis.deinit();
    const substitutions = parser.sideList();
    defer substitutions.deinit();

    try quasis.append(try addTemplateElement(parser, parser.current_token, false, tagged));
    try parser.advance() orelse return null;

    while (true) {
        try substitutions.append(try parseSubstitution(parser) orelse return null);

        // the lexer emits `}` as a punctuator, rescan it as template continuation
        if (!parser.at(.right_brace)) {
            try parser.reportExpected(
                "Expected '}' to close the template placeholder",
                .{},
            );
            return null;
        }

        const token = parser.lexer.reScanTemplateContinuation(
            parser.current_token.span.start,
        ) catch |e| {
            try parser.reportLexicalError(e);
            return null;
        };
        const is_tail = token.tag == .template_tail;

        try quasis.append(try addTemplateElement(parser, token, is_tail, tagged));
        try parser.advanceWithRescannedToken(token) orelse return null;

        if (is_tail) break;
    }

    return .{ .quasis = try quasis.finish(), .substitutions = try substitutions.finish() };
}

inline fn addTemplateElement(
    parser: *Parser,
    token: Token,
    tail: bool,
    tagged: bool,
) Error!ast.NodeIndex {
    const span: ast.Span = .{
        .start = token.span.start + 1,
        .end = token.span.end - @as(u32, if (tail) 1 else 2),
    };
    const is_cooked_undefined = token.hasInvalidEscape();

    if (!tagged and is_cooked_undefined) {
        try parser.report(span, "Invalid escape sequence in an untagged template literal", .{
            .help = "Only tagged templates can contain invalid escapes",
        });
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
    std.debug.assert(parser.at(.private_identifier));
    const token = parser.current_token;
    try parser.advance() orelse return null;

    return try parser.tree.addNode(.{
        .private_identifier = .{ .name = try parser.identifierName(token) },
    }, token.span);
}

pub fn parseIdentifierName(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    if (!token.tag.isIdentifierLike()) return parser.failExpected("Expected an identifier", .{
        .help = "Identifiers start with a letter, '_', or '$'",
    });
    try parser.advanceWithoutEscapeCheck() orelse return null;
    return try addIdentifierName(parser, token);
}

pub inline fn addIdentifierName(parser: *Parser, token: Token) Error!ast.NodeIndex {
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
    const tag = token.tag;
    if (tag == .identifier) return;

    const is_reserved = !tag.isIdentifierLike() or tag.isUnconditionallyReserved() or
        (tag == .yield and parser.context.yield) or
        (tag == .await and (parser.context.await or parser.tree.isModule()));
    if (is_reserved) try reportReservedIdentifier(parser, as_what, tag, token.span.start, token.span.end);
}

fn reportReservedIdentifier(
    parser: *Parser,
    comptime as_what: []const u8,
    tag: TokenTag,
    start: u32,
    end: u32,
) Error!void {
    @branchHint(.cold);
    const token: Token = .{ .span = .{ .start = start, .end = end }, .tag = tag };

    if (!tag.isIdentifierLike()) {
        try parser.reportExpected(
            "Expected an identifier",
            .{ .help = "Identifiers start with a letter, '_', or '$'" },
        );
    }

    if (tag.isUnconditionallyReserved()) {
        try parser.report(
            token.span,
            try parser.fmt(
                "{s} is a reserved word and cannot be used as " ++ as_what,
                .{try parser.describeToken(token)},
            ),
            .{},
        );
    }

    if (tag == .yield and parser.context.yield) {
        try parser.report(
            token.span,
            "'yield' is a reserved word in generators and cannot be used as " ++ as_what,
            .{},
        );
    }

    if (tag == .await and (parser.context.await or parser.tree.isModule())) {
        try parser.report(
            token.span,
            "'await' is a reserved word in async functions and modules and cannot be used as " ++
                as_what,
            .{},
        );
    }
}
