const std = @import("std");
const ast = @import("../ast.zig");
const lexer = @import("../lexer.zig");
const Parser = @import("../parser.zig").Parser;
const util = @import("util");
const expressions = @import("expressions.zig");

pub fn parseStringLiteral(parser: *Parser) ?ast.NodeIndex {
    const token = parser.current_token;
    parser.advance();
    return parser.addNode(.{
        .string_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub fn parseBooleanLiteral(parser: *Parser) ?ast.NodeIndex {
    const token = parser.current_token;
    parser.advance();
    return parser.addNode(.{
        .boolean_literal = .{ .value = token.type == .True },
    }, token.span);
}

pub fn parseNullLiteral(parser: *Parser) ?ast.NodeIndex {
    const token = parser.current_token;
    parser.advance();
    return parser.addNode(.null_literal, token.span);
}

pub fn parseNumericLiteral(parser: *Parser) ?ast.NodeIndex {
    const token = parser.current_token;
    parser.advance();
    return parser.addNode(.{
        .numeric_literal = .{
            .value = util.Number.parseJSNumeric(token.lexeme) catch unreachable,
        },
    }, token.span);
}

pub fn parseBigIntLiteral(parser: *Parser) ?ast.NodeIndex {
    const token = parser.current_token;
    parser.advance();
    return parser.addNode(.{
        .bigint_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub fn parseRegExpLiteral(parser: *Parser) ?ast.NodeIndex {
    const token = parser.current_token;
    const regex = parser.lexer.reScanAsRegex(token) catch |e| {
        parser.err(token.span.start, token.span.end, lexer.getLexicalErrorMessage(e), lexer.getLexicalErrorHelp(e));
        return null;
    };
    parser.replaceTokenAndAdvance(parser.lexer.createToken(
        .RegexLiteral,
        parser.source[regex.span.start..regex.span.end],
        regex.span.start,
        regex.span.end,
    ));
    return parser.addNode(.{
        .regexp_literal = .{
            .pattern_start = @intCast(regex.span.start + 1),
            .pattern_len = @intCast(regex.pattern.len),
            .flags_start = @intCast(regex.span.end - regex.flags.len),
            .flags_len = @intCast(regex.flags.len),
        },
    }, regex.span);
}

pub fn parseNoSubstitutionTemplate(parser: *Parser) ?ast.NodeIndex {
    const token = parser.current_token;
    parser.advance();
    const element_span = getTemplateElementSpan(token);
    const element = parser.addNode(.{
        .template_element = .{
            .raw_start = element_span.start,
            .raw_len = @intCast(element_span.end - element_span.start),
            .tail = true,
        },
    }, element_span);
    return parser.addNode(.{
        .template_literal = .{
            .quasis = parser.addExtra(&[_]ast.NodeIndex{element}),
            .expressions = ast.IndexRange.empty,
        },
    }, token.span);
}

pub fn parseTemplateLiteral(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;

    var quasis: [64]ast.NodeIndex = undefined;
    var template_expressions: [64]ast.NodeIndex = undefined;
    var quasis_length: usize = 0;
    var expressions_length: usize = 0;

    const head = parser.current_token;
    const head_span = getTemplateElementSpan(head);
    quasis[quasis_length] = parser.addNode(.{
        .template_element = .{
            .raw_start = head_span.start,
            .raw_len = @intCast(head_span.end - head_span.start),
            .tail = false,
        },
    }, head_span);
    quasis_length += 1;
    parser.advance();

    var end: u32 = undefined;
    while (true) {
        template_expressions[expressions_length] = expressions.parseExpression(parser, 0) orelse return null;
        expressions_length += 1;

        const token = parser.current_token;
        const is_tail = token.type == .TemplateTail;

        switch (token.type) {
            .TemplateMiddle, .TemplateTail => {
                const span = getTemplateElementSpan(token);
                quasis[quasis_length] = parser.addNode(.{
                    .template_element = .{
                        .raw_start = span.start,
                        .raw_len = @intCast(span.end - span.start),
                        .tail = is_tail,
                    },
                }, span);
                quasis_length += 1;
                if (is_tail) {
                    end = token.span.end;
                    parser.advance();
                    break;
                }
                parser.advance();
            },
            else => {
                parser.err(token.span.start, token.span.end, "Expected template continuation", null);
                return null;
            },
        }
    }

    return parser.addNode(.{
        .template_literal = .{
            .quasis = parser.addExtra(quasis[0..quasis_length]),
            .expressions = parser.addExtra(template_expressions[0..expressions_length]),
        },
    }, .{ .start = start, .end = end });
}

inline fn getTemplateElementSpan(token: @import("../token.zig").Token) ast.Span {
    return switch (token.type) {
        .TemplateHead, .TemplateMiddle => .{
            .start = token.span.start + 1,
            .end = token.span.end - 2,
        },
        .TemplateTail, .NoSubstitutionTemplate => .{
            .start = token.span.start + 1,
            .end = token.span.end - 1,
        },
        else => unreachable,
    };
}

pub fn parseIdentifier(parser: *Parser) ?ast.NodeIndex {
    const token = parser.current_token;
    parser.advance();
    return parser.addNode(.{
        .identifier = .{
            .name_start = token.span.start,
            .name_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub fn parsePrivateIdentifier(parser: *Parser) ?ast.NodeIndex {
    const token = parser.current_token;
    parser.advance();
    return parser.addNode(.{
        .private_identifier = .{
            .name_start = token.span.start,
            .name_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}
