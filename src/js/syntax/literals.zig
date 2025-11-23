const std = @import("std");
const token = @import("../token.zig");
const lexer = @import("../lexer.zig");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const util = @import("util");

const expressions = @import("expressions.zig");

pub fn parseStringLiteral(parser: *Parser) ?*ast.Expression {
    const value = parser.current_token.lexeme;
    const span = parser.current_token.span;
    parser.advance();

    const literal = ast.StringLiteral{
        .value = value, // TODO: handle escape sequences and remove quotes
        .raw = value,
        .span = span,
    };

    return parser.createNode(ast.Expression, .{ .string_literal = literal });
}

pub fn parseBooleanLiteral(parser: *Parser) ?*ast.Expression {
    const value = parser.current_token.type == .True;
    const raw = parser.current_token.lexeme;
    const span = parser.current_token.span;
    parser.advance();

    const literal = ast.BooleanLiteral{
        .value = value,
        .raw = raw,
        .span = span,
    };

    return parser.createNode(ast.Expression, .{ .boolean_literal = literal });
}

pub fn parseNullLiteral(parser: *Parser) ?*ast.Expression {
    const raw = parser.current_token.lexeme;
    const span = parser.current_token.span;
    parser.advance();

    const literal = ast.NullLiteral{
        .value = null,
        .raw = raw,
        .span = span,
    };

    return parser.createNode(ast.Expression, .{ .null_literal = literal });
}

pub fn parseNumericLiteral(parser: *Parser) ?*ast.Expression {
    const value = parser.current_token.lexeme;
    const span = parser.current_token.span;
    parser.advance();

    const literal = ast.NumericLiteral{
        .value = util.Number.parseJSNumeric(value) catch unreachable,
        .raw = value,
        .span = span,
    };

    return parser.createNode(ast.Expression, .{ .numeric_literal = literal });
}

pub fn parseBigIntLiteral(parser: *Parser) ?*ast.Expression {
    const raw = parser.current_token.lexeme;
    const span = parser.current_token.span;
    parser.advance();

    const bigint = raw[0..(raw.len - 1)]; // lexer only produces BigInt tokens for valid literals

    const literal = ast.BigIntLiteral{
        .value = raw,
        .raw = raw,
        .bigint = bigint,
        .span = span,
    };

    return parser.createNode(ast.Expression, .{ .bigint_literal = literal });
}

pub fn parseRegExpLiteral(parser: *Parser) ?*ast.Expression {
    const slash_token = parser.current_token;

    const regex = parser.lexer.reScanAsRegex(slash_token) catch |err_| {
        parser.err(
            slash_token.span.start,
            slash_token.span.end,
            lexer.getLexicalErrorMessage(err_),
            lexer.getLexicalErrorHelp(err_),
        );
        return null;
    };

    const start = regex.span.start;
    const end = regex.span.end;

    const regex_token = parser.lexer.createToken(.RegexLiteral, parser.source[start..end], start, end);

    parser.replaceTokenAndAdvance(regex_token);

    const literal = ast.RegExpLiteral{
        .value = regex.lexeme,
        .raw = regex.lexeme,
        .regex = .{
            .pattern = regex.pattern,
            .flags = regex.flags,
        },
        .span = regex.span,
    };

    return parser.createNode(ast.Expression, .{ .regex_literal = literal });
}

pub fn parseNoSubstitutionTemplateLiteral(parser: *Parser) ?*ast.Expression {
    const tok = parser.current_token;
    parser.advance();

    const element = createTemplateElement(parser, tok, true);

    const template_literal = ast.TemplateLiteral{
        .quasis = parser.dupe(*ast.TemplateElement, &[_]*ast.TemplateElement{element}),
        .expressions = parser.dupe(*ast.Expression, &[_]*ast.Expression{}),
        .span = .{
            .start = tok.span.start,
            .end = tok.span.end,
        },
    };

    return parser.createNode(ast.Expression, .{ .template_literal = template_literal });
}

pub fn parseTemplateLiteral(parser: *Parser) ?*ast.Expression {
    const template_literal_start = parser.current_token.span.start;

    parser.clear(&parser.scratch_template_elements);
    parser.clear(&parser.scratch_expressions);
    parser.ensureCapacity(&parser.scratch_template_elements, 4);
    parser.ensureCapacity(&parser.scratch_expressions, 4);

    // parse head element
    const head_token = parser.current_token;

    parser.append(&parser.scratch_template_elements, createTemplateElement(parser, head_token, false));
    parser.advance();

    var template_literal_end: u32 = undefined;

    // parse expressions and middle/tail elements
    while (true) {
        const expr_start = parser.current_token.span.start;
        const expr = expressions.parseExpression(parser, 0) orelse return null;
        parser.append(&parser.scratch_expressions, expr);

        const template_token = parser.current_token;
        const is_tail = template_token.type == .TemplateTail;

        switch (template_token.type) {
            .TemplateMiddle, .TemplateTail => {
                parser.append(&parser.scratch_template_elements, createTemplateElement(parser, template_token, is_tail));

                if (is_tail) {
                    template_literal_end = template_token.span.end;
                    parser.advance();
                    break;
                }

                parser.advance();
            },
            else => {
                parser.err(
                    expr_start,
                    template_token.span.start,
                    "Expected template continuation after expression",
                    "Add '}' here to close the template expression",
                );
                return null;
            },
        }
    }

    const template_literal = ast.TemplateLiteral{
        .quasis = parser.dupe(*ast.TemplateElement, parser.scratch_template_elements.items),
        .expressions = parser.dupe(*ast.Expression, parser.scratch_expressions.items),
        .span = .{ .start = template_literal_start, .end = template_literal_end },
    };

    return parser.createNode(ast.Expression, .{ .template_literal = template_literal });
}

inline fn createTemplateElement(parser: *Parser, tok: token.Token, is_tail: bool) *ast.TemplateElement {
    // the template literal tokens includes the template literal punctuators, like `, ${}
    const actual_start = tok.span.start;
    const actual_end = tok.span.end;

    // so we don't need those punctuators in the elements as per spec
    const span: token.Span = switch (tok.type) {
        .TemplateHead, .TemplateMiddle => token.Span{ .start = actual_start + 1, .end = actual_end - 2 },
        .TemplateTail, .NoSubstitutionTemplate => token.Span{ .start = actual_start + 1, .end = actual_end - 1 },
        else => unreachable,
    };

    const lexeme = parser.source[span.start..span.end];

    const element = ast.TemplateElement{
        .value = .{
            .raw = lexeme,
            .cooked = lexeme, // TODO: process escape sequences
        },
        .tail = is_tail,
        .span = span,
    };
    return parser.createNode(ast.TemplateElement, element);
}

pub fn parseIdentifierReference(parser: *Parser) ?*ast.Expression {
    const name = parser.current_token.lexeme;
    const span = parser.current_token.span;
    parser.advance();

    const identifier = ast.IdentifierReference{
        .name = name,
        .span = span,
    };

    return parser.createNode(ast.Expression, .{ .identifier_reference = identifier });
}

pub fn parsePrivateIdentifier(parser: *Parser) ?*ast.Expression {
    const name = parser.current_token.lexeme;
    const span = parser.current_token.span;
    parser.advance();

    const private_id = ast.PrivateIdentifier{
        .name = name,
        .span = span,
    };

    return parser.createNode(ast.Expression, .{ .private_identifier = private_id });
}
