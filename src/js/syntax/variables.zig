const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const expressions = @import("expressions.zig");
const patterns = @import("patterns.zig");

pub fn parseVariableDeclaration(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const kind = parseVariableKind(parser) orelse return null;

    var declarators: [64]ast.NodeIndex = undefined;
    declarators[0] = parseVariableDeclarator(parser, kind) orelse return null;
    var length: usize = 1;
    var end = parser.getSpan(declarators[0]).end;

    while (parser.current_token.type == .Comma) {
        parser.advance();
        declarators[length] = parseVariableDeclarator(parser, kind) orelse return null;
        end = parser.getSpan(declarators[length]).end;
        length += 1;
    }

    return parser.addNode(.{
        .variable_declaration = .{
            .declarators = parser.addExtra(declarators[0..length]),
            .kind = kind,
        },
    }, .{ .start = start, .end = parser.eatSemicolon(end) });
}

inline fn parseVariableKind(parser: *Parser) ?ast.VariableKind {
    const token_type = parser.current_token.type;
    parser.advance();
    return switch (token_type) {
        .Let => .Let,
        .Const => .Const,
        .Var => .Var,
        .Using => .Using,
        .Await => if (parser.current_token.type == .Using) blk: {
            parser.advance();
            break :blk .AwaitUsing;
        } else null,
        else => null,
    };
}

fn parseVariableDeclarator(parser: *Parser, kind: ast.VariableKind) ?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const id = patterns.parseBindingPattern(parser) orelse return null;

    var init: ast.NodeIndex = ast.null_node;
    var end = parser.getSpan(id).end;

    if (parser.current_token.type == .Assign) {
        parser.advance();
        if (expressions.parseExpression(parser, 0)) |expression| {
            init = expression;
            end = parser.getSpan(expression).end;
        }
    } else if (patterns.isDestructuringPattern(parser, id)) {
        parser.err(parser.getSpan(id).start, parser.getSpan(id).end, "Destructuring requires initializer", null);
        return null;
    } else if (kind == .Const or kind == .Using or kind == .AwaitUsing) {
        parser.err(parser.getSpan(id).start, parser.getSpan(id).end, parser.formatMessage("{s} requires initializer", .{@tagName(kind)}), null);
        return null;
    }

    return parser.addNode(.{
        .variable_declarator = .{ .id = id, .init = init },
    }, .{ .start = start, .end = end });
}
