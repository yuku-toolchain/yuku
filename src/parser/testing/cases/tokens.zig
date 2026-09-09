//! The token list, rescans, speculation rewinds, recovery, and the alignment of node
//! spans to token boundaries across the corpus.

const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const ast = parser.ast;
const Token = ast.Token;
const TokenTag = ast.TokenTag;
const Allocator = std.mem.Allocator;

const t = std.testing;

fn parseTokens(source: []const u8, opts: parser.Options) !ast.Tree {
    var o = opts;
    o.tokens = true;
    return parser.parse(t.allocator, source, o);
}

fn tags(tree: *const ast.Tree, buf: []TokenTag) []TokenTag {
    for (tree.tokens, 0..) |token, i| buf[i] = token.tag;
    return buf[0..tree.tokens.len];
}

fn text(tree: *const ast.Tree, token: Token) []const u8 {
    return tree.source[token.span.start..token.span.end];
}

fn countTag(tree: *const ast.Tree, tag: TokenTag) usize {
    var n: usize = 0;
    for (tree.tokens) |token| n += @intFromBool(token.tag == tag);
    return n;
}

fn expectWellFormed(tree: *const ast.Tree) !void {
    const tokens = tree.tokens;
    try t.expect(tokens.len >= 1);
    var prev_end: u32 = 0;
    for (tokens, 0..) |token, i| {
        try t.expect(token.span.start <= token.span.end);
        try t.expect(token.span.end <= tree.source.len);
        try t.expect(token.span.start >= prev_end);
        const is_last = i + 1 == tokens.len;
        if (!is_last) {
            try t.expect(token.tag != .eof);
            try t.expect(token.span.start < token.span.end);
        }
        prev_end = token.span.end;
    }
    const last = tokens[tokens.len - 1];
    try t.expectEqual(TokenTag.eof, last.tag);
    try t.expectEqual(@as(u32, @intCast(tree.source.len)), last.span.start);
    try t.expectEqual(tree.span(tree.root).end, last.span.end);
}

test "tokens are not collected unless requested" {
    var tree = try parser.parse(t.allocator, "let x = 1;", .{});
    defer tree.deinit();
    try t.expectEqual(@as(usize, 0), tree.tokens.len);
}

test "the token list covers a statement in source order and ends at eof" {
    var tree = try parseTokens("let x = 1 + y;", .{});
    defer tree.deinit();
    try expectWellFormed(&tree);

    var buf: [16]TokenTag = undefined;
    try t.expectEqualSlices(TokenTag, &.{
        .let, .identifier, .assign, .numeric_literal, .plus, .identifier, .semicolon, .eof,
    }, tags(&tree, &buf));
    try t.expectEqualStrings("x", text(&tree, tree.tokens[1]));
    try t.expectEqualStrings("y", text(&tree, tree.tokens[5]));
}

test "an empty source is a lone eof" {
    var tree = try parseTokens("", .{});
    defer tree.deinit();
    try t.expectEqual(@as(usize, 1), tree.tokens.len);
    try t.expectEqual(TokenTag.eof, tree.tokens[0].tag);
}

test "line terminator flags survive on the committed token" {
    var tree = try parseTokens("a\nb", .{});
    defer tree.deinit();
    try t.expect(!tree.tokens[0].hasLineTerminatorBefore());
    try t.expect(tree.tokens[1].hasLineTerminatorBefore());
}

test "a regex is committed as the rescanned literal, not a slash" {
    var tree = try parseTokens("x = /ab+/gi;", .{});
    defer tree.deinit();
    try expectWellFormed(&tree);
    try t.expectEqual(@as(usize, 0), countTag(&tree, .slash));
    try t.expectEqual(@as(usize, 1), countTag(&tree, .regex_literal));
    try t.expectEqualStrings("/ab+/gi", text(&tree, tree.tokens[2]));
}

test "a template is committed as head, expressions, and tail" {
    var tree = try parseTokens("`a${b}c${d}e`;", .{});
    defer tree.deinit();
    try expectWellFormed(&tree);
    var buf: [16]TokenTag = undefined;
    try t.expectEqualSlices(TokenTag, &.{
        .template_head, .identifier, .template_middle, .identifier, .template_tail,
        .semicolon,     .eof,
    }, tags(&tree, &buf));
    try t.expectEqual(@as(usize, 0), countTag(&tree, .right_brace));
}

test "a nested generic closer is committed as two greater-than tokens" {
    var tree = try parseTokens("let x: A<B<C>> = y;", .{ .lang = .ts });
    defer tree.deinit();
    try expectWellFormed(&tree);
    try t.expectEqual(@as(usize, 0), countTag(&tree, .right_shift));
    try t.expectEqual(@as(usize, 2), countTag(&tree, .greater_than));
    try t.expectEqual(@as(usize, 2), countTag(&tree, .less_than));
}

test "a generic call opened by a fused left shift commits two less-than tokens" {
    var tree = try parseTokens("f<<T>(x: T) => R>(g);", .{ .lang = .ts });
    defer tree.deinit();
    try expectWellFormed(&tree);
    try t.expect(!tree.hasErrors());
    try t.expectEqual(@as(usize, 0), countTag(&tree, .left_shift));
    try t.expectEqual(@as(usize, 2), countTag(&tree, .less_than));
}

test "jsx text and tag names are committed in jsx mode" {
    var tree = try parseTokens("<a href=\"x\">hi {y}</a>;", .{ .lang = .jsx });
    defer tree.deinit();
    try expectWellFormed(&tree);
    try t.expect(!tree.hasErrors());
    var buf: [32]TokenTag = undefined;
    try t.expectEqualSlices(TokenTag, &.{
        .less_than,    .jsx_identifier, .jsx_identifier, .assign,       .string_literal,
        .greater_than, .jsx_text,       .left_brace,     .identifier,   .right_brace,
        .less_than,    .slash,          .jsx_identifier, .greater_than, .semicolon,
        .eof,
    }, tags(&tree, &buf));
    try t.expectEqualStrings("hi ", text(&tree, tree.tokens[6]));
}

test "an empty jsx text run between elements is not a token" {
    var tree = try parseTokens("<a><b/></a>;", .{ .lang = .jsx });
    defer tree.deinit();
    try expectWellFormed(&tree);
    try t.expectEqual(@as(usize, 0), countTag(&tree, .jsx_text));
}

test "a rewound arrow speculation leaves no duplicate tokens" {
    var tree = try parseTokens("let f = (a, b);", .{ .lang = .ts });
    defer tree.deinit();
    try expectWellFormed(&tree);
    var buf: [16]TokenTag = undefined;
    try t.expectEqualSlices(TokenTag, &.{
        .let,   .identifier, .assign,      .left_paren, .identifier,
        .comma, .identifier, .right_paren, .semicolon,  .eof,
    }, tags(&tree, &buf));
}

test "a conditional that re-parses its consequent leaves no duplicate tokens" {
    var tree = try parseTokens("x = c ? (a): T => b : d;", .{ .lang = .ts });
    defer tree.deinit();
    try expectWellFormed(&tree);
    try t.expect(!tree.hasErrors());
    try t.expectEqual(@as(usize, 2), countTag(&tree, .colon));
    try t.expectEqual(@as(usize, 1), countTag(&tree, .question));
}

test "tokens skipped by error recovery are still committed" {
    var tree = try parseTokens("let = ) ;\nfoo(1);", .{});
    defer tree.deinit();
    try t.expect(tree.hasErrors());
    try expectWellFormed(&tree);
    try t.expectEqual(@as(usize, 1), countTag(&tree, .right_paren) - 1);
    try t.expectEqual(@as(usize, 2), countTag(&tree, .semicolon));
    try t.expectEqual(@as(usize, 1), countTag(&tree, .numeric_literal));
}

test "a lexical error mid-statement keeps the tokens before and after it" {
    var tree = try parseTokens("let a = 1;\nlet b = 0x;\nlet c = 2;", .{});
    defer tree.deinit();
    try t.expect(tree.hasErrors());
    try expectWellFormed(&tree);
    try t.expectEqual(@as(usize, 3), countTag(&tree, .let));
    try t.expectEqual(@as(usize, 3), countTag(&tree, .semicolon));
}

test "a lexical error in the first token resumes past it" {
    var tree = try parseTokens("0x; let a = 1;", .{});
    defer tree.deinit();
    try t.expect(tree.hasErrors());
    try expectWellFormed(&tree);
    try t.expectEqual(@as(usize, 1), countTag(&tree, .let));
    try t.expectEqual(@as(usize, 2), tree.extra(tree.data(tree.root).program.body).len);
}

test "a rescan that fails inside the current token does not commit it" {
    var tree = try parseTokens("const el = <di<a>v\\u{61} a={1}/>", .{ .lang = .tsx });
    defer tree.deinit();
    try t.expect(tree.hasErrors());
    try expectWellFormed(&tree);
}

test "escaped keywords keep their tag and escape flag" {
    var tree = try parseTokens("var \\u0061sync = 1;", .{});
    defer tree.deinit();
    try expectWellFormed(&tree);
    try t.expectEqual(TokenTag.async, tree.tokens[1].tag);
    try t.expect(tree.tokens[1].isEscaped());
}

// except the program, which starts before leading trivia, and the two nodes inside one token
const AlignmentChecker = struct {
    pub fn check(self: AlignmentChecker, path: []const u8, tree: *ast.Tree) !void {
        _ = self;
        var with_tokens = try parser.parse(t.allocator, tree.source, .{
            .lang = tree.lang,
            .source_type = tree.source_type,
            .tokens = true,
        });
        defer with_tokens.deinit();
        try expectWellFormed(&with_tokens);

        const tokens = with_tokens.tokens;
        var i: u32 = 0;
        while (i < with_tokens.nodes.len) : (i += 1) {
            const index: ast.NodeIndex = @enumFromInt(i);
            switch (with_tokens.data(index)) {
                .program, .template_element, .jsx_empty_expression => continue,
                else => {},
            }
            const span = with_tokens.span(index);
            if (!startsToken(tokens, span.start) or !endsToken(tokens, span.end)) {
                std.debug.print("{s}: {t} at {d}..{d} is not token aligned\n", .{
                    path, with_tokens.data(index), span.start, span.end,
                });
                return error.NodeNotTokenAligned;
            }
        }
    }
};

fn startsToken(tokens: []const Token, offset: u32) bool {
    var lo: usize = 0;
    var hi: usize = tokens.len;
    while (lo < hi) {
        const mid = lo + (hi - lo) / 2;
        if (tokens[mid].span.start < offset) lo = mid + 1 else hi = mid;
    }
    return lo < tokens.len and tokens[lo].span.start == offset;
}

fn endsToken(tokens: []const Token, offset: u32) bool {
    var lo: usize = 0;
    var hi: usize = tokens.len;
    while (lo < hi) {
        const mid = lo + (hi - lo) / 2;
        if (tokens[mid].span.end < offset) lo = mid + 1 else hi = mid;
    }
    return lo < tokens.len and tokens[lo].span.end == offset;
}

test "node spans align to token boundaries across the parser corpus" {
    try helpers.forEachCorpusTree(t.allocator, AlignmentChecker{});
}
