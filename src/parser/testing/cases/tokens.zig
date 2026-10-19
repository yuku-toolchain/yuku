const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const ast = parser.ast;
const Token = ast.Token;

fn checkAlignment(path: []const u8, tree: *const ast.Tree) !void {
    var with_tokens = try parser.parse(std.testing.allocator, tree.source, .{
        .lang = tree.lang,
        .source_type = tree.source_type,
        .tokens = true,
    });
    defer with_tokens.deinit();

    const tokens = with_tokens.tokens;
    var i: u32 = 0;
    while (i < with_tokens.nodes.len) : (i += 1) {
        const index: ast.NodeIndex = @fromBackingInt(i);
        switch (with_tokens.data(index)) {
            // the program starts before leading trivia, the other two sit inside one token
            .program, .template_element, .jsx_empty_expression => continue,
            else => {},
        }
        const span = with_tokens.span(index);
        if (!boundary(tokens, .start, span.start) or !boundary(tokens, .end, span.end)) {
            std.debug.print("{s}: {t} at {d}..{d} is not token aligned\n", .{
                path, with_tokens.data(index), span.start, span.end,
            });
            return error.NodeNotTokenAligned;
        }
    }
}

fn boundary(tokens: []const Token, comptime edge: enum { start, end }, offset: u32) bool {
    var lo: usize = 0;
    var hi: usize = tokens.len;
    while (lo < hi) {
        const mid = lo + (hi - lo) / 2;
        if (@field(tokens[mid].span, @tagName(edge)) < offset) lo = mid + 1 else hi = mid;
    }
    return lo < tokens.len and @field(tokens[lo].span, @tagName(edge)) == offset;
}

test "node spans align to token boundaries across the parser corpus" {
    try helpers.forEachCorpusTree(checkAlignment);
}

test "tokensIn returns the tokens wholly within a span" {
    var tree = try parser.parse(std.testing.allocator, "let x = f(a, b);", .{ .tokens = true });
    defer tree.deinit();

    try expectTexts(&tree, .{ .start = 8, .end = 15 }, &.{ "f", "(", "a", ",", "b", ")" });
    try expectTexts(&tree, .{ .start = 15, .end = 16 }, &.{ ";", "" });
    // `let` and `b` cross the edges
    try expectTexts(&tree, .{ .start = 1, .end = 13 }, &.{ "x", "=", "f", "(", "a", "," });
    try expectTexts(&tree, .{ .start = 10, .end = 10 }, &.{});
    try expectTexts(&tree, .{ .start = 1, .end = 2 }, &.{});
    const before_call = tree.tokensIn(.{ .start = 0, .end = 8 });
    try std.testing.expectEqual(.assign, before_call[before_call.len - 1].tag);

    var untokenized = try parser.parse(std.testing.allocator, "let x;", .{});
    defer untokenized.deinit();
    try std.testing.expectEqual(0, untokenized.tokensIn(.{ .start = 0, .end = 6 }).len);
}

fn expectTexts(tree: *const ast.Tree, range: ast.Span, expected: []const []const u8) !void {
    const tokens = tree.tokensIn(range);
    try std.testing.expectEqual(expected.len, tokens.len);
    for (tokens, expected) |token, text| {
        try std.testing.expectEqualStrings(text, token.text(tree.source));
    }
}
