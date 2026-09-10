const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const ast = parser.ast;
const Token = ast.Token;

// except the program, which starts before leading trivia, and the two nodes inside one token
const AlignmentChecker = struct {
    pub fn check(self: AlignmentChecker, path: []const u8, tree: *ast.Tree) !void {
        _ = self;
        var with_tokens = try parser.parse(std.testing.allocator, tree.source, .{
            .lang = tree.lang,
            .source_type = tree.source_type,
            .tokens = true,
        });
        defer with_tokens.deinit();

        const tokens = with_tokens.tokens;
        var i: u32 = 0;
        while (i < with_tokens.nodes.len) : (i += 1) {
            const index: ast.NodeIndex = @enumFromInt(i);
            switch (with_tokens.data(index)) {
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
};

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
    try helpers.forEachCorpusTree(std.testing.allocator, AlignmentChecker{});
}
