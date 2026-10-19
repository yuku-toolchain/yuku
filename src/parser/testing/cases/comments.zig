const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const ast = parser.ast;
const testing = std.testing;

fn parse(source: []const u8, comments: parser.CommentMode) !ast.Tree {
    var tree = try parser.parse(testing.allocator, source, .{ .lang = .js, .comments = comments });
    errdefer tree.deinit();
    try testing.expect(!tree.hasErrors());
    return tree;
}

fn spanOf(source: []const u8, text: []const u8) ast.Span {
    const start: u32 = @intCast(std.mem.find(u8, source, text).?);
    return .{ .start = start, .end = start + @as(u32, @intCast(text.len)) };
}

fn expectValues(
    tree: *const ast.Tree,
    comments: []const ast.Comment,
    expected: []const []const u8,
) !void {
    try testing.expectEqual(expected.len, comments.len);
    for (comments, expected) |comment, value| {
        try testing.expectEqualStrings(value, tree.string(comment.value));
    }
}

fn expectCommentsOf(
    tree: *const ast.Tree,
    node: ast.NodeIndex,
    expected: []const []const u8,
) !void {
    var it = tree.commentsOf(node);
    for (expected) |value| try testing.expectEqualStrings(value, tree.string(it.next().?.value));
    try testing.expectEqual(null, it.next());
    try testing.expectEqual(expected.len > 0, tree.hasComments(node));
}

fn statement(tree: *const ast.Tree, i: usize) ast.NodeIndex {
    return tree.extra(tree.data(tree.root).program.body)[i];
}

// a range keeps the comments it covers end to end, and drops one it cuts through
test "commentsIn returns the comments wholly within a range" {
    const source = "/* a */ f(/* b */ x, // c\n y) /* d */";
    var tree = try parse(source, .collect);
    defer tree.deinit();

    const call = spanOf(source, "f(/* b */ x, // c\n y)");
    const b = spanOf(source, "/* b */");
    try expectValues(&tree, tree.commentsIn(call), &.{ " b ", " c" });
    try expectValues(&tree, tree.commentsIn(.{ .start = 0, .end = @intCast(source.len) }), &.{
        " a ", " b ", " c", " d ",
    });
    try expectValues(&tree, tree.commentsIn(.{ .start = b.start + 1, .end = call.end }), &.{" c"});
    try expectValues(&tree, tree.commentsIn(.{ .start = call.start, .end = b.end - 1 }), &.{});
    try expectValues(&tree, tree.commentsIn(.{ .start = b.start, .end = b.start }), &.{});
}

test "collected comments belong to no node until attached" {
    var tree = try parse("/* a */ x;", .collect);
    defer tree.deinit();
    try testing.expectEqual(@as(usize, 1), tree.comments.len);
    try expectCommentsOf(&tree, statement(&tree, 0), &.{});
}

test "addComment and moveComments edit the comments of any node" {
    var tree = try parse("/* 1 */ a;\n/* 2 */ b;\n", .attach);
    defer tree.deinit();
    const a = statement(&tree, 0);
    const b = statement(&tree, 1);
    try expectCommentsOf(&tree, a, &.{" 1 "});
    try expectCommentsOf(&tree, b, &.{" 2 "});

    try tree.addComment(a, .{ .type = .line, .value = try tree.addString(" 3") });
    try tree.moveComments(b, a);
    try expectCommentsOf(&tree, a, &.{ " 1 ", " 2 ", " 3" });
    try expectCommentsOf(&tree, b, &.{});

    const fresh = try tree.addNode(.{ .debugger_statement = .{} }, .{ .start = 0, .end = 0 });
    try tree.moveComments(a, fresh);
    try expectCommentsOf(&tree, fresh, &.{ " 1 ", " 2 ", " 3" });
    try tree.moveComments(fresh, .null);
    try expectCommentsOf(&tree, fresh, &.{});
    try testing.expectEqual(@as(usize, 2), tree.comments.len);
}

test "codegen prints an added comment where it sits" {
    var tree = try parse("f();", .attach);
    defer tree.deinit();
    const call = tree.data(statement(&tree, 0)).expression_statement.expression;
    try tree.addComment(call, .{
        .type = .block,
        .value = try tree.addString("#__PURE__"),
        .same_line = true,
    });

    const printed = try parser.codegen.generate(testing.allocator, &tree, .{ .comments = .some });
    defer printed.deinit(testing.allocator);
    try testing.expectEqualStrings("/*#__PURE__*/ f();", printed.code);
}

// a comment ends its line wherever the lexer sees a line break, so it stays a trailing one
test "a comment before a line break of any kind trails its line" {
    for ([_][]const u8{ "\n", "\r", "\r\n", "\u{2028}", "\u{2029}" }) |line_break| {
        const parts: []const []const u8 = &.{ "a; /* c */", line_break, "b;" };
        const source = try std.mem.concat(testing.allocator, u8, parts);
        defer testing.allocator.free(source);
        var tree = try parse(source, .attach);
        defer tree.deinit();

        var comments = tree.commentsOf(statement(&tree, 0));
        const comment = comments.next().?;
        try testing.expectEqual(.after, comment.position);
        try testing.expect(comment.same_line);
        try testing.expect(!tree.hasComments(statement(&tree, 1)));
    }
}

// attaching never changes the list, and gives every source comment exactly one node
fn checkAttachment(path: []const u8, collected: *const ast.Tree) !void {
    var tree = try parser.parse(testing.allocator, collected.source, .{
        .lang = collected.lang,
        .source_type = collected.source_type,
        .comments = .attach,
    });
    defer tree.deinit();
    try testing.expectEqual(collected.comments.len, tree.comments.len);
    for (collected.comments, tree.comments) |a, b| {
        try testing.expect(std.meta.eql(a.span, b.span));
        try testing.expect(std.meta.eql(a.value, b.value));
    }

    const seen = try testing.allocator.alloc(bool, tree.comments.len);
    defer testing.allocator.free(seen);
    @memset(seen, false);
    var i: u32 = 0;
    while (i < tree.nodes.len) : (i += 1) {
        const node: ast.NodeIndex = @fromBackingInt(i);
        var it = tree.commentsOf(node);
        var any = false;
        while (it.next()) |comment| {
            any = true;
            const start = comment.span.start;
            const id = std.sort.binarySearch(ast.Comment, tree.comments, start, order).?;
            if (seen[id]) {
                std.debug.print("{s}: comment at {d} attached twice\n", .{ path, start });
                return error.CommentAttachedTwice;
            }
            seen[id] = true;
        }
        try testing.expectEqual(any, tree.hasComments(node));
    }
    for (seen) |attached| try testing.expect(attached);
}

fn order(start: u32, comment: ast.Comment) std.math.Order {
    return std.math.order(start, comment.span.start);
}

test "attaching gives every comment of the parser corpus exactly one node" {
    try helpers.forEachCorpusTree(checkAttachment);
}
