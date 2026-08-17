const std = @import("std");
const parser = @import("parser");
const extension = @import("extension");

const ast = parser.ast;
const Hook = extension.Hook;

const Case = struct { source: []const u8, lang: ast.Lang = .js };

/// between them these reach all 20 extension points; none of them is a parse error.
const corpus = [_]Case{
    .{ .source =
    \\let x = 1;
    \\const [a, { b }] = arr;
    \\function f(p) { return p; }
    \\class C { m() { return 2; } }
    \\for (const i of items) f(i);
    },
    .{ .source = "@dec class C {}\nconst c = (@dec class {});", .lang = .ts },
    .{ .source = "for (const i of items @tail) f(i);" },
    .{ .source = "import x from bare;" },
    .{ .source = "const el = <div a={1}>hi{ok}</div>;\nconst frag = <><b>bold</b></>;", .lang = .jsx },
    .{ .source = "const el = <p>!!shout</p>;", .lang = .jsx },
    .{ .source = "const el = <div>x</_>;\nconst d = <Deprecated />;", .lang = .jsx },
};

fn parseCase(case: Case) !ast.Tree {
    return parser.parse(std.testing.allocator, case.source, .{ .lang = case.lang });
}

fn firstNode(tree: *const ast.Tree, tag: std.meta.Tag(ast.NodeData)) !ast.NodeIndex {
    for (0..tree.nodes.len) |i| {
        const index: ast.NodeIndex = @enumFromInt(i);
        if (std.meta.activeTag(tree.data(index)) == tag) return index;
    }
    return error.NodeNotFound;
}

fn countNodes(tree: *const ast.Tree, tag: std.meta.Tag(ast.NodeData)) usize {
    var found: usize = 0;
    for (0..tree.nodes.len) |i| {
        if (std.meta.activeTag(tree.data(@enumFromInt(i))) == tag) found += 1;
    }
    return found;
}

test "every extension point is reached through the public parse API" {
    for (corpus) |case| {
        var tree = try parseCase(case);
        defer tree.deinit();
        try std.testing.expect(!tree.hasErrors());
    }

    inline for (@typeInfo(Hook).@"enum".fields) |field| {
        if (extension.counts[field.value] == 0) {
            std.debug.print("extension point never reached: {s}\n", .{field.name});
            return error.ExtensionPointUnreached;
        }
    }
}

test "declined positions keep the parser's own nodes" {
    var tree = try parseCase(corpus[0]);
    defer tree.deinit();

    try std.testing.expectEqual(@as(usize, 3), countNodes(&tree, .variable_declaration));
    try std.testing.expectEqual(@as(usize, 1), countNodes(&tree, .array_pattern));
    try std.testing.expectEqual(@as(usize, 1), countNodes(&tree, .for_of_statement));
    try std.testing.expectEqual(@as(usize, 2), countNodes(&tree, .function_body));

    var jsx = try parseCase(corpus[4]);
    defer jsx.deinit();
    try std.testing.expectEqualStrings("hi", jsx.string(jsx.data(try firstNode(&jsx, .jsx_text)).jsx_text.value));
}

test "handled positions carry the extension's own values" {
    var shout = try parseCase(corpus[5]);
    defer shout.deinit();
    try std.testing.expectEqualStrings("shout", shout.string(shout.data(try firstNode(&shout, .jsx_text)).jsx_text.value));

    var bare = try parseCase(corpus[3]);
    defer bare.deinit();
    const specifier = bare.data(try firstNode(&bare, .string_literal)).string_literal;
    try std.testing.expectEqualStrings("bare", bare.string(specifier.value));

    // `%` in prefix position: reported, then returned as handled-and-failed.
    var failed = try parser.parse(std.testing.allocator, "const bad = %;", .{});
    defer failed.deinit();
    try std.testing.expect(failed.hasErrors());
}
