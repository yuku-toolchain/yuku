const std = @import("std");
const parser = @import("parser");
const binding = @import("extension");

const ast = parser.ast;
const Point = parser.extension.Point;

const Case = struct { source: []const u8, lang: ast.Lang = .js };

// together these reach every point without a parse error
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
    .{
        .source = "const el = <div a={1}>hi{ok}</div>;\nconst frag = <><b>bold</b></>;",
        .lang = .jsx,
    },
    .{ .source = "const el = <p>!!shout</p>;", .lang = .jsx },
    .{ .source = "const el = <div>x</_>;\nconst d = <Deprecated />;", .lang = .jsx },
};

// an unimplemented point can never be reached, so report that separately
test "the reference binding implements every extension point" {
    inline for (@typeInfo(Point).@"enum".fields) |field| {
        if (!@hasDecl(binding, field.name)) {
            std.debug.print("extension point has no reference hook: {s}\n", .{field.name});
            return error.ExtensionPointUnimplemented;
        }
    }
}

test "every extension point is reached through the public parse API" {
    for (corpus) |case| {
        var tree = try parse(case);
        defer tree.deinit();
        try std.testing.expect(!tree.hasErrors());
    }

    var unreached: u32 = 0;
    inline for (@typeInfo(Point).@"enum".fields) |field| {
        if (!visited(field.name)) {
            std.debug.print("extension point never reached: {s}\n", .{field.name});
            unreached += 1;
        }
    }
    if (unreached > 0) return error.ExtensionPointUnreached;
}

test "declined positions keep the parser's nodes" {
    var tree = try parse(corpus[0]);
    defer tree.deinit();

    try std.testing.expectEqual(@as(usize, 3), countNodes(&tree, .variable_declaration));
    try std.testing.expectEqual(@as(usize, 1), countNodes(&tree, .array_pattern));
    try std.testing.expectEqual(@as(usize, 1), countNodes(&tree, .for_of_statement));
    try std.testing.expectEqual(@as(usize, 2), countNodes(&tree, .function_body));

    var jsx = try parse(corpus[4]);
    defer jsx.deinit();
    try std.testing.expectEqualStrings("hi", firstJsxText(&jsx));
}

test "handled positions carry the extension's own values" {
    var shout = try parse(corpus[5]);
    defer shout.deinit();
    try std.testing.expectEqualStrings("shout", firstJsxText(&shout));

    var bare = try parse(corpus[3]);
    defer bare.deinit();
    const specifier = bare.data(try firstNode(&bare, .string_literal)).string_literal;
    try std.testing.expectEqualStrings("bare", bare.string(specifier.value));
}

test "a failed hook reports instead of producing a node" {
    var tree = try parser.parse(std.testing.allocator, "const bad = %;", .{});
    defer tree.deinit();

    try std.testing.expect(tree.hasErrors());
    try std.testing.expectEqualStrings(
        "'%' is not a prefix operator",
        tree.diagnostics.items[0].message,
    );
}

test "an advisory hook reports without rejecting" {
    var tree = try parse(corpus[6]);
    defer tree.deinit();

    try std.testing.expect(!tree.hasErrors());
    try std.testing.expect(tree.hasDiagnostics());
    try std.testing.expectEqual(ast.Severity.warning, tree.diagnostics.items[0].severity);
}

fn parse(case: Case) !ast.Tree {
    return parser.parse(std.testing.allocator, case.source, .{ .lang = case.lang });
}

fn visited(name: []const u8) bool {
    for (binding.visited[0..binding.visited_len]) |seen| {
        if (std.mem.eql(u8, seen, name)) return true;
    }
    return false;
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

fn firstJsxText(tree: *const ast.Tree) []const u8 {
    const index = firstNode(tree, .jsx_text) catch return "";
    return tree.string(tree.data(index).jsx_text.value);
}
