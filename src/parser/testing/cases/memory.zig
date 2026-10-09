const std = @import("std");
const parser = @import("parser");

test "a parse that runs out of memory frees everything it allocated" {
    for ([_][]const u8{ "/*a*/ 0x", "/*a*/ let x = [1, 2];" }) |source| {
        var fail_index: usize = 0;
        while (fail_index < 64) : (fail_index += 1) {
            var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{
                .fail_index = fail_index,
            });
            const tree = parser.parse(failing.allocator(), source, .{}) catch continue;
            tree.deinit();
            if (!failing.has_induced_failure) break;
        }
    }
}

// fail codegen allocations independently of parsing
fn jsxCommentsAllocationTest(allocator: std.mem.Allocator) !void {
    const source =
        "<Box<{x: A<B /*inner*/> /*member*/} /*outer*/> enabled>" ++
        "{/*empty*/}text</Box /*close*/>;";
    var tree = try parser.parse(std.testing.allocator, source, .{
        .lang = .tsx,
        .comments = .attached,
    });
    defer tree.deinit();
    try std.testing.expectEqual(@as(usize, 0), tree.diagnostics.items.len);

    const result = try parser.codegen.generate(allocator, &tree, .{
        .jsx = .{ .pragma = "runtime.h", .pragma_frag = "runtime.Fragment" },
        .strip = true,
        .comments = .all,
        .source_map = .{ .source = source },
    });
    defer result.deinit(allocator);
    try std.testing.expect(result.map != null);
    for ([_][]const u8{ "inner", "member", "outer", "empty", "close" }) |marker| {
        try std.testing.expect(std.mem.indexOf(u8, result.code, marker) != null);
    }
}

test "JSX comment preservation frees every allocation on failure" {
    try std.testing.checkAllAllocationFailures(
        std.testing.allocator,
        jsxCommentsAllocationTest,
        .{},
    );
}

test "JSX generation rejects invalid factory names" {
    var tree = try parser.parse(std.testing.allocator, "<div/>;", .{ .lang = .jsx });
    defer tree.deinit();
    try std.testing.expectError(error.InvalidJSXFactory, parser.codegen.generate(
        std.testing.allocator,
        &tree,
        .{ .jsx = .{ .pragma = "h;evil()" } },
    ));
    try std.testing.expectError(error.InvalidJSXFactory, parser.codegen.generate(
        std.testing.allocator,
        &tree,
        .{ .jsx = .{ .pragma_frag = "a..b" } },
    ));
}

test "JSX fragments generate with comment scopes disabled" {
    // attached comments must be ignored in both preservation and lowering
    var tree = try parser.parse(
        std.testing.allocator,
        "</* opening */>text</ /* closing */>;",
        .{ .lang = .jsx, .comments = .attached },
    );
    defer tree.deinit();
    try std.testing.expectEqual(@as(usize, 0), tree.diagnostics.items.len);
    const cases = [_]struct { jsx: ?parser.codegen.JSXOptions, expected: []const u8 }{
        .{ .jsx = null, .expected = "<>text</>;" },
        .{
            .jsx = .{ .pure = false },
            .expected = "React.createElement(React.Fragment, null, \"text\");",
        },
    };
    for (cases) |case| {
        const result = try parser.codegen.generate(std.testing.allocator, &tree, .{
            .jsx = case.jsx,
            .comments = .none,
        });
        defer result.deinit(std.testing.allocator);
        try std.testing.expectEqualStrings(case.expected, result.code);
    }
}

// the marker claims no side effects, so only the bundled factories get it by default
test "JSX pure annotations default to the bundled factories" {
    var tree = try parser.parse(std.testing.allocator, "<div/>;", .{ .lang = .jsx });
    defer tree.deinit();
    const options = [_]struct { jsx: parser.codegen.JSXOptions, annotated: bool }{
        .{ .jsx = .{}, .annotated = true },
        .{ .jsx = .{ .pure = false }, .annotated = false },
        .{ .jsx = .{ .pragma = "h" }, .annotated = false },
        .{ .jsx = .{ .pragma = "h", .pragma_frag = "Fragment", .pure = true }, .annotated = true },
    };
    for (options) |case| {
        const result = try parser.codegen.generate(std.testing.allocator, &tree, .{
            .jsx = case.jsx,
        });
        defer result.deinit(std.testing.allocator);
        const annotated = std.mem.indexOf(u8, result.code, "/* @__PURE__ */") != null;
        try std.testing.expectEqual(case.annotated, annotated);
    }
}

// a synthetic text node has no raw lexeme, so preserve mode escapes markup
// syntax and normalizing whitespace back into entities
test "synthetic JSX text is already cooked" {
    var tree = try parser.parse(
        std.testing.allocator,
        "<div>&amp;lt; &lt;&#123;&#125;&gt;&#9;&#10;</div>",
        .{ .lang = .jsx },
    );
    defer tree.deinit();
    try std.testing.expectEqual(@as(usize, 0), tree.diagnostics.items.len);
    for (tree.nodes.items(.data)) |*data| {
        switch (data.*) {
            .jsx_text => |*text| text.raw = .empty,
            else => {},
        }
    }

    const result = try parser.codegen.generate(std.testing.allocator, &tree, .{ .jsx = .{} });
    defer result.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings(
        "/* @__PURE__ */ React.createElement(\"div\", null, \"&lt; <{}>\\t\\n\");",
        result.code,
    );

    const preserved = try parser.codegen.generate(std.testing.allocator, &tree, .{});
    defer preserved.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings(
        "<div>&amp;lt; &lt;&#123;&#125;&gt;&#9;&#10;</div>;",
        preserved.code,
    );
}

// a synthetic attribute string has no raw lexeme, so its cooked value is escaped and quoted
test "synthetic JSX attribute strings quote their cooked value" {
    var tree = try parser.parse(
        std.testing.allocator,
        "<div a=\"x\" b='y' c=\"&quot;'\" d=\"&amp;lt;\" e=\"&amp;\"/>",
        .{ .lang = .jsx },
    );
    defer tree.deinit();
    for (tree.nodes.items(.data)) |*data| {
        switch (data.*) {
            .string_literal => |*lit| lit.raw = .empty,
            else => {},
        }
    }

    const result = try parser.codegen.generate(std.testing.allocator, &tree, .{});
    defer result.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings(
        "<div a=\"x\" b=\"y\" c={\"\\\"'\"} d=\"&amp;lt;\" e=\"&amp;\" />;",
        result.code,
    );
}
