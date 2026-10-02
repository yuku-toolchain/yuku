const std = @import("std");
const parser = @import("parser");

// Decorators before `export` decorate only a class. Anything else after them is a
// diagnostic, never an assertion failure, so these run in Debug builds.
test "decorators before `export` and a declaration other than a class report the missing class" {
    for ([_][]const u8{
        "@d export const x = 1;",
        "@d export function f() {}",
        "@d export default function f() {}",
        "@d export default 1;",
    }) |source| {
        for ([_]parser.ast.Lang{ .js, .ts }) |lang| {
            var tree = try parser.parse(std.testing.allocator, source, .{
                .lang = lang,
                .source_type = .module,
            });
            defer tree.deinit();

            try std.testing.expect(tree.hasErrors());
            try std.testing.expect(std.mem.startsWith(
                u8,
                tree.diagnostics.items[0].message,
                "Expected 'class' keyword",
            ));
        }
    }
}
