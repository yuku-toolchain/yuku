// re-emits generated JS through yuku's own parser and codegen

const std = @import("std");
const parser = @import("parser");

const Writer = std.Io.Writer;

/// parses `js`, prints it minified
pub fn minified(allocator: std.mem.Allocator, w: *Writer, js: []const u8) !void {
    var tree = try parser.parse(allocator, js, .{});
    defer tree.deinit();
    if (tree.hasErrors()) return error.GeneratedJsDoesNotParse;

    const result = try parser.codegen.generate(allocator, &tree, .{
        .minify = true,
        .format = .compact,
        .quotes = .shortest,
        .comments = .none,
    });
    defer result.deinit(allocator);
    if (result.errors.len != 0) return error.GeneratedJsDoesNotPrint;

    if (std.mem.startsWith(u8, js, "//")) {
        const nl = std.mem.indexOfScalar(u8, js, '\n').?;
        try w.writeAll(js[0 .. nl + 1]);
    }
    try w.writeAll(result.code);
    if (!std.mem.endsWith(u8, result.code, "\n")) try w.writeAll("\n");
}
