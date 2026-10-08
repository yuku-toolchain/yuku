const std = @import("std");
const parser = @import("parser");

const Writer = std.Io.Writer;

pub fn minifiedToStdout(io: std.Io, generate: *const fn (*Writer) Writer.Error!void) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var generated: Writer.Allocating = .init(allocator);
    try generate(&generated.writer);

    var buf: [64 * 1024]u8 = undefined;
    var stdout = std.Io.File.stdout().writer(io, &buf);
    try minified(allocator, &stdout.interface, generated.written());
    try stdout.interface.flush();
}

fn minified(allocator: std.mem.Allocator, w: *Writer, js: []const u8) !void {
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
    if (result.diagnostics.len != 0) return error.GeneratedJsDoesNotPrint;

    if (std.mem.startsWith(u8, js, "//")) {
        const nl = std.mem.findScalar(u8, js, '\n').?;
        try w.writeAll(js[0 .. nl + 1]);
    }
    try w.writeAll(result.code);
    if (!std.mem.endsWith(u8, result.code, "\n")) try w.writeAll("\n");
}
