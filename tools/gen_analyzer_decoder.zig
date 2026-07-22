// generates the yuku-analyzer decode.js: the estree decoder in
// analyzer mode (index-memoized nodes, span readers, semantic section
// views). a separate root file instead of a cli flag keeps both
// generators free of argument parsing.

const std = @import("std");
const decoder = @import("estree/decoder.zig");
const emit = @import("estree/emit.zig");

pub fn main(init: std.process.Init) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var generated: std.Io.Writer.Allocating = .init(allocator);
    try decoder.generate(&generated.writer, .analyzer);

    const stdout = std.Io.File.stdout();
    var buf: [64 * 1024]u8 = undefined;
    var fw = stdout.writer(init.io, &buf);
    try emit.minified(allocator, &fw.interface, generated.written());
    try fw.flush();
}
