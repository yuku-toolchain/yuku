// generates the yuku-codegen encode.js: the estree-to-buffer encoder
// feeding the native printer.

const std = @import("std");
const encoder = @import("estree/encoder.zig");
const emit = @import("estree/emit.zig");

pub fn main(init: std.process.Init) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var generated: std.Io.Writer.Allocating = .init(allocator);
    try encoder.generate(&generated.writer);

    const stdout = std.Io.File.stdout();
    var buf: [128 * 1024]u8 = undefined;
    var fw = stdout.writer(init.io, &buf);
    try emit.minified(allocator, &fw.interface, generated.written());
    try fw.flush();
}
