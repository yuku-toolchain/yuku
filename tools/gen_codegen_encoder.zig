// generates the yuku-codegen encode.js: the estree-to-buffer encoder
// feeding the native printer.

const std = @import("std");
const encoder = @import("estree/encoder.zig");

pub fn main(init: std.process.Init) !void {
    const stdout = std.Io.File.stdout();
    var buf: [128 * 1024]u8 = undefined;
    var fw = stdout.writer(init.io, &buf);
    try encoder.generate(&fw.interface);
    try fw.flush();
}
