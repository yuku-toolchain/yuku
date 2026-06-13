// generates the yuku-parser decode.js: the lean estree decoder without
// analyzer extras. a separate root file per generated artifact keeps
// the generators free of argument parsing.

const std = @import("std");
const decoder = @import("estree/decoder.zig");

pub fn main(init: std.process.Init) !void {
    const stdout = std.Io.File.stdout();
    var buf: [64 * 1024]u8 = undefined;
    var fw = stdout.writer(init.io, &buf);
    try decoder.generate(&fw.interface, .parser);
    try fw.flush();
}
