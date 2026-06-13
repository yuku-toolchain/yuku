// generates the yuku-analyzer decode.js: the estree decoder in
// analyzer mode (index-memoized nodes, span readers, semantic section
// views). a separate root file instead of a cli flag keeps both
// generators free of argument parsing.

const std = @import("std");
const decoder = @import("estree/decoder.zig");

pub fn main(init: std.process.Init) !void {
    const stdout = std.Io.File.stdout();
    var buf: [64 * 1024]u8 = undefined;
    var fw = stdout.writer(init.io, &buf);
    try decoder.generate(&fw.interface, .analyzer);
    try fw.flush();
}
