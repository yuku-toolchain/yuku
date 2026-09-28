// generates the yuku-analyzer decode.js, the estree decoder in analyzer mode with
// index-memoized nodes, span readers, and semantic section views

const std = @import("std");
const decoder = @import("estree/decoder.zig");
const emit = @import("estree/emit.zig");

pub fn main(init: std.process.Init) !void {
    try emit.minifiedToStdout(init.io, generate);
}

fn generate(w: *std.Io.Writer) std.Io.Writer.Error!void {
    try decoder.generate(w, .analyzer);
}
