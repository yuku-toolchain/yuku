// generates the yuku-parser decode.js, the lean estree decoder without analyzer extras.
// a separate root file per generated artifact keeps the generators free of argument parsing

const std = @import("std");
const decoder = @import("estree/decoder.zig");
const emit = @import("estree/emit.zig");

pub fn main(init: std.process.Init) !void {
    try emit.minifiedToStdout(init.io, generate);
}

fn generate(w: *std.Io.Writer) std.Io.Writer.Error!void {
    try decoder.generate(w, .parser);
}
