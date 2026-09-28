// generates the yuku-codegen encode.js, the estree-to-buffer encoder feeding the native printer

const std = @import("std");
const encoder = @import("estree/encoder.zig");
const emit = @import("estree/emit.zig");

pub fn main(init: std.process.Init) !void {
    try emit.minifiedToStdout(init.io, encoder.generate);
}
