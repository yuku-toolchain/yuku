// generates the yuku-ast traversal tables as TypeScript, see
// decoder.generateWalkTables

const std = @import("std");
const decoder = @import("estree/decoder.zig");

pub fn main(init: std.process.Init) !void {
    const stdout = std.Io.File.stdout();
    var buf: [64 * 1024]u8 = undefined;
    var fw = stdout.writer(init.io, &buf);
    try decoder.generateWalkTables(&fw.interface);
    try fw.flush();
}
