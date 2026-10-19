const std = @import("std");
const decoder = @import("estree/decoder.zig");
const identifier_tables = @import("gen_identifier_tables.zig");

const Output = enum { decoder, walk_tables, token_types, identifier_tables };

pub fn main(init: std.process.Init) !void {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 2) return error.Usage;
    const output = std.meta.stringToEnum(Output, args[1]) orelse return error.Usage;

    var buffer: [64 * 1024]u8 = undefined;
    var stdout = std.Io.File.stdout().writer(init.io, &buffer);
    const w = &stdout.interface;
    switch (output) {
        .decoder => try decoder.generate(w),
        .walk_tables => try decoder.generateWalkTables(w),
        .token_types => try decoder.generateTokenTypes(w),
        .identifier_tables => try identifier_tables.generate(w),
    }
    try w.flush();
}
