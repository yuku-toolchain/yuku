const std = @import("std");
const decoder = @import("estree/decoder.zig");
const encoder = @import("estree/encoder.zig");
const identifier_tables = @import("gen_identifier_tables.zig");

const Output = enum { decoder, encoder, walk_tables, token_types, identifier_tables, jsx_entities };

pub fn main(init: std.process.Init) !void {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 2) return error.Usage;
    const output = std.meta.stringToEnum(Output, args[1]) orelse return error.Usage;

    var buffer: [64 * 1024]u8 = undefined;
    var stdout = std.Io.File.stdout().writer(init.io, &buffer);
    const w = &stdout.interface;
    switch (output) {
        .decoder => try decoder.generate(w),
        .encoder => try encoder.generate(w),
        .walk_tables => try decoder.generateWalkTables(w),
        .token_types => try decoder.generateTokenTypes(w),
        .identifier_tables => try identifier_tables.generate(w),
        .jsx_entities => try decoder.generateJSXEntities(w),
    }
    try w.flush();
}

pub fn writeString(w: *std.Io.Writer, name: []const u8, text: []const u8) !void {
    std.debug.assert(text.len > 0);
    const line_chars = 92;
    try w.print("export const {s}: string =", .{name});
    var offset: usize = 0;
    while (offset < text.len) : (offset += line_chars) {
        const end = @min(offset + line_chars, text.len);
        try w.print("\n  \"{s}\"{s}", .{ text[offset..end], if (end == text.len) ";" else " +" });
    }
    try w.writeAll("\n");
}
