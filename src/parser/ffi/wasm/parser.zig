//! Freestanding WebAssembly entry point:
//!
//!   alloc(len)             -> ptr   buffer for the source bytes
//!   parse(ptr, len, flags) -> ptr   length-prefixed result `[u32 N][N bytes]`
//!                                   (the v7 AST buffer decode.js reads), or 0
//!   free(ptr, len)         -> void
//!
//! The buffer format and decode.js are shared with the native binding.

const std = @import("std");
const parser = @import("parser");
const transfer = @import("transfer");

const gpa = std.heap.wasm_allocator;

// Option bits packed by index.js `packFlags`.
const flag = struct {
    const source_type_mask = 0b11; // bits 0..1: ast.SourceType index
    const lang_shift = 2; // bits 2..4: ast.Lang index
    const preserve_parens = 1 << 5;
    const semantic = 1 << 6;
    const attach_comments = 1 << 7;
    const tokens = 1 << 8;
};

export fn alloc(len: usize) [*]u8 {
    return (gpa.alloc(u8, len) catch @trap()).ptr;
}

export fn free(ptr: [*]u8, len: usize) void {
    gpa.free(ptr[0..len]);
}

export fn parse(ptr: [*]const u8, len: usize, flags: u32) usize {
    const out = run(ptr[0..len], flags) catch return 0;
    return @intFromPtr(out.ptr);
}

fn run(source: []const u8, flags: u32) ![]u8 {
    var tree = try parser.parse(gpa, source, .{
        .source_type = @enumFromInt(@as(u2, @truncate(flags & flag.source_type_mask))),
        .lang = @enumFromInt(@as(u3, @truncate(flags >> flag.lang_shift))),
        .preserve_parens = flags & flag.preserve_parens != 0,
        .comments = if (flags & flag.attach_comments != 0) .both else .flat,
        .tokens = flags & flag.tokens != 0,
    });
    defer tree.deinit();

    if (flags & flag.semantic != 0) _ = parser.semantic.analyze(&tree) catch {};

    const size = transfer.bufferSize(&tree);
    const out = try gpa.alloc(u8, 4 + size);
    std.mem.writeInt(u32, out[0..4], @intCast(size), .little);
    _ = transfer.serializeInto(&tree, out[4..]);
    return out;
}
