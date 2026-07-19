//! Freestanding WebAssembly codegen entry point for the yuku playground. Takes
//! a v7 AST buffer (from encode.js) and returns generated source as a
//! length-prefixed UTF-8 buffer `[u32 N][N bytes]`, or 0 on failure:
//!
//!   alloc(len)              -> ptr   buffer for the AST bytes
//!   codegen(ptr, len, opts) -> ptr   opts packed: bit 0 strip, bit 1 minify,
//!                                    bit 2 compact, bits 3-4 quotes
//!                                    (preserve/double/single/shortest),
//!                                    bits 5-7 comments (none/all/some/line/
//!                                    block), bits 8-15 indent
//!   free(ptr, len)          -> void

const std = @import("std");
const parser = @import("parser");
const transfer = @import("transfer");

const gpa = std.heap.wasm_allocator;

export fn alloc(len: usize) [*]u8 {
    return (gpa.alloc(u8, len) catch @trap()).ptr;
}

export fn free(ptr: [*]u8, len: usize) void {
    gpa.free(ptr[0..len]);
}

export fn codegen(ptr: [*]const u8, len: usize, opts: u32) usize {
    const out = run(ptr[0..len], opts) catch return 0;
    return @intFromPtr(out.ptr);
}

fn run(buf: []const u8, opts: u32) ![]u8 {
    var tree = try transfer.deserializeFromBuf(gpa, buf, "");
    defer tree.deinit();

    const result = try parser.codegen.generate(gpa, &tree, .{
        .strip = opts & 1 != 0,
        .minify = opts & 2 != 0,
        .format = if (opts & 4 != 0) .compact else .pretty,
        .quotes = @enumFromInt((opts >> 3) & 3),
        .comments = @enumFromInt(@min((opts >> 5) & 7, 4)),
        .indent = @intCast((opts >> 8) & 0xFF),
    });
    defer result.deinit(gpa);

    const out = try gpa.alloc(u8, 4 + result.code.len);
    std.mem.writeInt(u32, out[0..4], @intCast(result.code.len), .little);
    @memcpy(out[4..], result.code);
    return out;
}
