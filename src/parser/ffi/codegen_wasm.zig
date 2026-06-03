//! Freestanding WebAssembly codegen entry point for the yuku playground. Takes
//! a v7 AST buffer (from encode.js) and returns generated source as a
//! length-prefixed UTF-8 buffer `[u32 N][N bytes]`, or 0 on failure:
//!
//!   alloc(len)            -> ptr   buffer for the AST bytes
//!   codegen(ptr, len, op) -> ptr   op: 0 print, 1 strip, 2 minify
//!   free(ptr, len)        -> void

const std = @import("std");
const parser = @import("parser");
const transfer = @import("transfer.zig");

const gpa = std.heap.wasm_allocator;

export fn alloc(len: usize) [*]u8 {
    return (gpa.alloc(u8, len) catch @trap()).ptr;
}

export fn free(ptr: [*]u8, len: usize) void {
    gpa.free(ptr[0..len]);
}

export fn codegen(ptr: [*]const u8, len: usize, op: u32) usize {
    const out = run(ptr[0..len], op) catch return 0;
    return @intFromPtr(out.ptr);
}

fn run(buf: []const u8, op: u32) ![]u8 {
    var tree = try transfer.deserializeFromBuf(gpa, buf, "");
    defer tree.deinit();

    const result = switch (op) {
        1 => try parser.codegen.strip(gpa, &tree, .{}),
        2 => try parser.codegen.minify(gpa, &tree, .{ .format = .compact }),
        else => try parser.codegen.print(gpa, &tree, .{}),
    };
    defer result.deinit(gpa);

    const out = try gpa.alloc(u8, 4 + result.code.len);
    std.mem.writeInt(u32, out[0..4], @intCast(result.code.len), .little);
    @memcpy(out[4..], result.code);
    return out;
}
