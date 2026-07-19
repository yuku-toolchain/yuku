//! Freestanding WebAssembly analyzer entry point:
//!
//!   alloc(len)               -> ptr   buffer for the source bytes
//!   analyze(ptr, len, flags) -> ptr   length-prefixed analyzer buffer
//!                                     `[u32 N][N bytes]` (v7 AST sections
//!                                     plus semantic sections), or 0
//!   free(ptr, len)           -> void
//!
//! The buffer format and decode.js are shared with the native binding.
//! Flag bits match wasm/parser.zig; the semantic bit is ignored because
//! analysis always runs.

const std = @import("std");
const parser = @import("parser");
const transfer = @import("transfer");

const gpa = std.heap.wasm_allocator;

const flag = struct {
    const script = 1 << 0;
    const lang_shift = 1; // bits 1..3: ast.Lang index
    const preserve_parens = 1 << 4;
    const allow_return = 1 << 5;
    const attach_comments = 1 << 7;
};

export fn alloc(len: usize) [*]u8 {
    return (gpa.alloc(u8, len) catch @trap()).ptr;
}

export fn free(ptr: [*]u8, len: usize) void {
    gpa.free(ptr[0..len]);
}

export fn analyze(ptr: [*]const u8, len: usize, flags: u32) usize {
    const out = run(ptr[0..len], flags) catch return 0;
    return @intFromPtr(out.ptr);
}

fn run(source: []const u8, flags: u32) ![]u8 {
    var tree = try parser.parse(gpa, source, .{
        .source_type = if (flags & flag.script != 0) .script else .module,
        .lang = @enumFromInt(@as(u3, @truncate(flags >> flag.lang_shift))),
        .preserve_parens = flags & flag.preserve_parens != 0,
        .allow_return_outside_function = flags & flag.allow_return != 0,
        .comments = if (flags & flag.attach_comments != 0) .both else .flat,
    });
    defer tree.deinit();

    const sem = try parser.semantic.analyze(&tree);
    // collect before sizing: records may intern "default" into the pool
    const records = try parser.semantic.module_record.collect(&tree, &sem);

    const size = transfer.semantic.bufferSize(&tree, &sem, records);
    const out = try gpa.alloc(u8, 4 + size);
    std.mem.writeInt(u32, out[0..4], @intCast(size), .little);
    _ = transfer.semantic.serializeInto(&tree, &sem, records, out[4..]);
    return out;
}
