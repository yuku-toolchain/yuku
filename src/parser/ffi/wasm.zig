//! The freestanding WebAssembly core of `@yuku-core/wasm`, with no host imports.
//!
//!   alloc(len)               -> ptr   buffer for the source bytes
//!   parse(ptr, len, flags)   -> ptr   length-prefixed AST buffer `[u32 N][N bytes]`, or 0
//!   analyze(ptr, len, flags) -> ptr   length-prefixed analyzer buffer, or 0
//!   free(ptr, len)           -> void

const std = @import("std");
const parser = @import("parser");
const transfer = @import("transfer/root.zig");

const gpa = std.heap.wasm_allocator;

// packed by `@yuku-core/wasm`
const flag = struct {
    const source_type_mask = 0b11;
    const lang_shift = 2;
    const preserve_parens = 1 << 5;
    const semantic_errors = 1 << 6;
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
    const out = parseBuffer(ptr[0..len], flags) catch return 0;
    return @intFromPtr(out.ptr);
}

export fn analyze(ptr: [*]const u8, len: usize, flags: u32) usize {
    const out = analyzeBuffer(ptr[0..len], flags) catch return 0;
    return @intFromPtr(out.ptr);
}

fn parseBuffer(source: []const u8, flags: u32) ![]u8 {
    var tree = try parseTree(source, flags);
    defer tree.deinit();

    if (flags & flag.semantic_errors != 0) _ = parser.semantic.analyze(&tree) catch {};

    const out = try prefixed(transfer.bufferSize(&tree));
    _ = transfer.serializeInto(&tree, out[4..]);
    return out;
}

fn analyzeBuffer(source: []const u8, flags: u32) ![]u8 {
    var tree = try parseTree(source, flags);
    defer tree.deinit();

    const sem = try parser.semantic.analyze(&tree);
    // collect before sizing, records may intern into the string pool
    const records = try parser.semantic.module_record.collect(&tree, &sem);

    const out = try prefixed(transfer.semantic.bufferSize(&tree, &sem, records));
    _ = transfer.semantic.serializeInto(&tree, &sem, records, out[4..]);
    return out;
}

fn parseTree(source: []const u8, flags: u32) !parser.ast.Tree {
    return parser.parse(gpa, source, .{
        .source_type = @fromBackingInt(@truncate(flags & flag.source_type_mask)),
        .lang = @fromBackingInt(@truncate(flags >> flag.lang_shift)),
        .preserve_parens = flags & flag.preserve_parens != 0,
        .comments = if (flags & flag.attach_comments != 0) .both else .flat,
        .tokens = flags & flag.tokens != 0,
    });
}

// the length prefix lets the host copy the buffer out before freeing it
fn prefixed(size: usize) ![]u8 {
    const out = try gpa.alloc(u8, 4 + size);
    std.mem.writeInt(u32, out[0..4], @intCast(size), .little);
    return out;
}
