const std = @import("std");
const builtin = @import("builtin");
const entries = @import("entries.zig");
const transfer = @import("transfer/root.zig");

const gpa = std.heap.wasm_allocator;

comptime {
    std.debug.assert(builtin.cpu.arch.endian() == .little);
}

/// The lengths that start a tree's block, before its sections, each on a 4-byte boundary.
const TreeHeader = extern struct {
    node_words: u32,
    extra_words: u32,
    comment_words: u32,
    pool_units: u32,
    source_bytes: u32,
};

export fn alloc(len: usize) [*]align(4) u8 {
    return (gpa.alignedAlloc(u8, .of(u32), len) catch @trap()).ptr;
}

export fn free(ptr: [*]align(4) u8, len: usize) void {
    gpa.free(ptr[0..len]);
}

export fn parse(ptr: [*]const u8, len: usize, flags: u32) usize {
    return resultOf(withSource(ptr[0..len], flags, entries.parse));
}

export fn analyze(ptr: [*]const u8, len: usize, flags: u32) usize {
    return resultOf(withSource(ptr[0..len], flags, entries.analyze));
}

export fn printTree(block: [*]align(4) const u8, flags: u32, options: u32) usize {
    return resultOf(withTree(block, flags, options, .print));
}

export fn analyzeTree(block: [*]align(4) const u8, flags: u32) usize {
    return resultOf(withTree(block, flags, 0, .analyze));
}

fn resultOf(out: transfer.Error![]u8) usize {
    return @intFromPtr((out catch return 0).ptr) - 4;
}

fn withSource(input: []const u8, flags: u32, comptime run: anytype) transfer.Error![]u8 {
    const target = try gpa.alloc(u8, if (flags & entries.utf16 != 0) input.len / 2 * 3 else 0);
    defer gpa.free(target);
    return run(gpa, sourceOf(input, flags, target), flags, Prefixed{});
}

fn withTree(
    block: [*]align(4) const u8,
    flags: u32,
    options: u32,
    comptime entry: enum { print, analyze },
) transfer.Error![]u8 {
    const header: *const TreeHeader = @ptrCast(block);
    var at: usize = @sizeOf(TreeHeader);
    const encoded: transfer.Encoded = .{
        .nodes = try sectionAt(transfer.PackedNode, block, &at, header.node_words * 4),
        .extras = try sectionAt(u32, block, &at, header.extra_words * 4),
        .comments = try sectionAt(transfer.PackedComment, block, &at, header.comment_words * 4),
        .pool = try sectionAt(u16, block, &at, header.pool_units * 2),
    };
    const input = block[at..][0..header.source_bytes];
    const target = try gpa.alloc(u8, if (flags & entries.utf16 != 0) input.len / 2 * 3 else 0);
    defer gpa.free(target);
    const source = sourceOf(input, flags, target);
    return switch (entry) {
        .print => entries.printTree(gpa, encoded, source, flags, options, Prefixed{}),
        .analyze => entries.analyzeTree(gpa, encoded, flags, Prefixed{}),
    };
}

fn sourceOf(input: []const u8, flags: u32, target: []u8) []const u8 {
    if (flags & entries.utf16 == 0) return input;
    std.debug.assert(input.len % 2 == 0);
    return transfer.utf16.encodeUtf8(std.mem.bytesAsSlice(u16, input), target, .replace);
}

fn sectionAt(
    comptime T: type,
    block: [*]align(4) const u8,
    at: *usize,
    bytes: usize,
) error{InvalidTree}![]const T {
    std.debug.assert(at.* % 4 == 0);
    if (bytes % @sizeOf(T) != 0) return error.InvalidTree;
    const items: [*]const T = @ptrCast(@alignCast(block + at.*));
    at.* = std.mem.alignForward(usize, at.* + bytes, 4);
    return items[0 .. bytes / @sizeOf(T)];
}

const Prefixed = struct {
    pub fn bytes(_: Prefixed, size: usize) std.mem.Allocator.Error![]u8 {
        const out = try gpa.alignedAlloc(u8, .of(u32), 4 + size);
        std.mem.writeInt(u32, out[0..4], @intCast(size), .little);
        return out[4..];
    }
};
