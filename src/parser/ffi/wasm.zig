const std = @import("std");
const builtin = @import("builtin");
const entries = @import("entries.zig");
const transfer = @import("transfer/root.zig");

const gpa = std.heap.wasm_allocator;

comptime {
    std.debug.assert(builtin.cpu.arch.endian() == .little);
}

const TreeHeader = extern struct {
    node_words: u32,
    extra_words: u32,
    comment_words: u32,
    pool_units: u32,
    source_bytes: u32,
};

export fn alloc(len: usize) usize {
    return @intFromPtr((gpa.alignedAlloc(u8, .of(u32), len) catch return 0).ptr);
}

export fn free(ptr: [*]align(4) u8, len: usize) void {
    gpa.free(ptr[0..len]);
}

export fn parse(ptr: [*]const u8, len: usize, flags: u32) usize {
    return call(entries.parse, .{}, ptr[0..len], flags, .{});
}

export fn analyze(ptr: [*]const u8, len: usize, flags: u32) usize {
    return call(entries.analyze, .{}, ptr[0..len], flags, .{});
}

export fn printSource(ptr: [*]const u8, len: usize, flags: u32, options: u32) usize {
    return call(entries.printSource, .{}, ptr[0..len], flags, .{options});
}

export fn printTree(block: [*]align(4) const u8, flags: u32, options: u32) usize {
    const encoded, const input = treeOf(block) catch return 0;
    return call(entries.printTree, .{encoded}, input, flags, .{options});
}

export fn analyzeTree(block: [*]align(4) const u8, flags: u32) usize {
    const encoded, _ = treeOf(block) catch return 0;
    return resultOf(entries.analyzeTree(gpa, encoded, flags, Prefixed{}));
}

fn call(
    comptime entry: anytype,
    before: anytype,
    input: []const u8,
    flags: u32,
    after: anytype,
) usize {
    const utf16 = flags & entries.utf16 != 0;
    const target = gpa.alloc(u8, if (utf16) input.len / 2 * 3 else 0) catch return 0;
    defer gpa.free(target);
    const source = if (utf16)
        transfer.utf16.encodeUtf8(std.mem.bytesAsSlice(u16, input), target, .replace)
    else
        input;
    const args = .{gpa} ++ before ++ .{ source, flags } ++ after ++ .{Prefixed{}};
    return resultOf(@call(.auto, entry, args));
}

fn resultOf(out: transfer.Error![]u8) usize {
    return @intFromPtr((out catch return 0).ptr) - 4;
}

fn treeOf(block: [*]align(4) const u8) error{InvalidTree}!struct { transfer.Encoded, []const u8 } {
    const header: *const TreeHeader = @ptrCast(block);
    var at: usize = @sizeOf(TreeHeader);
    const encoded: transfer.Encoded = .{
        .nodes = try sectionAt(transfer.PackedNode, block, &at, header.node_words * 4),
        .extras = try sectionAt(u32, block, &at, header.extra_words * 4),
        .comments = try sectionAt(transfer.PackedComment, block, &at, header.comment_words * 4),
        .pool = try sectionAt(u16, block, &at, header.pool_units * 2),
    };
    return .{ encoded, block[at..][0..header.source_bytes] };
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
