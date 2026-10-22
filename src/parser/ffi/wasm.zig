const std = @import("std");
const builtin = @import("builtin");
const parser = @import("parser");
const flag = @import("flag.zig");
const transfer = @import("transfer/root.zig");

const gpa = std.heap.wasm_allocator;

comptime {
    std.debug.assert(builtin.cpu.arch.endian() == .little);
}

// static, so the host reads it without freeing
const description = std.mem.toBytes(std.mem.nativeToLittle(u32, parser.dialect.description.len)) ++
    parser.dialect.description[0..parser.dialect.description.len].*;

export fn alloc(len: usize) [*]u8 {
    return (gpa.alloc(u8, len) catch @trap()).ptr;
}

export fn free(ptr: [*]u8, len: usize) void {
    gpa.free(ptr[0..len]);
}

export fn parse(ptr: [*]const u8, len: usize, flags: u32) usize {
    const out = withSource(ptr[0..len], flags, parseBuffer) catch return 0;
    return @intFromPtr(out.ptr);
}

export fn analyze(ptr: [*]const u8, len: usize, flags: u32) usize {
    const out = withSource(ptr[0..len], flags, analyzeBuffer) catch return 0;
    return @intFromPtr(out.ptr);
}

export fn dialect() usize {
    if (!parser.dialect.bound) return 0;
    return @intFromPtr(&description);
}

fn withSource(
    input: []const u8,
    flags: u32,
    comptime run: fn ([]const u8, u32) std.mem.Allocator.Error![]u8,
) std.mem.Allocator.Error![]u8 {
    if (flags & flag.utf16 == 0) return run(input, flags);
    std.debug.assert(input.len % 2 == 0);
    const units = std.mem.bytesAsSlice(u16, input);

    const target = try gpa.alloc(u8, units.len * 3);
    defer gpa.free(target);

    return run(transfer.utf16.encodeUtf8(units, target), flags);
}

fn parseBuffer(source: []const u8, flags: u32) std.mem.Allocator.Error![]u8 {
    var tree = try parseTree(source, flags);
    defer tree.deinit();

    if (flags & flag.semantic_errors != 0) try parser.semantic.check(&tree);

    var offsets: transfer.utf16.Offsets = try .init(gpa, tree.source);
    defer offsets.deinit(gpa);

    const out = try prefixed(transfer.bufferSize(&tree));
    _ = transfer.serializeInto(&tree, &offsets, out[4..]);
    return out;
}

fn analyzeBuffer(source: []const u8, flags: u32) std.mem.Allocator.Error![]u8 {
    var tree = try parseTree(source, flags);
    defer tree.deinit();

    const sem = try parser.semantic.analyze(&tree, .{
        .check = flags & flag.semantic_errors != 0,
    });

    const records = try parser.semantic.module_record.collect(&tree, &sem);

    var offsets: transfer.utf16.Offsets = try .init(gpa, tree.source);
    defer offsets.deinit(gpa);

    const out = try prefixed(transfer.semantic.bufferSize(&tree, &sem, records));
    _ = transfer.semantic.serializeInto(&tree, &sem, records, &offsets, out[4..]);
    return out;
}

fn parseTree(source: []const u8, flags: u32) !parser.ast.Tree {
    return parser.parse(gpa, source, flag.parseOptions(flags));
}

fn prefixed(size: usize) ![]u8 {
    const out = try gpa.alloc(u8, 4 + size);
    std.mem.writeInt(u32, out[0..4], @intCast(size), .little);
    return out;
}
