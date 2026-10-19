const std = @import("std");
const parser = @import("parser");
const transfer = @import("transfer/root.zig");

const gpa = std.heap.wasm_allocator;

// packed by packages/yuku/core/wasm.ts
const flag = struct {
    const source_type_mask = 0b11;
    const lang_shift = 2;
    const preserve_parens = 1 << 5;
    const semantic_errors = 1 << 6;
    const attach_comments = 1 << 7;
    const tokens = 1 << 8;
    const dialect = 1 << 9;
};

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
    const out = parseBuffer(ptr[0..len], flags) catch return 0;
    return @intFromPtr(out.ptr);
}

export fn analyze(ptr: [*]const u8, len: usize, flags: u32) usize {
    const out = analyzeBuffer(ptr[0..len], flags) catch return 0;
    return @intFromPtr(out.ptr);
}

export fn dialect() usize {
    if (!parser.dialect.bound) return 0;
    return @intFromPtr(&description);
}

fn parseBuffer(source: []const u8, flags: u32) ![]u8 {
    var tree = try parseTree(source, flags);
    defer tree.deinit();

    if (flags & flag.semantic_errors != 0) try parser.semantic.check(&tree);

    const out = try prefixed(transfer.bufferSize(&tree));
    _ = transfer.serializeInto(&tree, out[4..]);
    return out;
}

fn analyzeBuffer(source: []const u8, flags: u32) ![]u8 {
    var tree = try parseTree(source, flags);
    defer tree.deinit();

    const sem = try parser.semantic.analyze(&tree, .{
        .check = flags & flag.semantic_errors != 0,
    });

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
        .comments = if (flags & flag.attach_comments != 0) .attach else .collect,
        .tokens = flags & flag.tokens != 0,
        .dialect = flags & flag.dialect != 0,
    });
}

fn prefixed(size: usize) ![]u8 {
    const out = try gpa.alloc(u8, 4 + size);
    std.mem.writeInt(u32, out[0..4], @intCast(size), .little);
    return out;
}
