const std = @import("std");
const builtin = @import("builtin");

/// 16 bytes as one vector.
pub const Chunk = @Vector(16, u8);

/// `byte` in every lane.
pub inline fn splat(byte: u8) Chunk {
    return @splat(byte);
}

/// Unaligned 16-byte load at `offset`.
pub inline fn loadChunk(bytes: []const u8, offset: usize) Chunk {
    std.debug.assert(offset <= bytes.len);
    std.debug.assert(bytes.len - offset >= 16);
    return @as(*align(1) const Chunk, @ptrCast(bytes.ptr + offset)).*;
}

/// First set lane, 16 if none.
pub inline fn firstTrueLane(lanes: @Vector(16, bool)) u32 {
    const index: u32 = if (comptime has_nibble_mask)
        @ctz(nibbleMask(lanes)) >> 2
    else
        @ctz(@as(u16, @bitCast(lanes)));
    std.debug.assert(index <= 16);
    return index;
}

/// Count of leading set lanes.
pub inline fn leadingTrueCount(lanes: @Vector(16, bool)) u32 {
    const count: u32 = if (comptime has_nibble_mask)
        @ctz(~nibbleMask(lanes)) >> 2
    else
        @ctz(~@as(u16, @bitCast(lanes)));
    std.debug.assert(count <= 16);
    return count;
}

// aarch64 lacks movemask, `shrn` packs a nibble per lane
const has_nibble_mask = builtin.target.cpu.arch.isAarch64();

inline fn nibbleMask(lanes: @Vector(16, bool)) u64 {
    comptime std.debug.assert(has_nibble_mask);
    const bytes = @select(u8, lanes, splat(0xFF), splat(0));
    const halves: @Vector(8, u16) = @bitCast(bytes);
    const nibbles: @Vector(8, u8) = @truncate(halves >> @splat(4));
    return @bitCast(nibbles);
}

const testing = std.testing;

test "lane scans find every boundary" {
    for (0..17) |boundary| {
        var bytes: [16]u8 = @splat('a');
        if (boundary < 16) bytes[boundary] = 'b';
        const chunk = loadChunk(&bytes, 0);
        const expected: u32 = @intCast(boundary);
        try testing.expectEqual(expected, firstTrueLane(chunk == splat('b')));
        try testing.expectEqual(expected, leadingTrueCount(chunk == splat('a')));
    }
}

test "loadChunk reads at an unaligned offset" {
    var bytes: [20]u8 = undefined;
    for (&bytes, 0..) |*byte, i| byte.* = @intCast(i);
    const chunk = loadChunk(&bytes, 3);
    try testing.expectEqual(@as(u8, 3), chunk[0]);
    try testing.expectEqual(@as(u8, 18), chunk[15]);
}
