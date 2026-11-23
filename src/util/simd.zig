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

pub inline fn loadTail(bytes: []const u8) Chunk {
    const n = bytes.len;
    std.debug.assert(n < 16);
    var low: u64 = 0;
    var high: u64 = 0;
    if (n >= 8) {
        low = std.mem.readInt(u64, bytes[0..8], .little);
        const last = std.mem.readInt(u64, bytes[n - 8 ..][0..8], .little);
        high = (last >> 1) >> @intCast(8 * (16 - n) - 1);
    } else if (n >= 4) {
        const first = std.mem.readInt(u32, bytes[0..4], .little);
        const last = std.mem.readInt(u32, bytes[n - 4 ..][0..4], .little);
        low = first | @as(u64, last) << @intCast(8 * (n - 4));
    } else if (n > 0) {
        low = bytes[0] | @as(u64, bytes[n >> 1]) << @intCast(8 * (n >> 1)) |
            @as(u64, bytes[n - 1]) << @intCast(8 * (n - 1));
    }
    var out: [16]u8 = undefined;
    std.mem.writeInt(u64, out[0..8], low, .little);
    std.mem.writeInt(u64, out[8..16], high, .little);
    return out;
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

pub inline fn mask64(bytes: *const [64]u8, comptime predicate: anytype) u64 {
    if (comptime has_nibble_mask) {
        const lane_bits: Chunk = .{ 1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128 };
        var bits: [4]Chunk = undefined;
        inline for (&bits, 0..) |*chunk_bits, k| {
            const chunk: Chunk = bytes[16 * k ..][0..16].*;
            chunk_bits.* = @select(u8, predicate(chunk), lane_bits, splat(0));
        }
        const halves = pairSums(pairSums(bits[0], bits[1]), pairSums(bits[2], bits[3]));
        const mask: @Vector(2, u64) = @bitCast(pairSums(halves, halves));
        return mask[0];
    }
    var mask: u64 = 0;
    inline for (0..4) |k| {
        const lanes: @Vector(16, bool) = predicate(@as(Chunk, bytes[16 * k ..][0..16].*));
        mask |= @as(u64, @as(u16, @bitCast(lanes))) << (16 * k);
    }
    return mask;
}

inline fn pairSums(a: Chunk, b: Chunk) Chunk {
    if (comptime has_neon) return @"llvm.aarch64.neon.addp.v16i8"(a, b);
    const even = comptime pairLanes(0);
    const odd = comptime pairLanes(1);
    return @shuffle(u8, a, b, even) +% @shuffle(u8, a, b, odd);
}

fn pairLanes(comptime first: comptime_int) @Vector(16, i32) {
    var lanes: [16]i32 = undefined;
    for (0..8) |i| {
        lanes[i] = 2 * i + first;
        lanes[8 + i] = ~@as(i32, 2 * i + first);
    }
    return lanes;
}

// aarch64 lacks movemask, `shrn` packs a nibble per lane
const has_nibble_mask = builtin.target.cpu.arch.isAarch64() and builtin.target.cpu.arch.endian() == .little;

const has_neon = has_nibble_mask and builtin.zig_backend == .stage2_llvm;

extern fn @"llvm.aarch64.neon.tbl1.v16i8"(table: Chunk, indices: Chunk) Chunk;
extern fn @"llvm.aarch64.neon.addp.v16i8"(a: Chunk, b: Chunk) Chunk;

pub const ByteSet = struct {
    low: [16]u8,
    high: [16]u8,

    pub const is_fast = has_neon;

    pub fn init(comptime members: [256]bool) ByteSet {
        comptime {
            @setEvalBranchQuota(10_000);
            var set: ByteSet = .{ .low = @splat(0), .high = @splat(0) };
            var classes: [8]u16 = undefined;
            var class_count: usize = 0;
            for (0..16) |high| {
                var lows: u16 = 0;
                for (0..16) |low| {
                    if (members[high << 4 | low]) lows |= 1 << low;
                }
                if (lows == 0) continue;
                const class = for (classes[0..class_count], 0..) |known, index| {
                    if (known == lows) break index;
                } else class: {
                    if (class_count == classes.len) @compileError("the set has over eight classes");
                    classes[class_count] = lows;
                    class_count += 1;
                    break :class class_count - 1;
                };
                set.high[high] = 1 << class;
                for (0..16) |low| {
                    if (lows >> low & 1 != 0) set.low[low] |= 1 << class;
                }
            }
            for (0..256) |byte| std.debug.assert(set.containsByte(byte) == members[byte]);
            return set;
        }
    }

    pub inline fn contains(comptime self: ByteSet, chunk: Chunk) @Vector(16, bool) {
        comptime std.debug.assert(is_fast);
        const low = @"llvm.aarch64.neon.tbl1.v16i8"(self.low, chunk & splat(0x0f));
        const high = @"llvm.aarch64.neon.tbl1.v16i8"(self.high, chunk >> @splat(4));
        return (low & high) != splat(0);
    }

    fn containsByte(self: ByteSet, byte: u8) bool {
        return self.low[byte & 0x0f] & self.high[byte >> 4] != 0;
    }
};

inline fn nibbleMask(lanes: @Vector(16, bool)) u64 {
    comptime std.debug.assert(has_nibble_mask);
    const bytes = @select(u8, lanes, splat(0xFF), splat(0));
    const halves: @Vector(8, u16) = @bitCast(bytes);
    const nibbles: @Vector(8, u8) = @truncate(halves >> @splat(4));
    return @bitCast(nibbles);
}

const testing = std.testing;

test "a tail loads its bytes and zero past them" {
    const text = "0123456789abcdef";
    for (0..16) |n| {
        var expected: [16]u8 = @splat(0);
        @memcpy(expected[0..n], text[0..n]);
        try testing.expectEqual(@as(Chunk, expected), loadTail(text[0..n]));
    }
}

test "a byte set holds its members alone" {
    const members = comptime members: {
        var table: [256]bool = @splat(false);
        for ("$_09azAZ\x7f") |byte| table[byte] = true;
        break :members table;
    };
    const set: ByteSet = comptime .init(members);
    for (0..256) |byte| try testing.expectEqual(members[byte], set.containsByte(@intCast(byte)));

    if (!ByteSet.is_fast) return;
    var bytes: [16]u8 = undefined;
    for (0..256) |first| {
        for (&bytes, 0..) |*byte, lane| byte.* = @truncate(first + lane * 37);
        const lanes: [16]bool = set.contains(bytes);
        for (bytes, lanes) |byte, lane| try testing.expectEqual(members[byte], lane);
    }
}

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

test "mask64 sets the bit of every matching byte" {
    const isX = struct {
        inline fn call(chunk: Chunk) @Vector(16, bool) {
            return chunk == splat('x');
        }
    }.call;
    var prng: std.Random.DefaultPrng = .init(0);
    const random = prng.random();
    for (0..256) |_| {
        var bytes: [64]u8 = undefined;
        var expected: u64 = 0;
        for (&bytes, 0..) |*byte, i| {
            const matches = random.boolean();
            byte.* = if (matches) 'x' else 'y';
            expected |= @as(u64, @intFromBool(matches)) << @intCast(i);
        }
        try testing.expectEqual(expected, mask64(&bytes, isX));
    }
}

test "loadChunk reads at an unaligned offset" {
    var bytes: [20]u8 = undefined;
    for (&bytes, 0..) |*byte, i| byte.* = @intCast(i);
    const chunk = loadChunk(&bytes, 3);
    try testing.expectEqual(@as(u8, 3), chunk[0]);
    try testing.expectEqual(@as(u8, 18), chunk[15]);
}
