const std = @import("std");

/// The UTF-16 offset of each byte offset into UTF-8 text, as a JavaScript string indexes it.
pub const Offsets = struct {
    /// The bytes before the first non-ASCII byte, where both offsets agree.
    ascii_len: u32,
    bytes_len: u32,
    units_len: u32,
    /// The 64-byte block that `bases` and `starts` begin at.
    block_first: u32,
    /// The UTF-16 offset at the start of each block.
    bases: []u32,
    /// The bytes of each block that begin a UTF-16 unit, one bit per byte.
    starts: []u64,

    pub fn init(gpa: std.mem.Allocator, text: []const u8) error{OutOfMemory}!Offsets {
        std.debug.assert(text.len <= std.math.maxInt(u32));
        const bytes_len: u32 = @intCast(text.len);
        const ascii_len = asciiPrefixLen(text);
        if (ascii_len == bytes_len) {
            return .{
                .ascii_len = ascii_len,
                .bytes_len = bytes_len,
                .units_len = bytes_len,
                .block_first = 0,
                .bases = &.{},
                .starts = &.{},
            };
        }
        std.debug.assert(ascii_len < bytes_len);

        const block_first = @divFloor(ascii_len, block_size);
        const blocks_count = @divFloor(bytes_len, block_size) - block_first + 1;

        const bases = try gpa.alloc(u32, blocks_count);
        errdefer gpa.free(bases);
        const starts = try gpa.alloc(u64, blocks_count);

        var units: u32 = block_first * block_size;
        var carry: u64 = 0;
        for (bases, starts, block_first..) |*base, *start, block| {
            const masks = blockMasks(text, block * block_size);
            start.* = (masks.starts | (masks.four_byte_leads << 1) | carry) & masks.in_text;
            carry = masks.four_byte_leads >> (block_size - 1);
            base.* = units;
            units += @popCount(start.*);
        }
        std.debug.assert(units < bytes_len);

        return .{
            .ascii_len = ascii_len,
            .bytes_len = bytes_len,
            .units_len = units,
            .block_first = block_first,
            .bases = bases,
            .starts = starts,
        };
    }

    pub fn deinit(offsets: *Offsets, gpa: std.mem.Allocator) void {
        gpa.free(offsets.bases);
        gpa.free(offsets.starts);
        offsets.* = undefined;
    }

    pub inline fn at(offsets: *const Offsets, offset: u32) u32 {
        std.debug.assert(offset <= offsets.bytes_len);
        if (offset <= offsets.ascii_len) return offset;
        const block = @divFloor(offset, block_size) - offsets.block_first;
        const below = (@as(u64, 1) << @intCast(offset % block_size)) - 1;
        const units = offsets.bases[block] + @popCount(offsets.starts[block] & below);
        std.debug.assert(units <= offsets.units_len);
        return units;
    }

    const block_size = 64;

    const Masks = struct { starts: u64, four_byte_leads: u64, in_text: u64 };

    fn blockMasks(text: []const u8, offset: usize) Masks {
        std.debug.assert(offset <= text.len);
        var block: [block_size]u8 = @splat(0);
        const count = @min(block_size, text.len - offset);
        @memcpy(block[0..count], text[offset..][0..count]);

        var masks: Masks = .{
            .starts = 0,
            .four_byte_leads = 0,
            .in_text = if (count == block_size)
                ~@as(u64, 0)
            else
                (@as(u64, 1) << @intCast(count)) - 1,
        };
        inline for (0..block_size / 16) |lane| {
            const chunk: @Vector(16, u8) = block[lane * 16 ..][0..16].*;
            const continuation = chunk & @as(@Vector(16, u8), @splat(0xC0)) ==
                @as(@Vector(16, u8), @splat(0x80));
            const not_continuation: u16 = ~@as(u16, @bitCast(continuation));
            const four_byte_lead: u16 = @bitCast(chunk >= @as(@Vector(16, u8), @splat(0xF0)));
            masks.starts |= @as(u64, not_continuation) << (lane * 16);
            masks.four_byte_leads |= @as(u64, four_byte_lead) << (lane * 16);
        }
        return masks;
    }
};

/// Encodes UTF-16 code units as UTF-8 into `target`, each lone surrogate as U+FFFD the way
/// `TextEncoder` does, and returns the bytes written.
pub fn encodeUtf8(units: []align(1) const u16, target: []u8) []u8 {
    std.debug.assert(target.len >= units.len * 3);
    var source_index: usize = 0;
    var target_index: usize = 0;
    while (source_index < units.len) {
        if (units.len - source_index >= 16) {
            const chunk: @Vector(16, u16) = units[source_index..][0..16].*;
            if (@reduce(.Or, chunk & @as(@Vector(16, u16), @splat(0xFF80))) == 0) {
                const bytes: @Vector(16, u8) = @truncate(chunk);
                target[target_index..][0..16].* = bytes;
                source_index += 16;
                target_index += 16;
                continue;
            }
        }
        const end = @min(source_index + 16, units.len);
        while (source_index < end) {
            const unit = units[source_index];
            const next: u16 = if (source_index + 1 < units.len) units[source_index + 1] else 0;
            const encoded = encodeUnit(unit, next, target[target_index..]);
            source_index += encoded.units;
            target_index += encoded.bytes;
        }
    }
    std.debug.assert(source_index == units.len);
    std.debug.assert(target_index <= target.len);
    return target[0..target_index];
}

const Encoded = struct { units: u2, bytes: u3 };

fn encodeUnit(unit: u16, next: u16, target: []u8) Encoded {
    std.debug.assert(target.len >= 3);
    if (unit < 0x80) {
        target[0] = @intCast(unit);
        return .{ .units = 1, .bytes = 1 };
    }
    if (unit < 0x800) {
        target[0] = @intCast(0xC0 | (unit >> 6));
        target[1] = @intCast(0x80 | (unit & 0x3F));
        return .{ .units = 1, .bytes = 2 };
    }
    if (unit >= 0xD800 and unit <= 0xDBFF and next >= 0xDC00 and next <= 0xDFFF) {
        const code_point = 0x10000 + ((@as(u21, unit) - 0xD800) << 10) + (next - 0xDC00);
        std.debug.assert(code_point <= 0x10FFFF);
        std.debug.assert(target.len >= 4);
        target[0] = @intCast(0xF0 | (code_point >> 18));
        target[1] = @intCast(0x80 | ((code_point >> 12) & 0x3F));
        target[2] = @intCast(0x80 | ((code_point >> 6) & 0x3F));
        target[3] = @intCast(0x80 | (code_point & 0x3F));
        return .{ .units = 2, .bytes = 4 };
    }
    const code_point: u16 = if (unit >= 0xD800 and unit <= 0xDFFF) 0xFFFD else unit;
    target[0] = @intCast(0xE0 | (code_point >> 12));
    target[1] = @intCast(0x80 | ((code_point >> 6) & 0x3F));
    target[2] = @intCast(0x80 | (code_point & 0x3F));
    return .{ .units = 1, .bytes = 3 };
}

fn asciiPrefixLen(text: []const u8) u32 {
    const high: @Vector(16, u8) = @splat(0x80);
    var index: usize = 0;
    while (text.len - index >= 64) : (index += 64) {
        const block: @Vector(64, u8) = text[index..][0..64].*;
        if (@reduce(.Or, block & @as(@Vector(64, u8), @splat(0x80))) != 0) break;
    }
    while (text.len - index >= 16) : (index += 16) {
        const chunk: @Vector(16, u8) = text[index..][0..16].*;
        if (@reduce(.Or, chunk & high) != 0) break;
    }
    while (index < text.len and text[index] < 0x80) index += 1;
    return @intCast(index);
}

const testing = std.testing;

fn expectEncoded(units: []const u16, expected: []const u8) !void {
    var target: [160]u8 = undefined;
    try testing.expectEqualSlices(u8, expected, encodeUtf8(units, target[0 .. units.len * 3]));
}

test "encodeUtf8 encodes every sequence length and replaces lone surrogates" {
    try expectEncoded(&.{}, "");
    try expectEncoded(&.{ 'a', 0x7F }, "a\x7F");
    try expectEncoded(&.{ 0x80, 0x7FF }, "\u{80}\u{7FF}");
    try expectEncoded(&.{ 0x800, 0xFFFF }, "\u{800}\u{FFFF}");
    try expectEncoded(&.{ 0xD83D, 0xDE00 }, "\u{1F600}");
    try expectEncoded(&.{ 0xDBFF, 0xDFFF }, "\u{10FFFF}");
    try expectEncoded(&.{0xD800}, "\u{FFFD}");
    try expectEncoded(&.{ 0xDC00, 0xD800 }, "\u{FFFD}\u{FFFD}");
    try expectEncoded(&.{ 0xD800, 'a' }, "\u{FFFD}a");
}

test "encodeUtf8 agrees with std across vector and scalar runs" {
    var prng: std.Random.DefaultPrng = .init(0x7556_7531);
    const random = prng.random();
    for (0..4000) |_| {
        var units: [40]u16 = undefined;
        var len: usize = 0;
        const len_max = random.uintAtMost(usize, units.len - 1);
        while (len < len_max) {
            switch (random.uintLessThan(u8, 8)) {
                0 => {
                    units[len] = random.intRangeAtMost(u16, 0x80, 0xD7FF);
                    len += 1;
                },
                1 => {
                    units[len] = random.intRangeAtMost(u16, 0xD800, 0xDBFF);
                    units[len + 1] = random.intRangeAtMost(u16, 0xDC00, 0xDFFF);
                    len += 2;
                },
                2 => {
                    units[len] = random.intRangeAtMost(u16, 0xE000, 0xFFFF);
                    len += 1;
                },
                else => {
                    units[len] = random.uintLessThan(u16, 0x80);
                    len += 1;
                },
            }
        }
        var expected: [160]u8 = undefined;
        const expected_len = try std.unicode.utf16LeToUtf8(&expected, units[0..len]);
        try expectEncoded(units[0..len], expected[0..expected_len]);
    }
}

fn expectOffsets(text: []const u8) !void {
    var offsets: Offsets = try .init(testing.allocator, text);
    defer offsets.deinit(testing.allocator);

    var units: u32 = 0;
    var index: usize = 0;
    while (index < text.len) {
        const len = std.unicode.utf8ByteSequenceLength(text[index]) catch unreachable;
        try testing.expectEqual(units, offsets.at(@intCast(index)));
        for (1..len) |k| {
            const inside: u32 = if (len == 4 and k > 1) 2 else 1;
            try testing.expectEqual(units + inside, offsets.at(@intCast(index + k)));
        }
        units += if (len == 4) 2 else 1;
        index += len;
    }
    try testing.expectEqual(units, offsets.at(@intCast(text.len)));
    try testing.expectEqual(units, offsets.units_len);
}

test "Offsets agree with ASCII text and count each sequence's units" {
    try expectOffsets("");
    try expectOffsets("plain ascii");
    try expectOffsets("\u{E9}");
    try expectOffsets("a\u{E9}b\u{4E2D}c\u{1F600}d");
    var emoji: [160]u8 = undefined;
    for (0..40) |i| @memcpy(emoji[i * 4 ..][0..4], "\u{1F600}");
    try expectOffsets(&emoji);
}

test "Offsets cross 64-byte blocks, including a sequence split between two" {
    for ([_][]const u8{ "\u{E9}", "\u{4E2D}", "\u{1F600}" }) |sequence| {
        for (0..130) |prefix| {
            var text: [200]u8 = @splat('a');
            @memcpy(text[prefix..][0..sequence.len], sequence);
            try expectOffsets(text[0 .. prefix + sequence.len]);
            try expectOffsets(text[0..]);
        }
    }
}
