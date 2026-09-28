const gen_unicode_id = @import("gen_unicode_id.zig");
const std = @import("std");
const util = @import("util");

const t = std.testing;

test "unicode_id can start and continue" {
    var id_starts, var id_continues = try gen_unicode_id.downloadAndParseProperties(
        t.io,
        t.allocator,
    );
    defer id_starts.deinit(t.allocator);
    defer id_continues.deinit(t.allocator);

    for (0..std.math.maxInt(u21)) |ch| {
        const expected = id_starts.contains(@intCast(ch));
        t.expectEqual(expected, util.UnicodeId.canStartId(@intCast(ch))) catch |err| {
            std.debug.print("ID Start failed for codepoint: {d} (U+{X:0>4})\n", .{ ch, ch });
            return err;
        };
    }

    for (0..std.math.maxInt(u21)) |ch| {
        const expected = id_continues.contains(@intCast(ch));
        t.expectEqual(expected, util.UnicodeId.canContinueId(@intCast(ch))) catch |err| {
            std.debug.print("ID Continue failed for codepoint: {d} (U+{X:0>4})\n", .{ ch, ch });
            return err;
        };
    }
}
