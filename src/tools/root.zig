const gen_unicode_id = @import("gen_unicode_id.zig");
const std = @import("std");
const util = @import("util");

const t = std.testing;

test "unicode_id can start and continue" {
    var id_starts, var id_contts = try gen_unicode_id.downloadAndParseProperties(t.allocator);
    defer id_starts.deinit();
    defer id_contts.deinit();

    for (0..std.math.maxInt(u21)) |ch| {
        const expected = id_starts.contains(@intCast(ch));
        if (t.expectEqual(expected, util.UnicodeId.canStartId(@intCast(ch)))) {} else |err| {
            std.debug.print("ID Start failed for codepoint: {d} (U+{X:0>4})\n", .{ ch, ch });
            return err;
        }
    }

    for (0..std.math.maxInt(u21)) |ch| {
        const expected = id_contts.contains(@intCast(ch));
        if (t.expectEqual(expected, util.UnicodeId.canContinueId(@intCast(ch)))) {} else |err| {
            std.debug.print("ID Continue failed for codepoint: {d} (U+{X:0>4})\n", .{ ch, ch });
            return err;
        }
    }
}
