const std = @import("std");
const parser = @import("parser");
const minifier = @import("minifier");

const Size = struct {
    bytes: usize,

    pub fn format(self: Size, w: *std.Io.Writer) std.Io.Writer.Error!void {
        const b = self.bytes;
        if (b < 1024) return w.print("{d} B", .{b});
        const f = @as(f64, @floatFromInt(b));
        if (b < 1024 * 1024) return w.print("{d:.2} KB", .{f / 1024.0});
        if (b < 1024 * 1024 * 1024) return w.print("{d:.2} MB", .{f / (1024.0 * 1024.0)});
        return w.print("{d:.2} GB", .{f / (1024.0 * 1024.0 * 1024.0)});
    }
};

fn size(bytes: usize) Size {
    return .{ .bytes = bytes };
}

pub fn main(init: std.process.Init) !void {
    const allocator = std.heap.page_allocator;

    const file_path = "test/fixture.ts";
    const source = try std.Io.Dir.cwd().readFileAlloc(
        init.io,
        file_path,
        init.arena.allocator(),
        std.Io.Limit.limited(64 * 1024 * 1024),
    );

    const t0 = std.Io.Clock.Timestamp.now(init.io, .awake);

    var tree = try parser.parse(allocator, source, .{ .lang = .ts });
    defer tree.deinit();
    const t1 = std.Io.Clock.Timestamp.now(init.io, .awake);

    const result = try minifier.minify(allocator, &tree, .{});
    defer result.deinit(allocator);
    const t2 = std.Io.Clock.Timestamp.now(init.io, .awake);

    const parse_ns: u64 = @intCast(t0.durationTo(t1).raw.nanoseconds);
    const minify_ns: u64 = @intCast(t1.durationTo(t2).raw.nanoseconds);

    const before = source.len;
    const after = result.code.len;
    const ratio = if (before == 0) 0.0 else @as(f64, @floatFromInt(after)) / @as(f64, @floatFromInt(before)) * 100.0;
    const saved = if (before >= after) before - after else 0;

    const parse_ms = @as(f64, @floatFromInt(parse_ns)) / std.time.ns_per_ms;
    const minify_ms = @as(f64, @floatFromInt(minify_ns)) / std.time.ns_per_ms;

    std.debug.print("file:    {s}\n", .{file_path});
    std.debug.print("before:  {f}\n", .{size(before)});
    std.debug.print("after:   {f}\n", .{size(after)});
    std.debug.print("saved:   {f} ({d:.2}% of original)\n", .{ size(saved), ratio });
    std.debug.print("parse:   {d:.3} ms\n", .{parse_ms});
    std.debug.print("minify:  {d:.3} ms\n", .{minify_ms});
    std.debug.print("total:   {d:.3} ms\n", .{parse_ms + minify_ms});
}
