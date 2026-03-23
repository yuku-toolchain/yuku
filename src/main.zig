const std = @import("std");
const parser = @import("parser");

pub fn main(init: std.process.Init) !void {
    const file_path = "../test/index.js";

    const source = try std.Io.Dir.cwd().readFileAlloc(init.io, file_path, init.arena.allocator(), std.Io.Limit.limited(10 * 1024 * 1024));

    const io = init.io;
    const n: i96 = 100;

    const parse_ns = try bench(io, source, false, n);
    const total_ns = try bench(io, source, true, n);
    const semantic_ns = total_ns - parse_ns;

    printMs("Parse   ", parse_ns);
    printMs("Semantic", semantic_ns);
    printMs("Total   ", total_ns);
}

fn bench(io: std.Io, source: []const u8, with_semantic: bool, n: i96) !i96 {
    const start = std.Io.Clock.awake.now(io);
    for (0..@intCast(n)) |_| {
        var tree = try parser.parse(std.heap.page_allocator, source, .{});
        if (with_semantic) _ = try parser.semantic.analyze(&tree);
        tree.deinit();
    }
    return @divTrunc(start.durationTo(std.Io.Clock.awake.now(io)).nanoseconds, n);
}

fn printMs(label: []const u8, ns: i96) void {
    const us: u64 = @intCast(@divTrunc(ns, std.time.ns_per_us));
    std.debug.print("{s}  {d}.{d:0>3} ms\n", .{ label, us / std.time.us_per_ms, us % std.time.us_per_ms });
}
