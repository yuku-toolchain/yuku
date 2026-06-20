const std = @import("std");
const parser = @import("parser");

const source_path = "profiler/files/typescript.js";

const warmup_runs = 50;
const measured_runs = 300;

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const allocator = std.heap.smp_allocator;

    const source = try std.Io.Dir.cwd().readFileAlloc(io, source_path, allocator, .unlimited);
    defer allocator.free(source);

    for (0..warmup_runs) |_| {
        var tree = try parser.parse(allocator, source, .{ .lang = .js });
        std.mem.doNotOptimizeAway(tree.nodes.len);
        tree.deinit();
    }

    var samples: [measured_runs]u64 = undefined;
    for (&samples) |*sample| {
        const start = std.Io.Clock.now(.awake, io);
        var tree = try parser.parse(allocator, source, .{ .lang = .js });
        const end = std.Io.Clock.now(.awake, io);

        std.mem.doNotOptimizeAway(tree.nodes.len);
        tree.deinit();

        sample.* = @intCast(start.durationTo(end).toNanoseconds());
    }

    std.mem.sort(u64, &samples, {}, std.sort.asc(u64));
    const median_ms = @as(f64, @floatFromInt(samples[samples.len / 2])) / std.time.ns_per_ms;

    std.debug.print("parse: {d:.3} ms\n", .{median_ms});
}
