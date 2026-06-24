const std = @import("std");
const parser = @import("parser");

// per-file throughput benchmark over the standard fixtures. reports min and
// median nanoseconds per byte plus MB/s so optimization work can be judged
// against real source of every shape: hand-written code, JSX, and a large
// compiler bundle. fetch the fixtures first with `bun profiler/load-parser-bench-files`.

const Fixture = struct {
    name: []const u8,
    path: []const u8,
    lang: parser.ast.Lang,
};

const fixtures = [_]Fixture{
    .{ .name = "react.js", .path = "files/react.js", .lang = .js },
    .{ .name = "calcom.tsx", .path = "files/calcom.tsx", .lang = .tsx },
    .{ .name = "typescript.js", .path = "files/typescript.js", .lang = .js },
};

const warmup_runs = 20;
const measured_runs = 200;

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const allocator = std.heap.smp_allocator;

    std.debug.print("fixture                bytes   min ns/B   med ns/B    MB/s\n", .{});
    std.debug.print("-------------------- -------- ---------- ---------- --------\n", .{});

    inline for (fixtures) |fixture| {
        const source = @embedFile(fixture.path);
        const bytes: f64 = @floatFromInt(source.len);

        for (0..warmup_runs) |_| {
            var tree = try parser.parse(allocator, source, .{ .lang = fixture.lang });
            std.mem.doNotOptimizeAway(tree.nodes.len);
            tree.deinit();
        }

        var samples: [measured_runs]u64 = undefined;
        for (&samples) |*sample| {
            const start = std.Io.Clock.now(.awake, io);
            var tree = try parser.parse(allocator, source, .{ .lang = fixture.lang });
            const end = std.Io.Clock.now(.awake, io);
            std.mem.doNotOptimizeAway(tree.nodes.len);
            tree.deinit();
            sample.* = @intCast(start.durationTo(end).toNanoseconds());
        }

        std.mem.sort(u64, &samples, {}, std.sort.asc(u64));
        const ns_min: f64 = @floatFromInt(samples[0]);
        const ns_med: f64 = @floatFromInt(samples[measured_runs / 2]);

        std.debug.print("{s:<20} {d:>8} {d:>10.3} {d:>10.3} {d:>8.1}\n", .{
            fixture.name,
            source.len,
            ns_min / bytes,
            ns_med / bytes,
            bytes / ns_med * 1000.0,
        });
    }
}
