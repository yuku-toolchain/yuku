const std = @import("std");
const parser = @import("parser");

const default_file = "typescript.js";
const max_file_size = 64 * 1024 * 1024;
const warmup_runs = 5;
const measured_runs = 30;

fn langFromPath(path: []const u8) parser.ast.Lang {
    if (std.mem.endsWith(u8, path, ".d.ts")) return .dts;
    if (std.mem.endsWith(u8, path, ".tsx")) return .tsx;
    if (std.mem.endsWith(u8, path, ".ts") or
        std.mem.endsWith(u8, path, ".mts") or
        std.mem.endsWith(u8, path, ".cts")) return .ts;
    if (std.mem.endsWith(u8, path, ".jsx")) return .jsx;
    return .js;
}

fn nowNs(io: std.Io) i96 {
    return std.Io.Timestamp.now(io, .awake).nanoseconds;
}

fn ms(ns: f64) f64 {
    return ns / std.time.ns_per_ms;
}

fn mbPerSec(bytes: usize, ns: f64) f64 {
    return (@as(f64, @floatFromInt(bytes)) / 1e6) / (ns / std.time.ns_per_s);
}

const Stats = struct {
    min: f64,
    median: f64,
    mean: f64,
    max: f64,
    stddev: f64,

    fn compute(samples: *[measured_runs]u64) Stats {
        std.mem.sort(u64, samples, {}, std.sort.asc(u64));

        var sum: f64 = 0;
        for (samples) |s| sum += @floatFromInt(s);
        const mean = sum / measured_runs;

        var variance: f64 = 0;
        for (samples) |s| {
            const d = @as(f64, @floatFromInt(s)) - mean;
            variance += d * d;
        }

        return .{
            .min = @floatFromInt(samples[0]),
            .median = @floatFromInt(
                (samples[measured_runs / 2] + samples[(measured_runs - 1) / 2]) / 2,
            ),
            .mean = mean,
            .max = @floatFromInt(samples[measured_runs - 1]),
            .stddev = @sqrt(variance / (measured_runs - 1)),
        };
    }

    fn print(self: Stats, bytes: usize) void {
        std.debug.print("  min:     {d:>8.3} ms  ({d:.1} MB/s)\n", .{
            ms(self.min), mbPerSec(bytes, self.min),
        });
        std.debug.print("  median:  {d:>8.3} ms  ({d:.1} MB/s)\n", .{
            ms(self.median), mbPerSec(bytes, self.median),
        });
        std.debug.print("  mean:    {d:>8.3} ms  ({d:.1} MB/s)\n", .{
            ms(self.mean), mbPerSec(bytes, self.mean),
        });
        std.debug.print("  max:     {d:>8.3} ms  ({d:.1} MB/s)\n", .{
            ms(self.max), mbPerSec(bytes, self.max),
        });
        std.debug.print("  stddev:  {d:>8.3} ms  ({d:.2}%)\n", .{
            ms(self.stddev), self.stddev / self.mean * 100,
        });
    }
};

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const gpa = std.heap.smp_allocator;

    var args: std.process.Args.Iterator = .init(init.minimal.args);
    defer args.deinit();
    _ = args.skip();
    const path: []const u8 = args.next() orelse default_file;

    const source = try std.Io.Dir.cwd().readFileAlloc(io, path, gpa, .limited(max_file_size));
    defer gpa.free(source);

    const lang = langFromPath(path);

    std.debug.print("file:    {s}\n", .{path});
    std.debug.print("size:    {d:.2} MB\n", .{@as(f64, @floatFromInt(source.len)) / 1e6});
    std.debug.print("lang:    {s}\n", .{@tagName(lang)});

    // Validation pass: make sure the input parses before timing it.
    {
        var tree = try parser.parse(gpa, source, .{ .lang = lang });
        defer tree.deinit();
        std.debug.print("nodes:   {d}\n", .{tree.nodes.len});
        if (tree.diagnostics.items.len != 0) {
            std.debug.print("warning: {d} diagnostics (first: {s})\n", .{
                tree.diagnostics.items.len,
                tree.diagnostics.items[0].message,
            });
        }
    }

    std.debug.print("warmup:  {d} runs, measured: {d} runs\n", .{ warmup_runs, measured_runs });

    // cold mode: every parse allocates from (and returns to) the general
    // purpose allocator, so each run pays the OS page-fault cost of mapping
    // ~50 MB of fresh AST memory. models parsing a single file in a
    // short-lived process
    {
        for (0..warmup_runs) |_| {
            var tree = try parser.parse(gpa, source, .{ .lang = lang });
            std.mem.doNotOptimizeAway(tree.nodes.len);
            tree.deinit();
        }

        var samples: [measured_runs]u64 = undefined;
        for (&samples) |*sample| {
            const start = nowNs(io);
            var tree = try parser.parse(gpa, source, .{ .lang = lang });
            const end = nowNs(io);
            std.mem.doNotOptimizeAway(tree.nodes.len);
            tree.deinit();
            sample.* = @intCast(end - start);
        }

        std.debug.print("\ncold allocator (per-parse mmap + page faults):\n", .{});
        Stats.compute(&samples).print(source.len);
    }

    // steady-state mode: an arena retained across parses keeps the AST pages
    // mapped, so runs measure pure parser CPU. models a long-lived process
    // (bundler, language server) parsing many files
    {
        var retained = std.heap.ArenaAllocator.init(gpa);
        defer retained.deinit();

        for (0..warmup_runs) |_| {
            var tree = try parser.parse(retained.allocator(), source, .{ .lang = lang });
            std.mem.doNotOptimizeAway(tree.nodes.len);
            tree.deinit();
            _ = retained.reset(.retain_capacity);
        }

        var samples: [measured_runs]u64 = undefined;
        for (&samples) |*sample| {
            const start = nowNs(io);
            var tree = try parser.parse(retained.allocator(), source, .{ .lang = lang });
            const end = nowNs(io);
            std.mem.doNotOptimizeAway(tree.nodes.len);
            tree.deinit();
            _ = retained.reset(.retain_capacity);
            sample.* = @intCast(end - start);
        }

        std.debug.print("\nsteady state (arena retained across parses):\n", .{});
        Stats.compute(&samples).print(source.len);
    }
}
