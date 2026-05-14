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

inline fn ms(ns: u64) f64 {
    return @as(f64, @floatFromInt(ns)) / std.time.ns_per_ms;
}

const Iters = 7;

const PhaseSamples = struct {
    parse: [Iters]u64 = undefined,
    walk: [Iters]u64 = undefined,
    resolve: [Iters]u64 = undefined,
    mangle: [Iters]u64 = undefined,
    print: [Iters]u64 = undefined,

    fn min(buf: []const u64) u64 {
        var m = buf[0];
        for (buf[1..]) |v| if (v < m) {
            m = v;
        };
        return m;
    }

    fn median(buf: []u64) u64 {
        std.mem.sort(u64, buf, {}, std.sort.asc(u64));
        return buf[buf.len / 2];
    }
};

pub fn main(init: std.process.Init) !void {
    const allocator = std.heap.page_allocator;

    const file_path = "test/fixture.ts";
    const source = try std.Io.Dir.cwd().readFileAlloc(
        init.io,
        file_path,
        init.arena.allocator(),
        std.Io.Limit.limited(64 * 1024 * 1024),
    );

    var samples = PhaseSamples{};
    var last_after: usize = 0;

    var nodes_count: usize = 0;
    var symbols_count: usize = 0;
    var refs_count: usize = 0;
    var scopes_count: usize = 0;

    // warm-up run, not measured
    {
        var tree = try parser.parse(allocator, source, .{ .lang = .ts });
        defer tree.deinit();
        const result = try minifier.minify(allocator, &tree, .{});
        defer result.deinit(allocator);
    }

    var i: u32 = 0;
    while (i < Iters) : (i += 1) {
        const t0 = std.Io.Clock.Timestamp.now(init.io, .awake);
        var tree = try parser.parse(allocator, source, .{ .lang = .ts });
        defer tree.deinit();
        const t1 = std.Io.Clock.Timestamp.now(init.io, .awake);

        var noop = NoopVisitor{};
        var sem = try parser.traverser.semantic.traverse(NoopVisitor, &tree, &noop);
        const t2 = std.Io.Clock.Timestamp.now(init.io, .awake);

        try sem.symbol_table.resolveAll(sem.scope_tree);
        const t3 = std.Io.Clock.Timestamp.now(init.io, .awake);

        try minifier.mangle.run(allocator, &tree, sem.scope_tree, sem.symbol_table, .{});
        const t4 = std.Io.Clock.Timestamp.now(init.io, .awake);

        const result = try parser.codegen.minify(allocator, &tree, .{ .format = .compact });
        defer result.deinit(allocator);
        const t5 = std.Io.Clock.Timestamp.now(init.io, .awake);

        samples.parse[i] = @intCast(t0.durationTo(t1).raw.nanoseconds);
        samples.walk[i] = @intCast(t1.durationTo(t2).raw.nanoseconds);
        samples.resolve[i] = @intCast(t2.durationTo(t3).raw.nanoseconds);
        samples.mangle[i] = @intCast(t3.durationTo(t4).raw.nanoseconds);
        samples.print[i] = @intCast(t4.durationTo(t5).raw.nanoseconds);

        last_after = result.code.len;
        if (i == 0) {
            nodes_count = tree.nodes.len;
            symbols_count = sem.symbol_table.symbols.len;
            refs_count = sem.symbol_table.references.len;
            scopes_count = sem.scope_tree.scopes.len;
        }
    }

    const before = source.len;
    const after = last_after;
    const ratio = if (before == 0) 0.0 else @as(f64, @floatFromInt(after)) / @as(f64, @floatFromInt(before)) * 100.0;
    const saved = if (before >= after) before - after else 0;

    std.debug.print("nodes: {d}  symbols: {d}  refs: {d}  scopes: {d}\n\n", .{
        nodes_count, symbols_count, refs_count, scopes_count,
    });

    std.debug.print("before: {f}  after: {f}  saved: {f} ({d:.2}% of orig)\n\n", .{
        size(before), size(after), size(saved), ratio,
    });

    std.debug.print("min over {d} runs (median in parens):\n", .{Iters});

    var buf: [Iters]u64 = undefined;
    inline for (.{
        .{ "parse", &samples.parse },
        .{ "walk", &samples.walk },
        .{ "resolve", &samples.resolve },
        .{ "mangle", &samples.mangle },
        .{ "print", &samples.print },
    }) |entry| {
        const label = entry[0];
        const arr = entry[1];
        const min_v = PhaseSamples.min(arr);
        @memcpy(buf[0..Iters], arr[0..Iters]);
        const med_v = PhaseSamples.median(buf[0..Iters]);
        std.debug.print("  {s:<8} {d:>7.3} ms  ({d:.3})\n", .{ label, ms(min_v), ms(med_v) });
    }

    var min_total: u64 = 0;
    for (0..Iters) |k| min_total += samples.parse[k] + samples.walk[k] + samples.resolve[k] + samples.mangle[k] + samples.print[k];
    std.debug.print("  {s:<8} {d:>7.3} ms  (avg)\n", .{ "total", ms(min_total) / @as(f64, @floatFromInt(Iters)) });

    var min_pipeline: u64 = std.math.maxInt(u64);
    for (0..Iters) |k| {
        const v = samples.walk[k] + samples.resolve[k] + samples.mangle[k];
        if (v < min_pipeline) min_pipeline = v;
    }
    std.debug.print("  {s:<8} {d:>7.3} ms  (min, walk+resolve+mangle)\n", .{ "mangle-pipeline", ms(min_pipeline) });
}

const NoopVisitor = struct {};
