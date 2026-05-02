const std = @import("std");
const parser = @import("parser");

pub fn main(init: std.process.Init) !void {
    const file_path = "test/index.ts";

    const source = try std.Io.Dir.cwd().readFileAlloc(init.io, file_path, init.arena.allocator(), std.Io.Limit.limited(10 * 1024 * 1024));

    {
        _ = try parser.parse(std.heap.page_allocator, source, .{ .lang = .ts });
    }

    const io = init.io;
    const n: i96 = 100;

    const parse_ns = try bench(io, source, .parse_only, n);
    // const semantic_ns = try bench(io, source, .with_semantic, n);
    // const resolve_ns = try bench(io, source, .with_resolve, n);

    printMs("Parse      ", parse_ns);
    // printMs("Semantic   ", semantic_ns - parse_ns);
    // printMs("resolveAll ", resolve_ns - semantic_ns);
    // printMs("Total      ", resolve_ns);
}

const BenchMode = enum { parse_only, with_semantic, with_resolve };

fn bench(io: std.Io, source: []const u8, mode: BenchMode, n: i96) !i96 {
    const start = std.Io.Clock.awake.now(io);
    for (0..@intCast(n)) |_| {
        var tree = try parser.parse(std.heap.page_allocator, source, .{ .lang = .ts });

        if (mode != .parse_only) {
            var result = try parser.semantic.analyze(&tree);
            if (mode == .with_resolve)
                try result.symbol_table.resolveAll(tree.allocator(), result.scope_tree);
        }
        tree.deinit();
    }
    return @divTrunc(start.durationTo(std.Io.Clock.awake.now(io)).nanoseconds, n);
}

fn printMs(label: []const u8, ns: i96) void {
    const us: u64 = @intCast(@divTrunc(ns, std.time.ns_per_us));
    std.debug.print("{s}  {d}.{d:0>3} ms\n", .{ label, us / std.time.us_per_ms, us % std.time.us_per_ms });
}
