const std = @import("std");
const parser = @import("parser");

pub fn main(init: std.process.Init) !void {
    const allocator = std.heap.page_allocator;

    const file_path = "test/index.ts";
    const source = try std.Io.Dir.cwd().readFileAlloc(init.io, file_path, init.arena.allocator(), std.Io.Limit.limited(10 * 1024 * 1024));

    const start = std.Io.Clock.awake.now(init.io);

    var tree = try parser.parse(allocator, source, .{ .lang = .ts });
    defer tree.deinit();

    const result = try parser.codegen.print(allocator, &tree, .{});
    defer result.deinit(allocator);

    const elapsed = @divTrunc(start.durationTo(std.Io.Clock.awake.now(init.io)).nanoseconds, 1_000_000);

    std.debug.print("elapsed: {}ms\n", .{elapsed});

    for (result.errors) |e| {
        std.debug.print("[error] {s} ({d}..{d})\n", .{ e.message, e.start, e.end });
    }
}
