const std = @import("std");
const js = @import("js");

const printError = @import("print-error.zig").printError;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file_path = "test.js";

    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    var buffer: [4096]u8 = undefined;
    var reader = file.reader(&buffer);
    const contents = try reader.interface.allocRemaining(allocator, std.Io.Limit.limited(10 * 1024 * 1024));
    defer allocator.free(contents);

    for (0..3) |_| {
        var parser = js.Parser.init(std.heap.page_allocator, contents, .{});
        var tree = try parser.parse();
        defer tree.deinit();
    }

    var times = try std.ArrayList(i128).initCapacity(allocator, iterations);
    defer times.deinit(allocator);

    var first_tree: ?js.ParseTree = null;
    var first_json: ?[]u8 = null;

    for (0..iterations) |i| {
        var parser = js.Parser.init(std.heap.page_allocator, contents, .{});

        const start = std.time.nanoTimestamp();
        var tree = try parser.parse();
        const end = std.time.nanoTimestamp();

        try times.append(allocator, end - start);

        if (i == 0) {
            if (tree.hasErrors()) {
                std.debug.print("\n", .{});
                for (tree.errors.items) |parse_err| {
                    printError(contents, parse_err);
                }
                std.debug.print("\n", .{});
            }

            first_json = try js.estree.toJSON(&tree, allocator);
            first_tree = tree;
        } else {
            tree.deinit();
        }
    }

    var total: i128 = 0;
    var min: i128 = times.items[0];
    var max: i128 = times.items[0];

    for (times.items) |time| {
        total += time;
        if (time < min) min = time;
        if (time > max) max = time;
    }

    const avg = @divTrunc(total, iterations);

    const avg_ms = @as(f64, @floatFromInt(avg)) / ns_to_ms;
    const min_ms = @as(f64, @floatFromInt(min)) / ns_to_ms;
    const max_ms = @as(f64, @floatFromInt(max)) / ns_to_ms;

    const size_kb = @as(f64, @floatFromInt(contents.len)) / 1024.0;
    const mb_per_sec = (size_kb / 1024.0) / (avg_ms / 1000.0);

    var line_count: usize = 1;
    for (contents) |c| {
        if (c == '\n') line_count += 1;
    }
    const million_lines_per_sec = (@as(f64, @floatFromInt(line_count)) / 1_000_000.0) / (avg_ms / 1000.0);

    std.debug.print("Min: {d:.3} ms | Max: {d:.3} ms | Avg: {d:.3} ms\n", .{ min_ms, max_ms, avg_ms });
    std.debug.print("Throughput: {d:.2} MB/sec | {d:.2} million lines/sec\n", .{ mb_per_sec, million_lines_per_sec });

    if (first_json) |json| {
        defer allocator.free(json);
        std.debug.print("\n{s}\n", .{json});
    }

    if (first_tree) |tree| {
        tree.deinit();
    }
}

const iterations = 10;
const ns_to_ms = 1_000_000.0;
