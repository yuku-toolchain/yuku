const std = @import("std");
const js = @import("js");

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

    var start = try std.time.Timer.start();

    const tree = try js.parse(std.heap.page_allocator, contents, .{});
    defer tree.deinit();

    const taken = start.read();

    const taken_ms = @as(f64, @floatFromInt(taken)) / ns_to_ms;

    var line_count: usize = 1;
    for (contents) |c| {
        if (c == '\n') line_count += 1;
    }

    const million_lines_per_sec = (@as(f64, @floatFromInt(line_count)) / 1_000_000.0) / (taken_ms / 1000.0);

    const mb_per_sec = (@as(f64, @floatFromInt(contents.len)) / 1_000_000.0) / (taken_ms / 1000.0);

    const json = try js.estree.toJSON(&tree, allocator);
    defer allocator.free(json);

    // std.debug.print("\n{s}\n", .{json});

    if (tree.hasDiagnostics()) {
        for (tree.diagnostics.items) |err| {
            const start_pos = getLineAndColumn(contents, err.span.start);
            const end_pos = getLineAndColumn(contents, err.span.end);

            std.debug.print("\nError: {s} at test.js:{d}:{d} to test.js:{d}:{d}\n", .{ err.message, start_pos.line, start_pos.col, end_pos.line, end_pos.col });
            if (err.help) |help| std.debug.print("  Help: {s}\n\n", .{help});
        }
    }

    std.debug.print("\n\n{d:.2}ms | {d:.2} million lines/sec | {d:.2} MB/s\n\n", .{ taken_ms, million_lines_per_sec, mb_per_sec });
}

const ns_to_ms = 1_000_000.0;

fn getLineAndColumn(contents: []const u8, offset: usize) struct { line: usize, col: usize } {
    var line: usize = 1;
    var col: usize = 1;

    for (contents[0..@min(offset, contents.len)]) |char| {
        if (char == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    return .{ .line = line, .col = col };
}
