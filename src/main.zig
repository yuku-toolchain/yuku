const std = @import("std");
const parser = @import("parser");

pub fn main(init: std.process.Init) !void {
    const Io = init.io;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file_path = "test.js";

    const contents = try std.Io.Dir.cwd().readFileAlloc(Io, file_path, allocator, std.Io.Limit.limited(10 * 1024 * 1024));
    defer allocator.free(contents);

    const start = std.Io.Clock.Timestamp.now(Io, .real);

    const tree = try parser.parse(std.heap.page_allocator, contents, .{ .lang = parser.Lang.fromPath(file_path), .source_type = .script });

    defer tree.deinit();

    const end = std.Io.Clock.Timestamp.now(Io, .real);

    const taken = start.durationTo(end);

    const taken_ms = @as(f64, @floatFromInt(taken.raw.toNanoseconds())) / std.time.ns_per_ms;

    var line_count: usize = 1;
    for (contents) |c| {
        if (c == '\n') line_count += 1;
    }

    const million_lines_per_sec = (@as(f64, @floatFromInt(line_count)) / 1_000_000.0) / (taken_ms / 1000.0);

    const mb_per_sec = (@as(f64, @floatFromInt(contents.len)) / 1_000_000.0) / (taken_ms / 1000.0);

    const json = try parser.estree.toJSON(&tree, allocator, .{});
    defer allocator.free(json);

    // std.debug.print("{s}", .{json});

    if (tree.hasDiagnostics()) {
        for (tree.diagnostics) |err| {
            const start_pos = getLineAndColumn(contents, err.span.start);
            const end_pos = getLineAndColumn(contents, err.span.end);

            std.debug.print("\nError: {s} at test.js:{d}:{d} to test.js:{d}:{d}\n", .{ err.message, start_pos.line, start_pos.col, end_pos.line, end_pos.col });
            if (err.help) |help| std.debug.print("  Help: {s}\n\n", .{help});
            if (err.labels.len > 0) {
                for (err.labels) |label| {
                    const label_start_pos = getLineAndColumn(contents, label.span.start);
                    const label_end_pos = getLineAndColumn(contents, label.span.end);

                    std.debug.print("  Label: {s} at test.js:{d}:{d} to test.js:{d}:{d}\n", .{ label.message, label_start_pos.line, label_start_pos.col, label_end_pos.line, label_end_pos.col });
                }
            }
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
