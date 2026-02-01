const std = @import("std");
const js = @import("js");

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file_path = "test.js";

    const contents = try std.fs.cwd().readFileAlloc(file_path, allocator, std.Io.Limit.limited(10 * 1024 * 1024));
    defer allocator.free(contents);

    var start = try std.time.Timer.start();

    const tree = try js.parse(std.heap.page_allocator, contents, .{ .lang = js.Lang.fromPath(file_path), .source_type = js.SourceType.fromPath(file_path) });

    defer tree.deinit();

    const taken = start.read();

    const taken_ms = @as(f64, @floatFromInt(taken)) / ns_to_ms;

    var line_count: usize = 1;
    for (contents) |c| {
        if (c == '\n') line_count += 1;
    }

    const million_lines_per_sec = (@as(f64, @floatFromInt(line_count)) / 1_000_000.0) / (taken_ms / 1000.0);

    const mb_per_sec = (@as(f64, @floatFromInt(contents.len)) / 1_000_000.0) / (taken_ms / 1000.0);

    var json_start = try std.time.Timer.start();
    const json = try js.estree.toJSON(&tree, allocator, .{});
    const json_taken = json_start.read();

    const json_taken_ms = @as(f64, @floatFromInt(json_taken)) / ns_to_ms;

    defer allocator.free(json);

    // std.debug.print("\n{s}\n", .{json});

    std.debug.print("estree time taken {d:.2}\n", .{json_taken_ms});

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
