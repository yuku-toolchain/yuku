const std = @import("std");
const js = @import("js");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = @embedFile("test.js");

    var parser = js.Parser.init(std.heap.page_allocator, source, .{
        .source_type = .Module,
        .lang = .Js,
        .is_strict = true,
    });

    var start = try std.time.Timer.start();

    const tree = try parser.parse();
    defer tree.deinit();

    const taken = start.read();

    const taken_ms = @as(f64, @floatFromInt(taken)) / ns_to_ms;

    var line_count: usize = 1;
        for (source) |c| {
            if (c == '\n') line_count += 1;
        }

    const million_lines_per_sec = (@as(f64, @floatFromInt(line_count)) / 1_000_000.0) / (taken_ms / 1000.0);

    const json = try js.estree.toJSON(&tree, allocator);
    defer allocator.free(json);

    // std.debug.print("\n{s}\n", .{json});

    if (tree.hasDiagnostics()) {
        for (tree.diagnostics.items) |err| {
            const start_pos = getLineAndColumn(source, err.span.start);
            const end_pos = getLineAndColumn(source, err.span.end);

            std.debug.print("Error: {s} at src/test.js:{d}:{d} to src/test.js:{d}:{d}\n", .{
                err.message,
                start_pos.line, start_pos.col,
                end_pos.line, end_pos.col
            });
            if (err.help) |help| std.debug.print("  Help: {s}", .{help});
        }
    }

    std.debug.print("\n\n{d:.2}ms | {d:.2} million lines/sec\n\n", .{ taken_ms, million_lines_per_sec });
}

const ns_to_ms = 1_000_000.0;

fn getLineAndColumn(source: []const u8, offset: usize) struct { line: usize, col: usize } {
    var line: usize = 1;
    var col: usize = 1;

    for (source[0..@min(offset, source.len)]) |char| {
        if (char == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    return .{ .line = line, .col = col };
}
