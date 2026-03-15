const std = @import("std");
const parser = @import("parser");

const semantic = parser.semantic;

pub fn main(init: std.process.Init) !void {
    const Io = init.io;

    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file_path = "test.js";

    const source = try std.Io.Dir.cwd().readFileAlloc(Io, file_path, allocator, std.Io.Limit.limited(10 * 1024 * 1024));
    defer allocator.free(source);

    var tree = try parser.parse(allocator, source, .{});
    defer tree.deinit();

    _ = try semantic.analyze(&tree);

    const json = try parser.estree.toJSON(&tree, allocator, .{});
    defer allocator.free(json);

    std.debug.print("{s}\n", .{json});

    for (tree.diagnostics.items) |err| {
        const start_pos = getLineAndColumn(source, err.span.start);
        const end_pos = getLineAndColumn(source, err.span.end);

        std.debug.print("\nError: {s} at test.js:{d}:{d} to test.js:{d}:{d}\n", .{ err.message, start_pos.line, start_pos.col, end_pos.line, end_pos.col });

        if (err.help) |help| std.debug.print("  Help: {s}\n\n", .{help});

        if (err.labels.len > 0) {
            for (err.labels) |label| {
                const label_start_pos = getLineAndColumn(source, label.span.start);
                const label_end_pos = getLineAndColumn(source, label.span.end);

                std.debug.print("  Label: {s} at test.js:{d}:{d} to test.js:{d}:{d}\n", .{ label.message, label_start_pos.line, label_start_pos.col, label_end_pos.line, label_end_pos.col });
            }
        }
    }
}

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
