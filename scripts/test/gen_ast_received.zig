const std = @import("std");
const js = @import("js");

const folders = [_][]const u8{ "test/pass" };

fn readFile(allocator: std.mem.Allocator, dir: std.fs.Dir, file_name: []const u8) ![]const u8 {
    const file = try dir.openFile(file_name, .{});
    defer file.close();

    var buffer: [4096]u8 = undefined;
    var reader = file.reader(&buffer);
    return try reader.interface.allocRemaining(allocator, std.Io.Limit.limited(10 * 1024 * 1024));
}

fn processFile(allocator: std.mem.Allocator, dir: std.fs.Dir, file_name: []const u8) !void {
    const source = readFile(allocator, dir, file_name) catch |err| {
        std.debug.print("error reading: {s} - {}\n", .{ file_name, err });
        return;
    };
    defer allocator.free(source);

    const is_module = std.mem.indexOf(u8, file_name, ".module.js") != null;

    const tree = js.parse(allocator, source, .{
        .source_type = if (is_module) .module else .script,
    }) catch |err| {
        std.debug.print("error parsing: {s} - {}\n", .{ file_name, err });
        return;
    };
    defer tree.deinit();

    const json = js.estree.toJSON(&tree, allocator, .{}) catch |err| {
        std.debug.print("error generating json: {s} - {}\n", .{ file_name, err });
        return;
    };
    defer allocator.free(json);

    const output_name = try std.fmt.allocPrint(allocator, "{s}.received.json", .{file_name});
    defer allocator.free(output_name);

    const output_file = try dir.createFile(output_name, .{});
    defer output_file.close();

    try output_file.writeAll(json);
}

fn processFolder(allocator: std.mem.Allocator, folder_path: []const u8) !void {
    var dir = std.fs.cwd().openDir(folder_path, .{ .iterate = true }) catch |err| {
        std.debug.print("cannot open {s}: {}\n", .{ folder_path, err });
        return;
    };
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".js")) continue;

        try processFile(allocator, dir, entry.name);
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    for (folders) |folder_path| {
        try processFolder(allocator, folder_path);
    }
}
