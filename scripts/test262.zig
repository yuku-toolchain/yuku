const std = @import("std");
const js = @import("js");

const TestKind = enum { snapshot, diagnostic };

const TestFolder = struct {
    name: []const u8,
    path: []const u8,
    kind: TestKind,
};

const test_folders = [_]TestFolder{
    .{ .name = "Pass", .path = "test/pass", .kind = .snapshot },
    .{ .name = "Fuzz", .path = "test/fuzz", .kind = .snapshot },
    .{ .name = "Fail", .path = "test/fail", .kind = .diagnostic },
};

fn readFile(allocator: std.mem.Allocator, dir: std.fs.Dir, name: []const u8) ![]const u8 {
    const file = try dir.openFile(name, .{});
    defer file.close();
    var buf: [4096]u8 = undefined;
    var reader = file.reader(&buf);
    return reader.interface.allocRemaining(allocator, std.Io.Limit.limited(10 * 1024 * 1024));
}

fn baseName(name: []const u8) []const u8 {
    return if (std.mem.endsWith(u8, name, ".module.js"))
        name[0 .. name.len - ".module.js".len]
    else if (std.mem.endsWith(u8, name, ".js"))
        name[0 .. name.len - ".js".len]
    else
        name;
}

fn runSnapshotTest(allocator: std.mem.Allocator, dir: std.fs.Dir, name: []const u8) !bool {
    const source = try readFile(allocator, dir, name);
    defer allocator.free(source);

    const expected_name = try std.fmt.allocPrint(allocator, "{s}.expected.json", .{baseName(name)});
    defer allocator.free(expected_name);

    const expected = try readFile(allocator, dir, expected_name);
    defer allocator.free(expected);

    const is_module = std.mem.indexOf(u8, name, ".module.") != null;
    const tree = try js.parse(allocator, source, .{ .source_type = if (is_module) .module else .script });
    defer tree.deinit();

    const actual = try js.estree.toJSON(&tree, allocator, .{});
    defer allocator.free(actual);

    return std.mem.eql(u8, actual, expected);
}

fn runDiagnosticTest(allocator: std.mem.Allocator, dir: std.fs.Dir, name: []const u8) !bool {
    const source = try readFile(allocator, dir, name);
    defer allocator.free(source);

    const is_module = std.mem.indexOf(u8, name, ".module.") != null;
    const tree = js.parse(allocator, source, .{ .source_type = if (is_module) .module else .script }) catch return true;
    defer tree.deinit();

    return tree.hasDiagnostics();
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var total_all: usize = 0;
    var passed_all: usize = 0;

    for (test_folders) |folder| {
        var dir = std.fs.cwd().openDir(folder.path, .{ .iterate = true }) catch |err| {
            std.debug.print("\n─── {s} ───\nCannot open: {}\n", .{ folder.name, err });
            continue;
        };
        defer dir.close();

        var total: usize = 0;
        var passed: usize = 0;
        var failed: std.ArrayList([]const u8) = .empty;
        defer {
            for (failed.items) |n| allocator.free(n);
            failed.deinit(allocator);
        }

        var iter = dir.iterate();
        while (try iter.next()) |entry| {
            if (entry.kind != .file or !std.mem.endsWith(u8, entry.name, ".js")) continue;
            total += 1;

            const ok = switch (folder.kind) {
                .snapshot => runSnapshotTest(allocator, dir, entry.name) catch false,
                .diagnostic => runDiagnosticTest(allocator, dir, entry.name) catch false,
            };

            if (ok) {
                passed += 1;
            } else {
                try failed.append(allocator, try allocator.dupe(u8, entry.name));
            }
        }

        std.debug.print("─── {s}: {}/{} ───\n\n", .{ folder.name, passed, total });
        for (failed.items) |n| std.debug.print("  ✗ {s}\n", .{n});

        total_all += total;
        passed_all += passed;
    }

    const failed_all = total_all - passed_all;
    const pct: f64 = if (total_all > 0) @as(f64, @floatFromInt(passed_all)) / @as(f64, @floatFromInt(total_all)) * 100.0 else 0.0;

    std.debug.print("\nTotal: {} | Passed: {} | Failed: {} | {d:.2}%\n", .{ total_all, passed_all, failed_all, pct });
}
