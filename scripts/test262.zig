const std = @import("std");
const js = @import("js");

const TestKind = enum { snapshot, should_pass, should_fail };

const TestFolder = struct {
    name: []const u8,
    path: []const u8,
    kind: TestKind,
};

const test_folders = [_]TestFolder{
    .{ .name = "Pass", .path = "test/pass", .kind = .snapshot },
    .{ .name = "Fuzz", .path = "test/fuzz", .kind = .snapshot },
    .{ .name = "Fail", .path = "test/fail", .kind = .should_fail },
};

const excluded_files = [_][]const u8{
    // these are the semantic tests, remove these from the list
    // when we implement semantic checks
    "67c714796e7f40a4.js",
    "e6559958e6954318.js",
    "4e2cce832b4449f1.js",
    "317c81f05510f4ad.js",
    "76465e2c7af91e73.js",
    "fb130c395c6aafe7.js",
    "c7ad2478fd72bffe.js",
    "5e6f67a0e748cc42.js",
    "efcb54b62e8f0e06.js",
    "8b72c44bd531621a.js",
    "d17d3aebb6a3cf43.js",
    "2b050de45ab44c8c.js",
    "3078b4fed5626e2a.js",
    "04bc213db9cd1130.js",
    "4a887c2761eb95fb.js",
    "16947dc1d11e5e70.js",
    "8d5ef4dee9c7c622.js",
    "e808e347646c2670.js",
    "f2db53245b89c72f.js",
    "73d061b5d635a807.js",
    "b88ab70205263170.module.js",
    "6cd36f7e68bdfb7a.js",
    "a4bfa8e3b523c466.module.js",
    "858b72be7f8f19d7.js",
    "2226edabbd2261a7.module.js",
    "d54b2db4548f1d82.module.js",
    "5059efc702f08060.js",
    "f063969b23239390.module.js"
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

fn runPassOrFailTest(allocator: std.mem.Allocator, dir: std.fs.Dir, name: []const u8, should_pass: bool) !bool {
    const source = try readFile(allocator, dir, name);
    defer allocator.free(source);

    const is_module = std.mem.indexOf(u8, name, ".module.") != null;
    const tree = js.parse(allocator, source, .{ .source_type = if (is_module) .module else .script, .is_strict = true }) catch return !should_pass;
    defer tree.deinit();

    const has_diagnostics = tree.hasDiagnostics();

    return if (should_pass) !has_diagnostics else has_diagnostics;
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

            var is_excluded = false;
            for (excluded_files) |excluded| {
                if (std.mem.eql(u8, entry.name, excluded)) {
                    is_excluded = true;
                    break;
                }
            }
            if (is_excluded) continue;

            total += 1;

            const ok = switch (folder.kind) {
                .snapshot => runSnapshotTest(allocator, dir, entry.name) catch false,
                .should_pass => runPassOrFailTest(allocator, dir, entry.name, true) catch false,
                .should_fail => runPassOrFailTest(allocator, dir, entry.name, false) catch false,
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
