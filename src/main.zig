const std = @import("std");
const js = @import("js");

const Color = enum {
    reset,
    red,
    green,
    dim,

    fn code(self: Color) []const u8 {
        return switch (self) {
            .reset => "\x1b[0m",
            .red => "\x1b[31m",
            .green => "\x1b[32m",
            .dim => "\x1b[2m",
        };
    }
};

fn printc(color: Color, comptime fmt: []const u8, args: anytype) void {
    std.debug.print("{s}" ++ fmt ++ "{s}", .{color.code()} ++ args ++ .{Color.reset.code()});
}

const TestResult = struct {
    name: []const u8,
    passed: bool,
    error_msg: ?[]const u8 = null,
    is_new: bool = false,
};

const TestStats = struct {
    total: usize = 0,
    passed: usize = 0,
    failed: usize = 0,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const update_snapshots = for (args) |arg| {
        if (std.mem.eql(u8, arg, "--update") or std.mem.eql(u8, arg, "-u")) break true;
    } else false;

    var test_dir = std.fs.cwd().openDir("test", .{ .iterate = true }) catch |err| {
        printc(.red, "Error: Could not open 'test' directory: {}\n", .{err});
        std.process.exit(1);
    };
    defer test_dir.close();

    test_dir.makeDir("snapshots") catch |err| {
        if (err != error.PathAlreadyExists) {
            printc(.red, "Error: Could not create 'snapshots' directory: {}\n", .{err});
            std.process.exit(1);
        }
    };

    var results = std.ArrayList(TestResult).empty;
    defer results.deinit(allocator);

    var stats = TestStats{};

    var iter = test_dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;

        const lang: ?js.Lang = if (std.mem.endsWith(u8, entry.name, ".ts"))
            .Ts
        else if (std.mem.endsWith(u8, entry.name, ".js"))
            .Js
        else
            null;

        if (lang == null) continue;

        const test_filename = try allocator.dupe(u8, entry.name);
        const result = try runTest(allocator, test_dir, test_filename, lang.?, update_snapshots);
        try results.append(allocator, result);

        stats.total += 1;
        if (result.passed) stats.passed += 1 else stats.failed += 1;
    }

    std.debug.print("\n", .{});

    for (results.items) |result| {
        if (result.passed) {
            printc(.green, "✓ ", .{});
            std.debug.print("{s}", .{result.name});
            if (result.is_new) printc(.dim, " (new)", .{});
            std.debug.print("\n", .{});
        } else {
            printc(.red, "✗ {s}\n", .{result.name});
            if (result.error_msg) |msg| {
                printc(.dim, "  {s}\n", .{msg});
                allocator.free(msg);
            }
        }
        allocator.free(result.name);
    }

    std.debug.print("\n", .{});

    std.debug.print("Total:  {d}\n", .{stats.total});
    printc(.green, "Passed: {d}\n", .{stats.passed});
    if (stats.failed > 0) {
        printc(.red, "Failed: {d}\n", .{stats.failed});
    } else {
        std.debug.print("Failed: {d}\n", .{stats.failed});
    }

    if (stats.failed > 0) std.process.exit(1);
}

fn runTest(
    allocator: std.mem.Allocator,
    test_dir: std.fs.Dir,
    test_filename: []const u8,
    lang: js.Lang,
    update_snapshots: bool,
) !TestResult {
    const base_name = test_filename[0 .. test_filename.len - 3];
    const snapshot_filename = try std.fmt.allocPrint(allocator, "snapshots/{s}.snap.json", .{base_name});
    defer allocator.free(snapshot_filename);

    const file = test_dir.openFile(test_filename, .{}) catch |err| {
        return TestResult{
            .name = test_filename,
            .passed = false,
            .error_msg = try std.fmt.allocPrint(allocator, "Failed to read test file: {}", .{err}),
        };
    };
    defer file.close();

    var buffer: [4096]u8 = undefined;
    var reader = file.reader(&buffer);
    const input = reader.interface.allocRemaining(allocator, std.Io.Limit.limited(10 * 1024 * 1024)) catch |err| {
        return TestResult{
            .name = test_filename,
            .passed = false,
            .error_msg = try std.fmt.allocPrint(allocator, "Failed to read file: {}", .{err}),
        };
    };
    defer allocator.free(input);

    var parser = js.Parser.init(std.heap.page_allocator, input, .{ .lang = lang });
    const tree = parser.parse() catch |err| {
        return TestResult{
            .name = test_filename,
            .passed = false,
            .error_msg = try std.fmt.allocPrint(allocator, "Failed to parse: {}", .{err}),
        };
    };

    const json = try js.estree.toJSON(&tree, allocator);
    defer allocator.free(json);

    const output = try std.fmt.allocPrint(allocator, "{s}\n", .{json});
    defer allocator.free(output);

    const snapshot_exists = blk: {
        test_dir.access(snapshot_filename, .{}) catch break :blk false;
        break :blk true;
    };

    if (!snapshot_exists or update_snapshots) {
        var snap_file = try test_dir.createFile(snapshot_filename, .{});
        defer snap_file.close();
        try snap_file.writeAll(output);

        return TestResult{
            .name = test_filename,
            .passed = true,
            .is_new = !snapshot_exists,
        };
    }

    const expected = test_dir.readFileAlloc(snapshot_filename, allocator, std.Io.Limit.limited(10 * 1024 * 1024)) catch |err| {
        return TestResult{
            .name = test_filename,
            .passed = false,
            .error_msg = try std.fmt.allocPrint(allocator, "Failed to read snapshot: {}", .{err}),
        };
    };
    defer allocator.free(expected);

    if (!std.mem.eql(u8, output, expected)) {
        return TestResult{
            .name = test_filename,
            .passed = false,
            .error_msg = try allocator.dupe(u8, "Snapshot mismatch. Run with --update to update snapshots"),
        };
    }

    return TestResult{
        .name = test_filename,
        .passed = true,
    };
}
