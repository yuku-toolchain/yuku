const std = @import("std");
const js = @import("js");

const Color = enum {
    reset,
    red,
    green,
    yellow,
    blue,
    cyan,
    dim,

    fn code(self: Color) []const u8 {
        return switch (self) {
            .reset => "\x1b[0m",
            .red => "\x1b[31m",
            .green => "\x1b[32m",
            .yellow => "\x1b[33m",
            .blue => "\x1b[34m",
            .cyan => "\x1b[36m",
            .dim => "\x1b[2m",
        };
    }
};

fn print(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
}

fn printColored(color: Color, comptime fmt: []const u8, args: anytype) void {
    print("{s}", .{color.code()});
    print(fmt, args);
    print("{s}", .{Color.reset.code()});
}

const TestResult = struct {
    name: []const u8,
    passed: bool,
    error_message: ?[]const u8 = null,
    is_new_snapshot: bool = false,
};

const TestStats = struct {
    total: usize = 0,
    passed: usize = 0,
    failed: usize = 0,
    created: usize = 0,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const update_snapshots = blk: {
        for (args) |arg| {
            if (std.mem.eql(u8, arg, "--update") or std.mem.eql(u8, arg, "-u")) {
                break :blk true;
            }
        }
        break :blk false;
    };

    const cwd = std.fs.cwd();

    var test_dir = cwd.openDir("test", .{ .iterate = true }) catch |err| {
        printColored(.red, "Error: Could not open 'test' directory: {}\n", .{err});
        std.process.exit(1);
    };

    defer test_dir.close();

    test_dir.makeDir("snapshots") catch |err| {
        if (err != error.PathAlreadyExists) {
            printColored(.red, "Error: Could not create 'snapshots' directory: {}\n", .{err});
            std.process.exit(1);
        }
    };

    var results = std.ArrayList(TestResult).empty;
    defer results.deinit(allocator);

    var stats = TestStats{};

    print("\n", .{});
    printColored(.cyan, "Running Parser Snapshot Tests\n", .{});
    print("\n", .{});

    var iter = test_dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".js")) continue;

        const result = try runTest(allocator, test_dir, entry.name, update_snapshots);
        try results.append(allocator, result);

        stats.total += 1;
        if (result.passed) {
            stats.passed += 1;
        } else {
            stats.failed += 1;
        }
    }

    print("\n", .{});
    printColored(.cyan, "Test Results:\n", .{});
    printColored(.cyan, "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});

    for (results.items) |result| {
        if (result.passed) {
            printColored(.green, "✓ ", .{});
            print("{s}", .{result.name});
            if (result.is_new_snapshot) {
                printColored(.dim, " (new)", .{});
            }
            print("\n", .{});
        } else {
            printColored(.red, "✗ ", .{});
            print("{s}\n", .{result.name});
            if (result.error_message) |msg| {
                printColored(.dim, "  {s}\n", .{msg});
                allocator.free(msg);
            }
        }
    }

    print("\n", .{});
    printColored(.cyan, "Summary:\n", .{});
    printColored(.cyan, "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
    print("Total:  {d}\n", .{stats.total});
    printColored(.green, "Passed: {d}\n", .{stats.passed});
    if (stats.failed > 0) {
        printColored(.red, "Failed: {d}\n", .{stats.failed});
    } else {
        print("Failed: {d}\n", .{stats.failed});
    }
    printColored(.cyan, "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
    print("\n", .{});

    if (stats.failed > 0) {
        std.process.exit(1);
    }
}

fn runTest(
    allocator: std.mem.Allocator,
    test_dir: std.fs.Dir,
    test_filename: []const u8,
    update_snapshots: bool,
) !TestResult {
    const snapshot_filename = try std.fmt.allocPrint(
        allocator,
        "snapshots/{s}.snap.json",
        .{test_filename[0 .. test_filename.len - 3]}, // remove .js extension
    );

    defer allocator.free(snapshot_filename);

    const input = test_dir.readFileAlloc(test_filename, allocator, std.Io.Limit.limited(10 * 1024 * 1024)) catch |err| {
        return TestResult{
            .name = test_filename,
            .passed = false,
            .error_message = try std.fmt.allocPrint(allocator, "Failed to read test file: {}", .{err}),
        };
    };

    defer allocator.free(input);

    var parser = js.Parser.init(std.heap.page_allocator, input);

    const tree = parser.parse() catch |err| {
        return TestResult{
            .name = test_filename,
            .passed = false,
            .error_message = try std.fmt.allocPrint(allocator, "Failed to parse: {}", .{err}),
        };
    };

    const json = try js.estree.toJSON(&tree, allocator);
    defer allocator.free(json);

    const output = try std.fmt.allocPrint(
        allocator,
        "{s}\n",
        .{json},
    );

    defer allocator.free(output);

    const snapshot_exists = blk: {
        var file = test_dir.openFile(snapshot_filename, .{}) catch |err| {
            if (err == error.FileNotFound) break :blk false;
            return TestResult{
                .name = test_filename,
                .passed = false,
                .error_message = try std.fmt.allocPrint(allocator, "Failed to check snapshot: {}", .{err}),
            };
        };
        file.close();
        break :blk true;
    };

    if (!snapshot_exists or update_snapshots) {
        var file = try test_dir.createFile(snapshot_filename, .{});
        defer file.close();
        try file.writeAll(output);

        return TestResult{
            .name = test_filename,
            .passed = true,
            .is_new_snapshot = !snapshot_exists,
        };
    }

    const expected = test_dir.readFileAlloc(snapshot_filename, allocator, std.Io.Limit.limited(10 * 1024 * 1024)) catch |err| {
        return TestResult{
            .name = test_filename,
            .passed = false,
            .error_message = try std.fmt.allocPrint(allocator, "Failed to read snapshot: {}", .{err}),
        };
    };

    defer allocator.free(expected);

    if (std.mem.eql(u8, output, expected)) {
        return TestResult{
            .name = test_filename,
            .passed = true,
        };
    } else {
        var i: usize = 0;
        while (i < @min(output.len, expected.len)) : (i += 1) {
            if (output[i] != expected[i]) {
                const context_start = if (i > 100) i - 100 else 0;
                const context_end = @min(i + 100, output.len);

                const error_msg = try std.fmt.allocPrint(
                    allocator,
                    "Snapshot mismatch.\n\n   Expected: ...{s}...\n\n    Got:      ...{s}...\n    Run with --update to update snapshots",
                    .{
                        expected[context_start..@min(i + 100, expected.len)],
                        output[context_start..context_end],
                    },
                );
                return TestResult{
                    .name = test_filename,
                    .passed = false,
                    .error_message = error_msg,
                };
            }
        }

        const error_msg = try std.fmt.allocPrint(
            allocator,
            "Snapshot length mismatch. Expected: {d} bytes, Got: {d} bytes\n    Run with --update to update snapshots",
            .{ expected.len, output.len },
        );
        return TestResult{
            .name = test_filename,
            .passed = false,
            .error_message = error_msg,
        };
    }
}
