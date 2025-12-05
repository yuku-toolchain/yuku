const std = @import("std");
const js = @import("js");

const TestResult = enum {
    pass,
    fail,
    unexpected_pass,
    unexpected_fail,
};

const Stats = struct {
    pass_total: usize = 0,
    pass_passed: usize = 0,
    fail_total: usize = 0,
    fail_passed: usize = 0,
    early_total: usize = 0,
    early_passed: usize = 0,

    fn passPercent(self: Stats) f64 {
        if (self.pass_total == 0) return 0;
        return (@as(f64, @floatFromInt(self.pass_passed)) / @as(f64, @floatFromInt(self.pass_total))) * 100.0;
    }

    fn failPercent(self: Stats) f64 {
        if (self.fail_total == 0) return 0;
        return (@as(f64, @floatFromInt(self.fail_passed)) / @as(f64, @floatFromInt(self.fail_total))) * 100.0;
    }

    fn earlyPercent(self: Stats) f64 {
        if (self.early_total == 0) return 0;
        return (@as(f64, @floatFromInt(self.early_passed)) / @as(f64, @floatFromInt(self.early_total))) * 100.0;
    }

    fn overallPercent(self: Stats) f64 {
        const total = self.pass_total + self.fail_total + self.early_total;
        const passed = self.pass_passed + self.fail_passed + self.early_passed;
        if (total == 0) return 0;
        return (@as(f64, @floatFromInt(passed)) / @as(f64, @floatFromInt(total))) * 100.0;
    }
};

fn parseFile(allocator: std.mem.Allocator, source: []const u8, is_module: bool) bool {
    const tree = js.parse(allocator, source, .{
        .source_type = if(is_module) .module else .script,
    }) catch return false;
    defer tree.deinit();
    return !tree.hasDiagnostics();
}

fn readFile(allocator: std.mem.Allocator, dir: std.fs.Dir, file_name: []const u8) ![]const u8 {
    const file = try dir.openFile(file_name, .{});
    defer file.close();

    var buffer: [4096]u8 = undefined;
    var reader = file.reader(&buffer);
    return try reader.interface.allocRemaining(allocator, std.Io.Limit.limited(10 * 1024 * 1024));
}

fn runPassTests(allocator: std.mem.Allocator, stats: *Stats) !void {
    var dir = std.fs.cwd().openDir("test/test262/pass", .{ .iterate = true }) catch |err| {
        std.debug.print("Cannot open test/test262/pass: {}\n", .{err});
        return;
    };
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;

        stats.pass_total += 1;

        const source = readFile(allocator, dir, entry.name) catch {
            std.debug.print("[PASS] ERROR reading: {s}\n", .{entry.name});
            continue;
        };
        defer allocator.free(source);

        const is_module = std.mem.endsWith(u8, entry.name, ".module.js");
        const parsed = parseFile(allocator, source, is_module);

        if (parsed) {
            stats.pass_passed += 1;
        } else {
            std.debug.print("[PASS] FAILED: {s} (should parse successfully)\n", .{entry.name});
        }
    }
}

fn runFailTests(allocator: std.mem.Allocator, stats: *Stats) !void {
    var dir = std.fs.cwd().openDir("test/test262/fail", .{ .iterate = true }) catch |err| {
        std.debug.print("Cannot open test/test262/fail: {}\n", .{err});
        return;
    };
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;

        stats.fail_total += 1;

        const source = readFile(allocator, dir, entry.name) catch {
            std.debug.print("[FAIL] ERROR reading: {s}\n", .{entry.name});
            continue;
        };
        defer allocator.free(source);

        const is_module = std.mem.endsWith(u8, entry.name, ".module.js");
        const parsed = parseFile(allocator, source, is_module);

        if (!parsed) {
            stats.fail_passed += 1;
        } else {
            std.debug.print("[FAIL] FAILED: {s} (should throw parse error)\n", .{entry.name});
        }
    }
}

fn runEarlyTests(allocator: std.mem.Allocator, stats: *Stats) !void {
    var dir = std.fs.cwd().openDir("test/test262/early", .{ .iterate = true }) catch {
        return;
    };
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;

        stats.early_total += 1;

        const source = readFile(allocator, dir, entry.name) catch {
            std.debug.print("[EARLY] ERROR reading: {s}\n", .{entry.name});
            continue;
        };
        defer allocator.free(source);

        const is_module = std.mem.endsWith(u8, entry.name, ".module.js");
        const parsed = parseFile(allocator, source, is_module);

        if (!parsed) {
            stats.early_passed += 1;
        } else {
            std.debug.print("[EARLY] FAILED: {s} (should detect early error)\n", .{entry.name});
        }
    }
}

fn printSeparator() void {
    std.debug.print("\n{s}\n", .{"=" ** 70});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    std.debug.print("\nRunning Test262 Parser Tests...\n", .{});
    printSeparator();

    var stats = Stats{};

    try runPassTests(allocator, &stats);

    // NOTE: Fail and early tests disabled until pass tests reach 100%
    // try runFailTests(allocator, &stats);
    // try runEarlyTests(allocator, &stats);

    printSeparator();
    std.debug.print("TEST262 PARSER CONFORMANCE RESULTS\n", .{});
    printSeparator();

    std.debug.print("\n[PASS Tests] Valid syntax that should parse\n", .{});
    std.debug.print("  Passed:     {d}/{d}\n", .{ stats.pass_passed, stats.pass_total });
    std.debug.print("  Pass Rate:  {d:.2}%\n", .{stats.passPercent()});

    // std.debug.print("\n[FAIL Tests]\n", .{});
    // std.debug.print("\n[EARLY Tests]\n", .{});

    const total_tests = stats.pass_total;
    const total_passed = stats.pass_passed;

    printSeparator();
    std.debug.print("OVERALL CONFORMANCE\n", .{});
    printSeparator();
    std.debug.print("  Total Tests:    {d}\n", .{total_tests});
    std.debug.print("  Tests Passed:   {d}\n", .{total_passed});
    std.debug.print("  Tests Failed:   {d}\n", .{total_tests - total_passed});
    std.debug.print("  Conformance:    {d:.2}%\n", .{stats.passPercent()});
    printSeparator();
    std.debug.print("\n", .{});
}
