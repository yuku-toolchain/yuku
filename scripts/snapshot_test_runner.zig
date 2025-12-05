const std = @import("std");
const js = @import("js");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var update = false;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--update") or std.mem.eql(u8, arg, "-u")) update = true;
    }

    var test_dir = std.fs.cwd().openDir("test/snapshot", .{ .iterate = true }) catch {
        std.debug.print("\x1b[31mError: Could not open 'test' directory\x1b[0m\n", .{});
        std.process.exit(1);
    };
    defer test_dir.close();

    var total: usize = 0;
    var passed: usize = 0;

    var iter = test_dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        const is_ts = std.mem.endsWith(u8, entry.name, ".ts");
        const is_js = std.mem.endsWith(u8, entry.name, ".js");
        if (!is_js and !is_ts) continue;

        const name = try allocator.dupe(u8, entry.name);
        defer allocator.free(name);

        total += 1;
        if (runTest(allocator, test_dir, name, if (is_ts) .Ts else .Js, update)) {
            passed += 1;
            std.debug.print("\x1b[32m✓\x1b[0m {s}\n", .{name});
        } else {
            std.debug.print("\x1b[31m✗\x1b[0m {s} (snapshot mismatch)\n", .{name});
        }
    }

    const failed = total - passed;

    std.debug.print("\n\nTotal: {d}  \x1b[32mPassed: {d}\x1b[0m  ", .{ total, passed });
    if (failed > 0) std.debug.print("\x1b[31mFailed: {d}\x1b[0m\n\n", .{failed}) else std.debug.print("Failed: 0\n\n", .{});

    if (failed > 0) std.process.exit(1);
}

fn runTest(allocator: std.mem.Allocator, test_dir: std.fs.Dir, name: []const u8, lang: js.Lang, update: bool) bool {
    const base = name[0 .. name.len - 3];
    const snap_name = std.fmt.allocPrint(allocator, "{s}.snap.json", .{base}) catch return false;
    defer allocator.free(snap_name);

    const input = readFile(allocator, test_dir, name) catch return false;
    defer allocator.free(input);

    const tree = js.parse(std.heap.page_allocator, input, .{ .lang = lang }) catch return false;
    defer tree.deinit();
    const json = js.estree.toJSON(&tree, allocator) catch return false;
    defer allocator.free(json);

    const output = std.fmt.allocPrint(allocator, "{s}\n", .{json}) catch return false;
    defer allocator.free(output);

    const snap_exists = if (test_dir.openFile(snap_name, .{})) |f| blk: {
        f.close();
        break :blk true;
    } else |_| false;

    if (!snap_exists or update) {
        var f = test_dir.createFile(snap_name, .{}) catch return false;
        defer f.close();
        f.writeAll(output) catch return false;
        return true;
    }

    const expected = readFile(allocator, test_dir, snap_name) catch return false;
    defer allocator.free(expected);

    return std.mem.eql(u8, output, expected);
}

fn readFile(allocator: std.mem.Allocator, dir: std.fs.Dir, name: []const u8) ![]u8 {
    const file = try dir.openFile(name, .{});
    defer file.close();
    var buf: [4096]u8 = undefined;
    var reader = file.reader(&buf);
    return try reader.interface.allocRemaining(allocator, std.Io.Limit.limited(10 * 1024 * 1024));
}
