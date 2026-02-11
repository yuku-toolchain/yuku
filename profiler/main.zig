const std = @import("std");
const parser = @import("parser");
const codspeed = @import("codspeed");

// these files loaded at the profile time
// check load-parser-bench-files.ts script in profiler dir
const parser_bench_files = .{
    .{ .name = "typescript", .path = "./files/typescript.js" },
    .{ .name = "three", .path = "./files/three.js" },
    .{ .name = "antd", .path = "./files/antd.js" },
};

pub fn main() !void {
    var session = try codspeed.initSession(std.heap.c_allocator);
    defer session.deinit();

    inline for (parser_bench_files) |file| {
        const BenchFn = struct {
            fn run() void {
                const contents = @embedFile(file.path);
                const tree = parser.parse(std.heap.page_allocator, contents, .{}) catch unreachable;
                defer tree.deinit();
            }
        };

        const bench_name = "parser/" ++ file.name;
        try session.bench(bench_name, BenchFn.run);
    }
}
