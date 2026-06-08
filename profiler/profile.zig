const std = @import("std");
const parser = @import("parser");
const codspeed = @import("codspeed");

const ast = parser.ast;
const codegen = parser.codegen;

// these files loaded at the profile time
// see profiler/load-parser-bench-files.ts
const bench_files = .{
    .{ .name = "typescript.js", .path = "files/typescript.js" },
    .{ .name = "calcom.tsx", .path = "files/calcom.tsx" },
    .{ .name = "react.js", .path = "files/react.js" },
};

const allocator = std.heap.smp_allocator;

pub fn main() !void {
    var session = try codspeed.initSession(std.heap.c_allocator);
    defer session.deinit();

    // parser benchmarks measure a full parse of each file.
    inline for (bench_files) |file| {
        const Bench = struct {
            fn run() void {
                const contents = @embedFile(file.path);
                const tree = parser.parse(allocator, contents, .{ .lang = comptime .fromPath(file.path) }) catch unreachable;
                defer tree.deinit();
            }
        };

        try session.bench("parser[" ++ file.name ++ "]", Bench.run);
    }

    // printer / minifier / stripper benchmarks
    inline for (bench_files) |file| {
        const Suite = struct {
            var tree: ast.Tree = undefined;

            fn print() void {
                const result = codegen.print(allocator, &tree, .{}) catch unreachable;
                result.deinit(allocator);
            }

            fn minify() void {
                const result = codegen.minify(allocator, &tree, .{ .format = .compact }) catch unreachable;
                result.deinit(allocator);
            }

            fn strip() void {
                const result = codegen.strip(allocator, &tree, .{}) catch unreachable;
                result.deinit(allocator);
            }
        };

        const contents = @embedFile(file.path);
        Suite.tree = parser.parse(allocator, contents, .{ .lang = comptime .fromPath(file.path) }) catch unreachable;

        try session.bench("printer[" ++ file.name ++ "]", Suite.print);
        try session.bench("minifier[" ++ file.name ++ "]", Suite.minify);
        try session.bench("stripper[" ++ file.name ++ "]", Suite.strip);
    }
}
