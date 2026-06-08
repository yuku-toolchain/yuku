const std = @import("std");
const parser = @import("parser");
const codspeed = @import("codspeed");

const ast = parser.ast;
const codegen = parser.codegen;
const traverser = parser.traverser;

// these files loaded at the profile time
// see profiler/load-parser-bench-files.ts
const bench_files = .{
    .{ .name = "typescript.js", .path = "files/typescript.js" },
    .{ .name = "calcom.tsx", .path = "files/calcom.tsx" },
    .{ .name = "react.js", .path = "files/react.js" },
};

const allocator = std.heap.smp_allocator;

const CountVisitor = struct {
    count: u64 = 0,

    pub fn enter_node(self: *CountVisitor, _: ast.NodeData, _: ast.NodeIndex, _: anytype) traverser.Action {
        self.count += 1;
        return .proceed;
    }
};

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

    // printer / minifier / stripper / traverser benchmarks
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

            fn traverseBasic() void {
                var visitor: CountVisitor = .{};
                traverser.basic.traverse(CountVisitor, &tree, &visitor) catch unreachable;
                std.mem.doNotOptimizeAway(visitor.count);
            }

            fn traverseScoped() void {
                var visitor: CountVisitor = .{};
                var scope_tree = traverser.scoped.traverse(CountVisitor, &tree, &visitor) catch unreachable;
                std.mem.doNotOptimizeAway(visitor.count);
                std.mem.doNotOptimizeAway(&scope_tree);
            }

            fn traverseSemantic() void {
                var visitor: CountVisitor = .{};
                var result = traverser.semantic.traverse(CountVisitor, &tree, &visitor) catch unreachable;
                std.mem.doNotOptimizeAway(visitor.count);
                std.mem.doNotOptimizeAway(&result);
            }
        };

        const contents = @embedFile(file.path);
        Suite.tree = parser.parse(allocator, contents, .{ .lang = comptime .fromPath(file.path) }) catch unreachable;

        try session.bench("printer[" ++ file.name ++ "]", Suite.print);
        try session.bench("minifier[" ++ file.name ++ "]", Suite.minify);
        try session.bench("stripper[" ++ file.name ++ "]", Suite.strip);
        try session.bench("traverser-basic[" ++ file.name ++ "]", Suite.traverseBasic);
        try session.bench("traverser-scoped[" ++ file.name ++ "]", Suite.traverseScoped);
        try session.bench("traverser-semantic[" ++ file.name ++ "]", Suite.traverseSemantic);
    }
}
