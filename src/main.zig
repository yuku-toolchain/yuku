const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;

const traverser = parser.traverser;
const scoped = traverser.scoped;

pub fn main(init: std.process.Init) !void {
    const Io = init.io;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file_path = "test.js";

    const contents = try std.Io.Dir.cwd().readFileAlloc(Io, file_path, allocator, std.Io.Limit.limited(10 * 1024 * 1024));
    defer allocator.free(contents);

    const tree = try parser.parse(std.heap.page_allocator, contents, .{ .lang = .fromPath(file_path), .source_type = .fromPath(file_path) });
    defer tree.deinit();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();

    const Linter = struct {
        pub fn enter_debugger_statement(_: *@This(), _: ast.DebuggerStatement, _: ast.NodeIndex, ctx: *scoped.ScopedCtx) traverser.Action {
            if(ctx.isStrict()) {
                std.debug.print("Debugger statement is not allowed in strict context", .{});
            }

            return .proceed;
        }
    };

    var linter = Linter{};

    const start = std.Io.Clock.Timestamp.now(Io, .real);

    scoped.traverse(Linter, &tree, &linter, arena_allocator);

    const end = std.Io.Clock.Timestamp.now(Io, .real);
    const taken_ms = @as(f64, @floatFromInt(start.durationTo(end).raw.toNanoseconds())) / std.time.ns_per_ms;

    std.debug.print("\ntook: {d:.2}ms\n", .{ taken_ms });
}

fn getLineAndColumn(contents: []const u8, offset: usize) struct { line: usize, col: usize } {
    var line: usize = 1;
    var col: usize = 1;

    for (contents[0..@min(offset, contents.len)]) |char| {
        if (char == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    return .{ .line = line, .col = col };
}
