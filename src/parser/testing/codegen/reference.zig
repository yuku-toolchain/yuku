const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const codegen = parser.codegen;

const source_bytes_max = 64 * 1024 * 1024;
const list_bytes_max = 64 * 1024 * 1024;

const Plan = struct {
    codegen: codegen.Options = .{},
    preserve_parens: bool = true,
};

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const gpa = std.heap.smp_allocator;
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len < 3) {
        std.debug.print("usage: codegen-reference <list> <out> [option...]\n", .{});
        std.process.exit(2);
    }

    const plan = try planParse(args[3..]);
    const list = try std.Io.Dir.cwd().readFileAlloc(io, args[1], gpa, .limited(list_bytes_max));
    defer gpa.free(list);

    const out_file = try std.Io.Dir.cwd().createFile(io, args[2], .{ .truncate = true });
    defer out_file.close(io);
    var out_buffer: [64 * 1024]u8 = undefined;
    var out_writer = out_file.writer(io, &out_buffer);
    const out = &out_writer.interface;

    var lines = std.mem.tokenizeScalar(u8, list, '\n');
    while (lines.next()) |line| {
        const space = std.mem.findScalar(u8, line, ' ') orelse return error.InvalidList;
        const source_type = std.meta.stringToEnum(ast.SourceType, line[0..space]) orelse
            return error.InvalidList;
        const path = line[space + 1 ..];
        std.debug.assert(path.len > 0);
        const source = try std.Io.Dir.cwd().readFileAlloc(
            io,
            path,
            gpa,
            .limited(source_bytes_max),
        );
        defer gpa.free(source);
        try printFile(gpa, out, path, source, source_type, &plan);
    }
    try out.flush();
}

fn printFile(
    gpa: std.mem.Allocator,
    out: *std.Io.Writer,
    path: []const u8,
    source: []const u8,
    source_type: ast.SourceType,
    plan: *const Plan,
) !void {
    var tree = try parser.parse(gpa, source, .{
        .lang = ast.Lang.fromPath(path),
        .source_type = source_type,
        .preserve_parens = plan.preserve_parens,
        .comments = .attach,
    });
    defer tree.deinit();

    if (tree.diagnostics.items.len > 0) {
        try out.writeByte(1);
        return;
    }

    const result = try codegen.generate(gpa, &tree, plan.codegen);
    defer result.deinit(gpa);
    std.debug.assert((plan.codegen.source_map != null) == (result.map != null));

    try out.writeByte(0);
    try writeBytes(out, result.code);
    try writeBytes(out, if (result.map) |map| map.mappings else "");
    try out.writeInt(u32, @intCast(result.diagnostics.len), .little);
    for (result.diagnostics) |e| {
        std.debug.assert(e.span.start <= e.span.end);
        try out.writeInt(u32, e.span.start, .little);
        try out.writeInt(u32, e.span.end, .little);
        try writeBytes(out, e.message);
    }
}

fn writeBytes(out: *std.Io.Writer, bytes: []const u8) !void {
    std.debug.assert(bytes.len <= std.math.maxInt(u32));
    try out.writeInt(u32, @intCast(bytes.len), .little);
    try out.writeAll(bytes);
}

fn planParse(args: []const []const u8) !Plan {
    var plan: Plan = .{};
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--minify")) {
            plan.codegen.minify = true;
        } else if (std.mem.eql(u8, arg, "--compact")) {
            plan.codegen.format = .compact;
        } else if (std.mem.eql(u8, arg, "--source-map")) {
            plan.codegen.source_map = .{};
        } else if (std.mem.eql(u8, arg, "--no-preserve-parens")) {
            plan.preserve_parens = false;
        } else if (std.mem.cutPrefix(u8, arg, "--indent=")) |value| {
            plan.codegen.indent = try std.fmt.parseInt(u8, value, 10);
        } else if (std.mem.cutPrefix(u8, arg, "--quotes=")) |value| {
            plan.codegen.quotes = std.meta.stringToEnum(codegen.Quotes, value) orelse
                return error.InvalidQuotes;
        } else if (std.mem.cutPrefix(u8, arg, "--comments=")) |value| {
            plan.codegen.comments = std.meta.stringToEnum(codegen.Comments, value) orelse
                return error.InvalidComments;
        } else {
            std.debug.print("unknown option {s}\n", .{arg});
            return error.InvalidOption;
        }
    }
    return plan;
}
