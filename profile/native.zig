const std = @import("std");
const parser = @import("parser");
const transfer = @import("transfer");

const ast = parser.ast;
const assert = std.debug.assert;

const source_bytes_max = 64 * 1024 * 1024;

const Operation = enum { parse, analyze };

pub fn main(init: std.process.Init) !void {
    const gpa = std.heap.c_allocator;
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 3) usage();
    const operation = std.meta.stringToEnum(Operation, args[1]) orelse usage();
    const path = args[2];
    assert(path.len > 0);

    const source = try std.Io.Dir.cwd().readFileAlloc(
        init.io,
        path,
        gpa,
        .limited(source_bytes_max),
    );
    defer gpa.free(source);

    switch (operation) {
        .parse => try parse(gpa, path, source),
        .analyze => try analyze(gpa, path, source),
    }
}

fn usage() noreturn {
    std.debug.print("usage: profile <parse|analyze> <path>\n", .{});
    std.process.exit(2);
}

fn parse(gpa: std.mem.Allocator, path: []const u8, source: []const u8) !void {
    var tree = try parseTree(gpa, path, source);
    defer tree.deinit();

    const out = try gpa.alignedAlloc(u8, .of(u32), transfer.bufferSize(&tree));
    defer gpa.free(out);
    const written = transfer.serializeInto(&tree, out);
    assert(written == out.len);
}

fn analyze(gpa: std.mem.Allocator, path: []const u8, source: []const u8) !void {
    var tree = try parseTree(gpa, path, source);
    defer tree.deinit();

    const sem = try parser.semantic.analyze(&tree);
    const records = try parser.semantic.module_record.collect(&tree, &sem);
    if (tree.diagnostics.items.len > 0) return error.InvalidCorpus;

    const out = try gpa.alignedAlloc(
        u8,
        .of(u32),
        transfer.semantic.bufferSize(&tree, &sem, records),
    );
    defer gpa.free(out);
    const written = transfer.semantic.serializeInto(&tree, &sem, records, out);
    assert(written == out.len);
}

fn parseTree(gpa: std.mem.Allocator, path: []const u8, source: []const u8) !ast.Tree {
    const tree = try parser.parse(gpa, source, .{
        .source_type = .fromPath(path),
        .lang = .fromPath(path),
        .preserve_parens = true,
        .comments = .flat,
        .tokens = false,
        .dialect = false,
    });
    errdefer tree.deinit();
    assert(tree.root != .null);
    if (tree.diagnostics.items.len > 0) return error.InvalidCorpus;
    return tree;
}
