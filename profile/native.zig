const std = @import("std");
const parser = @import("parser");
const transfer = @import("transfer");

const assert = std.debug.assert;

const source_bytes_max = 64 * 1024 * 1024;

const Operation = enum { parse, analyze, model, print };

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

    var tree = try parser.parse(gpa, source, .{
        .source_type = .fromPath(path),
        .lang = .fromPath(path),
        .preserve_parens = true,
        .comments = .collect,
        .tokens = false,
    });
    defer tree.deinit();
    assert(tree.root != .null);
    if (tree.diagnostics.items.len > 0) return error.InvalidCorpus;

    switch (operation) {
        .parse => {
            var offsets: transfer.utf16.Offsets = try .init(gpa, tree.source);
            defer offsets.deinit(gpa);

            const sections: transfer.Sections = .{ .nodes = true };
            const out = try gpa.alignedAlloc(u8, .of(u32), transfer.bufferSize(&tree, sections));
            defer gpa.free(out);
            const written = transfer.serializeInto(&tree, &offsets, out, sections);
            assert(written == out.len);
        },
        .analyze => {
            const sem = try parser.semantic.analyze(&tree, .{ .check = true });
            const records = try parser.semantic.module_record.collect(&tree, &sem);
            if (tree.diagnostics.items.len > 0) return error.InvalidCorpus;

            var offsets: transfer.utf16.Offsets = try .init(gpa, tree.source);
            defer offsets.deinit(gpa);

            const sections: transfer.Sections = .{ .nodes = true };
            const size = transfer.semantic.bufferSize(&tree, &sem, records, sections);
            const out = try gpa.alignedAlloc(u8, .of(u32), size);
            defer gpa.free(out);
            const written =
                transfer.semantic.serializeInto(&tree, &sem, records, &offsets, out, sections);
            assert(written == out.len);
        },
        .print => {
            const printed = try parser.codegen.generate(gpa, &tree, .{
                .strip = false,
                .minify = false,
                .format = .pretty,
                .indent = 2,
                .quotes = .preserve,
                .source_map = null,
                .comments = .all,
            });
            defer printed.deinit(gpa);
            assert(printed.code.len > 0);
            assert(printed.diagnostics.len == 0);
        },
        .model => {
            const sem = try parser.semantic.analyze(&tree, .{ .check = false });
            assert(sem.scopes.list.len > 0);
            assert(tree.diagnostics.items.len == 0);
        },
    }
}

fn usage() noreturn {
    std.debug.print("usage: profile <parse|analyze|model|print> <path>\n", .{});
    std.process.exit(2);
}
