const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const Allocator = std.mem.Allocator;

pub const corpus_dirs = [_][]const u8{
    "test/parser/suite/js/pass",
    "test/parser/suite/jsx/pass",
    "test/parser/suite/ts/pass",
    "test/parser/misc",
};

const source_extensions = [_][]const u8{
    ".js", ".jsx", ".ts", ".tsx", ".mjs", ".cjs", ".mts", ".cts",
};

fn isSourceFile(basename: []const u8) bool {
    for (source_extensions) |ext| {
        if (std.mem.endsWith(u8, basename, ext)) return true;
    }
    return false;
}

/// Parses every corpus file that parses cleanly and calls
/// `checker.check(path, &tree)`. Paths are resolved relative to the repo
/// root, where `zig build test` runs. Skips the test (`error.SkipZigTest`)
/// when the corpus has not been fetched (`bun fetch:fixtures`).
pub fn forEachCorpusTree(gpa: Allocator, checker: anytype) !void {
    const io = std.testing.io;
    var checked: usize = 0;
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;

    for (corpus_dirs) |dir_path| {
        var dir = std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true }) catch continue;
        defer dir.close(io);

        var walker = try dir.walk(gpa);
        defer walker.deinit();

        while (try walker.next(io)) |entry| {
            if (entry.kind != .file or !isSourceFile(entry.basename)) continue;

            const source = try entry.dir.readFileAlloc(
                io,
                entry.basename,
                gpa,
                .limited(16 * 1024 * 1024),
            );
            defer gpa.free(source);

            var tree = try parser.parse(gpa, source, .{
                .lang = ast.Lang.fromPath(entry.basename),
                .source_type = ast.SourceType.fromPath(entry.basename),
            });
            defer tree.deinit();
            // only files that parse cleanly exercise tree invariants
            if (tree.hasErrors()) continue;

            const path = try std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ dir_path, entry.path });
            try checker.check(path, &tree);
            checked += 1;
        }
    }

    if (checked == 0) return error.SkipZigTest;
}
