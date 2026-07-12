//! Shared helpers for the Zig-side parser tests. Inline-source analysis
//! with name and tag lookups, plus corpus iteration.

const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const semantic = parser.semantic;
const Allocator = std.mem.Allocator;

pub const NodeTag = std.meta.Tag(ast.NodeData);

pub const Analyzed = struct {
    tree: ast.Tree,
    sem: semantic.Semantic,

    pub fn deinit(self: *Analyzed) void {
        self.tree.deinit();
    }

    pub fn bindingNamed(self: *const Analyzed, name: []const u8) !ast.NodeIndex {
        return self.nthBindingNamed(name, 0);
    }

    pub fn nthBindingNamed(self: *const Analyzed, name: []const u8, n: usize) !ast.NodeIndex {
        return findNamed(&self.tree, .binding_identifier, name, n) orelse error.BindingNotFound;
    }

    pub fn referenceNamed(self: *const Analyzed, name: []const u8) !ast.NodeIndex {
        return self.nthReferenceNamed(name, 0);
    }

    pub fn nthReferenceNamed(self: *const Analyzed, name: []const u8, n: usize) !ast.NodeIndex {
        return findNamed(&self.tree, .identifier_reference, name, n) orelse error.ReferenceNotFound;
    }

    pub fn nthNode(self: *const Analyzed, tag: NodeTag, n: usize) !ast.NodeIndex {
        return findNth(&self.tree, tag, n) orelse error.NodeNotFound;
    }

    pub fn symbolNamed(self: *const Analyzed, name: []const u8) !semantic.Semantic.SymbolEntry {
        var found: ?semantic.Semantic.SymbolEntry = null;
        var it = self.sem.iterSymbols();
        while (it.next()) |entry| {
            if (std.mem.eql(u8, self.tree.string(entry.symbol.name), name)) {
                if (found != null) return error.AmbiguousSymbolName;
                found = entry;
            }
        }
        return found orelse error.SymbolNotFound;
    }

    pub fn symbolsNamed(
        self: *const Analyzed,
        name: []const u8,
        buf: []semantic.Semantic.SymbolEntry,
    ) []semantic.Semantic.SymbolEntry {
        var len: usize = 0;
        var it = self.sem.iterSymbols();
        while (it.next()) |entry| {
            if (std.mem.eql(u8, self.tree.string(entry.symbol.name), name)) {
                buf[len] = entry;
                len += 1;
            }
        }
        return buf[0..len];
    }

    pub fn onlyReferenceNamed(
        self: *const Analyzed,
        name: []const u8,
    ) !semantic.Semantic.ReferenceEntry {
        var found: ?semantic.Semantic.ReferenceEntry = null;
        var it = self.sem.iterReferences();
        while (it.next()) |entry| {
            if (std.mem.eql(u8, self.tree.string(entry.reference.name), name)) {
                if (found != null) return error.AmbiguousReferenceName;
                found = entry;
            }
        }
        return found orelse error.ReferenceNotFound;
    }
};

pub fn analyze(gpa: Allocator, source: []const u8, opts: parser.Options) !Analyzed {
    var result = try analyzeAllowErrors(gpa, source, opts);
    errdefer result.deinit();
    if (result.tree.hasErrors()) {
        dumpDiagnostics(&result.tree);
        return error.UnexpectedParseOrAnalysisErrors;
    }
    return result;
}

pub fn analyzeAllowErrors(gpa: Allocator, source: []const u8, opts: parser.Options) !Analyzed {
    var tree = try parser.parse(gpa, source, opts);
    errdefer tree.deinit();
    const sem = try semantic.analyze(&tree);
    return .{ .tree = tree, .sem = sem };
}

fn dumpDiagnostics(tree: *const ast.Tree) void {
    for (tree.diagnostics.items) |diag| {
        std.debug.print("diagnostic: {s}\n", .{diag.message});
    }
}

fn findNamed(
    tree: *const ast.Tree,
    comptime tag: NodeTag,
    name: []const u8,
    n: usize,
) ?ast.NodeIndex {
    var seen: usize = 0;
    var i: u32 = 0;
    while (i < tree.nodes.len) : (i += 1) {
        const index: ast.NodeIndex = @enumFromInt(i);
        const data = tree.data(index);
        if (std.meta.activeTag(data) != tag) continue;
        const node_name = @field(data, @tagName(tag)).name;
        if (!std.mem.eql(u8, tree.string(node_name), name)) continue;
        if (seen == n) return index;
        seen += 1;
    }
    return null;
}

fn findNth(tree: *const ast.Tree, tag: NodeTag, n: usize) ?ast.NodeIndex {
    var seen: usize = 0;
    var i: u32 = 0;
    while (i < tree.nodes.len) : (i += 1) {
        const index: ast.NodeIndex = @enumFromInt(i);
        if (std.meta.activeTag(tree.data(index)) != tag) continue;
        if (seen == n) return index;
        seen += 1;
    }
    return null;
}

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
/// `checker.check(path, &tree)`. Paths resolve relative to the repo
/// root, where `zig build test` runs. Skips with `error.SkipZigTest`
/// when the corpus has not been fetched by the fixtures script.
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
            if (tree.hasErrors()) continue;

            const path = try std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ dir_path, entry.path });
            try checker.check(path, &tree);
            checked += 1;
        }
    }

    if (checked == 0) return error.SkipZigTest;
}
