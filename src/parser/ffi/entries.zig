//! The core's entries, each writing its result into the buffer `output.bytes` returns.

const std = @import("std");
const parser = @import("parser");
const transfer = @import("transfer/root.zig");

const Allocator = std.mem.Allocator;
const Tree = parser.ast.Tree;

// packed by npm/yuku/core/read.ts and npm/yuku/print/index.ts
const source_type_mask = 0b11;
const lang_shift = 2;
const preserve_parens = 1 << 5;
const semantic_errors = 1 << 6;
const attach_comments = 1 << 7;
const tokens = 1 << 8;
pub const utf16 = 1 << 10;

const print_strip = 1 << 0;
const print_minify = 1 << 1;
const print_compact = 1 << 2;
const print_source_map = 1 << 3;
const print_quotes_shift = 4;
const print_comments_shift = 6;
const print_indent_shift = 9;

pub fn parse(gpa: Allocator, source: []const u8, flags: u32, output: anytype) ![]u8 {
    var tree = try parser.parse(gpa, source, parseOptions(flags));
    defer tree.deinit();
    if (flags & semantic_errors != 0) try parser.semantic.check(&tree);

    var offsets: transfer.utf16.Offsets = try .init(gpa, tree.source);
    defer offsets.deinit(gpa);

    const sections: transfer.Sections = .{ .nodes = true };
    const out = try output.bytes(transfer.bufferSize(&tree, sections));
    const written = transfer.serializeInto(&tree, &offsets, out, sections);
    std.debug.assert(written == out.len);
    return out;
}

pub fn analyze(gpa: Allocator, source: []const u8, flags: u32, output: anytype) ![]u8 {
    var tree = try parser.parse(gpa, source, parseOptions(flags));
    defer tree.deinit();
    const sem = try parser.semantic.analyze(&tree, .{ .check = flags & semantic_errors != 0 });
    return serializeModel(gpa, &tree, &sem, .{ .nodes = true }, output);
}

pub fn printTree(
    gpa: Allocator,
    encoded: transfer.Encoded,
    source: []const u8,
    flags: u32,
    options: u32,
    output: anytype,
) ![]u8 {
    var tree = try treeOf(gpa, encoded, source, flags);
    defer tree.deinit();

    const result = try parser.codegen.generate(gpa, &tree, try printOptions(options));
    defer result.deinit(gpa);

    var offsets: transfer.utf16.Offsets = try .init(gpa, tree.source);
    defer offsets.deinit(gpa);

    const out = try output.bytes(transfer.printedSize(&result));
    const spans = if (tree.source.len == 0) null else &offsets;
    const written = transfer.serializePrinted(&result, spans, out);
    std.debug.assert(written == out.len);
    return out;
}

pub fn analyzeTree(gpa: Allocator, encoded: transfer.Encoded, flags: u32, output: anytype) ![]u8 {
    var tree = try treeOf(gpa, encoded, "", flags);
    defer tree.deinit();
    const sem = try parser.semantic.analyze(&tree, .{ .check = false });
    return serializeModel(gpa, &tree, &sem, .{ .nodes = false }, output);
}

fn treeOf(
    gpa: Allocator,
    encoded: transfer.Encoded,
    source: []const u8,
    flags: u32,
) transfer.Error!Tree {
    const options = parseOptions(flags);
    var tree: Tree = .init(gpa, source);
    errdefer tree.deinit();
    tree.lang = options.lang;
    tree.source_type = options.source_type;
    try transfer.unpack(gpa, &tree, encoded);
    return tree;
}

fn serializeModel(
    gpa: Allocator,
    tree: *Tree,
    sem: *const parser.semantic.Semantic,
    sections: transfer.Sections,
    output: anytype,
) ![]u8 {
    const records = try parser.semantic.module_record.collect(tree, sem);

    var offsets: transfer.utf16.Offsets = try .init(gpa, tree.source);
    defer offsets.deinit(gpa);

    const out = try output.bytes(transfer.semantic.bufferSize(tree, sem, records, sections));
    const written = transfer.semantic.serializeInto(tree, sem, records, &offsets, out, sections);
    std.debug.assert(written == out.len);
    return out;
}

fn parseOptions(flags: u32) parser.Options {
    return .{
        .source_type = @fromBackingInt(@truncate(flags & source_type_mask)),
        .lang = @fromBackingInt(@truncate(flags >> lang_shift)),
        .preserve_parens = flags & preserve_parens != 0,
        .comments = if (flags & attach_comments != 0) .attach else .collect,
        .tokens = flags & tokens != 0,
    };
}

fn printOptions(options: u32) error{InvalidTree}!parser.codegen.Options {
    const quotes = std.enums.fromInt(parser.codegen.Quotes, (options >> print_quotes_shift) & 0b11);
    const comments =
        std.enums.fromInt(parser.codegen.Comments, (options >> print_comments_shift) & 0b111);
    return .{
        .strip = options & print_strip != 0,
        .minify = options & print_minify != 0,
        .format = if (options & print_compact != 0) .compact else .pretty,
        .indent = @truncate(options >> print_indent_shift),
        .quotes = quotes orelse return error.InvalidTree,
        .comments = comments orelse return error.InvalidTree,
        .source_map = if (options & print_source_map != 0) .{} else null,
    };
}
