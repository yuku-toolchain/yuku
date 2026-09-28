// inspired by https://github.com/dtolnay/unicode-ident

const std = @import("std");

const unicode_data_url = "https://www.unicode.org/Public/17.0.0/ucd/UCD.zip";
const temp_zip_path = ".zig-cache/ucd.zip";
const extraction_path = ".zig-cache/ucd";
const derived_core_properties_path = ".zig-cache/ucd/DerivedCoreProperties.txt";
const output_table_path = "./src/util/unicode_id.zig";

const chunk_elements = 16;
const bits_per_element = 32;
const elements_per_chunk = chunk_elements * bits_per_element;
const total_codepoints = std.math.maxInt(u21) + 1;
const total_chunks = total_codepoints / elements_per_chunk;

const CodepointSet = std.array_hash_map.Auto(u32, void);
const BitsetChunk = [chunk_elements]u32;
const TableData = struct { root: []u32, leaf: []u32 };
const CodepointRange = struct { start: u32, end: u32 };

pub fn main(init: std.process.Init) !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer _ = debug_allocator.deinit();
    const gpa = debug_allocator.allocator();

    const io = init.io;

    var start_set, var continue_set = try downloadAndParseProperties(io, gpa);
    defer start_set.deinit(gpa);
    defer continue_set.deinit(gpa);

    const start_tables = try buildLookupTables(gpa, start_set);
    const continue_tables = try buildLookupTables(gpa, continue_set);
    defer gpa.free(start_tables.root);
    defer gpa.free(start_tables.leaf);
    defer gpa.free(continue_tables.root);
    defer gpa.free(continue_tables.leaf);

    const output = try std.Io.Dir.cwd().createFile(io, output_table_path, .{});
    defer output.close(io);

    var buf: [1024]u8 = undefined;
    var buffered_writer = output.writer(io, &buf);
    const writer = &buffered_writer.interface;

    try writer.writeAll(
        \\// Generated file, do not edit.
        \\// See: tools/gen_unicode_id.zig
        \\
        \\// inspired by https://github.com/dtolnay/unicode-ident
        \\
        \\pub fn canStartId(cp: u32) bool {
        \\    return queryBitTable(cp, &id_start_root, &id_start_leaf);
        \\}
        \\
        \\pub fn canContinueId(cp: u32) bool {
        \\    return queryBitTable(cp, &id_continue_root, &id_continue_leaf);
        \\}
        \\
        \\const chunk_size = 512;
        \\const bits_per_word = 32;
        \\const leaf_chunk_width = 16;
        \\
        \\inline fn queryBitTable(
        \\    cp: u32,
        \\    comptime root: []const u8,
        \\    comptime leaf: []const u64,
        \\) bool {
        \\    const chunk_idx = cp / chunk_size;
        \\    const leaf_base = @as(u32, root[chunk_idx]) * leaf_chunk_width;
        \\    const offset_in_chunk = cp - (chunk_idx * chunk_size);
        \\    const word_idx = leaf_base + (offset_in_chunk / bits_per_word);
        \\    const bit_position: u5 = @truncate(offset_in_chunk % bits_per_word);
        \\    const word = leaf[word_idx];
        \\    return (word >> bit_position) & 1 == 1;
        \\}
        \\
    );

    try emitTableStructure(start_tables, writer, "id_start");
    try emitTableStructure(continue_tables, writer, "id_continue");

    try buffered_writer.end();
}

/// Downloads the Unicode data on first use, then returns the ID_Start and ID_Continue sets.
pub fn downloadAndParseProperties(
    io: std.Io,
    gpa: std.mem.Allocator,
) !struct { CodepointSet, CodepointSet } {
    if (!pathExists(io, derived_core_properties_path)) {
        try fetchUnicodeData(io, gpa);
    }
    return try parseUnicodeProperties(io, gpa);
}

// two-level table, the root maps each 512-codepoint chunk to a deduplicated leaf bitset
fn buildLookupTables(gpa: std.mem.Allocator, codepoints: CodepointSet) !TableData {
    // unique chunk bitsets in first-seen order, the insertion index is the leaf index
    var leaves: std.array_hash_map.Auto(BitsetChunk, void) = .empty;
    defer leaves.deinit(gpa);

    const root = try gpa.alloc(u32, total_chunks);
    errdefer gpa.free(root);

    for (root, 0..) |*leaf_index, chunk_index| {
        var bitset: BitsetChunk = @splat(0);
        for (&bitset, 0..) |*element, element_index| {
            for (0..bits_per_element) |bit_index| {
                const cp: u32 = @intCast(chunk_index * elements_per_chunk +
                    element_index * bits_per_element + bit_index);
                if (codepoints.contains(cp)) element.* |= @as(u32, 1) << @intCast(bit_index);
            }
        }
        const entry = try leaves.getOrPut(gpa, bitset);
        leaf_index.* = @intCast(entry.index);
    }

    const leaf = try gpa.alloc(u32, leaves.count() * chunk_elements);
    for (leaves.keys(), 0..) |*chunk, index| {
        @memcpy(leaf[index * chunk_elements ..][0..chunk_elements], chunk);
    }

    return .{ .root = root, .leaf = leaf };
}

// reads DerivedCoreProperties.txt lines like `0041..005A    ; ID_Start # ...`
fn parseUnicodeProperties(
    io: std.Io,
    gpa: std.mem.Allocator,
) !struct { CodepointSet, CodepointSet } {
    var data_dir = try std.Io.Dir.cwd().openDir(io, extraction_path, .{});
    defer data_dir.close(io);

    const file_data = try data_dir.readFileAlloc(
        io,
        "DerivedCoreProperties.txt",
        gpa,
        .limited(2 * 1024 * 1024),
    );
    defer gpa.free(file_data);

    var start_set: CodepointSet = .{};
    var continue_set: CodepointSet = .{};

    var line_iter = std.mem.splitScalar(u8, file_data, '\n');
    while (line_iter.next()) |line| {
        if (line.len == 0 or std.mem.startsWith(u8, line, "#")) continue;

        const target_set = if (std.mem.find(u8, line, "ID_Start") != null)
            &start_set
        else if (std.mem.find(u8, line, "ID_Continue") != null)
            &continue_set
        else
            continue;

        const range = try extractCodepointRange(line);
        var cp = range.start;
        while (cp < range.end) : (cp += 1) {
            try target_set.put(gpa, cp, {});
        }
    }

    return .{ start_set, continue_set };
}

// the half-open range of a `0041..005A` or `200C` codepoint column
fn extractCodepointRange(line: []const u8) !CodepointRange {
    const hex_part = line[0 .. std.mem.findScalar(u8, line, ' ') orelse line.len];

    if (std.mem.find(u8, hex_part, "..")) |range_sep| {
        const low = try std.fmt.parseInt(u32, hex_part[0..range_sep], 16);
        const high = try std.fmt.parseInt(u32, hex_part[range_sep + 2 ..], 16);
        return .{ .start = low, .end = high + 1 };
    } else {
        const single = try std.fmt.parseInt(u32, hex_part, 16);
        return .{ .start = single, .end = single + 1 };
    }
}

fn fetchUnicodeData(io: std.Io, gpa: std.mem.Allocator) !void {
    var http_client: std.http.Client = .{ .allocator = gpa, .io = io };
    defer http_client.deinit();

    const target_uri = try std.Uri.parse(unicode_data_url);
    var http_req = try http_client.request(.GET, target_uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
    });
    defer http_req.deinit();

    try http_req.sendBodiless();
    var http_resp = try http_req.receiveHead(&.{});

    const zip_file = try std.Io.Dir.cwd().createFile(io, temp_zip_path, .{});
    defer zip_file.close(io);
    defer std.Io.Dir.cwd().deleteFile(io, temp_zip_path) catch @panic("Zip deletion failed");

    var zip_file_writer = zip_file.writer(io, &.{});
    const zip_writer = &zip_file_writer.interface;

    var resp_buf: [1024]u8 = undefined;
    const resp_reader = http_resp.reader(&resp_buf);
    _ = try resp_reader.streamRemaining(zip_writer);

    std.Io.Dir.cwd().deleteTree(io, extraction_path) catch {};
    std.Io.Dir.cwd().createDir(io, extraction_path, .default_dir) catch {};

    var extract_dir = try std.Io.Dir.cwd().openDir(io, extraction_path, .{});
    defer extract_dir.close(io);

    const archive = try std.Io.Dir.cwd().openFile(io, temp_zip_path, .{});
    defer archive.close(io);

    var archive_buf: [1024]u8 = undefined;
    var archive_reader = archive.reader(io, &archive_buf);

    try std.zip.extract(extract_dir, &archive_reader, .{});

    std.log.info("Extracted successfully to {s}", .{extraction_path});
}

fn pathExists(io: std.Io, path: []const u8) bool {
    std.Io.Dir.cwd().access(io, path, .{}) catch return false;
    return true;
}

// the u8 root table indexes at most 256 unique leaf chunks
fn emitTableStructure(data: TableData, writer: *std.Io.Writer, table_name: []const u8) !void {
    try writer.print(
        \\
        \\pub const {s}_root = [_]u8{{
    , .{table_name});

    for (0.., data.root) |idx, val| {
        if (idx % 16 == 0) {
            try writer.writeAll("\n    ");
        } else {
            try writer.writeAll(" ");
        }
        try writer.print("0x{x:0>2},", .{val});
    }
    try writer.writeAll(
        \\
        \\};
        \\
    );

    try writer.print(
        \\
        \\pub const {s}_leaf = [_]u64{{
    , .{table_name});
    for (0.., data.leaf) |idx, val| {
        if (idx % 8 == 0) {
            try writer.writeAll("\n    ");
        } else {
            try writer.writeAll(" ");
        }
        try writer.print("0x{x:0>2},", .{val});
    }
    try writer.writeAll(
        \\
        \\};
        \\
    );

    std.log.info("Successfully wrote {s} to {s}", .{ table_name, output_table_path });
}
