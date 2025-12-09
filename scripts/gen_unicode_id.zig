const std = @import("std");

const unicode_data_url = "https://www.unicode.org/Public/17.0.0/ucd/UCD.zip";
const temp_zip_path = "/tmp/ucd.zip";
const extraction_path = "/tmp/ucd";
const output_table_path = "./src/util/unicode_id.zig";

const chunk_elements = 16;
const bits_per_element = 32;
const elements_per_chunk = chunk_elements * bits_per_element;
const total_codepoints = std.math.maxInt(u21) + 1;
const total_chunks = total_codepoints / elements_per_chunk;

const CodepointSet = std.AutoArrayHashMap(u32, void);
const BitsetChunk = [chunk_elements]u32;
const TableData = struct { root: []u32, leaf: []u32 };

const PropertyType = enum { Start, Continue };

pub fn main() !void {
    var arena = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = arena.deinit();
    const alloc = arena.allocator();

    if (!pathExists(extraction_path)) {
        try fetchUnicodeData(alloc);
    }

    var start_set, var continue_set = try parseUnicodeProperties(alloc);
    defer start_set.deinit();
    defer continue_set.deinit();

    const start_tables = try buildLookupTables(alloc, start_set);
    const continue_tables = try buildLookupTables(alloc, continue_set);
    defer alloc.free(start_tables.root);
    defer alloc.free(start_tables.leaf);
    defer alloc.free(continue_tables.root);
    defer alloc.free(continue_tables.leaf);

    const output = try std.fs.cwd().createFile(output_table_path, .{});
    defer output.close();

    var buf: [1024]u8 = undefined;
    var buffered_writer = output.writer(&buf);
    const writer = &buffered_writer.interface;

    try writer.writeAll(
        \\// Generated file, do not edit.
        \\// See: scripts/generate_unicode_id.zig
        \\
        \\// inspired by https://github.com/dtolnay/unicode-ident
        \\
        \\const std = @import("std");
        \\
        \\pub fn canStartIdentifier(cp: u32) bool {
        \\    if (cp < 128) {
        \\        return (cp >= 'a' and cp <= 'z') or
        \\            (cp >= 'A' and cp <= 'Z') or
        \\            cp == '_' or cp == '$';
        \\    }
        \\
        \\    return queryBitTable(cp, &id_start_root, &id_start_leaf);
        \\}
        \\
        \\pub fn canContinueIdentifier(cp: u32) bool {
        \\    if (cp < 128) {
        \\        return (cp >= 'a' and cp <= 'z') or
        \\            (cp >= 'A' and cp <= 'Z') or
        \\            cp == '_' or cp == '$' or
        \\            (cp >= '0' and cp <= '9');
        \\    }
        \\
        \\    return queryBitTable(cp, &id_continue_root, &id_continue_leaf);
        \\}
        \\
        \\const chunk_size = 512;
        \\const bits_per_word = 32;
        \\const leaf_chunk_width = 16;
        \\
        \\pub inline fn queryBitTable(cp: u32, root: []const u8, leaf: []const u64) bool {
        \\    const chunk_idx = cp / chunk_size;
        \\    const leaf_base = @as(u32, root[chunk_idx]) * leaf_chunk_width;
        \\    const offset_in_chunk = cp - (chunk_idx * chunk_size);
        \\    const word_idx = leaf_base + (offset_in_chunk / bits_per_word);
        \\    const bit_position: u5 = @truncate(offset_in_chunk % bits_per_word);
        \\    const word = leaf[word_idx];
        \\    return (word >> bit_position) & 1 == 1;
        \\}
    );

    try emitTableStructure(start_tables, writer, "id_start");
    try emitTableStructure(continue_tables, writer, "id_continue");

    try buffered_writer.end();
}

fn buildLookupTables(alloc: std.mem.Allocator, codepoints: CodepointSet) !TableData {
    var chunk_index_map = std.AutoArrayHashMap(usize, BitsetChunk).init(alloc);
    defer chunk_index_map.deinit();

    var leaf_dedup_map = std.AutoArrayHashMap(BitsetChunk, usize).init(alloc);
    defer leaf_dedup_map.deinit();

    const zero_chunk: BitsetChunk = .{0} ** chunk_elements;

    var chunk_idx: usize = 0;
    while (chunk_idx < total_chunks) : (chunk_idx += 1) {
        var bitset: BitsetChunk = zero_chunk;

        for (0.., &bitset) |elem_idx, *element| {
            var bit_idx: u32 = 0;
            while (bit_idx < bits_per_element) : (bit_idx += 1) {
                const cp: u32 = @intCast(chunk_idx * elements_per_chunk + elem_idx * bits_per_element + bit_idx);
                const is_set: u32 = if (codepoints.contains(cp)) 1 else 0;
                element.* = element.* | (is_set << @as(u5, @intCast(bit_idx)));
            }
        }

        try chunk_index_map.put(chunk_idx, bitset);

        const entry = try leaf_dedup_map.getOrPut(bitset);
        if (!entry.found_existing) {
            entry.value_ptr.* = leaf_dedup_map.count() - 1;
        }
    }

    const root = try alloc.alloc(u32, total_chunks);

    for (0..total_chunks) |idx| {
        const bitset = chunk_index_map.get(idx) orelse unreachable;
        const leaf_idx = leaf_dedup_map.get(bitset) orelse unreachable;
        root[idx] = @intCast(leaf_idx);
    }

    var leaf_buffer: std.ArrayList(u32) = .empty;

    for (leaf_dedup_map.keys()) |*chunk_bits| {
        for (chunk_bits) |bits| {
            try leaf_buffer.append(alloc, bits);
        }
    }

    const leaf = try leaf_buffer.toOwnedSlice(alloc);

    return .{ .root = root, .leaf = leaf };
}

pub fn parseUnicodeProperties(alloc: std.mem.Allocator) !struct { CodepointSet, CodepointSet } {
    const target_file = "DerivedCoreProperties.txt";

    var data_dir = try std.fs.openDirAbsolute(extraction_path, .{});
    defer data_dir.close();

    const file_data = try data_dir.readFileAlloc(target_file, alloc, .limited(2 * 1024 * 1024));
    defer alloc.free(file_data);

    var start_set = CodepointSet.init(alloc);
    var continue_set = CodepointSet.init(alloc);

    var line_iter = std.mem.splitScalar(u8, file_data, '\n');

    while (line_iter.next()) |line| {
        if (line.len == 0 or std.mem.startsWith(u8, line, "#")) continue;

        const prop_type: PropertyType = if (std.mem.indexOf(u8, line, "ID_Start")) |_|
            .Start
        else if (std.mem.indexOf(u8, line, "ID_Continue")) |_|
            .Continue
        else
            continue;

        const range = try extractCodepointRange(line) orelse continue;

        var cp = range.start;
        while (cp < range.end) : (cp += 1) {
            const target_set = if (prop_type == .Start) &start_set else &continue_set;
            try target_set.put(@intCast(cp), {});
        }
    }

    return .{ start_set, continue_set };
}

const CodepointRange = struct { start: u32, end: u32 };

fn extractCodepointRange(line: []const u8) !?CodepointRange {
    const sep_pos = std.mem.indexOfScalar(u8, line, ' ') orelse line.len;
    const hex_part = line[0..sep_pos];

    if (std.mem.indexOf(u8, hex_part, "..")) |range_sep| {
        const low = try std.fmt.parseInt(u32, hex_part[0..range_sep], 16);
        const high = try std.fmt.parseInt(u32, hex_part[range_sep + 2 ..], 16);
        return .{ .start = low, .end = high + 1 };
    } else {
        const single = try std.fmt.parseInt(u32, hex_part, 16);
        return .{ .start = single, .end = single + 1 };
    }
}

pub fn fetchUnicodeData(alloc: std.mem.Allocator) !void {
    var http_client: std.http.Client = .{ .allocator = alloc };
    defer http_client.deinit();

    const target_uri = try std.Uri.parse(unicode_data_url);
    var http_req = try http_client.request(.GET, target_uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
    });
    defer http_req.deinit();

    try http_req.sendBodiless();
    var http_resp = try http_req.receiveHead(&.{});

    const zip_file = try std.fs.createFileAbsolute(temp_zip_path, .{});
    defer zip_file.close();
    defer std.fs.deleteFileAbsolute(temp_zip_path) catch @panic("Zip deletion filed");

    var zip_file_writer = zip_file.writer(&.{});
    const zip_writer = &zip_file_writer.interface;

    var resp_buf: [1024]u8 = undefined;
    const resp_reader = http_resp.reader(&resp_buf);
    _ = try resp_reader.streamRemaining(zip_writer);

    try std.fs.deleteTreeAbsolute(extraction_path);
    try std.fs.makeDirAbsolute(extraction_path);

    var extract_dir = try std.fs.openDirAbsolute(extraction_path, .{});
    defer extract_dir.close();

    const archive = try std.fs.openFileAbsolute(temp_zip_path, .{});
    defer archive.close();

    var archive_buf: [1024]u8 = undefined;
    var archive_reader = archive.reader(&archive_buf);

    try std.zip.extract(extract_dir, &archive_reader, .{});

    std.log.info("Extracted successfully to {s}", .{extraction_path});
}

fn pathExists(path: []const u8) bool {
    if (std.fs.accessAbsolute(path, .{})) |_| {
        return true;
    } else |_| {
        return false;
    }
}

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
