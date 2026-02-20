// inspired by https://github.com/dtolnay/unicode-ident

const std = @import("std");

const unicode_data_url = "https://www.unicode.org/Public/17.0.0/ucd/UCD.zip";
const temp_zip_path = "/tmp/ucd.zip";
const extraction_path = "/tmp/ucd";
const derived_core_properties_path = "/tmp/ucd/DerivedCoreProperties.txt";
const output_table_path = "./src/util/unicode_id.zig";

const chunk_elements = 16; // number of 32-bit words per chunk
const bits_per_element = 32; // bits per word (standard u32)
const elements_per_chunk = chunk_elements * bits_per_element; // = 512 codepoints per chunk
const total_codepoints = std.math.maxInt(u21) + 1; // unicode max = 0x10FFFF + 1
const total_chunks = total_codepoints / elements_per_chunk; // total number of chunks to process

const CodepointSet = std.AutoArrayHashMap(u32, void);
const BitsetChunk = [chunk_elements]u32;
const TableData = struct { root: []u32, leaf: []u32 };

const PropertyType = enum { Start, Continue };

pub fn main(init: std.process.Init) !void {
    var arena = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = arena.deinit();
    const alloc = arena.allocator();

    const io = init.io;

    if (!pathExists(io, derived_core_properties_path)) {
        try fetchUnicodeData(io, alloc);
    }

    var start_set, var continue_set = try parseUnicodeProperties(io, alloc);
    defer start_set.deinit();
    defer continue_set.deinit();

    const start_tables = try buildLookupTables(alloc, start_set);
    const continue_tables = try buildLookupTables(alloc, continue_set);
    defer alloc.free(start_tables.root);
    defer alloc.free(start_tables.leaf);
    defer alloc.free(continue_tables.root);
    defer alloc.free(continue_tables.leaf);

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
        \\inline fn queryBitTable(cp: u32, comptime root: []const u8, comptime leaf: []const u64) bool {
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

/// builds compact two-level lookup tables from a set of codepoints
///
/// 1. iterate through all possible chunks (0x10FFFF / 512 = ~4290 chunks)
/// 2. for each chunk, create a 512-bit bitset where bit N is 1 if codepoint is in the set
/// 3. deduplicate identical chunks (many chunks are all-zeros or identical patterns)
/// 4. build root table mapping chunk_index -> deduplicated_leaf_index
/// 5. build leaf table containing only unique chunk bitsets
fn buildLookupTables(alloc: std.mem.Allocator, codepoints: CodepointSet) !TableData {
    // maps chunk index to its bitset representation
    var chunk_index_map = std.AutoArrayHashMap(usize, BitsetChunk).init(alloc);
    defer chunk_index_map.deinit();

    // deduplication map: bitset pattern -> unique leaf index
    var leaf_dedup_map = std.AutoArrayHashMap(BitsetChunk, usize).init(alloc);
    defer leaf_dedup_map.deinit();

    const zero_chunk: BitsetChunk = .{0} ** chunk_elements;

    // step 1: build bitsets for all chunks
    var chunk_idx: usize = 0;
    while (chunk_idx < total_chunks) : (chunk_idx += 1) {
        var bitset: BitsetChunk = zero_chunk;

        // for each of the 16 u32 elements in this chunk
        for (0.., &bitset) |elem_idx, *element| {
            // for each of the 32 bits in this element
            var bit_idx: u32 = 0;
            while (bit_idx < bits_per_element) : (bit_idx += 1) {
                // calculate the actual codepoint this bit represents
                const cp: u32 = @intCast(chunk_idx * elements_per_chunk + elem_idx * bits_per_element + bit_idx);

                // set the bit if this codepoint has the property
                const is_set: u32 = if (codepoints.contains(cp)) 1 else 0;
                element.* = element.* | (is_set << @as(u5, @intCast(bit_idx)));
            }
        }

        // store this chunk's bitset
        try chunk_index_map.put(chunk_idx, bitset);

        // stap 2: deduplicate, if we've seen this pattern before, reuse it
        const entry = try leaf_dedup_map.getOrPut(bitset);
        if (!entry.found_existing) {
            // new unique pattern, assign it the next leaf index
            entry.value_ptr.* = leaf_dedup_map.count() - 1;
        }
    }

    // step 3: build the root table
    // maps each chunk index to its deduplicated leaf index
    const root = try alloc.alloc(u32, total_chunks);

    for (0..total_chunks) |idx| {
        const bitset = chunk_index_map.get(idx) orelse unreachable;
        const leaf_idx = leaf_dedup_map.get(bitset) orelse unreachable;
        root[idx] = @intCast(leaf_idx);
    }

    // step 4: build the leaf table
    // contains only unique bitset patterns, in the order they were discovered
    var leaf_buffer: std.ArrayList(u32) = .empty;

    for (leaf_dedup_map.keys()) |*chunk_bits| {
        for (chunk_bits) |bits| {
            try leaf_buffer.append(alloc, bits);
        }
    }

    const leaf = try leaf_buffer.toOwnedSlice(alloc);

    return .{ .root = root, .leaf = leaf };
}

/// parses Unicode DerivedCoreProperties.txt to extract ID_Start and ID_Continue codepoints
///
/// the file contains lines like:
///   0041..005A    ; ID_Start # L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
///   0030..0039    ; ID_Continue # Nd  [10] DIGIT ZERO..DIGIT NINE
///   200C          ; ID_Continue # Cf       ZERO WIDTH NON-JOINER
///
/// each line can specify:
/// - a single codepoint (e.g., "200C")
/// - a range of codepoints (e.g., "0041..005A")
///
/// this function extracts both ID_Start and ID_Continue properties and returns them
/// as separate sets of codepoints.
pub fn parseUnicodeProperties(io: std.Io, alloc: std.mem.Allocator) !struct { CodepointSet, CodepointSet } {
    const target_file = "DerivedCoreProperties.txt";

    var data_dir = try std.Io.Dir.openDirAbsolute(io, extraction_path, .{});
    defer data_dir.close(io);

    const file_data = try data_dir.readFileAlloc(io, target_file, alloc, .limited(2 * 1024 * 1024));
    defer alloc.free(file_data);

    var start_set = CodepointSet.init(alloc);
    var continue_set = CodepointSet.init(alloc);

    var line_iter = std.mem.splitScalar(u8, file_data, '\n');

    while (line_iter.next()) |line| {
        // skip empty lines and comments
        if (line.len == 0 or std.mem.startsWith(u8, line, "#")) continue;

        // determine which property this line describes
        const prop_type: PropertyType = if (std.mem.indexOf(u8, line, "ID_Start")) |_|
            .Start
        else if (std.mem.indexOf(u8, line, "ID_Continue")) |_|
            .Continue
        else
            continue; // line doesn't contain a property we care about

        // extract the codepoint or codepoint range
        const range = try extractCodepointRange(line) orelse continue;

        // add all codepoints in the range to the appropriate set
        var cp = range.start;
        while (cp < range.end) : (cp += 1) {
            const target_set = if (prop_type == .Start) &start_set else &continue_set;
            try target_set.put(@intCast(cp), {});
        }
    }

    return .{ start_set, continue_set };
}

const CodepointRange = struct { start: u32, end: u32 };

/// extracts a codepoint range from a line in DerivedCoreProperties.txt
fn extractCodepointRange(line: []const u8) !?CodepointRange {
    // find the first space or semicolon (marks end of codepoint part)
    const sep_pos = std.mem.findScalar(u8, line, ' ') orelse line.len;
    const hex_part = line[0..sep_pos];

    // check if this is a range (contains "..")
    if (std.mem.indexOf(u8, hex_part, "..")) |range_sep| {
        const low = try std.fmt.parseInt(u32, hex_part[0..range_sep], 16);
        const high = try std.fmt.parseInt(u32, hex_part[range_sep + 2 ..], 16);

        return .{ .start = low, .end = high + 1 }; // +1 for half-open range
    } else {
        // single codepoint
        const single = try std.fmt.parseInt(u32, hex_part, 16);
        return .{ .start = single, .end = single + 1 }; // range of one element
    }
}

pub fn fetchUnicodeData(io: std.Io, alloc: std.mem.Allocator) !void {
    var http_client: std.http.Client = .{ .allocator = alloc, .io = io };
    defer http_client.deinit();

    const target_uri = try std.Uri.parse(unicode_data_url);
    var http_req = try http_client.request(.GET, target_uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
    });
    defer http_req.deinit();

    try http_req.sendBodiless();
    var http_resp = try http_req.receiveHead(&.{});

    const zip_file = try std.Io.Dir.createFileAbsolute(io, temp_zip_path, .{});
    defer zip_file.close(io);
    defer std.Io.Dir.deleteFileAbsolute(io, temp_zip_path) catch @panic("Zip deletion filed");

    var zip_file_writer = zip_file.writer(io, &.{});
    const zip_writer = &zip_file_writer.interface;

    var resp_buf: [1024]u8 = undefined;
    const resp_reader = http_resp.reader(&resp_buf);
    _ = try resp_reader.streamRemaining(zip_writer);

    std.Io.Dir.deleteDirAbsolute(io, extraction_path) catch {};
    std.Io.Dir.createDirAbsolute(io, extraction_path, .default_dir) catch {};

    var extract_dir = try std.Io.Dir.openDirAbsolute(io, extraction_path, .{});
    defer extract_dir.close(io);

    const archive = try std.Io.Dir.openFileAbsolute(io, temp_zip_path, .{});
    defer archive.close(io);

    var archive_buf: [1024]u8 = undefined;
    var archive_reader = archive.reader(io, &archive_buf);

    try std.zip.extract(extract_dir, &archive_reader, .{});

    std.log.info("Extracted successfully to {s}", .{extraction_path});
}

fn pathExists(io: std.Io, path: []const u8) bool {
    if (std.Io.Dir.accessAbsolute(io, path, .{})) |_| {
        return true;
    } else |_| {
        return false;
    }
}

/// writes a lookup table to the output file in zig array format
///
/// generates code like:
///   pub const id_start_root = [_]u8{
///       0x00, 0x01, 0x02, ...
///   };
///
/// The root table uses u8 (can index up to 256 unique patterns)
/// The leaf table uses u64 (8 bytes for better formatting and alignment)
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

pub fn downloadAndParseProperties(io: std.Io, alloc: std.mem.Allocator) !struct { CodepointSet, CodepointSet } {
    if (!pathExists(io, derived_core_properties_path)) {
        try fetchUnicodeData(io, alloc);
    }
    return try parseUnicodeProperties(io, alloc);
}
