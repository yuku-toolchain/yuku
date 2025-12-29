const std = @import("std");
const js = @import("js");
const wasm_allocator = std.heap.wasm_allocator;

pub export fn alloc(size: usize) ?[*]u8 {
    const buf = wasm_allocator.alloc(u8, size) catch return null;
    return buf.ptr;
}

pub export fn free(ptr: [*]u8, size: usize) void {
    wasm_allocator.free(ptr[0..size]);
}

/// memory layout of returned pointer:
/// [0..4]   = length (u32, little-endian)
/// [4..len] = JSON string data
///
/// returns 0 if parsing failed.
/// caller must free with: free(ptr, length + 4)
pub export fn parse(
    source_bytes: [*]const u8,
    len: u32,
    source_type: u32,
    lang: u32,
) u32 {
    const source: []const u8 = if (len == 0) &[_]u8{} else source_bytes[0..len];

    const st: js.SourceType = if (source_type == 0) .script else .module;
    const l: js.Lang = switch (lang) {
        0 => .js,
        1 => .ts,
        2 => .jsx,
        3 => .tsx,
        4 => .dts,
        else => .js,
    };

    const options = js.Options{
        .source_type = st,
        .lang = l,
    };

    var parse_tree = js.parse(wasm_allocator, source, options) catch {
        return 0;
    };
    defer parse_tree.deinit();

    const json_str = js.estree.toJSON(&parse_tree, wasm_allocator, .{ .pretty = false }) catch {
        return 0;
    };
    // note: json_str is now owned by us, we'll embed it in the result buffer

    // [4 bytes for length][json_str bytes]
    const total_size = @sizeOf(u32) + json_str.len;
    const result_buf = wasm_allocator.alloc(u8, total_size) catch {
        wasm_allocator.free(json_str);
        return 0;
    };

    // write length in first 4 bytes (little-endian)
    const json_len: u32 = @intCast(json_str.len);
    std.mem.writeInt(u32, result_buf[0..4], json_len, .little);

    // copy json string after the length
    @memcpy(result_buf[4..], json_str);

    wasm_allocator.free(json_str);

    return @intCast(@intFromPtr(result_buf.ptr));
}
