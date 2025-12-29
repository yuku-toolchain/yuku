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

/// returns pointer to json string (caller owns the memory).
/// use get_result_len() after parse() to get the length.
/// returns 0 if parsing failed.
/// caller must free with: free(ptr, get_result_len())
///
var last_result_len: u32 = 0;

pub export fn get_result_len() u32 {
    return last_result_len;
}

pub export fn parse(
    source_bytes: [*]const u8,
    len: u32,
    source_type: u32,
    lang: u32,
) u32 {
    last_result_len = 0;

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

    last_result_len = @intCast(json_str.len);

    return @intCast(@intFromPtr(json_str.ptr));
}
