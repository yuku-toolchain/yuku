// C API for Yuku parser. Exports yuku_parse() and yuku_free() as C ABI functions.
//
// Build:
//   zig build c-lib                      (debug)
//   zig build c-lib -Doptimize=ReleaseFast  (optimized)
//
// Output: zig-out/lib/libyuku-c.{dylib,so,dll}
//
// Usage from C:
//   #include "yuku.h"
//   YukuResult result = yuku_parse(source, len, YUKU_SOURCE_MODULE, YUKU_LANG_JS);
//   // ... walk the buffer using yuku.h inline accessors ...
//   yuku_free(&result);
//
// Header generation:
//   zig build gen-c-header
//   Output: zig-out/yuku-c.h

const std = @import("std");
const parser = @import("parser");
const ast = parser.ast;
const transfer = @import("transfer");

const YukuResult = extern struct {
    buf: ?[*]u8,
    size: usize,
    has_errors: c_int,
    source: ?[*]const u8,
};

const YukuParseSourceType = enum(c_int) {
    script = 0,
    module = 1,
};

const YukuParseLang = enum(c_int) {
    js = 0,
    ts = 1,
    jsx = 2,
    tsx = 3,
    dts = 4,
};

export fn yuku_parse(source: [*]const u8, source_len: usize, source_type: YukuParseSourceType, lang: YukuParseLang) YukuResult {
    const src = source[0..source_len];

    const st: ast.SourceType = switch (source_type) {
        .script => .script,
        .module => .module,
    };
    const l: ast.Lang = switch (lang) {
        .js => .js,
        .ts => .ts,
        .jsx => .jsx,
        .tsx => .tsx,
        .dts => .dts,
    };

    var result: YukuResult = .{
        .buf = null,
        .size = 0,
        .has_errors = 0,
        .source = null,
    };

    var tree = parser.parse(std.heap.page_allocator, src, .{
        .source_type = st,
        .lang = l,
    }) catch return result;
    defer tree.deinit();

    const buf_size = transfer.bufferSize(&tree);
    const buf = std.heap.page_allocator.alloc(u8, buf_size) catch return result;
    _ = transfer.serializeInto(&tree, buf);

    var has_err: c_int = 0;
    for (tree.diagnostics.items) |d| {
        if (d.severity == .@"error") {
            has_err = 1;
            break;
        }
    }

    result.buf = buf.ptr;
    result.size = buf_size;
    result.has_errors = has_err;
    result.source = source;
    return result;
}

export fn yuku_free(result: *YukuResult) void {
    if (result.buf) |ptr| {
        std.heap.page_allocator.free(ptr[0..result.size]);
        result.buf = null;
        result.size = 0;
        result.has_errors = 0;
        result.source = null;
    }
}
