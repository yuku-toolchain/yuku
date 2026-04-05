const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");

pub fn parse(env: napi.Env, source: []const u8, options: Options) !napi.Val {
    var tree = parser.parse(std.heap.c_allocator, source, .{
        .source_type = options.source_type,
        .lang = options.lang,
    }) catch return error.ParseFailed;
    defer tree.deinit();

    const size = parser.transfer.bufferSize(&tree);
    const ab = try env.createArrayBuffer(size);
    _ = parser.transfer.serializeInto(&tree, ab.data);

    return ab.val;
}

const Options = struct {
    source_type: parser.ast.SourceType = .module,
    lang: parser.ast.Lang = .js,
};

comptime {
    napi.module(@This());
}
