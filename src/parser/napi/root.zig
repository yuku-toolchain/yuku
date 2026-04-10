const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");
const transfer = @import("transfer.zig");

const Options = struct {
    source_type: parser.ast.SourceType = .module,
    lang: parser.ast.Lang = .js,
    preserve_parens: bool = true,
    semantic_errors: bool = false,
};

const alloc = std.heap.smp_allocator;

pub fn parse_sync(env: napi.Env, source: []const u8, options: Options) !napi.Val {
    var tree = parser.parse(alloc, source, .{
        .source_type = options.source_type,
        .lang = options.lang,
        .preserve_parens = options.preserve_parens,
    }) catch return error.ParseFailed;
    defer tree.deinit();

    if (options.semantic_errors) {
        _ = parser.semantic.analyze(&tree) catch {};
    }

    const size = transfer.bufferSize(&tree);
    const ab = try env.createArrayBuffer(size);
    _ = transfer.serializeInto(&tree, ab.data);

    return ab.val;
}

comptime {
    napi.module(@This());
}
