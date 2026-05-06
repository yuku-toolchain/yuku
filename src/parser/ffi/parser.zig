const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");
const transfer = @import("transfer.zig");

const Options = struct {
    source_type: parser.ast.SourceType = .module,
    lang: parser.ast.Lang = .js,
    preserve_parens: bool = true,
    allow_return_outside_function: bool = false,
    semantic_errors: bool = false,
};

pub fn parse(env: napi.Env, source: []const u8, options: Options) !napi.Val {
    var tree = parser.parse(std.heap.page_allocator, source, .{
        .source_type = options.source_type,
        .lang = options.lang,
        .preserve_parens = options.preserve_parens,
        .allow_return_outside_function = options.allow_return_outside_function,
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
