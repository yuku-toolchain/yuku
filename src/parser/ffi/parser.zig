const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");
const transfer = @import("transfer/root.zig");

const Options = struct {
    source_type: parser.ast.SourceType = .module,
    lang: parser.ast.Lang = .js,
    preserve_parens: bool = true,
    semantic_errors: bool = false,
    attach_comments: bool = false,
    tokens: bool = false,
};

pub fn parse(env: napi.Env, source: []const u8, options: Options) !napi.Val {
    var tree = parser.parse(std.heap.smp_allocator, source, .{
        .source_type = options.source_type,
        .lang = options.lang,
        .preserve_parens = options.preserve_parens,
        .comments = if (options.attach_comments) .both else .flat,
        .tokens = options.tokens,
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
