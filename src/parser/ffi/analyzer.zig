const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");
const transfer = @import("transfer/semantic.zig");

const Options = struct {
    source_type: parser.ast.SourceType = .module,
    lang: parser.ast.Lang = .js,
    preserve_parens: bool = true,
    attach_comments: bool = false,
};

/// Parses and analyzes one file into the analyzer buffer, the AST sections followed by
/// the semantic sections.
pub fn analyze(env: napi.Env, source: []const u8, options: Options) !napi.Val {
    var tree = parser.parse(std.heap.smp_allocator, source, .{
        .source_type = options.source_type,
        .lang = options.lang,
        .preserve_parens = options.preserve_parens,
        .comments = if (options.attach_comments) .both else .flat,
    }) catch return error.AnalyzeFailed;
    defer tree.deinit();

    // analysis is error tolerant, a tree with syntax errors still yields scopes and symbols
    const sem = parser.semantic.analyze(&tree) catch return error.AnalyzeFailed;

    // collect before sizing, records may intern into the string pool
    const records = parser.semantic.module_record.collect(
        &tree,
        &sem,
    ) catch return error.AnalyzeFailed;

    const size = transfer.bufferSize(&tree, &sem, records);
    const ab = try env.createArrayBuffer(size);
    _ = transfer.serializeInto(&tree, &sem, records, ab.data);

    return ab.val;
}

comptime {
    napi.module(@This());
}
