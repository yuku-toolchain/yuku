const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");
const transfer = @import("transfer/semantic.zig");

const Options = struct {
    source_type: parser.ast.SourceType = .module,
    lang: parser.ast.Lang = .js,
    preserve_parens: bool = true,
    allow_return_outside_function: bool = false,
    attach_comments: bool = false,
};

/// Parses and semantically analyzes one file, returning the analyzer
/// buffer: the v7 AST sections followed by the semantic sections
/// (scopes, symbols, resolved references, module records).
pub fn analyze(env: napi.Env, source: []const u8, options: Options) !napi.Val {
    var tree = parser.parse(std.heap.smp_allocator, source, .{
        .source_type = options.source_type,
        .lang = options.lang,
        .preserve_parens = options.preserve_parens,
        .allow_return_outside_function = options.allow_return_outside_function,
        .comments = if (options.attach_comments) .both else .flat,
    }) catch return error.AnalyzeFailed;
    defer tree.deinit();

    // analysis is error tolerant: a tree with syntax errors still
    // produces scopes, symbols, and diagnostics
    const sem = parser.semantic.analyze(&tree) catch return error.AnalyzeFailed;

    // collect before sizing: records may intern "default" into the pool
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
