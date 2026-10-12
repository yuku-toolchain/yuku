//! The Node-API core of `yuku-core`.

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

// reused across calls, so large parses skip mapping and faulting in fresh pages
const scratch_retained_bytes_max = 16 * 1024 * 1024;

threadlocal var scratch: std.heap.ArenaAllocator = .init(std.heap.smp_allocator);
threadlocal var scratch_hooked = false;

pub fn parse(env: napi.Env, source: []const u8, options: Options) !napi.Val {
    defer scratchReset();
    var tree = parseTree(env, source, options) catch return error.ParseFailed;
    defer tree.deinit();

    if (options.semantic_errors) _ = parser.semantic.analyze(&tree) catch {};

    const buffer = try env.createArrayBuffer(transfer.bufferSize(&tree));
    _ = transfer.serializeInto(&tree, buffer.data);
    return buffer.val;
}

pub fn analyze(env: napi.Env, source: []const u8, options: Options) !napi.Val {
    defer scratchReset();
    var tree = parseTree(env, source, options) catch return error.AnalyzeFailed;
    defer tree.deinit();

    // analysis is error tolerant, a tree with syntax errors still yields scopes and symbols
    const sem = parser.semantic.analyze(&tree) catch return error.AnalyzeFailed;
    // collect before sizing, records may intern into the string pool
    const records = parser.semantic.module_record.collect(&tree, &sem) catch
        return error.AnalyzeFailed;

    const buffer = try env.createArrayBuffer(transfer.semantic.bufferSize(&tree, &sem, records));
    _ = transfer.semantic.serializeInto(&tree, &sem, records, buffer.data);
    return buffer.val;
}

fn parseTree(env: napi.Env, source: []const u8, options: Options) !parser.ast.Tree {
    if (!scratch_hooked) {
        // frees the arena of a worker thread on exit
        const status = napi.c.napi_add_env_cleanup_hook(env.handle, &scratchFree, null);
        std.debug.assert(status == .ok);
        scratch_hooked = true;
    }
    return parser.parse(scratch.allocator(), source, .{
        .source_type = options.source_type,
        .lang = options.lang,
        .preserve_parens = options.preserve_parens,
        .comments = if (options.attach_comments) .both else .flat,
        .tokens = options.tokens,
    });
}

fn scratchReset() void {
    _ = scratch.reset(.{ .retain_with_limit = scratch_retained_bytes_max });
}

fn scratchFree(_: ?*anyopaque) callconv(.c) void {
    _ = scratch.reset(.free_all);
    scratch_hooked = false;
}

comptime {
    napi.module(@This());
}
