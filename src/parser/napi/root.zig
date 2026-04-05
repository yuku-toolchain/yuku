const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");
const transfer = @import("transfer.zig");

const Options = struct {
    source_type: parser.ast.SourceType = .module,
    lang: parser.ast.Lang = .js,
    semantic_errors: bool = false,
};

const alloc = std.heap.c_allocator;

pub fn parse_sync(env: napi.Env, source: []const u8, options: Options) !napi.Val {
    return serializeTree(env, source, options);
}

pub fn parse(env: napi.Env, source: []const u8, options: Options) !napi.Val {
    const owned = try alloc.dupe(u8, source);
    return env.runWorker("parse", ParseWork{
        .source = owned,
        .options = options,
    });
}

const ParseWork = struct {
    source: []const u8,
    options: Options,
    result: ?[]u8 = null,
    result_len: usize = 0,

    pub fn compute(self: *ParseWork) void {
        var tree = parser.parse(alloc, self.source, .{
            .source_type = self.options.source_type,
            .lang = self.options.lang,
        }) catch return;
        defer tree.deinit();

        if (self.options.semantic_errors) {
            _ = parser.semantic.analyze(&tree) catch {};
        }

        const size = transfer.bufferSize(&tree);
        const buf = alloc.alloc(u8, size) catch return;
        self.result_len = transfer.serializeInto(&tree, buf);
        self.result = buf;
    }

    pub fn resolve(self: *ParseWork, env: napi.Env) !napi.Val {
        defer alloc.free(self.source);
        defer if (self.result) |r| alloc.free(r);

        const buf = self.result orelse return error.ParseFailed;
        const ab = try env.createArrayBuffer(self.result_len);
        @memcpy(ab.data[0..self.result_len], buf[0..self.result_len]);
        return ab.val;
    }
};

fn serializeTree(env: napi.Env, source: []const u8, options: Options) !napi.Val {
    var tree = parser.parse(alloc, source, .{
        .source_type = options.source_type,
        .lang = options.lang,
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
