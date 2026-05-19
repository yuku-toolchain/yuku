const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");
const transfer = @import("transfer.zig");

const Options = struct {
    format: parser.codegen.Format = .pretty,
    indent: u8 = 2,
    quotes: parser.codegen.Quotes = .double,
    source_maps: ?parser.codegen.SourceMapOptions = null,
    comments: parser.codegen.Comments = .some,
};

const Result = struct {
    code: []const u8,
    errors: []const parser.codegen.Diagnostic,
    map: ?parser.codegen.SourceMap = null,
};

const Op = enum { print, strip, minify };

fn run(comptime op: Op, env: napi.Env, buffer: napi.Val, options: Options) !Result {
    const allocator = env.allocator();
    const bytes = try buffer.getArrayBufferData(env);

    var tree = transfer.deserializeFromBuf(allocator, bytes, "") catch return error.DecodeFailed;
    defer tree.deinit();

    const result = switch (op) {
        .print => try parser.codegen.print(allocator, &tree, asCodegenOptions(options)),
        .strip => try parser.codegen.strip(allocator, &tree, asCodegenOptions(options)),
        .minify => try parser.codegen.minify(allocator, &tree, asCodegenOptions(options)),
    };
    return .{ .code = result.code, .errors = result.errors, .map = result.map };
}

inline fn asCodegenOptions(o: Options) parser.codegen.Options {
    return .{
        .format = o.format,
        .indent = o.indent,
        .quotes = o.quotes,
        .source_maps = o.source_maps,
        .comments = o.comments,
    };
}

pub fn print(env: napi.Env, buffer: napi.Val, options: Options) !Result {
    return run(.print, env, buffer, options);
}

pub fn strip(env: napi.Env, buffer: napi.Val, options: Options) !Result {
    return run(.strip, env, buffer, options);
}

pub fn minify(env: napi.Env, buffer: napi.Val, options: Options) !Result {
    return run(.minify, env, buffer, options);
}

comptime {
    napi.module(@This());
}
