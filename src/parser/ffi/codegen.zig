const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");
const transfer = @import("transfer.zig");

const Options = struct {
    format: parser.codegen.Format = .pretty,
    indent: u8 = 2,
    quotes: parser.codegen.Quotes = .double,
};

const Result = struct {
    code: []const u8,
    errors: []const parser.codegen.Diagnostic,
};

const Op = enum { print, strip, minify };

fn run(comptime op: Op, env: napi.Env, buffer: napi.Val, source: ?[]const u8, options: Options) !Result {
    const allocator = env.allocator();
    const bytes = try buffer.getArrayBufferData(env);

    var tree = transfer.deserializeFromBuf(allocator, bytes, source orelse "") catch return error.DecodeFailed;
    defer tree.deinit();

    const codegen_opts: parser.codegen.Options = .{
        .format = options.format,
        .indent = options.indent,
        .quotes = options.quotes,
    };

    const result = switch (op) {
        .print => try parser.codegen.print(allocator, &tree, codegen_opts),
        .strip => try parser.codegen.strip(allocator, &tree, codegen_opts),
        .minify => try parser.codegen.minify(allocator, &tree, codegen_opts),
    };
    return .{ .code = result.code, .errors = result.errors };
}

pub fn print(env: napi.Env, buffer: napi.Val, source: ?[]const u8, options: Options) !Result {
    return run(.print, env, buffer, source, options);
}

pub fn strip(env: napi.Env, buffer: napi.Val, source: ?[]const u8, options: Options) !Result {
    return run(.strip, env, buffer, source, options);
}

pub fn minify(env: napi.Env, buffer: napi.Val, source: ?[]const u8, options: Options) !Result {
    return run(.minify, env, buffer, source, options);
}

comptime {
    napi.module(@This());
}
