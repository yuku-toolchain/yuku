const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");
const transfer = @import("transfer.zig");

const FormatOpts = struct {
    format: parser.codegen.Format = .pretty,
    indent: u8 = 2,
    quotes: parser.codegen.Quotes = .double,
};

const Mode = enum { print, strip, minify };

const Options = struct {
    mode: Mode = .print,
    format: FormatOpts = .{},
};

const CodegenResult = struct {
    code: []const u8,
    errors: []const parser.codegen.Diagnostic,
};

pub fn generate(env: napi.Env, buffer: napi.Val, source: ?[]const u8, options: Options) !CodegenResult {
    const allocator = env.allocator();
    const bytes = try buffer.getArrayBufferData(env);

    var tree = transfer.deserializeFromBuf(allocator, bytes, source orelse "") catch return error.DecodeFailed;
    defer tree.deinit();

    const codegen_opts: parser.codegen.Options = .{
        .format = options.format.format,
        .indent = options.format.indent,
        .quotes = options.format.quotes,
    };

    const result = switch (options.mode) {
        .print => try parser.codegen.print(allocator, &tree, codegen_opts),
        .strip => try parser.codegen.strip(allocator, &tree, codegen_opts),
        .minify => try parser.codegen.minify(allocator, &tree, codegen_opts),
    };

    return .{ .code = result.code, .errors = result.errors };
}

comptime {
    napi.module(@This());
}
