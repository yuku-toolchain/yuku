const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");

const Options = struct {
    source_type: parser.ast.SourceType = .module,
    lang: parser.ast.Lang = .ts,
    format: parser.codegen.Format = .pretty,
    indent: u8 = 2,
    quotes: parser.codegen.Quotes = .double,
    final_newline: bool = true,
};

const StripResult = struct {
    code: []const u8,
    errors: []const parser.codegen.Diagnostic,
};

pub fn strip(env: napi.Env, source: []const u8, options: Options) !StripResult {
    const allocator = env.allocator();

    var tree = parser.parse(allocator, source, .{
        .source_type = options.source_type,
        .lang = options.lang,
    }) catch return error.ParseFailed;
    defer tree.deinit();

    const result = try parser.codegen.strip(allocator, &tree, .{
        .format = options.format,
        .indent = options.indent,
        .quotes = options.quotes,
        .final_newline = options.final_newline,
    });

    return .{ .code = result.code, .errors = result.errors };
}

comptime {
    napi.module(@This());
}
