const std = @import("std");
const napi = @import("napi-zig");
const parser = @import("parser");
const minifier = @import("minifier");

const MangleOpts = struct {
    enabled: bool = true,
    keep_fnames: bool = false,
    keep_classnames: bool = false,
};

const FormatOpts = struct {
    quotes: parser.codegen.Quotes = .double,
    final_newline: bool = false,
};

const Options = struct {
    source_type: parser.ast.SourceType = .module,
    lang: parser.ast.Lang = .js,
    mangle: MangleOpts = .{},
    format: FormatOpts = .{},
};

const MinifyResult = struct {
    code: []const u8,
    errors: []const parser.codegen.Diagnostic,
};

pub fn minify(env: napi.Env, source: []const u8, options: Options) !MinifyResult {
    const allocator = env.allocator();

    var tree = parser.parse(allocator, source, .{
        .source_type = options.source_type,
        .lang = options.lang,
    }) catch return error.ParseFailed;
    defer tree.deinit();

    const result = try minifier.minify(allocator, &tree, .{
        .mangle = .{
            .enabled = options.mangle.enabled,
            .keep_fnames = options.mangle.keep_fnames,
            .keep_classnames = options.mangle.keep_classnames,
        },
        .format = .{
            .format = .compact,
            .quotes = options.format.quotes,
            .final_newline = options.format.final_newline,
        },
    });

    return .{ .code = result.code, .errors = result.errors };
}

comptime {
    napi.module(@This());
}
