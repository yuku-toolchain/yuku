const napi = @import("napi-zig");
const parser = @import("parser");

pub const SourceType = enum {
    script,
    module,
};

pub const Lang = enum {
    js,
    ts,
    jsx,
    tsx,
    dts,
};

pub const Options = struct {
    source_type: SourceType = .module,
    lang: Lang = .js,
};

pub const Diagnostic = struct {
    message: []const u8,
    start: u32,
    end: u32,
};

pub const ParseResult = struct {
    valid: bool,
    node_count: u32,
    diagnostics: []const Diagnostic,
};

/// parse JavaScript/TypeScript source and return result info.
pub fn parse(source: []const u8, options: Options) ParseResult {
    const st: parser.ast.SourceType = switch (options.source_type) {
        .script => .script,
        .module => .module,
    };

    const lang: parser.ast.Lang = switch (options.lang) {
        .js => .js,
        .ts => .ts,
        .jsx => .jsx,
        .tsx => .tsx,
        .dts => .dts,
    };

    var tree = parser.parse(std.heap.c_allocator, source, .{
        .source_type = st,
        .lang = lang,
    }) catch {
        return .{
            .valid = false,
            .node_count = 0,
            .diagnostics = &.{},
        };
    };
    defer tree.deinit();

    return .{
        .valid = !tree.hasErrors(),
        .node_count = @intCast(tree.nodes.len),
        .diagnostics = &.{},
    };
}

/// return the parser version.
pub fn version() []const u8 {
    return "0.1.0";
}

const std = @import("std");

comptime {
    napi.module(@This());
}
