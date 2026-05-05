const std = @import("std");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

const t = std.testing;

const FuzzConfig = struct {
    lang: ast.Lang,
    source_type: ast.SourceType,
};

fn fuzzParse(cfg: FuzzConfig, smith: *t.Smith) anyerror!void {
    var buf: [16 * 1024]u8 = undefined;
    const len = smith.slice(&buf);

    var tree = try parser.parse(t.allocator, buf[0..len], .{
        .lang = cfg.lang,
        .source_type = cfg.source_type,
    });
    defer tree.deinit();
}

test "parser fuzz: js module" {
    try t.fuzz(FuzzConfig{ .lang = .js, .source_type = .module }, fuzzParse, .{});
}

test "parser fuzz: js script" {
    try t.fuzz(FuzzConfig{ .lang = .js, .source_type = .script }, fuzzParse, .{});
}

test "parser fuzz: ts module" {
    try t.fuzz(FuzzConfig{ .lang = .ts, .source_type = .module }, fuzzParse, .{});
}

// truncated multi-byte UTF-8 lead byte at end of source caused
// an out-of-bounds read in codePointAt. parser must surface InvalidUtf8 via
// diagnostics, never panic.
test "parser regression: truncated utf8 lead byte at eof" {
    inline for (.{ 0xC3, 0xE2, 0xF0 }) |lead| {
        const src = &[_]u8{lead};
        var tree = try parser.parse(t.allocator, src, .{});
        defer tree.deinit();
    }
}

test "parser fuzz: tsx module" {
    try t.fuzz(FuzzConfig{ .lang = .tsx, .source_type = .module }, fuzzParse, .{});
}

test "parser fuzz: jsx module" {
    try t.fuzz(FuzzConfig{ .lang = .jsx, .source_type = .module }, fuzzParse, .{});
}

test "parser fuzz: dts" {
    try t.fuzz(FuzzConfig{ .lang = .dts, .source_type = .module }, fuzzParse, .{});
}
