const std = @import("std");
const util = @import("util");
const TokenTag = @import("token.zig").TokenTag;
const keywordTag = @import("token.zig").keywordTag;

const UnicodeId = util.UnicodeId;

/// Whether `code_point` can start an identifier, JSX ones included.
pub fn isIdentifierStart(code_point: u21) bool {
    if (code_point < latin1_start.len) return latin1_start[code_point];
    return UnicodeId.canStartId(code_point);
}

/// Whether `code_point` can continue an identifier.
pub fn isIdentifierChar(code_point: u21) bool {
    if (code_point < latin1_char.len) return latin1_char[code_point];
    return UnicodeId.canContinueId(code_point);
}

/// Whether `code_point` can continue a JSX identifier, which also takes `-`.
pub fn isJSXIdentifierChar(code_point: u21) bool {
    if (code_point == '-') return true;
    return isIdentifierChar(code_point);
}

/// Whether `name` is an IdentifierName, reserved words included.
pub fn isIdentifierName(name: []const u8) bool {
    if (name.len == 0) return false;
    var index: usize = 0;
    while (index < name.len) {
        const code_point = util.Utf.codePointAt(name, index) catch return false;
        const valid = if (index == 0)
            isIdentifierStart(code_point.value)
        else
            isIdentifierChar(code_point.value);
        if (!valid) return false;
        std.debug.assert(code_point.len > 0);
        index += code_point.len;
    }
    std.debug.assert(index == name.len);
    return true;
}

/// The token tag of the keyword `name`, contextual keywords included.
pub fn keyword(name: []const u8) ?TokenTag {
    const tag = keywordTag(name);
    std.debug.assert(tag.isIdentifierLike());
    if (tag.isKeyword()) {
        std.debug.assert(std.mem.eql(u8, tag.toString().?, name));
        return tag;
    }
    return null;
}

/// Whether `name` is an identifier that no code reserves.
pub fn isValidIdentifier(name: []const u8) bool {
    if (keyword(name)) |tag| {
        std.debug.assert(isIdentifierName(name));
        if (tag.isReserved()) return false;
        return tag != .await;
    }
    return isIdentifierName(name);
}

pub const lineBreakLen = util.Utf.lineBreakLen;
pub const decodeStringEscapes = util.Utf.decodeStringEscapes;
pub const decodeJSXEntities = util.Utf.decodeJSXEntities;

const latin1_start: [256]bool = blk: {
    @setEvalBranchQuota(10_000);
    var table: [256]bool = undefined;
    for (&table, 0..) |*entry, c| entry.* = c == '$' or c == '_' or UnicodeId.canStartId(c);
    break :blk table;
};

const latin1_char: [256]bool = blk: {
    @setEvalBranchQuota(10_000);
    var table: [256]bool = undefined;
    for (&table, 0..) |*entry, c| entry.* = c == '$' or UnicodeId.canContinueId(c);
    break :blk table;
};

comptime {
    std.debug.assert(!UnicodeId.canStartId(std.math.maxInt(u21)));
    std.debug.assert(!UnicodeId.canContinueId(std.math.maxInt(u21)));
    std.debug.assert(UnicodeId.canContinueId(0x200C));
    std.debug.assert(UnicodeId.canContinueId(0x200D));
}

const testing = std.testing;

test "identifier names" {
    for ([_][]const u8{ "a", "$", "_", "$_a1", "café", "\u{1D49C}", "a\u{200C}b" }) |name| {
        try testing.expect(isIdentifierName(name));
    }
    for ([_][]const u8{ "", "1a", "a-b", "\u{200C}a", "\\u0061", "\xFF", "\xED\xA0\x80" }) |name| {
        try testing.expect(!isIdentifierName(name));
    }
    try testing.expect(isJSXIdentifierChar('-'));
    try testing.expect(!isIdentifierChar('-'));
    var code_point: u21 = 0;
    while (true) : (code_point += 1) {
        if (isIdentifierStart(code_point)) try testing.expect(isIdentifierChar(code_point));
        if (code_point > 0x10FFFF) try testing.expect(!isIdentifierChar(code_point));
        if (code_point == std.math.maxInt(u21)) break;
    }
}

test "keywords" {
    for (std.enums.values(TokenTag)) |tag| {
        if (tag.isKeyword()) try testing.expectEqual(tag, keyword(tag.toString().?).?);
    }
    for ([_][]const u8{ "", "foo", "Class", "constructor", "accessor" }) |name| {
        try testing.expectEqual(null, keyword(name));
    }
    for ([_][]const u8{ "foo", "async", "of", "eval", "arguments", "constructor" }) |name| {
        try testing.expect(isValidIdentifier(name));
    }
    for ([_][]const u8{ "class", "enum", "null", "let", "yield", "await", "1a" }) |name| {
        try testing.expect(!isValidIdentifier(name));
    }
}
