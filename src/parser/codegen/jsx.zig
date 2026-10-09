const std = @import("std");
const assert = std.debug.assert;
const utils = @import("utils.zig");
const TokenTag = @import("../token.zig").TokenTag;

const entities = @import("util").XHTMLEntities.code_points;

pub fn isFactoryName(name: []const u8) bool {
    assert(name.len <= std.math.maxInt(u32));
    var parts = std.mem.splitScalar(u8, name, '.');
    const root = parts.next().?;
    assert(root.len <= name.len);
    if (!utils.isIdentifierName(root)) return false;
    inline for (comptime std.meta.tags(TokenTag)) |tag| {
        if (comptime tag != .this and (tag.isReserved() or tag == .await)) {
            if (std.mem.eql(u8, root, tag.toString().?)) return false;
        }
    }
    while (parts.next()) |part| {
        if (!utils.isIdentifierName(part)) return false;
    }
    return true;
}

pub fn isEmptyText(raw: []const u8) bool {
    assert(raw.len <= std.math.maxInt(u32));
    if (raw.len == 0) return true;
    if (std.mem.indexOfAny(u8, raw, "\r\n") == null) return false;
    const trimmed = std.mem.trim(u8, raw, " \t\r\n");
    assert(trimmed.len <= raw.len);
    return trimmed.len == 0;
}

pub fn text(
    allocator: std.mem.Allocator,
    scratch: *std.ArrayList(u8),
    raw: []const u8,
    normalize: bool,
) error{OutOfMemory}![]const u8 {
    assert(raw.len <= std.math.maxInt(u32));
    if (std.mem.indexOfAny(u8, raw, if (normalize) "&\t\r\n" else "&") == null) return raw;
    scratch.clearRetainingCapacity();
    try scratch.ensureTotalCapacity(allocator, raw.len);
    assert(scratch.capacity >= raw.len);
    if (normalize) {
        var start: usize = 0;
        while (start < raw.len) {
            var end = start;
            while (end < raw.len and raw[end] != '\r' and raw[end] != '\n') : (end += 1) {}
            var left = start;
            var right = end;
            if (start != 0) {
                while (left < right and (raw[left] == ' ' or raw[left] == '\t')) : (left += 1) {}
            }
            if (end < raw.len) {
                while (right > left) {
                    if (raw[right - 1] != ' ' and raw[right - 1] != '\t') break;
                    right -= 1;
                }
            }
            if (left < right) {
                if (scratch.items.len != 0) scratch.appendAssumeCapacity(' ');
                for (raw[left..right]) |c| scratch.appendAssumeCapacity(if (c == '\t') ' ' else c);
            }
            start = end;
            if (start < raw.len and raw[start] == '\r') start += 1;
            if (start < raw.len and raw[start] == '\n') start += 1;
        }
    } else {
        scratch.appendSliceAssumeCapacity(raw);
    }
    assert(scratch.items.len <= raw.len);
    const input = scratch.items;
    var read: usize = 0;
    var write: usize = 0;
    while (read < input.len) {
        if (input[read] == '&') {
            if (entity(input[read..])) |decoded| {
                var bytes: [4]u8 = undefined;
                const count = std.unicode.utf8Encode(decoded.code, &bytes) catch unreachable;
                @memcpy(input[write..][0..count], bytes[0..count]);
                write += count;
                read += decoded.len;
                continue;
            }
        }
        input[write] = input[read];
        write += 1;
        read += 1;
    }
    assert(write <= read);
    scratch.items.len = write;
    return scratch.items;
}

const entity_name_bytes_max = 8;
comptime {
    for (entities.keys()) |name| assert(name.len <= entity_name_bytes_max);
}

const Entity = struct { code: u21, len: u32 };

fn entity(raw: []const u8) ?Entity {
    assert(raw.len > 0);
    assert(raw[0] == '&');
    if (raw.len < 3) return null;
    var end: usize = 1;
    var code: u32 = 0;
    if (raw[end] == '#') {
        end += 1;
        const hex = raw[end] == 'x';
        if (hex) end += 1;
        const start = end;
        const radix: u32 = if (hex) 16 else 10;
        while (end < raw.len) : (end += 1) {
            const digit = std.fmt.charToDigit(raw[end], @intCast(radix)) catch break;
            code = code * radix + digit;
            if (code > 0x10ffff) return null;
        }
        if (end == start) return null;
    } else {
        // every named entity fits in eight bytes, bounding failed lookups
        while (end < raw.len and end <= entity_name_bytes_max and
            std.ascii.isAlphanumeric(raw[end])) : (end += 1)
        {}
        code = entities.get(raw[1..end]) orelse return null;
    }
    if (end == raw.len or raw[end] != ';') return null;
    if (code >= 0xd800 and code <= 0xdfff) return null;
    return .{ .code = @intCast(code), .len = @intCast(end + 1) };
}

// exercises normalization, malformed entities, and scratch reuse under allocation failures
fn textAllocationTest(allocator: std.mem.Allocator) !void {
    var scratch: std.ArrayList(u8) = .empty;
    defer scratch.deinit(allocator);
    try std.testing.expectEqualStrings(
        " hello world  ",
        try text(allocator, &scratch, " hello\r\n\tworld  ", true),
    );
    try std.testing.expectEqualStrings(
        "&lt; 😀 &#xD800; &#x110000; &#X41; &#x; &#;",
        try text(
            allocator,
            &scratch,
            "&amp;lt; &#x1F600; &#xD800; &#x110000; &#X41; &#x; &#;",
            false,
        ),
    );
    try std.testing.expectEqualStrings("a  ", try text(allocator, &scratch, "\n a &#32; \n", true));
    try std.testing.expectEqualStrings("\n", try text(allocator, &scratch, "&#10;", true));
    try std.testing.expectEqualStrings("<", try text(allocator, &scratch, "&lt;", false));
}

test "JSX text survives allocation failures and reuses scratch" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, textAllocationTest, .{});
}

test "JSX factory names contain only identifier references and property names" {
    for ([_][]const u8{
        "h",              "runtime.h", "this.h", "runtime.default",
        "工厂.h",
        "React.Fragment",
    }) |name| try std.testing.expect(isFactoryName(name));
    for ([_][]const u8{
        "", "factory()", "foo..bar", "h;evil()", "foo[bar]", "class", "await.h",
    }) |name| try std.testing.expect(!isFactoryName(name));
}
