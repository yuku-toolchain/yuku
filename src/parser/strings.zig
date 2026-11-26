const std = @import("std");

/// Text in the AST, as offsets into the source or, past its end, into the pool's extra buffer.
pub const String = struct {
    start: u32 = 0,
    end: u32 = 0,

    pub const empty: String = .{};
};

pub const ASTStringPool = struct {
    source: []const u8 = "",
    extra: std.ArrayList(u8) = .empty,
    dedup: DedupMap = .empty,

    const DedupMap = std.HashMapUnmanaged(String, void, TextContext, 80);

    /// Returns the string content for a `String`.
    pub inline fn get(self: *const ASTStringPool, id: String) []const u8 {
        std.debug.assert(id.end >= id.start);
        if (id.start == id.end) return "";
        const src_len: u32 = @intCast(self.source.len);
        if (id.start < src_len) {
            std.debug.assert(id.end <= src_len);
            return self.source[id.start..id.end];
        }
        return self.extra.items[id.start - src_len .. id.end - src_len];
    }

    /// A `String` for a range of the source text.
    pub inline fn sourceSlice(self: *const ASTStringPool, start: u32, end: u32) String {
        std.debug.assert(start <= end);
        std.debug.assert(end <= self.source.len);
        return .{ .start = start, .end = end };
    }

    /// Interns `str`, returning the same `String` for equal text.
    pub fn addString(
        self: *ASTStringPool,
        alloc: std.mem.Allocator,
        str: []const u8,
    ) error{OutOfMemory}!String {
        if (str.len == 0) return .empty;
        std.debug.assert(self.source.len + self.extra.items.len + str.len <= std.math.maxInt(u32));

        const src_len: u32 = @intCast(self.source.len);
        const context: TextContext = .{ .extra = self.extra.items, .src_len = src_len };
        const gop = try self.dedup.getOrPutContextAdapted(alloc, str, context, context);
        if (gop.found_existing) return gop.key_ptr.*;

        const start = src_len + @as(u32, @intCast(self.extra.items.len));
        try self.extra.appendSlice(alloc, str);
        gop.key_ptr.* = .{ .start = start, .end = start + @as(u32, @intCast(str.len)) };
        return gop.key_ptr.*;
    }

    const TextContext = struct {
        extra: []const u8,
        src_len: u32,

        pub fn hash(ctx: TextContext, key: anytype) u64 {
            return std.hash.Wyhash.hash(0, ctx.text(key));
        }

        pub fn eql(ctx: TextContext, a: anytype, b: String) bool {
            return std.mem.eql(u8, ctx.text(a), ctx.text(b));
        }

        fn text(ctx: TextContext, key: anytype) []const u8 {
            if (@TypeOf(key) != String) return key;
            return ctx.extra[key.start - ctx.src_len .. key.end - ctx.src_len];
        }
    };
};
