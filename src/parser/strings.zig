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

    const DedupMap = std.HashMapUnmanaged(String, void, MapCtx, 80);

    /// Returns the string content for a `String`.
    pub fn get(self: *const ASTStringPool, id: String) []const u8 {
        std.debug.assert(id.end >= id.start);
        if (id.start == id.end) return "";
        const src_len: u32 = @intCast(self.source.len);
        if (id.start < src_len) {
            std.debug.assert(id.end <= src_len);
            return self.source[id.start..id.end];
        }
        return resolve(self.extra.items, src_len, id);
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

        const extra = self.extra.items;
        const src_len: u32 = @intCast(self.source.len);
        const gop = try self.dedup.getOrPutContextAdapted(
            alloc,
            str,
            AdaptedCtx{ .extra = extra, .src_len = src_len },
            MapCtx{ .extra = extra, .src_len = src_len },
        );
        if (gop.found_existing) return gop.key_ptr.*;

        const start: u32 = src_len + @as(u32, @intCast(extra.len));
        try self.extra.appendSlice(alloc, str);
        const id = String{
            .start = start,
            .end = src_len + @as(u32, @intCast(self.extra.items.len)),
        };
        gop.key_ptr.* = id;
        return id;
    }

    inline fn resolve(extra: []const u8, src_len: u32, id: String) []const u8 {
        return extra[id.start - src_len .. id.end - src_len];
    }

    const MapCtx = struct {
        extra: []const u8,
        src_len: u32,

        pub fn hash(ctx: @This(), id: String) u64 {
            return std.hash.Wyhash.hash(0, resolve(ctx.extra, ctx.src_len, id));
        }
        pub fn eql(ctx: @This(), a: String, b: String) bool {
            return std.mem.eql(
                u8,
                resolve(ctx.extra, ctx.src_len, a),
                resolve(ctx.extra, ctx.src_len, b),
            );
        }
    };

    const AdaptedCtx = struct {
        extra: []const u8,
        src_len: u32,

        pub fn hash(_: @This(), text: []const u8) u64 {
            return std.hash.Wyhash.hash(0, text);
        }
        pub fn eql(ctx: @This(), text: []const u8, id: String) bool {
            return std.mem.eql(u8, text, resolve(ctx.extra, ctx.src_len, id));
        }
    };
};
