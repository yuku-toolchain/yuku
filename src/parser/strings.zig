const std = @import("std");

/// A string in the AST, referencing either the source text or the extra buffer.
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
        if (id.start == id.end) return "";
        const src_len: u32 = @intCast(self.source.len);
        if (id.start < src_len) return self.source[id.start..id.end];
        return self.extra.items[id.start - src_len .. id.end - src_len];
    }

    /// Returns a `String` referencing a range in the original source.
    pub inline fn sourceSlice(_: *const ASTStringPool, start: u32, end: u32) String {
        return .{ .start = start, .end = end };
    }

    pub fn ensureCapacity(self: *ASTStringPool, alloc: std.mem.Allocator, bytes: u32, entries: u32) error{OutOfMemory}!void {
        try self.extra.ensureTotalCapacity(alloc, bytes);
        try self.dedup.ensureTotalCapacityContext(alloc, entries, MapCtx{
            .extra = self.extra.items,
            .src_len = @intCast(self.source.len),
        });
    }

    /// Interns a string into the extra buffer with deduplication.
    /// Used for programmatic AST building.
    pub fn addString(self: *ASTStringPool, alloc: std.mem.Allocator, str: []const u8) error{OutOfMemory}!String {
        if (str.len == 0) return .empty;

        const extra = self.extra.items;
        const src_len: u32 = @intCast(self.source.len);
        const gop = try self.dedup.getOrPutContextAdapted(
            alloc, str,
            AdaptedCtx{ .extra = extra, .src_len = src_len },
            MapCtx{ .extra = extra, .src_len = src_len },
        );
        if (gop.found_existing) return gop.key_ptr.*;

        const start: u32 = src_len + @as(u32, @intCast(extra.len));
        try self.extra.appendSlice(alloc, str);
        const id = String{ .start = start, .end = src_len + @as(u32, @intCast(self.extra.items.len)) };
        gop.key_ptr.* = id;
        return id;
    }

    // resolves a String handle to bytes in the extra buffer.
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
            return std.mem.eql(u8, resolve(ctx.extra, ctx.src_len, a), resolve(ctx.extra, ctx.src_len, b));
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
