const std = @import("std");

/// A string in the AST, referencing either the source text or the extra buffer.
pub const String = struct {
    start: u32 = 0,
    end: u32 = 0,

    pub const empty: String = .{};
};

/// String storage for the AST. Source-range strings reference the original
/// source text directly. Additional strings are interned into the extra buffer via `addString()`,
/// which deduplicates, identical strings share the same `String`.
pub const ASTStringPool = struct {
    source: []const u8 = "",
    extra: std.ArrayList(u8) = .empty,
    intern_map: InternMap = .empty,

    const InternMap = std.HashMapUnmanaged(String, void, InternCtx, 80);

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

    /// Interns a string into the extra buffer with deduplication.
    /// Used for programmatic AST building.
    pub fn addString(self: *ASTStringPool, alloc: std.mem.Allocator, str: []const u8) error{OutOfMemory}!String {
        if (str.len == 0) return .empty;

        const src_len: u32 = @intCast(self.source.len);
        const gop = try self.intern_map.getOrPutContextAdapted(
            alloc, str,
            SliceCtx{ .extra = self.extra.items, .src_len = src_len },
            InternCtx{ .extra = self.extra.items, .src_len = src_len },
        );
        if (gop.found_existing) return gop.key_ptr.*;

        const start: u32 = src_len + @as(u32, @intCast(self.extra.items.len));
        try self.extra.appendSlice(alloc, str);
        const end: u32 = src_len + @as(u32, @intCast(self.extra.items.len));
        const id = String{ .start = start, .end = end };
        gop.key_ptr.* = id;
        return id;
    }

    const InternCtx = struct {
        extra: []const u8,
        src_len: u32 = 0,
        pub fn hash(self: @This(), id: String) u64 {
            return std.hash.Wyhash.hash(0, self.extra[id.start - self.src_len .. id.end - self.src_len]);
        }
        pub fn eql(self: @This(), a: String, b: String) bool {
            return std.mem.eql(u8, self.extra[a.start - self.src_len .. a.end - self.src_len], self.extra[b.start - self.src_len .. b.end - self.src_len]);
        }
    };

    const SliceCtx = struct {
        extra: []const u8,
        src_len: u32 = 0,
        pub fn hash(_: @This(), text: []const u8) u64 {
            return std.hash.Wyhash.hash(0, text);
        }
        pub fn eql(self: @This(), text: []const u8, id: String) bool {
            return std.mem.eql(u8, text, self.extra[id.start - self.src_len .. id.end - self.src_len]);
        }
    };
};
