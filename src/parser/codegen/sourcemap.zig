const std = @import("std");
const util = @import("util");
const spec = @import("spec");
const Simd = util.Simd;

const Allocator = std.mem.Allocator;

pub const SourceMap = struct {
    version: u8 = 3,
    file: ?[]const u8 = null,
    source_root: ?[]const u8 = null,
    sources: []const []const u8,
    sources_content: ?[]const ?[]const u8 = null,
    names: []const []const u8 = &.{},
    mappings: []const u8,

    pub fn deinit(self: SourceMap, allocator: Allocator) void {
        allocator.free(self.sources);
        if (self.sources_content) |sc| allocator.free(sc);
        allocator.free(self.mappings);
    }
};

pub const Options = struct {
    /// Output filename, embedded as the map's `file`.
    file: ?[]const u8 = null,
    /// Source filename, embedded as the single entry of `sources`.
    source_file_name: ?[]const u8 = null,
    /// Prefix embedded as `sourceRoot`.
    source_root: ?[]const u8 = null,
    /// When set, embedded as the single entry of `sourcesContent`.
    sources_content: ?[]const u8 = null,
};

/// A zero-based source position, with `col` counted in UTF-16 code units.
pub const LineCol = struct { line: u32, col: u32 };

pub const State = struct {
    options: Options,
    source: []const u8,
    cursor: SourceCursor = .{},
    out: std.ArrayList(u8) = .empty,
    gen_line: u32 = 0,
    gen_col: u32 = 0,

    pending: ?Segment = null,
    prev: Segment = .{},

    const Segment = struct {
        gen_line: u32 = 0,
        gen_col: u32 = 0,
        orig_line: u32 = 0,
        orig_col: u32 = 0,
    };

    pub fn advance(self: *State, bytes: []const u8) void {
        const len: u32 = @intCast(bytes.len);
        var i: usize = 0;
        while (i < bytes.len) : (i += 16) {
            if (@reduce(.Or, notPlainAscii(Simd.loadAt(bytes, i)))) break;
        } else {
            self.gen_col += len;
            return;
        }
        const start: SourceCursor = .{ .line = self.gen_line, .col = self.gen_col };
        const end = start.forward(bytes, len);
        self.gen_line = end.line;
        self.gen_col = end.col;
    }

    /// Maps the generated position `col_offset` columns ahead to the source `offset`. A later
    /// record at the same position replaces it.
    pub fn record(
        self: *State,
        allocator: Allocator,
        offset: u32,
        col_offset: u32,
    ) Allocator.Error!void {
        const orig = self.cursor.resolve(self.source, offset);
        const segment: Segment = .{
            .gen_line = self.gen_line,
            .gen_col = self.gen_col + col_offset,
            .orig_line = orig.line,
            .orig_col = orig.col,
        };
        if (self.pending) |pending| {
            if (pending.gen_line != segment.gen_line or pending.gen_col != segment.gen_col) {
                try self.flush(allocator, pending);
            }
        }
        self.pending = segment;
    }

    fn flush(self: *State, allocator: Allocator, segment: Segment) Allocator.Error!void {
        const line_gap = segment.gen_line - self.prev.gen_line;
        try self.out.ensureUnusedCapacity(allocator, line_gap + 2 + 3 * 7);
        var dst = self.out.items.ptr + self.out.items.len;
        if (line_gap > 0) {
            @memset(dst[0..line_gap], ';');
            dst += line_gap;
            self.prev.gen_col = 0;
        } else if (self.out.items.len > 0) {
            dst[0] = ',';
            dst += 1;
        }
        dst = writeVlq(dst, segment.gen_col, self.prev.gen_col);
        // the source index delta, always zero with one source
        dst[0] = 'A';
        dst = writeVlq(dst + 1, segment.orig_line, self.prev.orig_line);
        dst = writeVlq(dst, segment.orig_col, self.prev.orig_col);
        self.prev = segment;
        self.out.items.len = dst - self.out.items.ptr;
    }

    pub fn build(self: *State, allocator: Allocator) Allocator.Error!SourceMap {
        if (self.pending) |pending| try self.flush(allocator, pending);
        self.pending = null;
        const mappings = try self.out.toOwnedSlice(allocator);
        errdefer allocator.free(mappings);

        const sources = try allocator.dupe([]const u8, &.{self.options.source_file_name orelse ""});
        errdefer allocator.free(sources);

        const sources_content: ?[]const ?[]const u8 = if (self.options.sources_content) |content|
            try allocator.dupe(?[]const u8, &.{content})
        else
            null;

        return .{
            .file = self.options.file,
            .source_root = self.options.source_root,
            .sources = sources,
            .sources_content = sources_content,
            .mappings = mappings,
        };
    }
};

/// Maps byte offsets in a UTF-8 source to zero-based `(line, col)`, with `col` in UTF-16
/// code units, scanning incrementally from the last position.
pub const SourceCursor = struct {
    byte: u32 = 0,
    line: u32 = 0,
    col: u32 = 0,

    pub fn resolve(self: *SourceCursor, source: []const u8, target: u32) LineCol {
        if (target >= self.byte) {
            self.* = self.forward(source, @intCast(@min(target, source.len)));
        } else {
            while (self.byte > target) {
                self.byte = codePointStart(source, self.byte - 1);
                // a crlf counts once
                const is_break = if (source[self.byte] == '\n')
                    self.byte == 0 or source[self.byte - 1] != '\r'
                else
                    spec.line_terminators.lineBreakLen(source, self.byte) > 0;
                if (is_break) self.line -= 1;
            }
            self.col = 0;
            var at = self.byte;
            while (at > 0) {
                const prev = codePointStart(source, at - 1);
                if (spec.line_terminators.lineBreakLen(source, prev) > 0) break;
                self.col += utf16Width(std.unicode.utf8ByteSequenceLength(source[prev]) catch 1);
                at = prev;
            }
        }
        return .{ .line = self.line, .col = self.col };
    }

    inline fn forward(self: SourceCursor, text: []const u8, to: u32) SourceCursor {
        var at = self;
        while (at.byte < to) {
            const plain_lanes = Simd.firstTrueLane(notPlainAscii(Simd.loadAt(text, at.byte)));
            const plain = @min(plain_lanes, to - at.byte);
            at.byte += plain;
            at.col += plain;
            if (at.byte == to) break;
            if (plain == 16) continue;
            const brk = spec.line_terminators.lineBreakLen(text, at.byte);
            if (brk > 0) {
                at = .{ .byte = at.byte + brk, .line = at.line + 1, .col = 0 };
            } else {
                const len = std.unicode.utf8ByteSequenceLength(text[at.byte]) catch 1;
                at.byte += len;
                at.col += utf16Width(len);
            }
        }
        return at;
    }
};

inline fn notPlainAscii(chunk: Simd.Chunk) @Vector(16, bool) {
    return (chunk >= Simd.splat(0x80)) | (chunk == Simd.splat('\n')) | (chunk == Simd.splat('\r'));
}

fn codePointStart(source: []const u8, at: u32) u32 {
    var start = at;
    while (start > 0 and source[start] & 0xC0 == 0x80) start -= 1;
    return start;
}

// a 4-byte utf-8 code point is a utf-16 surrogate pair
inline fn utf16Width(utf8_len: u8) u32 {
    return if (utf8_len == 4) 2 else 1;
}

const VLQ_CHARS: [64]u8 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".*;

inline fn writeVlq(dst: [*]u8, now: u32, before: u32) [*]u8 {
    const v: i32 = @bitCast(now -% before);
    var bits: u32 = if (v < 0) (@as(u32, @intCast(-v)) << 1) | 1 else @as(u32, @intCast(v)) << 1;
    var p = dst;
    while (true) {
        const digit: u8 = @intCast(bits & 0x1F);
        bits >>= 5;
        p[0] = VLQ_CHARS[if (bits != 0) digit | 0x20 else digit];
        p += 1;
        if (bits == 0) return p;
    }
}

test "a cursor resolves byte offsets past non-ascii text to utf-16 columns" {
    const source = "\"日😀\"; foo;";
    var cursor: SourceCursor = .{};
    try std.testing.expectEqual(LineCol{ .line = 0, .col = 7 }, cursor.resolve(source, 11));
    try std.testing.expectEqual(LineCol{ .line = 0, .col = 0 }, cursor.resolve(source, 0));
    try std.testing.expectEqual(LineCol{ .line = 0, .col = 7 }, cursor.resolve(source, 11));
}
