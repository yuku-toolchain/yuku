const std = @import("std");

const Allocator = std.mem.Allocator;

/// Source Map V3 output.
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

/// Configures source map generation.
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

/// A single mapping from a generated position to its original position.
pub const Segment = struct {
    gen_line: u32,
    gen_col: u32,
    orig_line: u32,
    orig_col: u32,
};

/// Source map generation state for one codegen run.
pub const State = struct {
    options: Options,
    // vlq mappings string, encoded as segments are recorded
    out: std.ArrayList(u8) = .empty,
    gen_line: u32 = 0,
    gen_col: u32 = 0,
    // last line resolved by Tree.lineColNear, reused as the next search hint
    line_cursor: u32 = 0,

    // most recent segment, held until one at a different generated position
    // arrives so a deeper node can overwrite it in place
    pending: Segment = undefined,
    has_pending: bool = false,

    // vlq deltas of the last segment flushed to out
    prev_gen_col: i32 = 0,
    prev_orig_line: i32 = 0,
    prev_orig_col: i32 = 0,
    enc_gen_line: i32 = 0,
    first_in_line: bool = true,

    /// State needed to undo a speculative emit.
    pub const Snapshot = struct {
        out_len: u32,
        gen_line: u32,
        gen_col: u32,
        pending: Segment,
        has_pending: bool,
        prev_gen_col: i32,
        prev_orig_line: i32,
        prev_orig_col: i32,
        enc_gen_line: i32,
        first_in_line: bool,
    };

    pub fn init(options: Options) State {
        return .{ .options = options };
    }

    pub fn deinit(self: *State, allocator: Allocator) void {
        self.out.deinit(allocator);
    }

    /// Advances the generated position over `bytes` just written, counting
    /// columns in UTF-16 code units.
    pub fn advance(self: *State, bytes: []const u8) void {
        if (isPlainAscii(bytes)) {
            self.gen_col += @intCast(bytes.len);
            return;
        }

        var i: usize = 0;
        var col_inc: u32 = 0;
        var saw_nl = false;
        while (i < bytes.len) : (i += 1) {
            const b = bytes[i];
            if (b == '\n') {
                self.gen_line += 1;
                col_inc = 0;
                saw_nl = true;
                continue;
            }
            const n = std.unicode.utf8ByteSequenceLength(b) catch continue;
            col_inc += if (n == 4) @as(u32, 2) else 1;
            i += n - 1;
        }
        self.gen_col = if (saw_nl) col_inc else self.gen_col + col_inc;
    }

    // true when every byte is below 0x80 and none is a newline
    inline fn isPlainAscii(bytes: []const u8) bool {
        const N = 16;
        const Vec = @Vector(N, u8);
        const hi: Vec = @splat(0x80);
        const nl: Vec = @splat('\n');
        var i: usize = 0;
        while (i + N <= bytes.len) : (i += N) {
            const chunk: Vec = bytes[i..][0..N].*;
            if (@reduce(.Or, chunk >= hi) or @reduce(.Or, chunk == nl)) return false;
        }
        while (i < bytes.len) : (i += 1) {
            if (bytes[i] >= 0x80 or bytes[i] == '\n') return false;
        }
        return true;
    }

    /// Records a mapping at the current generated position. A later record at
    /// the same generated position replaces this one.
    pub fn record(self: *State, allocator: Allocator, orig_line: u32, orig_col: u32) Allocator.Error!void {
        if (self.has_pending and
            self.pending.gen_line == self.gen_line and
            self.pending.gen_col == self.gen_col)
        {
            self.pending.orig_line = orig_line;
            self.pending.orig_col = orig_col;
            return;
        }
        if (self.has_pending) try self.flush(allocator);
        self.pending = .{
            .gen_line = self.gen_line,
            .gen_col = self.gen_col,
            .orig_line = orig_line,
            .orig_col = orig_col,
        };
        self.has_pending = true;
    }

    // encodes the pending segment into out and folds it into the running deltas
    fn flush(self: *State, allocator: Allocator) Allocator.Error!void {
        const seg = self.pending;
        const gen_line: i32 = @intCast(seg.gen_line);
        const line_gap: usize = @intCast(gen_line - self.enc_gen_line);

        const needed = line_gap + 2 + 3 * 7;
        if (self.out.capacity - self.out.items.len < needed) {
            try self.out.ensureTotalCapacity(allocator, self.out.items.len + needed);
        }
        var dst: [*]u8 = self.out.items.ptr + self.out.items.len;
        const base = dst;

        if (gen_line > self.enc_gen_line) {
            var gap = line_gap;
            while (gap > 0) : (gap -= 1) {
                dst[0] = ';';
                dst += 1;
            }
            self.enc_gen_line = gen_line;
            self.prev_gen_col = 0;
            self.first_in_line = true;
        }
        if (!self.first_in_line) {
            dst[0] = ',';
            dst += 1;
        }
        self.first_in_line = false;

        const gen_col: i32 = @intCast(seg.gen_col);
        dst = writeVlq(dst, gen_col - self.prev_gen_col);
        self.prev_gen_col = gen_col;
        // single source, so the source index delta is always zero
        dst[0] = 'A';
        dst += 1;
        const orig_line: i32 = @intCast(seg.orig_line);
        dst = writeVlq(dst, orig_line - self.prev_orig_line);
        self.prev_orig_line = orig_line;
        const orig_col: i32 = @intCast(seg.orig_col);
        dst = writeVlq(dst, orig_col - self.prev_orig_col);
        self.prev_orig_col = orig_col;

        self.out.items.len += @intFromPtr(dst) - @intFromPtr(base);
    }

    /// Returns the most recently recorded segment, or null if none.
    pub fn lastMapping(self: *const State) ?Segment {
        return if (self.has_pending) self.pending else null;
    }

    pub fn snapshot(self: *const State) Snapshot {
        return .{
            .out_len = @intCast(self.out.items.len),
            .gen_line = self.gen_line,
            .gen_col = self.gen_col,
            .pending = self.pending,
            .has_pending = self.has_pending,
            .prev_gen_col = self.prev_gen_col,
            .prev_orig_line = self.prev_orig_line,
            .prev_orig_col = self.prev_orig_col,
            .enc_gen_line = self.enc_gen_line,
            .first_in_line = self.first_in_line,
        };
    }

    pub fn restore(self: *State, s: Snapshot) void {
        std.debug.assert(s.out_len <= self.out.items.len);
        self.out.shrinkRetainingCapacity(s.out_len);
        self.gen_line = s.gen_line;
        self.gen_col = s.gen_col;
        self.pending = s.pending;
        self.has_pending = s.has_pending;
        self.prev_gen_col = s.prev_gen_col;
        self.prev_orig_line = s.prev_orig_line;
        self.prev_orig_col = s.prev_orig_col;
        self.enc_gen_line = s.enc_gen_line;
        self.first_in_line = s.first_in_line;
    }

    /// Finalizes the map. Output buffers are owned by `allocator` and freed by
    /// `SourceMap.deinit`.
    pub fn build(self: *State, allocator: Allocator) Allocator.Error!SourceMap {
        if (self.has_pending) {
            try self.flush(allocator);
            self.has_pending = false;
        }
        const mappings = try self.out.toOwnedSlice(allocator);
        errdefer allocator.free(mappings);

        const sources = try allocator.alloc([]const u8, 1);
        errdefer allocator.free(sources);
        sources[0] = self.options.source_file_name orelse "";

        var sources_content: ?[]const ?[]const u8 = null;
        if (self.options.sources_content) |content| {
            const sc = try allocator.alloc(?[]const u8, 1);
            sc[0] = content;
            sources_content = sc;
        }

        return .{
            .file = self.options.file,
            .source_root = self.options.source_root,
            .sources = sources,
            .sources_content = sources_content,
            .mappings = mappings,
        };
    }
};

const VLQ_CHARS: [64]u8 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".*;

// writes one signed vlq integer. caller ensures at least 7 bytes of headroom.
inline fn writeVlq(dst: [*]u8, v: i32) [*]u8 {
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
