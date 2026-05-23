const std = @import("std");

const Allocator = std.mem.Allocator;

/// Source Map V3 output.
pub const SourceMap = struct {
    version: u8 = 3,
    file: ?[]const u8 = null,
    source_root: ?[]const u8 = null,
    sources: []const []const u8,
    sources_content: ?[]const ?[]const u8 = null,
    names: []const []const u8,
    /// VLQ-encoded mappings string.
    mappings: []const u8,

    pub fn deinit(self: SourceMap, allocator: Allocator) void {
        allocator.free(self.sources);
        if (self.sources_content) |sc| allocator.free(sc);
        allocator.free(self.names);
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

/// One V3 segment. Positions are in UTF-16 code units.
pub const Segment = struct {
    gen_line: u32,
    gen_col: u32,
    orig_line: u32,
    orig_col: u32,
    /// -1 when the segment carries no name.
    name_idx: i32,
};

/// Per-call state. Created when source maps are enabled, threaded through
/// every emit, finalized via `build`.
pub const State = struct {
    options: Options,
    mappings: std.ArrayList(Segment) = .empty,
    names: std.ArrayList([]const u8) = .empty,
    names_dedup: std.StringHashMapUnmanaged(u32) = .empty,
    gen_line: u32 = 0,
    gen_col: u32 = 0,

    /// Captures enough state for the printer to undo a speculative emit.
    pub const Snapshot = struct {
        mappings_len: u32,
        gen_line: u32,
        gen_col: u32,
    };

    pub fn init(_: Allocator, options: Options) Allocator.Error!State {
        return .{ .options = options };
    }

    pub fn deinit(self: *State, allocator: Allocator) void {
        self.mappings.deinit(allocator);
        self.names.deinit(allocator);
        self.names_dedup.deinit(allocator);
    }

    /// Returns a stable `names` index for `name`, copying the bytes so the
    /// table outlives the AST it referenced.
    pub fn intern(self: *State, allocator: Allocator, name: []const u8) Allocator.Error!u32 {
        if (self.names_dedup.get(name)) |idx| return idx;
        try self.names.ensureUnusedCapacity(allocator, 1);
        try self.names_dedup.ensureUnusedCapacity(allocator, 1);
        const owned = try allocator.alloc(u8, name.len);
        @memcpy(owned, name);
        const idx: u32 = @intCast(self.names.items.len);
        self.names.appendAssumeCapacity(owned);
        self.names_dedup.putAssumeCapacity(owned, idx);
        return idx;
    }

    /// Advances `gen_line` and `gen_col` over `bytes` just appended to the
    /// output. Columns are counted in UTF-16 code units to match the V3
    /// convention.
    pub fn advance(self: *State, bytes: []const u8) void {
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

    /// Records a mapping at the current generated position. When the
    /// previous segment is at the same generated position it is
    /// overwritten. Because the printer walks parents before children,
    /// the later call is always for a more deeply nested node, so the
    /// leaf wins at any given output character.
    pub fn record(
        self: *State,
        allocator: Allocator,
        orig_line: u32,
        orig_col: u32,
        name_idx: i32,
    ) Allocator.Error!void {
        const items = self.mappings.items;
        if (items.len > 0) {
            const last = &items[items.len - 1];
            if (last.gen_line == self.gen_line and last.gen_col == self.gen_col) {
                last.orig_line = orig_line;
                last.orig_col = orig_col;
                last.name_idx = name_idx;
                return;
            }
        }
        try self.mappings.append(allocator, .{
            .gen_line = self.gen_line,
            .gen_col = self.gen_col,
            .orig_line = orig_line,
            .orig_col = orig_col,
            .name_idx = name_idx,
        });
    }

    /// Returns the most recently recorded mapping, or null if there is none.
    pub fn lastMapping(self: *const State) ?Segment {
        const items = self.mappings.items;
        return if (items.len > 0) items[items.len - 1] else null;
    }

    pub fn snapshot(self: *const State) Snapshot {
        return .{
            .mappings_len = @intCast(self.mappings.items.len),
            .gen_line = self.gen_line,
            .gen_col = self.gen_col,
        };
    }

    pub fn restore(self: *State, s: Snapshot) void {
        std.debug.assert(s.mappings_len <= self.mappings.items.len);
        self.mappings.shrinkRetainingCapacity(s.mappings_len);
        self.gen_line = s.gen_line;
        self.gen_col = s.gen_col;
    }

    /// Finalizes the map. Allocates all output buffers via `allocator`.
    /// The caller frees them via `SourceMap.deinit`.
    pub fn build(self: *const State, allocator: Allocator) Allocator.Error!SourceMap {
        const mappings = try encodeMappings(allocator, self.mappings.items);
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

        const names = try allocator.alloc([]const u8, self.names.items.len);
        @memcpy(names, self.names.items);

        return .{
            .file = self.options.file,
            .source_root = self.options.source_root,
            .sources = sources,
            .sources_content = sources_content,
            .names = names,
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

// encodes segments to the v3 `mappings` string.
fn encodeMappings(allocator: Allocator, mappings: []const Segment) Allocator.Error![]u8 {
    const max_gen_line: usize = if (mappings.len > 0) mappings[mappings.len - 1].gen_line else 0;
    const buf = try allocator.alloc(u8, mappings.len * 40 + max_gen_line + 64);
    errdefer allocator.free(buf);
    var dst: [*]u8 = buf.ptr;

    var prev_gen_line: i32 = 0;
    var prev_gen_col: i32 = 0;
    var prev_orig_line: i32 = 0;
    var prev_orig_col: i32 = 0;
    var prev_name_idx: i32 = 0;
    var first_in_line = true;

    for (mappings) |seg| {
        const gen_line: i32 = @intCast(seg.gen_line);
        if (gen_line > prev_gen_line) {
            var gap = gen_line - prev_gen_line;
            while (gap > 0) : (gap -= 1) {
                dst[0] = ';';
                dst += 1;
            }
            prev_gen_line = gen_line;
            prev_gen_col = 0;
            first_in_line = true;
        }

        if (!first_in_line) {
            dst[0] = ',';
            dst += 1;
        }
        first_in_line = false;

        const gen_col: i32 = @intCast(seg.gen_col);
        dst = writeVlq(dst, gen_col - prev_gen_col);
        prev_gen_col = gen_col;
        // single source, src_idx delta is always 0.
        dst[0] = 'A';
        dst += 1;
        const orig_line: i32 = @intCast(seg.orig_line);
        dst = writeVlq(dst, orig_line - prev_orig_line);
        prev_orig_line = orig_line;
        const orig_col: i32 = @intCast(seg.orig_col);
        dst = writeVlq(dst, orig_col - prev_orig_col);
        prev_orig_col = orig_col;

        if (seg.name_idx >= 0) {
            dst = writeVlq(dst, seg.name_idx - prev_name_idx);
            prev_name_idx = seg.name_idx;
        }
    }

    const written = @intFromPtr(dst) - @intFromPtr(buf.ptr);
    std.debug.assert(written <= buf.len);
    return buf[0..written];
}
