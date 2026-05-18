const std = @import("std");

const Allocator = std.mem.Allocator;

/// Source Map V3 output.
///
/// All slices are owned by the allocator passed to `State.build`. Call
/// `deinit` with the same allocator to release them.
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
    /// Original source text. Required.
    source: []const u8,
    /// Output filename, embedded as the map's `file`.
    file: ?[]const u8 = null,
    /// Source filename, embedded as the single entry of `sources`.
    source_file_name: ?[]const u8 = null,
    /// Prefix embedded as `sourceRoot`.
    source_root: ?[]const u8 = null,
    /// When true, embeds `source` into `sourcesContent`.
    sources_content: bool = false,
};

const Segment = struct {
    gen_line: u32,
    gen_col: u32,
    orig_line: u32,
    orig_col: u32,
    name_idx: i32, // -1 = no name
};

/// Per-call state. Created by the printer when source maps are enabled,
/// threaded through every emit, finalized via `build`.
pub const State = struct {
    options: Options,
    line_starts: []u32,
    mappings: std.ArrayList(Segment) = .empty,
    names: std.ArrayList([]const u8) = .empty,
    names_dedup: std.StringHashMapUnmanaged(u32) = .empty,
    cur_orig_line: u32 = 0,
    gen_line: u32 = 0,
    gen_col: u32 = 0,

    pub const Snapshot = struct {
        mappings_len: u32,
        gen_line: u32,
        gen_col: u32,
    };

    pub fn init(allocator: Allocator, options: Options) Allocator.Error!State {
        return .{
            .options = options,
            .line_starts = try buildLineStarts(allocator, options.source),
        };
    }

    pub fn deinit(self: *State, allocator: Allocator) void {
        allocator.free(self.line_starts);
        self.mappings.deinit(allocator);
        self.names.deinit(allocator);
        self.names_dedup.deinit(allocator);
    }

    /// Resolves byte offset `pos` (into `options.source`) to a 0-indexed
    /// (line, col). Walks `line_starts` monotonically — O(1) amortized
    /// since the printer visits nodes in source order.
    pub fn locate(self: *State, pos: u32) struct { line: u32, col: u32 } {
        const starts = self.line_starts;
        if (starts.len == 0) return .{ .line = 0, .col = 0 };
        var line = self.cur_orig_line;
        while (line + 1 < starts.len and starts[line + 1] <= pos) line += 1;
        self.cur_orig_line = line;
        return .{ .line = line, .col = pos -% starts[line] };
    }

    /// Returns a stable `names` index for `name`, copying the bytes so the
    /// table outlives the AST it referenced.
    pub fn intern(self: *State, allocator: Allocator, name: []const u8) Allocator.Error!u32 {
        if (self.names_dedup.get(name)) |idx| return idx;
        const owned = try allocator.alloc(u8, name.len);
        @memcpy(owned, name);
        const idx: u32 = @intCast(self.names.items.len);
        try self.names.append(allocator, owned);
        try self.names_dedup.put(allocator, owned, idx);
        return idx;
    }

    /// Advances `gen_line` / `gen_col` over `bytes` just appended to the
    /// output. Called by the printer after every write.
    pub fn advance(self: *State, bytes: []const u8) void {
        var line_inc: u32 = 0;
        var last_nl: ?usize = null;
        for (bytes, 0..) |c, i| {
            if (c == '\n') {
                line_inc += 1;
                last_nl = i;
            }
        }
        if (line_inc > 0) {
            self.gen_line += line_inc;
            self.gen_col = @intCast(bytes.len - last_nl.? - 1);
        } else {
            self.gen_col += @intCast(bytes.len);
        }
    }

    /// Records a mapping at the current generated position. Skips
    /// consecutive duplicates at the same `(gen, orig, name)`.
    pub fn record(
        self: *State,
        allocator: Allocator,
        orig_line: u32,
        orig_col: u32,
        name_idx: i32,
    ) Allocator.Error!void {
        const items = self.mappings.items;
        if (items.len > 0) {
            const last = items[items.len - 1];
            if (last.gen_line == self.gen_line and last.gen_col == self.gen_col and
                last.orig_line == orig_line and last.orig_col == orig_col and
                last.name_idx == name_idx) return;
        }
        try self.mappings.append(allocator, .{
            .gen_line = self.gen_line,
            .gen_col = self.gen_col,
            .orig_line = orig_line,
            .orig_col = orig_col,
            .name_idx = name_idx,
        });
    }

    pub inline fn snapshot(self: *const State) Snapshot {
        return .{
            .mappings_len = @intCast(self.mappings.items.len),
            .gen_line = self.gen_line,
            .gen_col = self.gen_col,
        };
    }

    pub inline fn restore(self: *State, s: Snapshot) void {
        self.mappings.shrinkRetainingCapacity(s.mappings_len);
        self.gen_line = s.gen_line;
        self.gen_col = s.gen_col;
    }

    /// Finalizes the map. Allocates all output buffers via `allocator`;
    /// caller frees via `SourceMap.deinit`.
    pub fn build(self: *const State, allocator: Allocator) Allocator.Error!SourceMap {
        const mappings = try encodeMappings(allocator, self.mappings.items);
        errdefer allocator.free(mappings);

        const sources = try allocator.alloc([]const u8, 1);
        errdefer allocator.free(sources);
        sources[0] = self.options.source_file_name orelse "";

        var sources_content: ?[]const ?[]const u8 = null;
        if (self.options.sources_content) {
            const sc = try allocator.alloc(?[]const u8, 1);
            sc[0] = self.options.source;
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

/// Pre-computes the byte offset where every line begins in `source`.
/// Single SIMD pass.
fn buildLineStarts(allocator: Allocator, source: []const u8) Allocator.Error![]u32 {
    var starts: std.ArrayList(u32) = .empty;
    // ~40 chars/line average; pre-size to skip early growth.
    try starts.ensureTotalCapacity(allocator, @max(8, source.len / 32));
    try starts.append(allocator, 0);

    const Vec = @Vector(16, u8);
    const nl: Vec = @splat('\n');
    var i: usize = 0;
    while (i + 16 <= source.len) : (i += 16) {
        const v: Vec = source[i..][0..16].*;
        const mask = v == nl;
        if (@reduce(.Or, mask)) {
            inline for (0..16) |k| {
                if (mask[k]) try starts.append(allocator, @intCast(i + k + 1));
            }
        }
    }
    for (source[i..], 0..) |c, k| {
        if (c == '\n') try starts.append(allocator, @intCast(i + k + 1));
    }
    return starts.toOwnedSlice(allocator);
}

const VLQ_CHARS: [64]u8 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".*;

/// Writes one signed VLQ integer. Caller ensures ≥ 7 bytes of headroom.
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

/// Encodes segments to the V3 `mappings` string. Writes directly through
/// an over-reserved tail so the inner loop has no growth path.
fn encodeMappings(allocator: Allocator, mappings: []const Segment) Allocator.Error![]u8 {
    // 5 fields × 7 chars + separator ≤ 40 bytes per segment.
    const buf = try allocator.alloc(u8, mappings.len * 40 + 64);
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
        dst[0] = 'A'; // single source: src_idx delta is always 0.
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

    const len = @intFromPtr(dst) - @intFromPtr(buf.ptr);
    return allocator.realloc(buf, len) catch buf[0..len];
}
