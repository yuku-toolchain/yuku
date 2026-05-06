const std = @import("std");

const Allocator = std.mem.Allocator;

pub const Error = error{OutOfMemory};

pub const no_source: u32 = std.math.maxInt(u32);
pub const no_name: u32 = std.math.maxInt(u32);

/// One source-position mapping. 24 bytes; AoS to match esbuild and oxc.
pub const Mapping = struct {
    generated_line: u32,
    generated_column: u32,
    source: u32 = no_source,
    original_line: u32,
    original_column: u32,
    name: u32 = no_name,
};

/// A source map. Mappings stay structured for easy programmatic access and
/// bundler-side merging. Call `toV3` to serialize to wire form.
pub const SourceMap = struct {
    file: ?[]const u8 = null,
    source_root: ?[]const u8 = null,
    sources: []const []const u8,
    sources_content: ?[]const ?[]const u8 = null,
    names: []const []const u8,
    mappings: []const Mapping,

    pub fn deinit(self: SourceMap, allocator: Allocator) void {
        for (self.sources) |s| allocator.free(s);
        allocator.free(self.sources);
        if (self.sources_content) |c| allocator.free(c);
        for (self.names) |n| allocator.free(n);
        allocator.free(self.names);
        allocator.free(self.mappings);
    }

    /// Returns the mapping at or before `(line, column)`, or null. Useful for
    /// resolving stack-trace positions back to source.
    pub fn find(self: SourceMap, line: u32, column: u32) ?Mapping {
        var lo: usize = 0;
        var hi: usize = self.mappings.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            const m = self.mappings[mid];
            if (m.generated_line < line or (m.generated_line == line and m.generated_column <= column)) {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        if (lo == 0) return null;
        const m = self.mappings[lo - 1];
        return if (m.generated_line == line) m else null;
    }

    /// Encodes to v3 wire form. The `mappings` field becomes a VLQ string;
    /// every other field aliases this `SourceMap`.
    pub fn toV3(self: SourceMap, allocator: Allocator) Error!SourceMapV3 {
        return .{
            .file = self.file,
            .source_root = self.source_root,
            .sources = self.sources,
            .sources_content = self.sources_content,
            .names = self.names,
            .mappings = try encodeMappings(self.mappings, allocator),
        };
    }
};

/// V3 wire form: `mappings` is the VLQ-encoded string. What crosses the FFI
/// or gets written to a `.map` file.
pub const SourceMapV3 = struct {
    version: u8 = 3,
    file: ?[]const u8 = null,
    source_root: ?[]const u8 = null,
    sources: []const []const u8,
    sources_content: ?[]const ?[]const u8 = null,
    names: []const []const u8,
    mappings: []const u8,

    /// Frees only the `mappings` buffer. All other fields alias the
    /// originating `SourceMap` and are freed by `SourceMap.deinit`.
    pub fn deinit(self: SourceMapV3, allocator: Allocator) void {
        allocator.free(self.mappings);
    }
};

/// Builds a `SourceMap` during codegen. Captures pending mappings as byte
/// offsets and resolves them to `(line, column)` form in `finish` via a
/// single SIMD-accelerated newline scan.
///
/// Single-source by design. Multi-source merges (bundler) belong in a
/// separate `ConcatBuilder` over finished `SourceMap`s.
pub const Builder = struct {
    allocator: Allocator,
    source_filename: []const u8,
    source_content: ?[]const u8,
    names: std.ArrayList([]const u8) = .empty,
    name_intern: std.StringHashMapUnmanaged(u32) = .empty,
    pending: std.ArrayList(PendingMapping) = .empty,

    /// 12 bytes: just the data the SIMD finalize pass needs.
    const PendingMapping = struct {
        generated_offset: u32,
        original_offset: u32,
        name: u32,
    };

    /// `expected_mappings` is a capacity hint. The pending list grows as needed.
    pub fn init(
        allocator: Allocator,
        source_filename: []const u8,
        source_content: ?[]const u8,
        expected_mappings: usize,
    ) Error!Builder {
        var b = Builder{
            .allocator = allocator,
            .source_filename = source_filename,
            .source_content = source_content,
        };
        try b.pending.ensureTotalCapacity(allocator, expected_mappings);
        return b;
    }

    pub fn deinit(self: *Builder) void {
        for (self.names.items) |n| self.allocator.free(n);
        self.names.deinit(self.allocator);
        self.name_intern.deinit(self.allocator);
        self.pending.deinit(self.allocator);
    }

    /// Interns `name`, returns its index for `addMapping`.
    pub fn addName(self: *Builder, name: []const u8) Error!u32 {
        const gop = try self.name_intern.getOrPut(self.allocator, name);
        if (!gop.found_existing) {
            const dup = try self.allocator.dupe(u8, name);
            errdefer self.allocator.free(dup);
            gop.key_ptr.* = dup;
            gop.value_ptr.* = @intCast(self.names.items.len);
            try self.names.append(self.allocator, dup);
        }
        return gop.value_ptr.*;
    }

    /// Records a mapping. Mappings sharing the same `generated_offset` are
    /// deduplicated in place: the latest write wins, which keeps the
    /// deepest (most specific) AST node's source position.
    pub fn addMapping(self: *Builder, generated_offset: u32, original_offset: u32, name: u32) Error!void {
        const new = PendingMapping{
            .generated_offset = generated_offset,
            .original_offset = original_offset,
            .name = name,
        };
        if (self.pending.items.len > 0) {
            const last = &self.pending.items[self.pending.items.len - 1];
            if (last.generated_offset == generated_offset) {
                last.* = new;
                return;
            }
        }
        try self.pending.append(self.allocator, new);
    }

    /// Resolves all pending mappings to `(line, column)` and returns the
    /// finished `SourceMap`. Builder state is consumed.
    pub fn finish(self: *Builder, output: []const u8) Error!SourceMap {
        const source = self.source_content orelse "";
        const gen_starts = try computeLineStarts(output, self.allocator);
        defer self.allocator.free(gen_starts);
        const src_starts = try computeLineStarts(source, self.allocator);
        defer self.allocator.free(src_starts);

        const mappings = try self.allocator.alloc(Mapping, self.pending.items.len);
        errdefer self.allocator.free(mappings);

        // gen offsets are monotonic by construction; src offsets can jump
        // backward in transformed ASTs, so reset the cursor on a regression.
        var gen_cursor: usize = 0;
        var src_cursor: usize = 0;
        var prev_src_off: u32 = 0;
        for (self.pending.items, mappings) |p, *m| {
            while (gen_cursor + 1 < gen_starts.len and gen_starts[gen_cursor + 1] <= p.generated_offset)
                gen_cursor += 1;
            if (p.original_offset < prev_src_off) src_cursor = 0;
            while (src_cursor + 1 < src_starts.len and src_starts[src_cursor + 1] <= p.original_offset)
                src_cursor += 1;
            prev_src_off = p.original_offset;
            m.* = .{
                .generated_line = @intCast(gen_cursor),
                .generated_column = p.generated_offset - gen_starts[gen_cursor],
                .source = 0,
                .original_line = @intCast(src_cursor),
                .original_column = p.original_offset - src_starts[src_cursor],
                .name = p.name,
            };
        }

        const sources = try self.allocator.alloc([]const u8, 1);
        errdefer self.allocator.free(sources);
        sources[0] = try self.allocator.dupe(u8, self.source_filename);

        var sources_content: ?[]const ?[]const u8 = null;
        if (self.source_content) |c| {
            const sc = try self.allocator.alloc(?[]const u8, 1);
            sc[0] = c;
            sources_content = sc;
        }

        return .{
            .sources = sources,
            .sources_content = sources_content,
            .names = try self.names.toOwnedSlice(self.allocator),
            .mappings = mappings,
        };
    }
};

// === internals ===

fn computeLineStarts(bytes: []const u8, allocator: Allocator) Error![]u32 {
    var starts: std.ArrayList(u32) = .empty;
    errdefer starts.deinit(allocator);
    try starts.append(allocator, 0);
    var i: usize = 0;
    while (std.mem.indexOfScalarPos(u8, bytes, i, '\n')) |pos| {
        try starts.append(allocator, @intCast(pos + 1));
        i = pos + 1;
    }
    return starts.toOwnedSlice(allocator);
}

const base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

fn encodeVLQ(out: *std.ArrayList(u8), allocator: Allocator, value: i32) Error!void {
    var v: u32 = if (value < 0)
        (@as(u32, @intCast(-@as(i64, value))) << 1) | 1
    else
        @as(u32, @intCast(value)) << 1;
    while (true) {
        var digit: u32 = v & 0b11111;
        v >>= 5;
        if (v != 0) digit |= 0b100000;
        try out.append(allocator, base64[digit]);
        if (v == 0) return;
    }
}

fn encodeMappings(mappings: []const Mapping, allocator: Allocator) Error![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    // ~6 base64 chars per mapping is a generous average, undershoots are cheap to grow
    try out.ensureTotalCapacity(allocator, mappings.len * 6);

    var line: u32 = 0;
    var col: i32 = 0;
    var src: i32 = 0;
    var orig_line: i32 = 0;
    var orig_col: i32 = 0;
    var name: i32 = 0;
    var first = true;

    for (mappings) |m| {
        while (line < m.generated_line) {
            try out.append(allocator, ';');
            line += 1;
            col = 0;
            first = true;
        }
        if (!first) try out.append(allocator, ',');
        first = false;

        try encodeVLQ(&out, allocator, @as(i32, @intCast(m.generated_column)) - col);
        col = @intCast(m.generated_column);

        if (m.source != no_source) {
            try encodeVLQ(&out, allocator, @as(i32, @intCast(m.source)) - src);
            src = @intCast(m.source);
            try encodeVLQ(&out, allocator, @as(i32, @intCast(m.original_line)) - orig_line);
            orig_line = @intCast(m.original_line);
            try encodeVLQ(&out, allocator, @as(i32, @intCast(m.original_column)) - orig_col);
            orig_col = @intCast(m.original_column);
            if (m.name != no_name) {
                try encodeVLQ(&out, allocator, @as(i32, @intCast(m.name)) - name);
                name = @intCast(m.name);
            }
        }
    }

    return out.toOwnedSlice(allocator);
}
