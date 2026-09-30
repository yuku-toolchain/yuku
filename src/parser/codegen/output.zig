const std = @import("std");
const Simd = @import("util").Simd;
const ast = @import("../ast.zig");
const sourcemap = @import("sourcemap.zig");
const utils = @import("utils.zig");

const source_maps = @import("codegen_options").source_maps;

const Allocator = std.mem.Allocator;
const Error = Allocator.Error;

// where a leading `{`, `function`, `class`, or `let[` would misparse as a block or declaration
const Lead = enum { none, stmt, arrow, export_default };

pub const Output = struct {
    allocator: Allocator,
    code: std.ArrayList(u8) = .empty,
    pretty: bool,
    map: ?sourcemap.State = null,
    map_start: ?u32 = null,
    held_spaces: u32 = 0,
    held_literal: bool = false,
    bare_integer_end: usize = std.math.maxInt(usize),
    lead: Lead = .none,

    const Text = enum { token, literal, comment };

    pub const Capacity = struct { code: usize, map: usize };

    pub fn init(
        allocator: Allocator,
        pretty: bool,
        capacity: Capacity,
        map_options: ?sourcemap.Options,
    ) Error!Output {
        var out: Output = .{ .allocator = allocator, .pretty = pretty };
        try out.code.ensureTotalCapacity(allocator, capacity.code);
        errdefer out.code.deinit(allocator);
        if (comptime source_maps) if (map_options) |options| {
            if (options.source != null) {
                out.map = sourcemap.State.init(options);
                try out.map.?.out.ensureTotalCapacity(allocator, capacity.map);
            }
        };
        return out;
    }

    pub fn deinit(self: *Output) void {
        self.code.deinit(self.allocator);
        if (comptime source_maps) if (self.map) |*map| map.deinit(self.allocator);
    }

    pub inline fn len(self: *const Output) usize {
        return self.code.items.len;
    }

    pub inline fn lastByte(self: *const Output) u8 {
        if (self.held_spaces > 0) return ' ';
        const items = self.code.items;
        return if (items.len == 0) 0 else items[items.len - 1];
    }

    pub inline fn writeByte(self: *Output, b: u8) Error!void {
        self.dropHeldSpace(b);
        self.lead = .none;
        if (b == ' ') {
            try self.placeMapping(self.held_spaces);
            self.held_spaces += 1;
            self.held_literal = false;
            return;
        }
        if (self.held_spaces != 0) try self.commitSpaces() else try self.separateToken(b);
        try self.placeMapping(0);
        try self.pushByte(b);
        if (comptime source_maps) if (self.map != null) self.advanceMap(&.{b});
    }

    pub inline fn writeStr(self: *Output, s: []const u8) Error!void {
        if (s.len == 0) return;
        try self.writeText(s, .token);
    }

    pub inline fn writeRawByte(self: *Output, b: u8) Error!void {
        try self.writeText(&.{b}, .literal);
    }

    pub inline fn writeRawStr(self: *Output, s: []const u8) Error!void {
        if (s.len == 0) return;
        try self.writeText(s, .literal);
    }

    pub inline fn writeComment(self: *Output, s: []const u8) Error!void {
        if (s.len == 0) return;
        try self.writeText(s, .comment);
    }

    pub inline fn atLineStart(self: *const Output) bool {
        const items = self.code.items;
        return items.len > 0 and items[items.len - 1] == '\n';
    }

    pub inline fn space(self: *Output) Error!void {
        if (self.pretty) try self.writeByte(' ');
    }

    pub fn endLine(self: *Output, indent: u32) Error!void {
        const items = self.code.items;
        if (items.len == 0) return;
        if (items[items.len - 1] != '\n') {
            try self.pushByte('\n');
            if (comptime source_maps) if (self.map) |*map| {
                map.gen_line += 1;
                map.gen_col = 0;
            };
        }
        if (comptime source_maps) if (self.map) |map| std.debug.assert(map.gen_col == 0);
        self.held_spaces = if (self.pretty) indent else 0;
        self.held_literal = false;
    }

    /// Maps the next token to `span`, unless the node is synthetic.
    pub inline fn recordMapping(self: *Output, span: ast.Span) void {
        if (span.start == 0 and span.end == 0) return;
        self.map_start = span.start;
    }

    pub inline fn markBareInteger(self: *Output) void {
        std.debug.assert(self.held_spaces == 0);
        self.bare_integer_end = self.code.items.len;
    }

    inline fn writeText(self: *Output, s: []const u8, comptime kind: Text) Error!void {
        std.debug.assert(s.len > 0);
        if (kind == .token) self.dropHeldSpace(s[0]);
        var end = s.len;
        while (end > 0 and s[end - 1] == ' ') end -= 1;
        if (end > 0) {
            if (self.held_spaces != 0) {
                try self.commitSpaces();
            } else if (kind == .token) {
                try self.separateToken(s[0]);
            }
        }
        // a comment leaves the lead and the mapping to the next token
        if (kind != .comment) {
            self.lead = .none;
            // spaces alone map where the held spaces end
            try self.placeMapping(self.held_spaces);
        }
        if (end > 0) {
            try self.pushSlice(s[0..end]);
            if (comptime source_maps) if (self.map != null) self.advanceMap(s[0..end]);
        }
        if (end < s.len) {
            self.held_spaces += @intCast(s.len - end);
            self.held_literal = kind == .literal;
        }
    }

    // compact mode drops a keyword's lone trailing space before punctuation, never a literal's
    inline fn dropHeldSpace(self: *Output, next: u8) void {
        if (self.pretty) return;
        if (self.held_spaces != 1 or self.held_literal) return;
        const items = self.code.items;
        if (items.len == 0) return;
        if (utils.isIdCont(items[items.len - 1]) and !utils.isIdCont(next)) {
            self.held_spaces = 0;
        }
    }

    inline fn separateToken(self: *Output, next: u8) Error!void {
        std.debug.assert(self.held_spaces == 0);
        const prev = fuses_after[next];
        if (prev == 0) return;
        const items = self.code.items;
        const fuses = if (prev == after_bare_integer)
            self.bare_integer_end == items.len
        else
            items.len > 0 and items[items.len - 1] == prev;
        if (!fuses) return;
        try self.pushByte(' ');
        if (comptime source_maps) if (self.map) |*map| {
            map.gen_col += 1;
        };
    }

    inline fn commitSpaces(self: *Output) Error!void {
        const n = self.held_spaces;
        std.debug.assert(n > 0);
        if (n > space_chunk_len or self.code.capacity - self.code.items.len < space_chunk_len) {
            return self.commitSpaceRun();
        }
        const code_len = self.code.items.len;
        (self.code.items.ptr + code_len)[0..space_chunk_len].* = Simd.splat(' ');
        self.code.items.len = code_len + n;
        self.held_spaces = 0;
        self.held_literal = false;
        if (comptime source_maps) if (self.map) |*map| {
            map.gen_col += n;
        };
    }

    noinline fn commitSpaceRun(self: *Output) Error!void {
        const n = self.held_spaces;
        std.debug.assert(n > 0);
        self.held_spaces = 0;
        self.held_literal = false;
        if (self.code.capacity - self.code.items.len < n + space_chunk_len) {
            try self.grow(n + space_chunk_len);
        }
        const dst = self.code.items.ptr + self.code.items.len;
        var written: usize = 0;
        while (written < n) : (written += space_chunk_len) {
            (dst + written)[0..space_chunk_len].* = Simd.splat(' ');
        }
        self.code.items.len += n;
        if (comptime source_maps) if (self.map) |*map| {
            map.gen_col += n;
        };
    }

    inline fn placeMapping(self: *Output, col_offset: u32) Error!void {
        if (comptime !source_maps) return;
        if (self.map == null or self.map_start == null) return;
        try self.recordPlacement(col_offset);
    }

    noinline fn recordPlacement(self: *Output, col_offset: u32) Error!void {
        const start = self.map_start.?;
        self.map_start = null;
        const map = &self.map.?;
        const orig = map.resolve(start);
        try map.record(self.allocator, orig.line, orig.col, col_offset);
    }

    noinline fn advanceMap(self: *Output, written: []const u8) void {
        self.map.?.advance(written);
    }

    inline fn pushByte(self: *Output, b: u8) Error!void {
        if (self.code.items.len == self.code.capacity) try self.grow(1);
        self.code.appendAssumeCapacity(b);
    }

    inline fn pushSlice(self: *Output, s: []const u8) Error!void {
        if (self.code.capacity - self.code.items.len < s.len) try self.grow(s.len);
        const old = self.code.items.len;
        self.code.items.len = old + s.len;
        copyShort(self.code.items.ptr + old, s);
    }

    noinline fn grow(self: *Output, additional: usize) Error!void {
        try self.code.ensureUnusedCapacity(self.allocator, additional);
    }
};

inline fn copyShort(dst: [*]u8, s: []const u8) void {
    const n = s.len;
    if (n > 32) {
        @memcpy(dst[0..n], s);
    } else if (n > 16) {
        copyOverlapping(Simd.Chunk, dst, s);
    } else if (n >= 8) {
        copyOverlapping(u64, dst, s);
    } else if (n >= 4) {
        copyOverlapping(u32, dst, s);
    } else if (n > 0) {
        dst[0] = s[0];
        dst[n >> 1] = s[n >> 1];
        dst[n - 1] = s[n - 1];
    }
}

inline fn copyOverlapping(comptime Word: type, dst: [*]u8, s: []const u8) void {
    const width = @sizeOf(Word);
    std.debug.assert(s.len >= width);
    std.debug.assert(s.len <= 2 * width);
    const head: Word = @bitCast(s[0..width].*);
    const tail: Word = @bitCast(s[s.len - width ..][0..width].*);
    dst[0..width].* = @bitCast(head);
    dst[s.len - width ..][0..width].* = @bitCast(tail);
}

const space_chunk_len = @sizeOf(Simd.Chunk);

// the byte before `.` in `1 .x`, which would lex as a fraction unspaced
const after_bare_integer: u8 = 0xFF;

// the byte each punctuator fuses with, as in `<!` or `!=`
const fuses_after: [256]u8 = blk: {
    var t: [256]u8 = @splat(0);
    t['.'] = after_bare_integer;
    t['+'] = '+';
    t['-'] = '-';
    t['/'] = '/';
    t['<'] = '<';
    t['!'] = '<';
    t['='] = '!';
    t['?'] = '?';
    break :blk t;
};
