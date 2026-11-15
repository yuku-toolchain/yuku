const std = @import("std");
const Simd = @import("util").Simd;
const ast = @import("../ast.zig");
const sourcemap = @import("sourcemap.zig");
const utils = @import("utils.zig");

const source_maps = @import("codegen_options").source_maps;

const Allocator = std.mem.Allocator;
const Error = Allocator.Error;

// where a leading `{`, `function`, `class`, or `let[` would misparse as a block or declaration
const Lead = enum(u8) { none, stmt, arrow, export_default };

pub const Output = struct {
    allocator: Allocator,
    code: std.ArrayList(u8) = .empty,
    pretty: bool,
    map: ?sourcemap.State = null,
    map_start: ?u32 = null,
    held_spaces: u32 = 0,
    held_literal: bool = false,
    // where the last number ends and how long it is, read when a `.` follows it
    number_end: usize = std.math.maxInt(usize),
    number_len: usize = 0,
    lead: Lead = .none,
    mapped: bool = false,
    // set when an allocation fails, after which writes are dropped and `generate` reports it
    failed: bool = false,
    // text ending within `backed_span` of `backed_from` has its last 16 bytes inside the source
    backed_from: usize = std.math.maxInt(usize),
    backed_span: usize = 0,

    const Text = enum { token, literal, comment };

    // the most spaces a fast write commits and the most text it copies, so it never calls out
    const fast_spaces = 4 * space_chunk_len;
    const fast_text = 32;

    pub const Capacity = struct { code: usize, map: usize };

    pub fn init(
        allocator: Allocator,
        pretty: bool,
        capacity: Capacity,
        map_options: ?sourcemap.Options,
        source: []const u8,
    ) Error!Output {
        var out: Output = .{ .allocator = allocator, .pretty = pretty };
        if (source.len >= backed_len) {
            out.backed_from = @intFromPtr(source.ptr) + backed_len;
            out.backed_span = source.len - backed_len;
        }
        try out.code.ensureTotalCapacity(allocator, capacity.code);
        errdefer out.code.deinit(allocator);
        if (comptime source_maps) if (map_options) |options| {
            out.map = sourcemap.State.init(options, source);
            out.mapped = true;
            try out.map.?.out.ensureTotalCapacity(allocator, capacity.map);
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

    pub inline fn writeByte(self: *Output, b: u8) void {
        const held = self.held_spaces;
        if (b == ' ') {
            if (self.mapped or (!self.pretty and held == 1)) return self.writeByteSlow(b);
            self.lead = .none;
            self.held_spaces = held + 1;
            self.held_literal = false;
            return;
        }
        const at = self.code.items.len;
        const dst = self.fastSpot(held, at) orelse return self.writeByteSlow(b);
        if (!self.joins(b, held, at)) return self.writeByteSlow(b);
        dst[0] = b;
        self.code.items.len = at + held + 1;
        self.held_spaces = 0;
        self.lead = .none;
    }

    /// Where a fast write lands, with the held spaces already in place before it, or null
    /// when the write needs the slow path. Nothing is committed until the caller moves the end.
    inline fn fastSpot(self: *Output, held: u32, at: usize) ?[*]u8 {
        if (self.mapped or held > fast_spaces) return null;
        if (self.code.capacity - at < fast_spaces + fast_text) return null;
        const dst = self.code.items.ptr + at;
        dst[0..space_chunk_len].* = Simd.splat(' ');
        if (held > space_chunk_len) {
            inline for (1..fast_spaces / space_chunk_len) |i| {
                dst[i * space_chunk_len ..][0..space_chunk_len].* = Simd.splat(' ');
            }
        }
        return dst + held;
    }

    /// Whether a token starting with `next` follows the held spaces as they are, with no
    /// space for compact mode to drop and none to add against a fusing byte.
    inline fn joins(self: *const Output, next: u8, held: u32, at: usize) bool {
        if (held == 0) return !self.fuses(next, at);
        return self.pretty or utils.isIdCont(next);
    }

    noinline fn writeByteSlow(self: *Output, b: u8) void {
        self.dropHeldSpace(b);
        self.lead = .none;
        if (b == ' ') {
            self.placeMapping(self.held_spaces);
            self.held_spaces += 1;
            self.held_literal = false;
            return;
        }
        if (self.held_spaces != 0) self.commitSpaces() else self.separateToken(b);
        self.placeMapping(0);
        self.pushByte(b);
        if (comptime source_maps) if (self.map != null) self.advanceMap(&.{b});
    }

    /// Writes a token the printer spells itself, a keyword or an operator.
    pub inline fn writeStr(self: *Output, s: []const u8) void {
        self.writeText(s, .token, false);
    }

    /// Writes a token whose text comes from the tree, a name or a number.
    pub inline fn writeToken(self: *Output, s: []const u8) void {
        self.writeText(s, .token, true);
    }

    pub inline fn writeRawByte(self: *Output, b: u8) void {
        self.writeText(&.{b}, .literal, false);
    }

    pub inline fn writeRawStr(self: *Output, s: []const u8) void {
        self.writeText(s, .literal, true);
    }

    pub inline fn writeComment(self: *Output, s: []const u8) void {
        self.writeTextSlow(s, .comment);
    }

    pub inline fn atLineStart(self: *const Output) bool {
        const items = self.code.items;
        return items.len > 0 and items[items.len - 1] == '\n';
    }

    pub inline fn space(self: *Output) void {
        if (self.pretty) self.writeByte(' ');
    }

    pub fn endLine(self: *Output, indent: u32) void {
        const items = self.code.items;
        if (items.len == 0) return;
        if (items[items.len - 1] != '\n') {
            self.pushByte('\n');
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
        // a synthetic node has no source position
        if (span.start == 0 and span.end == 0) return;
        self.map_start = span.start;
    }

    /// Writes a numeric literal and remembers where, since `1.x` would lex as a fraction.
    pub inline fn writeNumber(self: *Output, text: []const u8) void {
        self.writeToken(text);
        self.number_end = self.code.items.len;
        self.number_len = text.len;
    }

    /// Writes `text` between two `quote` bytes. The text prints as it is, with no escape.
    pub inline fn writeQuoted(self: *Output, quote: u8, text: []const u8) void {
        const held = self.held_spaces;
        const at = self.code.items.len;
        if (text.len -% 1 >= fast_text - 2) return self.writeQuotedSlow(quote, text);
        const dst = self.fastSpot(held, at) orelse return self.writeQuotedSlow(quote, text);
        if (!self.joins(quote, held, at)) return self.writeQuotedSlow(quote, text);
        dst[0] = quote;
        if (@intFromPtr(text.ptr) + text.len -% self.backed_from <= self.backed_span) {
            copyBacked(dst + 1, text.ptr, text.len);
        } else {
            copyShort(dst + 1, text);
        }
        dst[text.len + 1] = quote;
        self.code.items.len = at + held + text.len + 2;
        self.held_spaces = 0;
        self.lead = .none;
    }

    noinline fn writeQuotedSlow(self: *Output, quote: u8, text: []const u8) void {
        self.writeByte(quote);
        self.writeRawStr(text);
        self.writeRawByte(quote);
    }

    /// `from_tree` text is copied without a branch when it lies in the source.
    inline fn writeText(
        self: *Output,
        s: []const u8,
        comptime kind: Text,
        comptime from_tree: bool,
    ) void {
        comptime std.debug.assert(kind != .comment);
        var end = s.len;
        while (end > 0 and s[end - 1] == ' ') end -= 1;
        if (end -% 1 >= fast_text) return self.writeTextSlow(s, kind);
        const held = self.held_spaces;
        const at = self.code.items.len;
        const dst = self.fastSpot(held, at) orelse return self.writeTextSlow(s, kind);
        if (kind == .token and !self.joins(s[0], held, at)) return self.writeTextSlow(s, kind);
        if (from_tree and @intFromPtr(s.ptr) + end -% self.backed_from <= self.backed_span) {
            copyBacked(dst, s.ptr, end);
        } else {
            copyShort(dst, s[0..end]);
        }
        self.code.items.len = at + held + end;
        self.held_spaces = @intCast(s.len - end);
        if (end < s.len) self.held_literal = kind == .literal;
        self.lead = .none;
    }

    noinline fn writeTextSlow(self: *Output, s: []const u8, comptime kind: Text) void {
        if (s.len == 0) return;
        if (kind == .token) self.dropHeldSpace(s[0]);
        var end = s.len;
        while (end > 0 and s[end - 1] == ' ') end -= 1;
        if (end > 0) {
            if (self.held_spaces != 0) {
                self.commitSpaces();
            } else if (kind == .token) {
                self.separateToken(s[0]);
            }
        }
        // a comment leaves the lead and the mapping to the next token
        if (kind != .comment) {
            self.lead = .none;
            // spaces alone map where the held spaces end
            self.placeMapping(self.held_spaces);
        }
        if (end > 0) {
            self.pushSlice(s[0..end]);
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

    /// Whether `next` would fuse with the last byte written, as in `+ +` or `1 .x`.
    inline fn fuses(self: *const Output, next: u8, at: usize) bool {
        const prev = fuses_after[next];
        if (prev == 0) return false;
        if (prev == after_bare_integer) {
            return self.number_end == at and self.number_len <= at and
                isBareInteger(self.code.items[at - self.number_len .. at]);
        }
        return at > 0 and self.code.items.ptr[at - 1] == prev;
    }

    inline fn separateToken(self: *Output, next: u8) void {
        std.debug.assert(self.held_spaces == 0);
        if (!self.fuses(next, self.code.items.len)) return;
        self.pushByte(' ');
        if (comptime source_maps) if (self.map) |*map| {
            map.gen_col += 1;
        };
    }

    fn commitSpaces(self: *Output) void {
        const n = self.held_spaces;
        std.debug.assert(n > 0);
        self.held_spaces = 0;
        self.held_literal = false;
        if (!self.hasRoom(n + space_chunk_len)) return;
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

    inline fn placeMapping(self: *Output, col_offset: u32) void {
        if (comptime !source_maps) return;
        if (self.map == null or self.map_start == null) return;
        self.recordPlacement(col_offset);
    }

    noinline fn recordPlacement(self: *Output, col_offset: u32) void {
        const start = self.map_start.?;
        self.map_start = null;
        const map = &self.map.?;
        const orig = map.resolve(start);
        map.record(self.allocator, orig.line, orig.col, col_offset) catch {
            self.failed = true;
        };
    }

    noinline fn advanceMap(self: *Output, written: []const u8) void {
        self.map.?.advance(written);
    }

    inline fn pushByte(self: *Output, b: u8) void {
        if (!self.hasRoom(1)) return;
        self.code.appendAssumeCapacity(b);
    }

    inline fn pushSlice(self: *Output, s: []const u8) void {
        if (!self.hasRoom(s.len)) return;
        const old = self.code.items.len;
        self.code.items.len = old + s.len;
        copyShort(self.code.items.ptr + old, s);
    }

    /// Whether `additional` bytes fit, growing the buffer when they do not yet.
    inline fn hasRoom(self: *Output, additional: usize) bool {
        return self.code.capacity - self.code.items.len >= additional or self.grow(additional);
    }

    noinline fn grow(self: *Output, additional: usize) bool {
        if (self.failed) return false;
        self.code.ensureUnusedCapacity(self.allocator, additional) catch {
            self.failed = true;
            return false;
        };
        return true;
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

// how many bytes before a text's end `copyBacked` reads
const backed_len = 16;

/// Copies 1 to 32 bytes without a branch on the length. The `backed_len` bytes before the
/// text's end must be readable, and `dst` must have room for 32.
inline fn copyBacked(dst: [*]u8, src: [*]const u8, n: usize) void {
    std.debug.assert(n >= 1 and n <= 2 * backed_len);
    const long = n > backed_len;
    const tail = src + n - backed_len;
    const low = std.mem.readInt(u64, tail[0..8], .little);
    const high = std.mem.readInt(u64, tail[8..16], .little);
    // a long text copies its first and last 16 bytes
    dst[0..backed_len].* = (if (long) src else tail)[0..backed_len].*;
    // a short one shifts out the bytes that precede it
    const drop: u64 = if (long) 0 else 8 * (backed_len - n);
    const wide = drop >= 64;
    const shift: u6 = @truncate(drop);
    const end = if (long) dst + n - backed_len else dst;
    std.mem.writeInt(u64, end[0..8], if (wide) high >> shift else (low >> shift) | (high << 1 << (63 - shift)), .little);
    std.mem.writeInt(u64, end[8..16], if (wide) 0 else high >> shift, .little);
}

test "a backed copy moves every length" {
    const text = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOP";
    for (1..2 * backed_len + 1) |n| {
        for (backed_len..text.len + 1) |end_at| {
            if (end_at < n) continue;
            const start = end_at - n;
            var out: [2 * backed_len]u8 = @splat('.');
            copyBacked(&out, text.ptr + start, n);
            try std.testing.expectEqualStrings(text[start..end_at], out[0..n]);
        }
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

fn isBareInteger(text: []const u8) bool {
    if (text.len == 0 or !std.ascii.isDigit(text[0])) return false;
    for (text[1..]) |c| if (!std.ascii.isDigit(c) and c != '_') return false;
    return true;
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
