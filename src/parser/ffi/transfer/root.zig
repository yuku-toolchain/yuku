// AST transfer format v12, little endian with no version byte
// the decoder tools/estree/decoder.zig generates shares the extern struct layouts below
//
// sections, in order
//
//   header             HEADER_SIZE bytes
//   nodes              node_count * NODE_SIZE, one PackedNode each
//   extra              extra_count * 4, the u32 array backing IndexRange
//   strings            string_pool_len bytes of UTF-8, zero-padded to a 4-byte boundary
//   comments           comment_count * COMMENT_SIZE in source order
//   tokens             token_count * TOKEN_SIZE, each `Token` as is
//   diagnostics        diag_count variable-length entries
//
// node fields
//
//   bool               1 flag bit
//   dialect kind       16 flag bits, one layout for every dialect
//   enum               ceil(log2(n)) flag bits for n variants
//   NodeIndex          1 u32 slot, 0xFFFFFFFF for null
//   IndexRange         2 u32 slots (start, len)
//   String             2 u32 slots (start, end)
//   ?ImportPhase       1 presence flag bit, then the enum bits
//   ?Hashbang          1 presence flag bit, plus 2 u32 slots (start, end)
//
// a String starting below source_len slices the source, else the pool at start - source_len
//
// a diagnostic, each string a u32 length then its UTF-8 bytes
//
//   u8 severity (0 error, 1 warning, 2 hint, 3 info), u32 span_start, u32 span_end, message
//   u8 has_help, then the help string when 1
//   u32 label_count, then per label u32 span_start, u32 span_end, message

const std = @import("std");
const builtin = @import("builtin");
const ast = @import("parser").ast;
const dialect = @import("parser").dialect;

pub const semantic = @import("semantic.zig");

// field order defines the wire layout
const Header = extern struct {
    node_count: u32,
    extra_count: u32,
    string_pool_len: u32,
    source_len: u32,
    comment_count: u32,
    token_count: u32,
    diag_count: u32,
    program_index: u32,
    flags: u32,
    // positions below this need no UTF-16 conversion
    first_non_ascii: u32,
};

const PackedNode = extern struct {
    tag: u8,
    _pad0: u8 = 0,
    flags: u16,
    slots: [9]u32,
    span_start: u32,
    span_end: u32,
};

// `node` is the node a comment is attached to, 0xFFFFFFFF for none, and `flags` packs
//   bit 0     type       (0 line, 1 block)
//   bits 1-2  position   (0 before, 1 after, 2 inside)
//   bit 3     same_line
const PackedComment = extern struct {
    flags: u8,
    _pad: [3]u8 = .{ 0, 0, 0 },
    node: u32,
    value_start: u32,
    value_end: u32,
    span_start: u32,
    span_end: u32,
};

pub const COMMENT_TYPE_BIT: u8 = 0;
pub const COMMENT_POSITION_SHIFT: u8 = 1;
pub const COMMENT_SAME_LINE_BIT: u8 = 3;

pub const HEADER_SIZE: u32 = @sizeOf(Header);
pub const NODE_SIZE: u32 = @sizeOf(PackedNode);
pub const COMMENT_SIZE: u32 = @sizeOf(PackedComment);
pub const TOKEN_SIZE: u32 = @sizeOf(ast.Token);

pub const HDR_NODE_COUNT_U32: u32 = @offsetOf(Header, "node_count") / 4;
pub const HDR_EXTRA_COUNT_U32: u32 = @offsetOf(Header, "extra_count") / 4;
pub const HDR_STRING_POOL_LEN_U32: u32 = @offsetOf(Header, "string_pool_len") / 4;
pub const HDR_SOURCE_LEN_U32: u32 = @offsetOf(Header, "source_len") / 4;
pub const HDR_COMMENT_COUNT_U32: u32 = @offsetOf(Header, "comment_count") / 4;
pub const HDR_TOKEN_COUNT_U32: u32 = @offsetOf(Header, "token_count") / 4;
pub const HDR_DIAG_COUNT_U32: u32 = @offsetOf(Header, "diag_count") / 4;
pub const HDR_PROGRAM_INDEX_U32: u32 = @offsetOf(Header, "program_index") / 4;
pub const HDR_FLAGS_U32: u32 = @offsetOf(Header, "flags") / 4;
pub const HDR_FIRST_NON_ASCII_U32: u32 = @offsetOf(Header, "first_non_ascii") / 4;

pub const FLAG_TS: u32 = 1 << 0;
pub const FLAG_ATTACHED_COMMENTS: u32 = 1 << 1;
pub const FLAG_COMMENTS: u32 = 1 << 2;
pub const FLAG_SEMANTIC: u32 = 1 << 3;

pub const NODE_FLAGS_OFFSET: u8 = @offsetOf(PackedNode, "flags");
pub const NODE_HEADER_U32S: u8 = @offsetOf(PackedNode, "slots") / 4;
pub const NODE_SPAN_START_U32: u8 = @offsetOf(PackedNode, "span_start") / 4;
pub const NODE_SPAN_END_U32: u8 = @offsetOf(PackedNode, "span_end") / 4;

const NODE_DATA_SLOTS: u8 = NODE_SPAN_START_U32 - NODE_HEADER_U32S;
pub const NODE_FLAG_BITS: u8 = @bitSizeOf(@FieldType(PackedNode, "flags"));

pub const COMMENT_FLAGS_OFFSET: u8 = @offsetOf(PackedComment, "flags");
pub const COMMENT_NODE_OFFSET: u8 = @offsetOf(PackedComment, "node");
pub const COMMENT_VALUE_START_OFFSET: u8 = @offsetOf(PackedComment, "value_start");
pub const COMMENT_VALUE_END_OFFSET: u8 = @offsetOf(PackedComment, "value_end");
pub const COMMENT_SPAN_START_OFFSET: u8 = @offsetOf(PackedComment, "span_start");
pub const COMMENT_SPAN_END_OFFSET: u8 = @offsetOf(PackedComment, "span_end");

pub const TOKEN_START_U32: u8 = (@offsetOf(ast.Token, "span") + @offsetOf(ast.Span, "start")) / 4;
pub const TOKEN_END_U32: u8 = (@offsetOf(ast.Token, "span") + @offsetOf(ast.Span, "end")) / 4;
pub const TOKEN_TAG_U32: u8 = @offsetOf(ast.Token, "tag") / 4;
pub const TOKEN_FLAGS_U32: u8 = @offsetOf(ast.Token, "flags") / 4;

comptime {
    std.debug.assert(TOKEN_SIZE == 16);
    std.debug.assert(@offsetOf(ast.Token, "flags") % 4 == 0);

    @setEvalBranchQuota(10_000);
    const info = @typeInfo(ast.NodeData).@"union";
    for (info.field_names, info.field_types) |name, T| {
        if (@typeInfo(T) != .@"struct") continue;
        if (totalU32Slots(T) > NODE_DATA_SLOTS) {
            @compileError(std.fmt.comptimePrint(
                "node '{s}' needs more than {d} u32 slots",
                .{ name, NODE_DATA_SLOTS },
            ));
        }
        if (totalFlagBits(T) > NODE_FLAG_BITS) {
            @compileError(std.fmt.comptimePrint(
                "node '{s}' needs more than {d} flag bits",
                .{ name, NODE_FLAG_BITS },
            ));
        }
    }
}

// layout helpers shared with the decoder generator in tools/estree/decoder.zig

pub fn flagBitCount(comptime T: type, comptime field_idx: usize) u8 {
    const Field = @typeInfo(T).@"struct".field_types[field_idx];
    if (Field == bool) return 1;
    if (Field == dialect.Kind) return 16;
    if (Field == ?ast.ImportPhase) return 1 + enumBitWidth(ast.ImportPhase);
    if (Field == ?ast.Hashbang) return 1;
    if (comptime isEnumType(Field)) return enumBitWidth(Field);
    if (Field == ast.NodeIndex or Field == ast.IndexRange or Field == ast.String) return 0;
    @compileError("unsupported field type '" ++ @typeName(Field) ++ "' in " ++ @typeName(T));
}

pub fn flagBitForField(comptime T: type, comptime target: usize) u8 {
    comptime {
        var bit: u8 = 0;
        for (0..target) |i| bit += flagBitCount(T, i);
        return bit;
    }
}

pub fn fieldU32Count(comptime T: type, comptime field_idx: usize) u8 {
    const Field = @typeInfo(T).@"struct".field_types[field_idx];
    if (Field == ast.NodeIndex) return 1;
    if (Field == ast.IndexRange or Field == ast.String) return 2;
    if (Field == ?ast.Hashbang) return 2;
    if (Field == bool or Field == ?ast.ImportPhase or comptime isEnumType(Field)) return 0;
    @compileError("unsupported field type '" ++ @typeName(Field) ++ "' in " ++ @typeName(T));
}

pub fn u32SlotForField(comptime T: type, comptime target: usize) u8 {
    comptime {
        var slot: u8 = 0;
        for (0..target) |i| slot += fieldU32Count(T, i);
        return slot;
    }
}

pub fn enumBitWidth(comptime E: type) u8 {
    return std.math.log2_int_ceil(usize, @typeInfo(E).@"enum".field_names.len);
}

pub fn isEnumType(comptime T: type) bool {
    return @typeInfo(T) == .@"enum";
}

pub fn totalU32Slots(comptime T: type) u8 {
    return u32SlotForField(T, @typeInfo(T).@"struct".field_types.len);
}

pub fn totalFlagBits(comptime T: type) u8 {
    return flagBitForField(T, @typeInfo(T).@"struct".field_types.len);
}

pub fn bufferSize(tree: *const ast.Tree) usize {
    std.debug.assert(tree.nodes.len <= std.math.maxInt(u32));
    std.debug.assert(tree.extras.items.len <= std.math.maxInt(u32));
    std.debug.assert(tree.source.len <= std.math.maxInt(u32));
    // severity, span_start, span_end, message_len, has_help, label_count
    const diag_fixed = 1 + 4 + 4 + 4 + 1 + 4;
    // span_start, span_end, message_len
    const label_fixed = 4 + 4 + 4;

    var size: usize = HEADER_SIZE +
        tree.nodes.len * NODE_SIZE +
        tree.extras.items.len * 4 +
        std.mem.alignForward(usize, tree.strings.extra.items.len, 4) +
        tree.comments.len * COMMENT_SIZE +
        tree.tokens.len * TOKEN_SIZE;

    for (tree.diagnostics.items) |d| {
        size += diag_fixed + d.message.len;
        if (d.help) |help| size += 4 + help.len;
        for (d.labels) |label| size += label_fixed + label.message.len;
    }
    return size;
}

pub fn serializeInto(tree: *const ast.Tree, buf: []u8) usize {
    std.debug.assert(buf.len >= bufferSize(tree));
    std.debug.assert(tree.root != .null);
    std.debug.assert(@intFromPtr(buf.ptr) % 4 == 0);
    std.debug.assert(@backingInt(tree.root) + 1 == tree.nodes.len);
    if (builtin.optimize.runtimeSafety()) assertPostOrder(tree);

    const string_pool_len: u32 = @intCast(tree.strings.extra.items.len);

    var hdr_flags: u32 = 0;
    if (tree.isTs()) hdr_flags |= FLAG_TS;
    if (tree.comment_index.chains.count() != 0) hdr_flags |= FLAG_ATTACHED_COMMENTS;
    if (tree.comments.len != 0) hdr_flags |= FLAG_COMMENTS;

    const hdr: Header = .{
        .node_count = @intCast(tree.nodes.len),
        .extra_count = @intCast(tree.extras.items.len),
        .string_pool_len = string_pool_len,
        .source_len = @intCast(tree.source.len),
        .comment_count = @intCast(tree.comments.len),
        .token_count = @intCast(tree.tokens.len),
        .diag_count = @intCast(tree.diagnostics.items.len),
        .program_index = @backingInt(tree.root),
        .flags = hdr_flags,
        .first_non_ascii = @intCast(firstNonAsciiOffset(tree.source)),
    };
    var pos: usize = 0;
    writeBytes(buf, &pos, std.mem.asBytes(&hdr));

    const nodes_out: [*]PackedNode = @ptrCast(@alignCast(buf.ptr + pos));
    for (tree.nodes.items(.data), tree.nodes.items(.span), 0..) |*data, span, i| {
        nodes_out[i] = packNode(data, span);
    }
    pos += tree.nodes.len * NODE_SIZE;

    writeBytes(buf, &pos, std.mem.sliceAsBytes(tree.extras.items));

    writeBytes(buf, &pos, tree.strings.extra.items);
    const pool_pad = std.mem.alignForward(u32, string_pool_len, 4) - string_pool_len;
    @memset(buf[pos..][0..pool_pad], 0);
    pos += pool_pad;

    const comments_out: [*]PackedComment = @ptrCast(@alignCast(buf.ptr + pos));
    for (tree.comments, 0..) |c, i| {
        var flags: u8 = @as(u8, @backingInt(c.type)) << COMMENT_TYPE_BIT;
        flags |= @as(u8, @backingInt(c.position)) << COMMENT_POSITION_SHIFT;
        flags |= @as(u8, @intFromBool(c.same_line)) << COMMENT_SAME_LINE_BIT;
        comments_out[i] = .{
            .flags = flags,
            .node = @backingInt(ast.NodeIndex.null),
            .value_start = c.value.start,
            .value_end = c.value.end,
            .span_start = c.span.start,
            .span_end = c.span.end,
        };
    }
    writeCommentNodes(tree, comments_out[0..tree.comments.len]);
    pos += tree.comments.len * COMMENT_SIZE;

    writeBytes(buf, &pos, std.mem.sliceAsBytes(tree.tokens));

    for (tree.diagnostics.items) |d| {
        writeBytes(buf, &pos, &.{@backingInt(d.severity)});
        writeU32(buf, &pos, d.span.start);
        writeU32(buf, &pos, d.span.end);
        writeString(buf, &pos, d.message);
        writeBytes(buf, &pos, &.{@intFromBool(d.help != null)});
        if (d.help) |help| writeString(buf, &pos, help);
        writeU32(buf, &pos, @intCast(d.labels.len));
        for (d.labels) |label| {
            writeU32(buf, &pos, label.span.start);
            writeU32(buf, &pos, label.span.end);
            writeString(buf, &pos, label.message);
        }
    }

    std.debug.assert(pos == bufferSize(tree));
    return pos;
}

pub inline fn writeBytes(buf: []u8, pos: *usize, bytes: []const u8) void {
    @memcpy(buf[pos.*..][0..bytes.len], bytes);
    pos.* += bytes.len;
}

inline fn writeU32(buf: []u8, pos: *usize, value: u32) void {
    std.mem.writeInt(u32, buf[pos.*..][0..4], value, .little);
    pos.* += 4;
}

inline fn writeString(buf: []u8, pos: *usize, string: []const u8) void {
    writeU32(buf, pos, @intCast(string.len));
    writeBytes(buf, pos, string);
}

fn assertPostOrder(tree: *const ast.Tree) void {
    for (tree.nodes.items(.data), 0..) |data, index| {
        switch (data) {
            inline else => |payload| {
                const T = @TypeOf(payload);
                if (@typeInfo(T) == .@"struct") {
                    const info = @typeInfo(T).@"struct";
                    inline for (info.field_names, info.field_types) |name, Field| {
                        if (Field == ast.NodeIndex) {
                            assertChildBefore(@field(payload, name), index);
                        } else if (Field == ast.IndexRange) {
                            for (tree.extra(@field(payload, name))) |child| {
                                assertChildBefore(child, index);
                            }
                        }
                    }
                }
            },
        }
    }
}

fn assertChildBefore(child: ast.NodeIndex, index: usize) void {
    if (child != .null) std.debug.assert(@backingInt(child) < index);
}

fn packNode(data: *const ast.NodeData, span: ast.Span) PackedNode {
    std.debug.assert(span.start <= span.end);
    var n: PackedNode = std.mem.zeroes(PackedNode);
    n.span_start = span.start;
    n.span_end = span.end;
    // by pointer, so no node copies the whole union
    switch (data.*) {
        inline else => |*payload, tag| {
            n.tag = @backingInt(tag);
            packPayload(&n, payload);
        },
    }
    return n;
}

fn packPayload(n: *PackedNode, payload: anytype) void {
    const T = @typeInfo(@TypeOf(payload)).pointer.child;
    if (@typeInfo(T) != .@"struct") return;
    const info = @typeInfo(T).@"struct";
    inline for (info.field_names, info.field_types, 0..) |name, Field, i| {
        const val = @field(payload, name);
        const bit = comptime flagBitForField(T, i);
        const slot = comptime u32SlotForField(T, i);

        if (Field == bool) {
            if (val) setFlagBit(n, bit);
        } else if (Field == ast.NodeIndex) {
            n.slots[slot] = @backingInt(val);
        } else if (Field == ast.IndexRange) {
            n.slots[slot] = val.start;
            n.slots[slot + 1] = val.len;
        } else if (Field == ast.String) {
            n.slots[slot] = val.start;
            n.slots[slot + 1] = val.end;
        } else if (comptime isEnumType(Field)) {
            setFlagBits(n, bit, @backingInt(val));
        } else if (Field == ?ast.ImportPhase) {
            if (val) |v| {
                setFlagBit(n, bit);
                setFlagBits(n, bit + 1, @backingInt(v));
            }
        } else if (Field == ?ast.Hashbang) {
            if (val) |h| {
                setFlagBit(n, bit);
                n.slots[slot] = h.value.start;
                n.slots[slot + 1] = h.value.end;
            }
        } else {
            @compileError("unsupported field type '" ++ @typeName(Field) ++
                "' in " ++ @typeName(T));
        }
    }
}

inline fn setFlagBit(n: *PackedNode, comptime bit: u8) void {
    n.flags |= @as(@FieldType(PackedNode, "flags"), 1) << @intCast(bit);
}

inline fn setFlagBits(n: *PackedNode, comptime bit: u8, val: anytype) void {
    n.flags |= @as(@FieldType(PackedNode, "flags"), @intCast(val)) << @intCast(bit);
}

fn firstNonAsciiOffset(source: []const u8) usize {
    const high: @Vector(16, u8) = @splat(0x80);
    var i: usize = 0;
    while (i + 16 <= source.len) : (i += 16) {
        if (@reduce(.Or, source[i..][0..16].* & high) != 0) break;
    }
    for (source[i..], i..) |c, offset| if (c >= 0x80) return offset;
    return source.len;
}

// a node's comments chain in id order, so each source comment learns its node
fn writeCommentNodes(tree: *const ast.Tree, out: []PackedComment) void {
    const index = &tree.comment_index;
    std.debug.assert(index.added.items.len == 0);
    var chains = index.chains.iterator();
    while (chains.next()) |entry| {
        var id = entry.value_ptr.first;
        for (0..out.len) |_| {
            out[id].node = @backingInt(entry.key_ptr.*);
            id = index.links.items[id];
            if (id == std.math.maxInt(u32)) break;
        } else unreachable;
    }
}
