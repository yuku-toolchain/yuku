// AST transfer format v13, little endian with no version byte
// tools/estree generates the JavaScript decoder and encoder from these layouts
//
// sections, in order
//
//   header             one Header
//   nodes              node_count PackedNode
//   extra              extra_count u32, the array backing IndexRange
//   strings            string_pool_len bytes of UTF-8, zero-padded to a 4-byte boundary
//   comments           comment_count PackedComment in source order
//   tokens             token_count ast.Token, each as is
//   diagnostics        diag_count variable-length entries
//
// node fields
//
//   bool               1 flag bit
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
const parser = @import("parser");
const ast = parser.ast;

pub const schema = @import("schema.zig");
pub const semantic = @import("semantic.zig");
pub const utf16 = @import("utf16.zig");

pub const Header = extern struct {
    node_count: u32,
    extra_count: u32,
    string_pool_len: u32,
    source_len: u32,
    comment_count: u32,
    token_count: u32,
    diag_count: u32,
    program_index: u32,
    flags: u32,
};

pub const PackedNode = extern struct {
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
pub const PackedComment = extern struct {
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

pub const FLAG_TS: u32 = 1 << 0;
pub const FLAG_ATTACHED_COMMENTS: u32 = 1 << 1;
pub const FLAG_COMMENTS: u32 = 1 << 2;
pub const FLAG_SEMANTIC: u32 = 1 << 3;

pub const Layout = struct { bit: u8, slot: u8 };

comptime {
    std.debug.assert(@sizeOf(ast.Token) == 16);
    std.debug.assert(@offsetOf(ast.Token, "flags") % 4 == 0);

    @setEvalBranchQuota(10_000);
    const room: Layout = .{
        .bit = @bitSizeOf(@FieldType(PackedNode, "flags")),
        .slot = @typeInfo(@FieldType(PackedNode, "slots")).array.len,
    };
    const info = @typeInfo(ast.NodeData).@"union";
    for (info.field_names, info.field_types) |name, T| {
        const end = layout(T, @typeInfo(T).@"struct".field_types.len);
        if (end.bit > room.bit or end.slot > room.slot) {
            @compileError("node '" ++ name ++ "' has more flag bits or slots than a PackedNode");
        }
    }
}

pub fn layout(comptime T: type, comptime index: usize) Layout {
    comptime {
        var at: Layout = .{ .bit = 0, .slot = 0 };
        for (@typeInfo(T).@"struct".field_types[0..index]) |Field| {
            const size = fieldSize(Field);
            at.bit += size.bit;
            at.slot += size.slot;
        }
        return at;
    }
}

pub fn enumBitWidth(comptime E: type) u8 {
    return std.math.log2_int_ceil(usize, @typeInfo(E).@"enum".field_names.len);
}

pub fn fieldSize(comptime Field: type) Layout {
    if (Field == bool) return .{ .bit = 1, .slot = 0 };
    if (Field == ?ast.ImportPhase) return .{ .bit = 1 + enumBitWidth(ast.ImportPhase), .slot = 0 };
    if (Field == ?ast.Hashbang) return .{ .bit = 1, .slot = 2 };
    if (Field == ast.NodeIndex) return .{ .bit = 0, .slot = 1 };
    if (Field == ast.IndexRange or Field == ast.String) return .{ .bit = 0, .slot = 2 };
    if (@typeInfo(Field) == .@"enum") return .{ .bit = enumBitWidth(Field), .slot = 0 };
    @compileError("a node field cannot be a '" ++ @typeName(Field) ++ "'");
}

pub const Sections = struct { nodes: bool };

pub fn bufferSize(tree: *const ast.Tree, sections: Sections) usize {
    std.debug.assert(tree.nodes.len <= std.math.maxInt(u32));
    std.debug.assert(tree.extras.items.len <= std.math.maxInt(u32));
    std.debug.assert(tree.source.len <= std.math.maxInt(u32));
    var size: usize = @sizeOf(Header) +
        std.mem.alignForward(usize, tree.strings.extra.items.len, 4) +
        diagnosticsSize(tree.diagnostics.items);
    if (sections.nodes) {
        size += tree.nodes.len * @sizeOf(PackedNode) +
            tree.extras.items.len * 4 +
            tree.comments.len * @sizeOf(PackedComment) +
            tree.tokens.len * @sizeOf(ast.Token);
    }
    return size;
}

pub fn serializeInto(
    tree: *const ast.Tree,
    offsets: *const utf16.Offsets,
    buf: []u8,
    sections: Sections,
) usize {
    std.debug.assert(buf.len >= bufferSize(tree, sections));
    std.debug.assert(offsets.bytes_len == tree.source.len);
    std.debug.assert(@intFromPtr(buf.ptr) % 4 == 0);
    std.debug.assert(tree.root != .null);
    std.debug.assert(@backingInt(tree.root) + 1 == tree.nodes.len);
    if (sections.nodes and builtin.optimize.runtimeSafety()) assertPostOrder(tree);
    const local = offsets.*;

    const pool = tree.strings.extra.items;
    var flags: u32 = 0;
    if (tree.isTs()) flags |= FLAG_TS;
    if (tree.comment_index.chains.count() != 0) flags |= FLAG_ATTACHED_COMMENTS;
    if (tree.comments.len != 0) flags |= FLAG_COMMENTS;
    const header: Header = .{
        .node_count = if (sections.nodes) @intCast(tree.nodes.len) else 0,
        .extra_count = if (sections.nodes) @intCast(tree.extras.items.len) else 0,
        .string_pool_len = @intCast(pool.len),
        .source_len = offsets.units_len,
        .comment_count = if (sections.nodes) @intCast(tree.comments.len) else 0,
        .token_count = if (sections.nodes) @intCast(tree.tokens.len) else 0,
        .diag_count = @intCast(tree.diagnostics.items.len),
        .program_index = @backingInt(tree.root),
        .flags = flags,
    };
    var pos: usize = 0;
    writeBytes(buf, &pos, std.mem.asBytes(&header));

    if (sections.nodes) {
        const nodes: [*]PackedNode = @ptrCast(@alignCast(buf.ptr + pos));
        for (tree.nodes.items(.data), tree.nodes.items(.span), nodes) |*data, span, *out| {
            packNode(out, data, span, &local);
        }
        pos += tree.nodes.len * @sizeOf(PackedNode);
        writeBytes(buf, &pos, std.mem.sliceAsBytes(tree.extras.items));
    }

    writeBytes(buf, &pos, pool);
    const padded = std.mem.alignForward(usize, pos, 4);
    @memset(buf[pos..padded], 0);
    pos = padded;

    if (sections.nodes) {
        const comments: [*]PackedComment = @ptrCast(@alignCast(buf.ptr + pos));
        for (tree.comments, comments) |comment, *out| {
            const value = stringIn(&local, comment.value);
            out.* = .{
                .flags = @as(u8, @backingInt(comment.type)) << COMMENT_TYPE_BIT |
                    @as(u8, @backingInt(comment.position)) << COMMENT_POSITION_SHIFT |
                    @as(u8, @intFromBool(comment.same_line)) << COMMENT_SAME_LINE_BIT,
                .node = @backingInt(ast.NodeIndex.null),
                .value_start = value.start,
                .value_end = value.end,
                .span_start = local.at(comment.span.start),
                .span_end = local.at(comment.span.end),
            };
        }
        writeCommentNodes(tree, comments[0..tree.comments.len]);
        pos += tree.comments.len * @sizeOf(PackedComment);

        const tokens: [*]ast.Token = @ptrCast(@alignCast(buf.ptr + pos));
        writeBytes(buf, &pos, std.mem.sliceAsBytes(tree.tokens));
        if (local.ascii_len != local.bytes_len) {
            for (tokens[0..tree.tokens.len]) |*token| {
                token.span = .{
                    .start = local.at(token.span.start),
                    .end = local.at(token.span.end),
                };
            }
        }
    }
    pos = writeDiagnostics(buf, pos, tree.diagnostics.items, offsets);

    std.debug.assert(pos == bufferSize(tree, sections));
    return pos;
}

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

fn diagnosticsSize(diagnostics: []const ast.Diagnostic) usize {
    const diag_fixed = 1 + 4 + 4 + 4 + 1 + 4;
    const label_fixed = 4 + 4 + 4;
    var size: usize = 0;
    for (diagnostics) |d| {
        size += diag_fixed + d.message.len;
        if (d.help) |help| size += 4 + help.len;
        for (d.labels) |label| size += label_fixed + label.message.len;
    }
    return size;
}

fn writeDiagnostics(
    buf: []u8,
    start: usize,
    diagnostics: []const ast.Diagnostic,
    offsets: ?*const utf16.Offsets,
) usize {
    var pos = start;
    for (diagnostics) |d| {
        writeBytes(buf, &pos, &.{@backingInt(d.severity)});
        writeSpan(buf, &pos, d.span, offsets);
        writeString(buf, &pos, d.message);
        writeBytes(buf, &pos, &.{@intFromBool(d.help != null)});
        if (d.help) |help| writeString(buf, &pos, help);
        writeU32(buf, &pos, @intCast(d.labels.len));
        for (d.labels) |label| {
            writeSpan(buf, &pos, label.span, offsets);
            writeString(buf, &pos, label.message);
        }
    }
    return pos;
}

pub fn printedSize(result: *const parser.codegen.Result) usize {
    const mappings = if (result.map) |map| map.mappings else "";
    return 12 + std.mem.alignForward(usize, result.code.len + mappings.len, 4) +
        diagnosticsSize(result.diagnostics);
}

pub fn serializePrinted(
    result: *const parser.codegen.Result,
    offsets: ?*const utf16.Offsets,
    buf: []u8,
) usize {
    std.debug.assert(buf.len >= printedSize(result));
    const mappings = if (result.map) |map| map.mappings else "";
    var pos: usize = 0;
    writeU32(buf, &pos, @intCast(result.code.len));
    writeU32(buf, &pos, @intCast(mappings.len));
    writeU32(buf, &pos, @intCast(result.diagnostics.len));
    writeBytes(buf, &pos, result.code);
    writeBytes(buf, &pos, mappings);
    const end = std.mem.alignForward(usize, pos, 4);
    @memset(buf[pos..end], 0);
    pos = writeDiagnostics(buf, end, result.diagnostics, offsets);
    std.debug.assert(pos == printedSize(result));
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

inline fn writeSpan(buf: []u8, pos: *usize, span: ast.Span, offsets: ?*const utf16.Offsets) void {
    writeU32(buf, pos, if (offsets) |o| o.at(span.start) else span.start);
    writeU32(buf, pos, if (offsets) |o| o.at(span.end) else span.end);
}

pub fn stringIn(offsets: *const utf16.Offsets, string: ast.String) ast.String {
    std.debug.assert(string.start <= string.end);
    if (string.start < offsets.bytes_len) {
        std.debug.assert(string.end <= offsets.bytes_len);
        return .{ .start = offsets.at(string.start), .end = offsets.at(string.end) };
    }
    const shift = offsets.bytes_len - offsets.units_len;
    return .{ .start = string.start - shift, .end = string.end - shift };
}

fn assertPostOrder(tree: *const ast.Tree) void {
    for (tree.nodes.items(.data), 0..) |data, index| switch (data) {
        inline else => |node| inline for (comptime std.meta.fieldNames(@TypeOf(node))) |name| {
            const field = @field(node, name);
            const children: []const ast.NodeIndex = switch (@TypeOf(field)) {
                ast.NodeIndex => &.{field},
                ast.IndexRange => tree.extra(field),
                else => &.{},
            };
            for (children) |child| std.debug.assert(child == .null or @backingInt(child) < index);
        },
    };
}

fn packNode(
    n: *PackedNode,
    data: *const ast.NodeData,
    span: ast.Span,
    offsets: *const utf16.Offsets,
) void {
    std.debug.assert(span.start <= span.end);
    n.* = .{
        .tag = @backingInt(std.meta.activeTag(data.*)),
        .flags = 0,
        .slots = @splat(0),
        .span_start = offsets.at(span.start),
        .span_end = offsets.at(span.end),
    };
    // by pointer, so no node copies the whole union
    switch (data.*) {
        inline else => |*payload| packPayload(n, payload, offsets),
    }
}

fn packPayload(n: *PackedNode, payload: anytype, offsets: *const utf16.Offsets) void {
    const T = @TypeOf(payload.*);
    const info = @typeInfo(T).@"struct";
    inline for (info.field_names, info.field_types, 0..) |name, Field, i| {
        const value = @field(payload, name);
        const at = comptime layout(T, i);
        const slots = n.slots[at.slot..];
        if (Field == bool) {
            if (value) n.flags |= 1 << at.bit;
        } else if (Field == ast.NodeIndex) {
            slots[0] = @backingInt(value);
        } else if (Field == ast.IndexRange) {
            slots[0] = value.start;
            slots[1] = value.len;
        } else if (Field == ast.String) {
            const string = stringIn(offsets, value);
            slots[0] = string.start;
            slots[1] = string.end;
        } else if (Field == ?ast.ImportPhase) {
            if (value) |phase| n.flags |= (1 | @as(u16, @backingInt(phase)) << 1) << at.bit;
        } else if (Field == ?ast.Hashbang) {
            if (value) |hashbang| {
                const string = stringIn(offsets, hashbang.value);
                n.flags |= 1 << at.bit;
                slots[0] = string.start;
                slots[1] = string.end;
            }
        } else {
            n.flags |= @as(u16, @backingInt(value)) << at.bit;
        }
    }
}

pub const Encoded = struct {
    nodes: []const PackedNode,
    extras: []const u32,
    comments: []const PackedComment,
    pool: []align(1) const u16,
};

pub const Error = error{ InvalidTree, OutOfMemory };

pub fn unpack(gpa: std.mem.Allocator, tree: *ast.Tree, encoded: Encoded) Error!void {
    std.debug.assert(tree.nodes.len == 0);
    std.debug.assert(tree.strings.extra.items.len == 0);
    const max = std.math.maxInt(u32);
    if (encoded.nodes.len == 0 or encoded.nodes.len >= max) return error.InvalidTree;
    if (encoded.extras.len >= max) return error.InvalidTree;
    if (tree.source.len + encoded.pool.len * 3 > max) return error.InvalidTree;
    const arena = tree.allocator();
    try tree.strings.extra.ensureTotalCapacity(arena, encoded.pool.len * 3);

    var source_bytes: utf16.Bytes = try .init(gpa, tree.source);
    defer source_bytes.deinit(gpa);

    try tree.extras.appendSlice(arena, @ptrCast(encoded.extras));
    try tree.nodes.ensureTotalCapacity(arena, encoded.nodes.len);
    var reader: Reader = .{
        .nodes = encoded.nodes,
        .extras = tree.extras.items,
        .pool = encoded.pool,
        .strings = &tree.strings.extra,
        .source = if (tree.source.len == 0) null else &source_bytes,
        .strings_base = @intCast(tree.source.len),
    };
    for (encoded.nodes, 0..) |*n, index| {
        tree.nodes.appendAssumeCapacity(.{
            .data = try reader.node(n, @intCast(index)),
            .span = reader.span(n.span_start, n.span_end),
        });
    }
    for (encoded.comments) |*c| {
        if (c.node >= encoded.nodes.len) return error.InvalidTree;
        const position_bits = (c.flags >> COMMENT_POSITION_SHIFT) & 0b11;
        try tree.addComment(@fromBackingInt(c.node), .{
            .type = if ((c.flags >> COMMENT_TYPE_BIT) & 1 != 0) .block else .line,
            .value = try reader.string(c.value_start, c.value_end),
            .position = try enumOf(ast.Comment.Position, position_bits),
            .same_line = (c.flags >> COMMENT_SAME_LINE_BIT) & 1 != 0,
        });
    }
    tree.root = @fromBackingInt(@as(u32, @intCast(encoded.nodes.len - 1)));
}

const Reader = struct {
    nodes: []const PackedNode,
    extras: []const ast.NodeIndex,
    pool: []align(1) const u16,
    strings: *std.ArrayList(u8),
    source: ?*const utf16.Bytes,
    strings_base: u32,

    fn node(reader: *Reader, n: *const PackedNode, index: u32) Error!ast.NodeData {
        switch (try enumOf(schema.Tag, n.tag)) {
            inline else => |tag| {
                const name = @tagName(tag);
                const T = @FieldType(ast.NodeData, name);
                return @unionInit(ast.NodeData, name, try reader.payload(T, name, n, index));
            },
        }
    }

    fn payload(
        reader: *Reader,
        comptime T: type,
        comptime tag: []const u8,
        n: *const PackedNode,
        index: u32,
    ) Error!T {
        const info = @typeInfo(T).@"struct";
        var value: T = undefined;
        inline for (info.field_names, info.field_types, 0..) |name, Field, i| {
            const at = comptime layout(T, i);
            const rule = comptime schema.of(tag, name);
            const slots = n.slots[at.slot..];
            @field(value, name) = if (Field == bool)
                flagBits(n, at.bit, 1) != 0
            else if (Field == ast.NodeIndex)
                try reader.child(rule, slots[0], index)
            else if (Field == ast.IndexRange)
                try reader.range(rule, slots[0], slots[1], index)
            else if (Field == ast.String)
                try reader.string(slots[0], slots[1])
            else if (Field == ?ast.ImportPhase)
                (if (flagBits(n, at.bit, 1) == 0) null else try enumOf(
                    ast.ImportPhase,
                    flagBits(n, at.bit + 1, enumBitWidth(ast.ImportPhase)),
                ))
            else if (Field == ?ast.Hashbang)
                (if (flagBits(n, at.bit, 1) == 0) null else .{
                    .value = try reader.string(slots[0], slots[1]),
                })
            else
                try enumOf(Field, flagBits(n, at.bit, enumBitWidth(Field)));
        }
        return value;
    }

    fn child(
        reader: *const Reader,
        comptime field: schema.Field,
        slot: u32,
        index: u32,
    ) Error!ast.NodeIndex {
        const child_index: ast.NodeIndex = @fromBackingInt(slot);
        if (child_index == .null) return if (field.nullable) .null else error.InvalidTree;
        if (slot >= index or !field.allows(reader.nodes[slot].tag)) return error.InvalidTree;
        return child_index;
    }

    fn range(
        reader: *const Reader,
        comptime field: schema.Field,
        start: u32,
        len: u32,
        index: u32,
    ) Error!ast.IndexRange {
        if (start > reader.extras.len or len > reader.extras.len - start) return error.InvalidTree;
        for (reader.extras[start..][0..len]) |item| {
            _ = try reader.child(field, @backingInt(item), index);
        }
        return .{ .start = start, .len = len };
    }

    fn string(reader: *Reader, start: u32, end: u32) Error!ast.String {
        if (start > end or end > reader.pool.len) return error.InvalidTree;
        if (start == end) return .empty;
        const strings = reader.strings;
        const units = reader.pool[start..end];
        const target = strings.unusedCapacitySlice();
        if (target.len < units.len * 3) return error.InvalidTree;
        const from: u32 = @intCast(strings.items.len);
        const ascii = for (units, 0..) |unit, k| {
            if (unit >= 0x80) break k;
            target[k] = @intCast(unit);
        } else units.len;
        strings.items.len += ascii;
        if (ascii < units.len) {
            strings.items.len += utf16.encodeUtf8(units[ascii..], target[ascii..], .keep).len;
        }
        return .{
            .start = reader.strings_base + from,
            .end = reader.strings_base + @as(u32, @intCast(strings.items.len)),
        };
    }

    fn span(reader: *const Reader, start: u32, end: u32) ast.Span {
        if (start > end) return .{ .start = 0, .end = 0 };
        const source = reader.source orelse return .{ .start = start, .end = end };
        if (end > source.units_len) return .{ .start = 0, .end = 0 };
        return .{ .start = source.at(start), .end = source.at(end) };
    }
};

inline fn flagBits(n: *const PackedNode, comptime bit: u8, comptime width: u8) u16 {
    return (n.flags >> bit) & ((1 << width) - 1);
}

pub fn enumOf(comptime E: type, bits: anytype) error{InvalidTree}!E {
    const values = comptime std.enums.values(E);
    comptime for (values, 0..) |value, i| std.debug.assert(@backingInt(value) == i);
    if (bits >= values.len) return error.InvalidTree;
    return @fromBackingInt(@intCast(bits));
}

test {
    _ = utf16;
}
