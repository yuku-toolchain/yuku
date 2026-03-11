const std = @import("std");
const ast = @import("../ast.zig");

const Allocator = std.mem.Allocator;

/// A mutable overlay on a ParseTree that supports in-place node edits
/// and new node/extra/source allocation.
pub const MutableTree = struct {
    // mutable views of original data
    data: []ast.NodeData,
    spans: []ast.Span,
    extra: []const ast.NodeIndex,
    source: []const u8,
    program: ast.NodeIndex,
    source_type: ast.SourceType,

    // append-only buffers for newly created nodes
    new_data: std.ArrayList(ast.NodeData),
    new_spans: std.ArrayList(ast.Span),
    // append-only buffer for new child lists
    new_extra: std.ArrayList(ast.NodeIndex),
    // append-only buffer for synthetic source text (new identifiers, strings)
    new_source: std.ArrayList(u8),

    // boundaries between original and new data
    base_count: u32,
    base_extra_count: u32,
    base_source_len: u32,

    allocator: Allocator,

    /// Creates a mutable overlay from an existing ParseTree.
    /// The ParseTree must outlive the MutableTree.
    pub fn init(tree: *const ast.ParseTree, allocator: Allocator) Allocator.Error!MutableTree {
        const node_count = tree.nodes.items(.data).len;
        const extra_count = tree.extra.len;

        const estimated_new_nodes: u32 = @max(16, @as(u32, @intCast(node_count / 32)));
        const estimated_new_extra: u32 = @max(16, @as(u32, @intCast(extra_count / 32)));

        var self = MutableTree{
            .data = @constCast(tree.nodes.items(.data)),
            .spans = @constCast(tree.nodes.items(.span)),
            .extra = tree.extra,
            .source = tree.source,
            .program = tree.program,
            .source_type = tree.source_type,
            .new_data = .{},
            .new_spans = .{},
            .new_extra = .{},
            .new_source = .{},
            .base_count = @intCast(node_count),
            .base_extra_count = @intCast(extra_count),
            .base_source_len = @intCast(tree.source.len),
            .allocator = allocator,
        };

        try self.new_data.ensureTotalCapacity(allocator, estimated_new_nodes);
        try self.new_spans.ensureTotalCapacity(allocator, estimated_new_nodes);
        try self.new_extra.ensureTotalCapacity(allocator, estimated_new_extra);
        try self.new_source.ensureTotalCapacity(allocator, 256);

        return self;
    }

    /// Gets the data for the node at the given index.
    pub inline fn getData(self: *const MutableTree, index: ast.NodeIndex) ast.NodeData {
        const i = @intFromEnum(index);
        return if (i < self.base_count) self.data[i] else self.new_data.items[i - self.base_count];
    }

    /// Gets the span for the node at the given index.
    pub inline fn getSpan(self: *const MutableTree, index: ast.NodeIndex) ast.Span {
        const i = @intFromEnum(index);
        return if (i < self.base_count) self.spans[i] else self.new_spans.items[i - self.base_count];
    }

    /// Gets the extra node indices for the given range.
    pub inline fn getExtra(self: *const MutableTree, range: ast.IndexRange) []const ast.NodeIndex {
        return if (range.start < self.base_extra_count)
            self.extra[range.start..][0..range.len]
        else
            self.new_extra.items[range.start - self.base_extra_count ..][0..range.len];
    }

    /// Gets a slice of the source text at the given position.
    pub inline fn getSourceText(self: *const MutableTree, start: u32, len: u16) []const u8 {
        return if (start < self.base_source_len)
            self.source[start..][0..len]
        else
            self.new_source.items[start - self.base_source_len ..][0..len];
    }

    /// Overwrites an existing node's data in-place.
    pub inline fn setData(self: *MutableTree, index: ast.NodeIndex, new: ast.NodeData) void {
        const i = @intFromEnum(index);
        if (i < self.base_count) {
            self.data[i] = new;
        } else {
            self.new_data.items[i - self.base_count] = new;
        }
    }

    /// Overwrites an existing node's span in-place.
    pub inline fn setSpan(self: *MutableTree, index: ast.NodeIndex, new: ast.Span) void {
        const i = @intFromEnum(index);
        if (i < self.base_count) {
            self.spans[i] = new;
        } else {
            self.new_spans.items[i - self.base_count] = new;
        }
    }

    /// Allocates a new node. Returns its index.
    pub fn addNode(self: *MutableTree, d: ast.NodeData, s: ast.Span) Allocator.Error!ast.NodeIndex {
        const id: u32 = self.base_count + @as(u32, @intCast(self.new_data.items.len));
        try self.new_data.append(self.allocator, d);
        try self.new_spans.append(self.allocator, s);
        return @enumFromInt(id);
    }

    /// Allocates a new child list from a slice of node indices. Returns its range.
    pub fn addExtra(self: *MutableTree, children: []const ast.NodeIndex) Allocator.Error!ast.IndexRange {
        const start: u32 = self.base_extra_count + @as(u32, @intCast(self.new_extra.items.len));
        try self.new_extra.appendSlice(self.allocator, children);
        return .{ .start = start, .len = @intCast(children.len) };
    }

    /// Registers synthetic source text (for new identifiers, string literals, etc.).
    /// Returns a name handle usable in `BindingIdentifier`, `StringLiteral`, etc.
    pub fn addSource(self: *MutableTree, text: []const u8) Allocator.Error!struct { start: u32, len: u16 } {
        const start: u32 = self.base_source_len + @as(u32, @intCast(self.new_source.items.len));
        try self.new_source.appendSlice(self.allocator, text);
        return .{ .start = start, .len = @intCast(text.len) };
    }

    pub fn deinit(self: *MutableTree) void {
        self.new_data.deinit(self.allocator);
        self.new_spans.deinit(self.allocator);
        self.new_extra.deinit(self.allocator);
        self.new_source.deinit(self.allocator);
    }
};
