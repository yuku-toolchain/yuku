const std = @import("std");
const ast = @import("ast.zig");
const lineBreakLen = @import("lexical.zig").lineBreakLen;

const Error = error{OutOfMemory};
const none = std.math.maxInt(u32);

/// Each node's comments as chains of ids in id order, where ids past `Tree.comments.len` index
/// `added`.
pub const Index = struct {
    owners: std.ArrayList(u64) = .empty,
    chains: std.AutoHashMapUnmanaged(ast.NodeIndex, Chain) = .empty,
    links: std.ArrayList(u32) = .empty,
    added: std.ArrayList(ast.Comment) = .empty,

    const Chain = struct { first: u32, last: u32 };

    pub inline fn has(self: *const Index, node: ast.NodeIndex) bool {
        std.debug.assert(node != .null);
        const i = @backingInt(node);
        if (i / 64 >= self.owners.items.len) return false;
        return self.owners.items[i / 64] & bit(i) != 0;
    }

    pub inline fn first(self: *const Index, node: ast.NodeIndex) u32 {
        return if (self.has(node)) self.chains.get(node).?.first else none;
    }

    pub fn add(
        self: *Index,
        gpa: std.mem.Allocator,
        source_len: usize,
        node: ast.NodeIndex,
        comment: ast.Comment,
    ) Error!void {
        const id: u32 = @intCast(source_len + self.added.items.len);
        var added = comment;
        added.span = .{ .start = 0, .end = 0 };
        try self.added.append(gpa, added);
        try self.link(gpa, node, id);
    }

    fn link(self: *Index, gpa: std.mem.Allocator, node: ast.NodeIndex, id: u32) Error!void {
        std.debug.assert(node != .null);
        std.debug.assert(id != none);
        if (self.links.items.len <= id) {
            try self.links.appendNTimes(gpa, none, id + 1 - self.links.items.len);
        }
        const entry = try self.chains.getOrPut(gpa, node);
        if (entry.found_existing) {
            std.debug.assert(entry.value_ptr.last < id);
            self.links.items[entry.value_ptr.last] = id;
            entry.value_ptr.last = id;
        } else {
            entry.value_ptr.* = .{ .first = id, .last = id };
            try self.mark(gpa, node);
        }
    }

    pub fn move(
        self: *Index,
        gpa: std.mem.Allocator,
        from: ast.NodeIndex,
        to: ast.NodeIndex,
    ) Error!void {
        std.debug.assert(from != .null);
        std.debug.assert(from != to);
        const moved = (self.chains.fetchRemove(from) orelse return).value;
        self.owners.items[@backingInt(from) / 64] &= ~bit(@backingInt(from));
        if (to == .null) return;
        const entry = try self.chains.getOrPut(gpa, to);
        if (entry.found_existing) {
            entry.value_ptr.* = self.merge(entry.value_ptr.*, moved);
        } else {
            entry.value_ptr.* = moved;
            try self.mark(gpa, to);
        }
    }

    fn merge(self: *Index, a: Chain, b: Chain) Chain {
        const links = self.links.items;
        var left = a.first;
        var right = b.first;
        var tail: u32 = none;
        for (0..links.len + 1) |_| {
            if (left == none and right == none) break;
            const take_left = right == none or (left != none and left < right);
            const id = if (take_left) left else right;
            if (take_left) left = links[id] else right = links[id];
            if (tail != none) links[tail] = id;
            tail = id;
        } else unreachable;
        links[tail] = none;
        return .{ .first = @min(a.first, b.first), .last = @max(a.last, b.last) };
    }

    fn mark(self: *Index, gpa: std.mem.Allocator, node: ast.NodeIndex) Error!void {
        const i = @backingInt(node);
        if (i / 64 >= self.owners.items.len) {
            try self.owners.appendNTimes(gpa, 0, i / 64 + 1 - self.owners.items.len);
        }
        self.owners.items[i / 64] |= bit(i);
    }

    inline fn bit(i: u32) u64 {
        return @as(u64, 1) << @intCast(i % 64);
    }
};

/// Yields the comments of a node in order, from `Tree.commentsOf`.
pub const Iterator = struct {
    tree: *const ast.Tree,
    id: u32,

    pub inline fn next(self: *Iterator) ?ast.Comment {
        const id = self.id;
        if (id == none) return null;
        const index = &self.tree.comment_index;
        self.id = index.links.items[id];
        if (id < self.tree.comments.len) return self.tree.comments[id];
        return index.added.items[id - self.tree.comments.len];
    }
};

/// Attaches every comment of `list` to the node it prints with, and records
/// where it sits.
pub fn attach(tree: *ast.Tree, list: []ast.Comment) Error!void {
    std.debug.assert(tree.root != .null);
    if (list.len == 0) return;
    const gpa = tree.allocator();
    var walk: Walk = .{
        .spans = tree.nodes.items(.span),
        .data = tree.nodes.items(.data),
        .extras = tree.extras.items,
        .source = tree.source,
        .list = list,
        .index = &tree.comment_index,
        .gpa = gpa,
    };
    defer walk.children.deinit(gpa);
    defer walk.frames.deinit(gpa);
    try walk.children.ensureTotalCapacity(gpa, 256);
    try walk.frames.ensureTotalCapacity(gpa, 64);

    try walk.run(tree.root);
    while (walk.cursor < list.len) try walk.place(tree.root, .inside, false);
    std.debug.assert(walk.frames.items.len == 0);
}

const Child = struct { node: ast.NodeIndex, start: u32, end: u32 };

const Frame = struct { host: ast.NodeIndex, end: u32, children_start: u32, next_child: u32 };

const Walk = struct {
    spans: []const ast.Span,
    data: []const ast.NodeData,
    extras: []const ast.NodeIndex,
    source: []const u8,
    list: []ast.Comment,
    cursor: u32 = 0,
    index: *Index,
    gpa: std.mem.Allocator,
    children: std.ArrayList(Child) = .empty,
    frames: std.ArrayList(Frame) = .empty,

    fn run(self: *Walk, root: ast.NodeIndex) Error!void {
        try self.enter(root, self.spans[@backingInt(root)].end, .null);
        while (self.frames.items.len > 0) {
            const frame = &self.frames.items[self.frames.items.len - 1];
            const prev: Child = if (frame.next_child > frame.children_start)
                self.children.items[frame.next_child - 1]
            else
                .{ .node = .null, .start = 0, .end = 0 };
            if (frame.next_child < self.children.items.len) {
                const child = self.children.items[frame.next_child];
                frame.next_child += 1;
                try self.between(frame.host, prev.node, prev.end, child.node, child.start);
                // invalidates `frame`
                try self.enter(child.node, child.end, frame.host);
            } else {
                try self.between(frame.host, prev.node, prev.end, .null, frame.end);
                self.children.shrinkRetainingCapacity(frame.children_start);
                _ = self.frames.pop();
            }
        }
    }

    fn enter(self: *Walk, node: ast.NodeIndex, end: u32, parent_host: ast.NodeIndex) Error!void {
        if (self.cursor >= self.list.len) return;
        if (self.list[self.cursor].span.start >= end) return;
        const host = if (self.hosts(node)) node else parent_host;
        std.debug.assert(self.hosts(host));

        const children_start: u32 = @intCast(self.children.items.len);
        try self.collectChildren(node);
        std.sort.insertion(Child, self.children.items[children_start..], {}, startsBefore);
        try self.frames.append(self.gpa, .{
            .host = host,
            .end = end,
            .children_start = children_start,
            .next_child = children_start,
        });
    }

    inline fn hosts(self: *const Walk, node: ast.NodeIndex) bool {
        return self.data[@backingInt(node)] != .formal_parameters;
    }

    fn collectChildren(self: *Walk, node: ast.NodeIndex) Error!void {
        switch (self.data[@backingInt(node)]) {
            // quasis are literal text, never comment hosts
            .template_literal => |t| try self.pushRange(t.expressions),
            .ts_template_literal_type => |t| try self.pushRange(t.types),
            inline else => |payload| {
                const T = @TypeOf(payload);
                if (@typeInfo(T) != .@"struct") return;
                const info = @typeInfo(T).@"struct";
                inline for (info.field_names, info.field_types) |name, Field| {
                    if (Field == ast.NodeIndex) {
                        const child = @field(payload, name);
                        if (child != .null) try self.pushChild(child);
                    } else if (Field == ast.IndexRange) {
                        try self.pushRange(@field(payload, name));
                    }
                }
            },
        }
    }

    fn pushRange(self: *Walk, range: ast.IndexRange) Error!void {
        for (self.extras[range.start..][0..range.len]) |child| {
            if (child != .null) try self.pushChild(child);
        }
    }

    inline fn pushChild(self: *Walk, child: ast.NodeIndex) Error!void {
        const node = switch (self.data[@backingInt(child)]) {
            .formal_parameter => |param| param.pattern,
            else => child,
        };
        const span = self.spans[@backingInt(node)];
        std.debug.assert(std.meta.eql(span, self.spans[@backingInt(child)]));
        try self.children.append(self.gpa, .{ .node = node, .start = span.start, .end = span.end });
    }

    fn between(
        self: *Walk,
        host: ast.NodeIndex,
        prev: ast.NodeIndex,
        prev_end: u32,
        next: ast.NodeIndex,
        next_start: u32,
    ) Error!void {
        const has_prev = prev != .null and self.hosts(prev);
        const has_next = next != .null and self.hosts(next);
        while (self.cursor < self.list.len) {
            const span = self.list[self.cursor].span;
            if (span.start >= next_start) return;
            if (has_prev and has_next) {
                if (self.sameLine(span.end, next_start)) {
                    try self.place(next, .before, true);
                } else if (self.sameLine(prev_end, span.start)) {
                    try self.place(prev, .after, true);
                } else {
                    try self.place(next, .before, false);
                }
            } else if (has_next) {
                try self.place(next, .before, self.sameLine(span.end, next_start));
            } else if (has_prev) {
                try self.place(prev, .after, self.sameLine(prev_end, span.start));
            } else {
                try self.place(host, .inside, false);
            }
        }
    }

    fn place(
        self: *Walk,
        node: ast.NodeIndex,
        position: ast.Comment.Position,
        same_line: bool,
    ) Error!void {
        const comment = &self.list[self.cursor];
        comment.position = position;
        comment.same_line = same_line;
        try self.index.link(self.gpa, node, self.cursor);
        self.cursor += 1;
    }

    fn sameLine(self: *const Walk, a: u32, b: u32) bool {
        const end = @max(a, b);
        var at: usize = @min(a, b);
        while (std.mem.findAnyPos(u8, self.source[0..end], at, "\n\r\xe2")) |found| {
            if (lineBreakLen(self.source, found) > 0) return false;
            at = found + 1;
        }
        return true;
    }

    fn startsBefore(_: void, a: Child, b: Child) bool {
        return a.start < b.start;
    }
};
