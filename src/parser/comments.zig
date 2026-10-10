// one forward sweep over the comments driven by a source-order dfs, then a
// counting sort by host into a prefix-sum offsets array

const std = @import("std");
const ast = @import("ast.zig");

const Error = error{OutOfMemory};

const ChildInfo = struct {
    idx: ast.NodeIndex,
    start: u32,
    end: u32,
};

const Frame = struct {
    host: ast.NodeIndex,
    node_end: u32,
    children_start: u32,
    next_child: u32,
};

pub fn attach(tree: *ast.Tree, raw: []const ast.Comment) Error!void {
    std.debug.assert(tree.root != .null);
    const alloc = tree.allocator();
    const node_count = tree.nodes.len;
    std.debug.assert(node_count > 0);
    const offsets = try alloc.alloc(u32, node_count + 1);

    if (raw.len == 0) {
        @memset(offsets, 0);
        tree.attached_comment_offsets = offsets;
        tree.attached_comments = &.{};
        return;
    }

    const host = try alloc.alloc(u32, raw.len);
    defer alloc.free(host);
    const unsorted = try alloc.alloc(ast.AttachedComment, raw.len);
    defer alloc.free(unsorted);

    var ctx: Ctx = .{
        .spans = tree.nodes.items(.span),
        .data_items = tree.nodes.items(.data),
        .extras = tree.extras.items,
        .source = tree.source,
        .raw = raw,
        .out = unsorted,
        .host = host,
        .cursor = 0,
        .alloc = alloc,
        .scratch = .empty,
        .frames = .empty,
    };
    defer ctx.scratch.deinit(alloc);
    defer ctx.frames.deinit(alloc);
    try ctx.scratch.ensureTotalCapacity(alloc, 256);
    try ctx.frames.ensureTotalCapacity(alloc, 64);

    try ctx.walk(tree.root);

    while (ctx.cursor < raw.len) : (ctx.cursor += 1) {
        ctx.write(@backingInt(tree.root), .inside, false);
    }

    const counts = try alloc.alloc(u32, node_count);
    defer alloc.free(counts);
    @memset(counts, 0);
    for (host) |h| counts[h] += 1;

    var sum: u32 = 0;
    for (counts, 0..) |n, i| {
        offsets[i] = sum;
        sum += n;
        counts[i] = 0;
    }
    offsets[node_count] = sum;

    const final = try alloc.alloc(ast.AttachedComment, raw.len);
    for (unsorted, 0..) |c, i| {
        const h = host[i];
        final[offsets[h] + counts[h]] = c;
        counts[h] += 1;
    }

    tree.attached_comments = final;
    tree.attached_comment_offsets = offsets;
}

const Ctx = struct {
    spans: []const ast.Span,
    data_items: []const ast.NodeData,
    extras: []const ast.NodeIndex,
    source: []const u8,
    raw: []const ast.Comment,
    out: []ast.AttachedComment,
    host: []u32,
    cursor: usize,
    alloc: std.mem.Allocator,
    scratch: std.ArrayList(ChildInfo),
    frames: std.ArrayList(Frame),

    fn walk(self: *Ctx, root: ast.NodeIndex) Error!void {
        std.debug.assert(self.frames.items.len == 0);
        try self.walkEnter(root, self.spans[@backingInt(root)].end, .null);

        while (self.frames.items.len > 0) {
            const frame = &self.frames.items[self.frames.items.len - 1];
            const prev: ChildInfo = if (frame.next_child > frame.children_start)
                self.scratch.items[frame.next_child - 1]
            else
                .{ .idx = .null, .start = 0, .end = 0 };
            if (frame.next_child < self.scratch.items.len) {
                const child = self.scratch.items[frame.next_child];
                frame.next_child += 1;
                try self.consumeBetween(frame.host, prev.idx, prev.end, child.idx, child.start);
                // invalidates `frame`
                try self.walkEnter(child.idx, child.end, frame.host);
            } else {
                try self.consumeBetween(frame.host, prev.idx, prev.end, .null, frame.node_end);
                self.scratch.shrinkRetainingCapacity(frame.children_start);
                _ = self.frames.pop();
            }
        }
    }

    fn walkEnter(
        self: *Ctx,
        node: ast.NodeIndex,
        node_end: u32,
        parent_host: ast.NodeIndex,
    ) Error!void {
        if (self.cursor >= self.raw.len) return;
        if (self.raw[self.cursor].span.start >= node_end) return;
        const inside_host = if (self.hosts(node)) node else parent_host;
        std.debug.assert(self.hosts(inside_host));

        const children_start: u32 = @intCast(self.scratch.items.len);
        try self.collectChildren(node);
        sortByStart(self.scratch.items[children_start..]);
        try self.frames.append(self.alloc, .{
            .host = inside_host,
            .node_end = node_end,
            .children_start = children_start,
            .next_child = children_start,
        });
    }

    // a parameter list has no ESTree node, so it bounds the comments inside its parens but
    // never hosts one
    inline fn hosts(self: *const Ctx, node: ast.NodeIndex) bool {
        return self.data_items[@backingInt(node)] != .formal_parameters;
    }

    fn collectChildren(self: *Ctx, node: ast.NodeIndex) Error!void {
        switch (self.data_items[@backingInt(node)]) {
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

    fn pushRange(self: *Ctx, range: ast.IndexRange) Error!void {
        for (self.extras[range.start..][0..range.len]) |child| {
            if (child != .null) try self.pushChild(child);
        }
    }

    // a parameter is its pattern in ESTree, with the same span
    inline fn pushChild(self: *Ctx, child: ast.NodeIndex) Error!void {
        const node = switch (self.data_items[@backingInt(child)]) {
            .formal_parameter => |param| param.pattern,
            else => child,
        };
        const s = self.spans[@backingInt(node)];
        std.debug.assert(std.meta.eql(s, self.spans[@backingInt(child)]));
        try self.scratch.append(self.alloc, .{ .idx = node, .start = s.start, .end = s.end });
    }

    fn consumeBetween(
        self: *Ctx,
        host_node: ast.NodeIndex,
        prev_idx: ast.NodeIndex,
        prev_end: u32,
        next_idx: ast.NodeIndex,
        next_start: u32,
    ) Error!void {
        const has_prev = prev_idx != .null and self.hosts(prev_idx);
        const has_next = next_idx != .null and self.hosts(next_idx);
        while (self.cursor < self.raw.len) {
            const c = &self.raw[self.cursor];
            if (c.span.start >= next_start) return;

            if (has_prev and has_next) {
                if (self.sameLine(c.span.end, next_start)) {
                    self.write(@backingInt(next_idx), .before, true);
                } else if (self.sameLine(prev_end, c.span.start)) {
                    self.write(@backingInt(prev_idx), .after, true);
                } else {
                    self.write(@backingInt(next_idx), .before, false);
                }
            } else if (has_next) {
                self.write(@backingInt(next_idx), .before, self.sameLine(c.span.end, next_start));
            } else if (has_prev) {
                self.write(@backingInt(prev_idx), .after, self.sameLine(prev_end, c.span.start));
            } else {
                self.write(@backingInt(host_node), .inside, false);
            }
            self.cursor += 1;
        }
    }

    inline fn write(
        self: *Ctx,
        host_idx: u32,
        position: ast.AttachedComment.Position,
        same_line: bool,
    ) void {
        const r = self.raw[self.cursor];
        self.host[self.cursor] = host_idx;
        self.out[self.cursor] = .{
            .type = r.type,
            .position = position,
            .same_line = same_line,
            .value = r.value,
        };
    }

    // a and b are always close, so a direct newline scan is cheap
    inline fn sameLine(self: *const Ctx, a: u32, b: u32) bool {
        const lo = if (a < b) a else b;
        const hi = if (a < b) b else a;
        return std.mem.findScalar(u8, self.source[lo..hi], '\n') == null;
    }
};

// children are few and almost always already in source order
fn sortByStart(children: []ChildInfo) void {
    var i: usize = 1;
    while (i < children.len) : (i += 1) {
        const key = children[i];
        var j: usize = i;
        while (j > 0 and children[j - 1].start > key.start) : (j -= 1) {
            children[j] = children[j - 1];
        }
        children[j] = key;
    }
}
