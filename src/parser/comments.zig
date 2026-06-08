// post-parse pass that assigns each lexer-collected raw comment to a
// host node and a position. one forward sweep through the comments
// array driven by a dfs of the tree in source order. at each gap we
// claim every comment whose start lies in that gap, with this rule:
//
//   between siblings a and b, block comment ending same-line as b.start:
//     `before b`, sameLine = true (covers `/*#__PURE__*/ foo()`)
//   between siblings, comment starting same-line as a.end:
//     `after a`, sameLine = true
//   between siblings, gap with newlines on both sides:
//     `before b`, sameLine = false
//   leading a node:           `before node`
//   trailing a node:          `after node`
//   inside a childless host:  `inside host`
//
// after assignment we counting-sort by host into a flat prefix-sum
// offsets array, producing the public `Comment` shape.

const std = @import("std");
const ast = @import("ast.zig");

const Error = error{OutOfMemory};

const ChildInfo = struct {
    idx: ast.NodeIndex,
    start: u32,
    end: u32,
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
    };
    defer ctx.scratch.deinit(alloc);
    try ctx.scratch.ensureTotalCapacity(alloc, 256);

    try ctx.walkAt(tree.root, ctx.spans[@intFromEnum(tree.root)]);

    // anything left over attaches as inside-root
    while (ctx.cursor < raw.len) : (ctx.cursor += 1) {
        ctx.write(@intFromEnum(tree.root), .inside, false);
    }

    var counts = try alloc.alloc(u32, node_count);
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

    fn walkAt(self: *Ctx, node: ast.NodeIndex, node_span: ast.Span) Error!void {
        if (self.cursor >= self.raw.len) return;
        if (self.raw[self.cursor].span.start >= node_span.end) return;

        const checkpoint = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(checkpoint);

        try self.collectChildren(node);
        const children = self.scratch.items[checkpoint..];
        sortByStart(children);

        var prev_idx: ast.NodeIndex = .null;
        var prev_end: u32 = 0;
        for (children) |child| {
            try self.consumeBetween(node, prev_idx, prev_end, child.idx, child.start);
            try self.walkAt(child.idx, .{ .start = child.start, .end = child.end });
            prev_idx = child.idx;
            prev_end = child.end;
        }

        try self.consumeBetween(node, prev_idx, prev_end, .null, node_span.end);
    }

    fn collectChildren(self: *Ctx, node: ast.NodeIndex) Error!void {
        switch (self.data_items[@intFromEnum(node)]) {
            // quasis are literal text, never comment hosts. a comment inside a
            // `${ }` belongs to the interpolated expression or type.
            .template_literal => |t| try self.pushRange(t.expressions),
            .ts_template_literal_type => |t| try self.pushRange(t.types),
            inline else => |payload| {
                const T = @TypeOf(payload);
                if (@typeInfo(T) != .@"struct") return;
                inline for (std.meta.fields(T)) |f| {
                    if (f.type == ast.NodeIndex) {
                        const child = @field(payload, f.name);
                        if (child != .null) try self.pushChild(child);
                    } else if (f.type == ast.IndexRange) {
                        try self.pushRange(@field(payload, f.name));
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

    inline fn pushChild(self: *Ctx, child: ast.NodeIndex) Error!void {
        const s = self.spans[@intFromEnum(child)];
        try self.scratch.append(self.alloc, .{ .idx = child, .start = s.start, .end = s.end });
    }

    fn consumeBetween(
        self: *Ctx,
        host_node: ast.NodeIndex,
        prev_idx: ast.NodeIndex,
        prev_end: u32,
        next_idx: ast.NodeIndex,
        next_start: u32,
    ) Error!void {
        while (self.cursor < self.raw.len) {
            const c = &self.raw[self.cursor];
            if (c.span.start >= next_start) return;

            const has_prev = prev_idx != .null;
            const has_next = next_idx != .null;

            if (has_prev and has_next) {
                if (self.sameLine(c.span.end, next_start)) {
                    self.write(@intFromEnum(next_idx), .before, true);
                } else if (self.sameLine(prev_end, c.span.start)) {
                    self.write(@intFromEnum(prev_idx), .after, true);
                } else {
                    self.write(@intFromEnum(next_idx), .before, false);
                }
            } else if (has_next) {
                self.write(@intFromEnum(next_idx), .before, self.sameLine(c.span.end, next_start));
            } else if (has_prev) {
                self.write(@intFromEnum(prev_idx), .after, self.sameLine(prev_end, c.span.start));
            } else {
                self.write(@intFromEnum(host_node), .inside, false);
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

    // a and b are always close, so memchr beats a `line_starts` binary
    // search.
    inline fn sameLine(self: *const Ctx, a: u32, b: u32) bool {
        const lo = if (a < b) a else b;
        const hi = if (a < b) b else a;
        return std.mem.findScalar(u8, self.source[lo..hi], '\n') == null;
    }
};

// children are usually <16 and almost always already in source order.
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
