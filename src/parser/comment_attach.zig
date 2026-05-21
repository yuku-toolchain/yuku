// Post-parse pass that assigns each lexer-collected comment to a host
// node and a position (before, after, inside). Only runs when
// `Options.attach_comments` is true.
//
// algorithm: one forward sweep through the comments array driven by a
// dfs of the tree in source order. at each gap (before first child,
// between two siblings, after last child, or inside a childless node)
// we claim every comment whose start lies in that gap.
//
//   between siblings a and b: `after a` when same-line as a.end,
//                             else `before b`
//   leading a node:           `before node`
//   trailing a node:          `after node`
//   inside a childless host:  `inside host`
//
// after assignment we counting-sort the comments by host into a flat
// prefix-sum offsets array.

const std = @import("std");
const ast = @import("ast.zig");

const Error = error{OutOfMemory};

const ChildInfo = struct {
    idx: ast.NodeIndex,
    start: u32,
    end: u32,
};

pub fn attach(tree: *ast.Tree, raw: []ast.Comment) Error!void {
    const alloc = tree.allocator();
    const node_count = tree.nodes.len;
    const offsets = try alloc.alloc(u32, node_count + 1);

    if (raw.len == 0) {
        @memset(offsets, 0);
        tree.node_comment_offsets = offsets;
        tree.comments = &.{};
        return;
    }

    const host = try alloc.alloc(u32, raw.len);
    defer alloc.free(host);

    var ctx: Ctx = .{
        .spans = tree.nodes.items(.span),
        .data_items = tree.nodes.items(.data),
        .extras = tree.extras.items,
        .source = tree.source,
        .comments = raw,
        .host = host,
        .cursor = 0,
        .alloc = alloc,
        .scratch = .empty,
    };
    defer ctx.scratch.deinit(alloc);
    try ctx.scratch.ensureTotalCapacity(alloc, 256);

    try ctx.walkAt(tree.root, ctx.spans[@intFromEnum(tree.root)]);

    // any leftover comment past the root's last child attaches as inside-root
    while (ctx.cursor < raw.len) : (ctx.cursor += 1) {
        host[ctx.cursor] = @intFromEnum(tree.root);
        raw[ctx.cursor].position = .inside;
        raw[ctx.cursor].same_line = false;
    }

    // counting-sort by host into `out`, with `offsets` as the prefix sum
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

    const out = try alloc.alloc(ast.Comment, raw.len);
    for (raw, 0..) |c, i| {
        const h = host[i];
        out[offsets[h] + counts[h]] = c;
        counts[h] += 1;
    }

    tree.comments = out;
    tree.node_comment_offsets = offsets;
}

const Ctx = struct {
    spans: []const ast.Span,
    data_items: []const ast.NodeData,
    extras: []const ast.NodeIndex,
    source: []const u8,
    comments: []ast.Comment,
    host: []u32,
    cursor: usize,
    alloc: std.mem.Allocator,
    scratch: std.ArrayList(ChildInfo),

    fn walkAt(self: *Ctx, node: ast.NodeIndex, node_span: ast.Span) Error!void {
        if (self.cursor >= self.comments.len) return;
        if (self.comments[self.cursor].span.start >= node_span.end) return;

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
            inline else => |payload| {
                const T = @TypeOf(payload);
                if (@typeInfo(T) != .@"struct") return;
                inline for (std.meta.fields(T)) |f| {
                    if (f.type == ast.NodeIndex) {
                        const child = @field(payload, f.name);
                        if (child != .null) try self.pushChild(child);
                    } else if (f.type == ast.IndexRange) {
                        const range = @field(payload, f.name);
                        for (self.extras[range.start..][0..range.len]) |child| {
                            if (child != .null) try self.pushChild(child);
                        }
                    }
                }
            },
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
        while (self.cursor < self.comments.len) {
            const c = &self.comments[self.cursor];
            if (c.span.start >= next_start) return;

            const has_prev = prev_idx != .null;
            const has_next = next_idx != .null;

            if (has_prev and has_next) {
                if (self.sameLine(prev_end, c.span.start)) {
                    self.assign(c, prev_idx, .after, true);
                } else {
                    self.assign(c, next_idx, .before, self.sameLine(c.span.end, next_start));
                }
            } else if (has_next) {
                self.assign(c, next_idx, .before, self.sameLine(c.span.end, next_start));
            } else if (has_prev) {
                self.assign(c, prev_idx, .after, self.sameLine(prev_end, c.span.start));
            } else {
                self.assign(c, host_node, .inside, false);
            }
            self.cursor += 1;
        }
    }

    inline fn assign(
        self: *Ctx,
        c: *ast.Comment,
        host_node: ast.NodeIndex,
        position: ast.Comment.Position,
        same_line: bool,
    ) void {
        self.host[self.cursor] = @intFromEnum(host_node);
        c.position = position;
        c.same_line = same_line;
    }

    // a and b are immediate-neighbor node edges across a comment and thus
    // always close, so a memchr over source beats a line_starts binary search
    inline fn sameLine(self: *const Ctx, a: u32, b: u32) bool {
        const lo = if (a < b) a else b;
        const hi = if (a < b) b else a;
        return std.mem.indexOfScalar(u8, self.source[lo..hi], '\n') == null;
    }
};

// children are usually <16 and almost always already in source order
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
